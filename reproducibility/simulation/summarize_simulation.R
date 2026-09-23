#!/usr/bin/env Rscript

# Summarize replicate-level simulation results using the analytic variance
# target corresponding to each replicate.

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Missing required package: data.table.")
}

get_script_dir <- function() {
  cmd <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd, value = TRUE)
  if (length(file_arg) == 0L) {
    return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  }
  normalizePath(dirname(sub("^--file=", "", file_arg[[1L]])), winslash = "/", mustWork = TRUE)
}

script_dir <- get_script_dir()
project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
output_root <- file.path(project_root, "outputs")

run_dirs <- list.dirs(output_root, recursive = FALSE, full.names = TRUE)
run_dirs <- run_dirs[
  file.exists(file.path(run_dirs, "replicate_results.csv")) &
    file.exists(file.path(run_dirs, "qe_diagnostics.csv"))
]

if (length(run_dirs) == 0L) {
  stop("No completed simulation outputs were found in: ", output_root)
}

summarize_method <- function(data, diagnostics, method) {
  if (method == "DiVE") {
    estimate <- data$dive_estimate
    variance_estimate <- data$dive_variance
    variance_target <- data$true_variance_dive
    fail_rate <- 0
  } else if (method == "QE-RE") {
    estimate <- data$qe_re_estimate
    variance_estimate <- data$qe_re_variance
    variance_target <- data$true_variance_qe_re
    fail_rate <- mean(diagnostics$qe_failed_studies / data$num_studies)
  } else if (method == "QE-FE") {
    estimate <- data$qe_fe_estimate
    variance_estimate <- data$qe_fe_variance
    variance_target <- data$true_variance_qe_fe
    fail_rate <- mean(diagnostics$qe_failed_studies / data$num_studies)
  } else {
    stop("Unknown method: ", method)
  }

  true_effect <- data$true_effect
  if (any(!is.finite(true_effect)) || any(abs(true_effect) < .Machine$double.eps)) {
    stop("The point-estimate relative metrics require a non-zero finite true effect.")
  }
  if (any(!is.finite(variance_target)) || any(variance_target <= 0)) {
    stop("The variance target must be finite and positive for every replicate.")
  }
  if (any(!is.finite(variance_estimate)) || any(variance_estimate < 0)) {
    stop("A variance estimate is non-finite or negative.")
  }

  point_relative_error <- (estimate - true_effect) / true_effect
  variance_relative_error <- (variance_estimate - variance_target) / variance_target

  # Legacy v6.10 used 1.96 for the stored DiVE z-interval limits, while
  # metafor used the standard-normal quantile for QE. Its reported average
  # z-interval widths used qnorm(0.975) for all three methods. Preserve that
  # behavior here so the cleanup does not alter the interval specification.
  z_critical_coverage <- if (method == "DiVE") 1.96 else stats::qnorm(0.975)
  z_critical_width <- stats::qnorm(0.975)
  t_critical <- stats::qt(0.975, df = data$num_studies[[1L]] - 1L)
  standard_error <- sqrt(variance_estimate)

  z_lower <- estimate - z_critical_coverage * standard_error
  z_upper <- estimate + z_critical_coverage * standard_error
  t_lower <- estimate - t_critical * standard_error
  t_upper <- estimate + t_critical * standard_error

  data.frame(
    method = method,
    point_bias_pct = 100 * mean(point_relative_error),
    point_mse_pct = 100 * mean(point_relative_error^2),
    variance_bias_pct = 100 * mean(variance_relative_error),
    variance_mse_pct = 100 * mean(variance_relative_error^2),
    coverage_z = mean(z_lower <= true_effect & z_upper >= true_effect),
    average_width_z = mean(2 * z_critical_width * standard_error),
    coverage_t = mean(t_lower <= true_effect & t_upper >= true_effect),
    average_width_t = mean(2 * t_critical * standard_error),
    fail_rate = fail_rate,
    stringsAsFactors = FALSE
  )
}

summarize_run <- function(run_dir) {
  data <- data.table::fread(file.path(run_dir, "replicate_results.csv"))
  diagnostics <- data.table::fread(file.path(run_dir, "qe_diagnostics.csv"))

  id_columns <- c(
    "distribution",
    "sample_size_mode",
    "num_studies",
    "average_group_n",
    "heterogeneity_ratio"
  )

  expected_rows_per_setting <- 1000L
  grouping_key <- do.call(
    interaction,
    c(as.list(data[, ..id_columns]), list(drop = TRUE, lex.order = TRUE))
  )
  diagnostic_key <- do.call(
    interaction,
    c(as.list(diagnostics[, ..id_columns]), list(drop = TRUE, lex.order = TRUE))
  )

  if (!identical(levels(grouping_key), levels(diagnostic_key))) {
    stop("Replicate and diagnostic files do not contain identical settings in: ", run_dir)
  }

  split_data <- split(data, grouping_key)
  split_diagnostics <- split(diagnostics, diagnostic_key)
  output <- vector("list", length(split_data) * 3L)
  output_index <- 0L

  for (setting_name in names(split_data)) {
    setting_data <- split_data[[setting_name]]
    setting_diagnostics <- split_diagnostics[[setting_name]]

    if (nrow(setting_data) != expected_rows_per_setting) {
      stop(
        "Expected ", expected_rows_per_setting,
        " replicates but found ", nrow(setting_data),
        " for setting ", setting_name, "."
      )
    }
    if (nrow(setting_diagnostics) != nrow(setting_data)) {
      stop("Diagnostic row count does not match replicate row count for: ", setting_name)
    }

    setting_data <- setting_data[order(setting_data$replicate), ]
    setting_diagnostics <- setting_diagnostics[order(setting_diagnostics$replicate), ]
    if (!identical(setting_data$replicate, setting_diagnostics$replicate)) {
      stop("Replicate IDs do not align for setting: ", setting_name)
    }

    identifiers <- data.frame(
      distribution = setting_data$distribution[[1L]],
      sample_size_mode = setting_data$sample_size_mode[[1L]],
      num_studies = setting_data$num_studies[[1L]],
      average_group_n = setting_data$average_group_n[[1L]],
      heterogeneity_ratio = setting_data$heterogeneity_ratio[[1L]],
      true_i2 = setting_data$true_i2[[1L]],
      stringsAsFactors = FALSE
    )

    for (method in c("DiVE", "QE-RE", "QE-FE")) {
      output_index <- output_index + 1L
      output[[output_index]] <- cbind(
        identifiers,
        summarize_method(setting_data, setting_diagnostics, method)
      )
    }
  }

  result <- data.table::rbindlist(output)
  result[, method_order := match(method, c("DiVE", "QE-RE", "QE-FE"))]
  data.table::setorder(
    result,
    num_studies,
    average_group_n,
    heterogeneity_ratio,
    method_order
  )
  result[, method_order := NULL]
  data.table::fwrite(result, file.path(run_dir, "summary_results.csv"))
  result
}

all_summaries <- lapply(run_dirs, summarize_run)
all_summaries <- data.table::rbindlist(all_summaries)
all_summaries[, method_order := match(method, c("DiVE", "QE-RE", "QE-FE"))]
data.table::setorder(
  all_summaries,
  distribution,
  sample_size_mode,
  num_studies,
  average_group_n,
  heterogeneity_ratio,
  method_order
)
all_summaries[, method_order := NULL]
data.table::fwrite(all_summaries, file.path(output_root, "summary_results_all.csv"))

message("Summary completed: ", output_root)
