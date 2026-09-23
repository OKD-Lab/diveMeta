#!/usr/bin/env Rscript

# Reproduce the manuscript simulation for one outcome distribution and one
# sample-size pattern. Only settings and methods reported in the manuscript
# are implemented here.

# Final manuscript environment used metamedian 1.2.2, estmeansd 1.0.1,
# and metafor 5.2.1. The QE implementation below uses the maintained
# metamedian package directly; estmeansd is its QE fitting backend.
required_packages <- c(
  "data.table", "gtools", "metafor", "pwr", "sn", "metamedian", "estmeansd"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]
if (length(missing_packages) > 0L) {
  stop("Missing required package(s): ", paste(missing_packages, collapse = ", "), ".")
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: Rscript run_simulation.R <normal|skew_normal|log_normal> <fixed|varying>")
}

distribution <- args[[1L]]
sample_size_mode <- args[[2L]]

if (!distribution %in% c("normal", "skew_normal", "log_normal")) {
  stop("Unknown distribution: ", distribution)
}
if (!sample_size_mode %in% c("fixed", "varying")) {
  stop("Unknown sample-size mode: ", sample_size_mode)
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
output_dir <- file.path(project_root, "outputs", paste0(distribution, "_", sample_size_mode))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Frozen manuscript settings.
set.seed(123)
num_replicates <- 1000L
num_studies_values <- c(10L, 30L)
average_group_n_values <- c(100L, 300L)
heterogeneity_ratio_values <- c(0, 0.3333, 1, 3)
minimum_group_n <- 50L
maximum_study_share <- 0.50
candidate_families <- c("normal", "weibull", "lognormal", "gamma")

# Outcome-distribution parameters.
normal_mean <- 5
normal_sd <- 1

skew_group1 <- list(location = 5, scale = 5, shape = 5)
skew_group2 <- list(location = 5, scale = 10, shape = 10)
skew_median1 <- sn::qsn(0.5, xi = skew_group1$location, omega = skew_group1$scale, alpha = skew_group1$shape)
skew_median2 <- sn::qsn(0.5, xi = skew_group2$location, omega = skew_group2$scale, alpha = skew_group2$shape)

lognormal_group1 <- list(meanlog = 2, sdlog = 1)
lognormal_group2 <- list(meanlog = 3, sdlog = 2)
lognormal_median1 <- exp(lognormal_group1$meanlog)
lognormal_median2 <- exp(lognormal_group2$meanlog)

# Retain the legacy v6.10 normal-shift calibration exactly. pwr.norm.test()
# is a known-variance normal-mean power calculation; 0.60 is used here only
# to reproduce that original calibration, not as the power of a two-sample
# median test.
legacy_normal_shift_calibration <- 0.60

generate_varying_group_sizes <- function(total_group_n, num_studies,
                                         min_group_n = minimum_group_n,
                                         alpha = 1,
                                         max_share = maximum_study_share,
                                         max_attempts = 1000L) {
  stopifnot(num_studies >= 2L, max_share > 0, max_share < 1)
  remaining_n <- total_group_n - num_studies * min_group_n
  stopifnot(remaining_n >= 0)

  for (attempt in seq_len(max_attempts)) {
    allocation_probabilities <- as.numeric(gtools::rdirichlet(1L, rep(alpha, num_studies)))
    group_n <- as.vector(stats::rmultinom(
      n = 1L,
      size = remaining_n,
      prob = allocation_probabilities
    )) + min_group_n

    # Preserves the DiVE no-dominance condition under equal group sizes and
    # sample-size weights.
    if (all(group_n < total_group_n * max_share)) {
      return(group_n)
    }
  }

  stop(sprintf(
    "Unable to allocate study sizes below %.0f%% of the group total after %d attempts.",
    100 * max_share,
    max_attempts
  ))
}

get_population_densities <- function(distribution) {
  if (distribution == "normal") {
    return(c(
      group1 = stats::dnorm(normal_mean, normal_mean, normal_sd),
      group2 = stats::dnorm(normal_mean, normal_mean, normal_sd)
    ))
  }
  if (distribution == "skew_normal") {
    return(c(
      group1 = sn::dsn(skew_median1, xi = skew_group1$location, omega = skew_group1$scale, alpha = skew_group1$shape),
      group2 = sn::dsn(skew_median2, xi = skew_group2$location, omega = skew_group2$scale, alpha = skew_group2$shape)
    ))
  }
  if (distribution == "log_normal") {
    return(c(
      group1 = stats::dlnorm(lognormal_median1, meanlog = lognormal_group1$meanlog, sdlog = lognormal_group1$sdlog),
      group2 = stats::dlnorm(lognormal_median2, meanlog = lognormal_group2$meanlog, sdlog = lognormal_group2$sdlog)
    ))
  }
  stop("Unsupported distribution: ", distribution)
}

get_normal_shift <- function(average_group_n) {
  standardized_shift <- pwr::pwr.norm.test(
    n = average_group_n,
    sig.level = 0.05,
    power = legacy_normal_shift_calibration,
    alternative = "two.sided"
  )$d
  pooled_sd <- sqrt((normal_sd^2 + normal_sd^2) / 2)
  standardized_shift * pooled_sd
}

get_true_effect <- function(distribution, normal_shift) {
  if (distribution == "normal") return(normal_shift)
  if (distribution == "skew_normal") return(skew_median1 - skew_median2)
  if (distribution == "log_normal") return(lognormal_median1 - lognormal_median2)
  stop("Unsupported distribution: ", distribution)
}

generate_group_outcomes <- function(distribution, n_group1, n_group2) {
  if (distribution == "normal") {
    # Publication implementation follows the stated normal DGM directly.
    return(list(
      group1 = stats::rnorm(n_group1, mean = normal_mean, sd = normal_sd),
      group2 = stats::rnorm(n_group2, mean = normal_mean, sd = normal_sd)
    ))
  }
  if (distribution == "skew_normal") {
    return(list(
      group1 = sn::rsn(n_group1, xi = skew_group1$location, omega = skew_group1$scale, alpha = skew_group1$shape),
      group2 = sn::rsn(n_group2, xi = skew_group2$location, omega = skew_group2$scale, alpha = skew_group2$shape)
    ))
  }
  if (distribution == "log_normal") {
    return(list(
      group1 = stats::rlnorm(n_group1, meanlog = lognormal_group1$meanlog, sdlog = lognormal_group1$sdlog),
      group2 = stats::rlnorm(n_group2, meanlog = lognormal_group2$meanlog, sdlog = lognormal_group2$sdlog)
    ))
  }
  stop("Unsupported distribution: ", distribution)
}

# QE implementation for S2 (first quartile, median, third quartile).
# The final manuscript analysis uses the maintained metamedian implementation
# directly. single.family = FALSE permits group-specific family selection and
# loc.shift = FALSE does not impose a location-shift-only assumption.
#
# Package calls are isolated from the simulation RNG stream. This guarantees
# that the generated datasets remain identical to the frozen simulation even
# if a package implementation were ever to consume random numbers internally.
with_preserved_rng <- function(fun) {
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    stop(".Random.seed does not exist before the QE package call.")
  }
  seed_before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit(
    assign(".Random.seed", seed_before, envir = .GlobalEnv),
    add = TRUE
  )
  fun()
}

normalize_qe_family <- function(x) {
  x <- as.character(x)
  x[x == "log-normal"] <- "lognormal"
  x
}

run_qe_package <- function(q1_g1, median_g1, q3_g1, n_g1,
                           q1_g2, median_g2, q3_g2, n_g2,
                           method) {
  with_preserved_rng(function() {
    suppressMessages(
      metamedian::qe(
        q1.g1 = q1_g1,
        med.g1 = median_g1,
        q3.g1 = q3_g1,
        n.g1 = n_g1,
        q1.g2 = q1_g2,
        med.g2 = median_g2,
        q3.g2 = q3_g2,
        n.g2 = n_g2,
        single.family = FALSE,
        loc.shift = FALSE,
        pool_studies = TRUE,
        method = method
      )
    )
  })
}

# The one-group package call is used only to retain the existing public
# qe_diagnostics.csv columns (group-specific variance and selected family).
# With single.family = FALSE and loc.shift = FALSE, these are the same
# group-specific QE fits used by the two-group package analysis.
run_qe_group_diagnostic <- function(q1, median, q3, n) {
  with_preserved_rng(function() {
    fit <- suppressMessages(
      metamedian::qe.study.level(
        q1.g1 = q1,
        med.g1 = median,
        q3.g1 = q3,
        n.g1 = n
      )
    )

    list(
      variance = as.numeric(fit$var),
      selected_family = normalize_qe_family(fit$selected.dist)
    )
  })
}

run_one_replicate <- function(replicate_id,
                              distribution,
                              sample_size_mode,
                              num_studies,
                              average_group_n,
                              heterogeneity_ratio) {
  total_group_n <- num_studies * average_group_n

  if (sample_size_mode == "fixed") {
    n_group1 <- rep(average_group_n, num_studies)
  } else {
    n_group1 <- generate_varying_group_sizes(total_group_n, num_studies)
  }
  # The manuscript simulation uses 1:1 allocation within every study.
  n_group2 <- n_group1

  densities <- get_population_densities(distribution)
  true_within_var_g1 <- 1 / (4 * n_group1 * densities[["group1"]]^2)
  true_within_var_g2 <- 1 / (4 * n_group2 * densities[["group2"]]^2)
  true_within_var <- true_within_var_g1 + true_within_var_g2

  inverse_within_var <- 1 / true_within_var
  s2_typical <- (num_studies - 1) * sum(inverse_within_var) /
    (sum(inverse_within_var)^2 - sum(inverse_within_var^2))
  true_tau2 <- heterogeneity_ratio * s2_typical
  true_i2 <- if (true_tau2 == 0) 0 else true_tau2 / (true_tau2 + s2_typical)

  normal_shift <- if (distribution == "normal") get_normal_shift(average_group_n) else 0
  true_effect <- get_true_effect(distribution, normal_shift)

  medians_g1 <- numeric(num_studies)
  medians_g2 <- numeric(num_studies)
  q1_g1 <- numeric(num_studies)
  q1_g2 <- numeric(num_studies)
  q3_g1 <- numeric(num_studies)
  q3_g2 <- numeric(num_studies)

  for (study in seq_len(num_studies)) {
    outcomes <- generate_group_outcomes(
      distribution,
      n_group1[[study]],
      n_group2[[study]]
    )

    random_effect <- stats::rnorm(1L, mean = 0, sd = sqrt(true_tau2))
    outcomes$group1 <- outcomes$group1 + normal_shift + random_effect

    quantiles_g1 <- stats::quantile(
      outcomes$group1,
      probs = c(0.25, 0.50, 0.75),
      names = FALSE
    )
    quantiles_g2 <- stats::quantile(
      outcomes$group2,
      probs = c(0.25, 0.50, 0.75),
      names = FALSE
    )

    q1_g1[[study]] <- quantiles_g1[[1L]]
    medians_g1[[study]] <- quantiles_g1[[2L]]
    q3_g1[[study]] <- quantiles_g1[[3L]]
    q1_g2[[study]] <- quantiles_g2[[1L]]
    medians_g2[[study]] <- quantiles_g2[[2L]]
    q3_g2[[study]] <- quantiles_g2[[3L]]
  }

  study_effects <- medians_g1 - medians_g2

  # DiVE.
  dive_raw_weights <- n_group1 + n_group2
  dive_weights <- dive_raw_weights / sum(dive_raw_weights)
  true_variance_dive <- sum(dive_weights^2 * (true_within_var + true_tau2))
  dive_estimate <- sum(dive_weights * study_effects)
  h <- dive_weights^2 / (1 - 2 * dive_weights)
  dive_variance <- sum(h * (study_effects - dive_estimate)^2) / (1 + sum(h))

  # QE meta-analysis: package-direct McGrath QE implementation.
  qe_fe_fit <- run_qe_package(
    q1_g1, medians_g1, q3_g1, n_group1,
    q1_g2, medians_g2, q3_g2, n_group2,
    method = "FE"
  )
  qe_re_fit <- run_qe_package(
    q1_g1, medians_g1, q3_g1, n_group1,
    q1_g2, medians_g2, q3_g2, n_group2,
    method = "DL"
  )

  # Retain the existing diagnostic output structure using package-direct
  # one-group QE fits. There is no custom fallback in the final analysis: a
  # package error stops the run rather than silently substituting a variance.
  qe_within_var_g1 <- numeric(num_studies)
  qe_within_var_g2 <- numeric(num_studies)
  selected_g1 <- character(num_studies)
  selected_g2 <- character(num_studies)

  for (study in seq_len(num_studies)) {
    qe_g1 <- run_qe_group_diagnostic(
      q1_g1[[study]], medians_g1[[study]], q3_g1[[study]], n_group1[[study]]
    )
    qe_g2 <- run_qe_group_diagnostic(
      q1_g2[[study]], medians_g2[[study]], q3_g2[[study]], n_group2[[study]]
    )

    qe_within_var_g1[[study]] <- qe_g1$variance
    qe_within_var_g2[[study]] <- qe_g2$variance
    selected_g1[[study]] <- qe_g1$selected_family
    selected_g2[[study]] <- qe_g2$selected_family
  }

  qe_within_var <- qe_within_var_g1 + qe_within_var_g2
  qe_failed_studies <- 0L

  qe_fe_estimate <- as.numeric(qe_fe_fit$b)
  qe_fe_variance <- as.numeric(stats::vcov(qe_fe_fit)[1L, 1L])
  qe_re_estimate <- as.numeric(qe_re_fit$b)
  qe_re_variance <- as.numeric(stats::vcov(qe_re_fit)[1L, 1L])

  # Method-specific analytic targets for this replicate.
  true_fe_weights_raw <- 1 / true_within_var
  true_fe_weights <- true_fe_weights_raw / sum(true_fe_weights_raw)
  true_variance_qe_fe <- sum(
    true_fe_weights^2 * (true_within_var + true_tau2)
  )

  true_re_weights_raw <- 1 / (true_within_var + true_tau2)
  true_variance_qe_re <- 1 / sum(true_re_weights_raw)

  selection_levels <- c(candidate_families, "fail")
  counts_g1 <- table(factor(selected_g1, levels = selection_levels))
  counts_g2 <- table(factor(selected_g2, levels = selection_levels))

  replicate_result <- data.frame(
    distribution = distribution,
    sample_size_mode = sample_size_mode,
    replicate = replicate_id,
    num_studies = num_studies,
    average_group_n = average_group_n,
    heterogeneity_ratio = heterogeneity_ratio,
    true_i2 = true_i2,
    true_tau2 = true_tau2,
    s2_typical = s2_typical,
    true_effect = true_effect,
    min_group_n = min(n_group1),
    max_group_n = max(n_group1),
    sd_group_n = stats::sd(n_group1),
    true_variance_dive = true_variance_dive,
    dive_estimate = dive_estimate,
    dive_variance = dive_variance,
    true_variance_qe_re = true_variance_qe_re,
    qe_re_estimate = qe_re_estimate,
    qe_re_variance = qe_re_variance,
    qe_re_tau2_estimate = as.numeric(qe_re_fit$tau2),
    qe_re_i2_estimate = as.numeric(qe_re_fit$I2),
    true_variance_qe_fe = true_variance_qe_fe,
    qe_fe_estimate = qe_fe_estimate,
    qe_fe_variance = qe_fe_variance,
    stringsAsFactors = FALSE
  )

  qe_diagnostic <- data.frame(
    distribution = distribution,
    sample_size_mode = sample_size_mode,
    replicate = replicate_id,
    num_studies = num_studies,
    average_group_n = average_group_n,
    heterogeneity_ratio = heterogeneity_ratio,
    qe_failed_studies = qe_failed_studies,
    qe_g1_count_normal = as.integer(counts_g1[["normal"]]),
    qe_g1_count_weibull = as.integer(counts_g1[["weibull"]]),
    qe_g1_count_lognormal = as.integer(counts_g1[["lognormal"]]),
    qe_g1_count_gamma = as.integer(counts_g1[["gamma"]]),
    qe_g1_count_fail = as.integer(counts_g1[["fail"]]),
    qe_g2_count_normal = as.integer(counts_g2[["normal"]]),
    qe_g2_count_weibull = as.integer(counts_g2[["weibull"]]),
    qe_g2_count_lognormal = as.integer(counts_g2[["lognormal"]]),
    qe_g2_count_gamma = as.integer(counts_g2[["gamma"]]),
    qe_g2_count_fail = as.integer(counts_g2[["fail"]]),
    mean_true_within_variance = mean(true_within_var),
    mean_estimated_within_variance = mean(qe_within_var),
    mean_true_within_variance_g1 = mean(true_within_var_g1),
    mean_estimated_within_variance_g1 = mean(qe_within_var_g1),
    mean_true_within_variance_g2 = mean(true_within_var_g2),
    mean_estimated_within_variance_g2 = mean(qe_within_var_g2),
    stringsAsFactors = FALSE
  )

  list(replicate_result = replicate_result, qe_diagnostic = qe_diagnostic)
}

# Full 16-setting run for one distribution x sample-size mode.
num_settings <- length(num_studies_values) *
  length(average_group_n_values) *
  length(heterogeneity_ratio_values)
replicate_results <- vector("list", num_settings)
qe_diagnostics <- vector("list", num_settings)
setting_index <- 0L

for (num_studies in num_studies_values) {
  for (average_group_n in average_group_n_values) {
    for (heterogeneity_ratio in heterogeneity_ratio_values) {
      setting_index <- setting_index + 1L
      message(sprintf(
        "Running %s / %s: N=%d, average n=%d, heterogeneity ratio=%s",
        distribution,
        sample_size_mode,
        num_studies,
        average_group_n,
        format(heterogeneity_ratio, trim = TRUE)
      ))

      replicate_list <- vector("list", num_replicates)
      diagnostic_list <- vector("list", num_replicates)

      for (replicate_id in seq_len(num_replicates)) {
        result <- run_one_replicate(
          replicate_id = replicate_id,
          distribution = distribution,
          sample_size_mode = sample_size_mode,
          num_studies = num_studies,
          average_group_n = average_group_n,
          heterogeneity_ratio = heterogeneity_ratio
        )
        replicate_list[[replicate_id]] <- result$replicate_result
        diagnostic_list[[replicate_id]] <- result$qe_diagnostic
      }

      replicate_results[[setting_index]] <- data.table::rbindlist(replicate_list)
      qe_diagnostics[[setting_index]] <- data.table::rbindlist(diagnostic_list)
    }
  }
}

replicate_results <- data.table::rbindlist(replicate_results)
qe_diagnostics <- data.table::rbindlist(qe_diagnostics)

data.table::fwrite(replicate_results, file.path(output_dir, "replicate_results.csv"))
data.table::fwrite(qe_diagnostics, file.path(output_dir, "qe_diagnostics.csv"))

message("Completed: ", output_dir)
