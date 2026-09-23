#!/usr/bin/env Rscript

# Generate manuscript-ready simulation tables
# from summary_results_all.csv.
#
# This script formats and subsets existing summary results.
# It does not recompute simulation performance metrics.

if (!requireNamespace(
  "data.table",
  quietly = TRUE
)) {
  stop(
    "Missing required package: data.table."
  )
}


get_script_dir <- function() {
  
  cmd <- commandArgs(
    trailingOnly = FALSE
  )
  
  file_arg <- grep(
    "^--file=",
    cmd,
    value = TRUE
  )
  
  if (length(file_arg) > 0L) {
    
    return(
      normalizePath(
        dirname(
          sub(
            "^--file=",
            "",
            file_arg[[1L]]
          )
        ),
        winslash = "/",
        mustWork = TRUE
      )
    )
  }
  
  frames <- sys.frames()
  
  ofiles <- vapply(
    frames,
    function(x) {
      if (!is.null(x$ofile)) {
        x$ofile
      } else {
        NA_character_
      }
    },
    FUN.VALUE = character(1)
  )
  
  ofiles <- ofiles[
    !is.na(ofiles)
  ]
  
  if (length(ofiles) > 0L) {
    
    return(
      normalizePath(
        dirname(
          tail(
            ofiles,
            1L
          )
        ),
        winslash = "/",
        mustWork = TRUE
      )
    )
  }
  
  normalizePath(
    getwd(),
    winslash = "/",
    mustWork = TRUE
  )
}


script_dir <- get_script_dir()

project_root <- normalizePath(
  file.path(
    script_dir,
    ".."
  ),
  winslash = "/",
  mustWork = TRUE
)


input_file <- file.path(
  project_root,
  "outputs",
  "summary_results_all.csv"
)

output_dir <- file.path(
  project_root,
  "manuscript_outputs",
  "tables"
)


dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


if (!file.exists(input_file)) {
  
  stop(
    "Simulation summary file not found: ",
    input_file
  )
}


results <- data.table::fread(
  input_file
)


i2_from_ratio <- function(x) {
  
  out <- rep(
    NA_integer_,
    length(x)
  )
  
  out[
    abs(x - 0) < 1e-8
  ] <- 0L
  
  out[
    abs(x - 0.3333) < 1e-6
  ] <- 25L
  
  out[
    abs(x - 1) < 1e-8
  ] <- 50L
  
  out[
    abs(x - 3) < 1e-8
  ] <- 75L
  
  out
}


results[
  ,
  I2_pct := i2_from_ratio(
    heterogeneity_ratio
  )
]


if (anyNA(results$I2_pct)) {
  
  stop(
    "An unrecognized heterogeneity ratio was found."
  )
}


method_levels <- c(
  "DiVE",
  "QE-RE",
  "QE-FE"
)


results[
  ,
  method_order := match(
    method,
    method_levels
  )
]


format3 <- function(x) {
  sprintf(
    "%.3f",
    x
  )
}


format_table <- function(data) {
  
  out <- data.table::copy(
    data
  )
  
  data.table::setorder(
    out,
    I2_pct,
    method_order
  )
  
  data.table::data.table(
    
    I2 = paste0(
      out$I2_pct,
      "%"
    ),
    
    Method =
      out$method,
    
    `Point %Bias` =
      format3(
        out$point_bias_pct
      ),
    
    `Point %MSE` =
      format3(
        out$point_mse_pct
      ),
    
    `Variance %Bias` =
      format3(
        out$variance_bias_pct
      ),
    
    `Variance %MSE` =
      format3(
        out$variance_mse_pct
      ),
    
    `z-based CP` =
      format3(
        out$coverage_z
      ),
    
    `z-based AW` =
      format3(
        out$average_width_z
      ),
    
    `t-based CP` =
      format3(
        out$coverage_t
      ),
    
    `t-based AW` =
      format3(
        out$average_width_t
      )
  )
}


write_table <- function(
    data,
    filename) {
  
  if (nrow(data) != 12L) {
    
    stop(
      "Unexpected number of rows for ",
      filename,
      ": ",
      nrow(data),
      "."
    )
  }
  
  data.table::fwrite(
    format_table(data),
    file.path(
      output_dir,
      filename
    )
  )
}


# Main Table 2.

main_table <- results[
  distribution == "log_normal" &
    sample_size_mode == "varying" &
    num_studies == 30L &
    average_group_n == 100L
]


write_table(
  main_table,
  "table_2.csv"
)


# Supplementary Tables S2-S24.
#
# The log-normal setting with N = 30,
# varying average n = 100 is reported in Table 2
# and is therefore omitted from the supplementary tables.

supplementary_map <- data.frame(
  
  table_number = 2:24,
  
  distribution = c(
    rep(
      "normal",
      4
    ),
    rep(
      "skew_normal",
      4
    ),
    rep(
      "log_normal",
      4
    ),
    rep(
      "normal",
      4
    ),
    rep(
      "skew_normal",
      4
    ),
    rep(
      "log_normal",
      3
    )
  ),
  
  sample_size_mode = c(
    rep(
      "fixed",
      12
    ),
    rep(
      "varying",
      11
    )
  ),
  
  num_studies = c(
    10, 10, 30, 30,
    10, 10, 30, 30,
    10, 10, 30, 30,
    10, 10, 30, 30,
    10, 10, 30, 30,
    10, 10, 30
  ),
  
  average_group_n = c(
    100, 300, 100, 300,
    100, 300, 100, 300,
    100, 300, 100, 300,
    100, 300, 100, 300,
    100, 300, 100, 300,
    100, 300, 300
  ),
  
  stringsAsFactors = FALSE
)


for (i in seq_len(
  nrow(supplementary_map)
)) {
  
  spec <- supplementary_map[
    i,
  ]
  
  table_data <- results[
    distribution ==
      spec$distribution &
      sample_size_mode ==
      spec$sample_size_mode &
      num_studies ==
      spec$num_studies &
      average_group_n ==
      spec$average_group_n
  ]
  
  output_name <- paste0(
    "supplementary_table_S",
    spec$table_number,
    ".csv"
  )
  
  write_table(
    table_data,
    output_name
  )
}


message(
  "Simulation tables written to: ",
  output_dir
)