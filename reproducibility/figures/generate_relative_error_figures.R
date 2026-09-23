#!/usr/bin/env Rscript

# Generate Figure 2 and Supplementary Figures S1-S12
# from replicate-level simulation results.

required_packages <- c(
  "data.table",
  "ggplot2",
  "scales"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    "."
  )
}


get_script_dir <- function() {
  
  cmd <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd, value = TRUE)
  
  if (length(file_arg) > 0L) {
    
    return(
      normalizePath(
        dirname(sub("^--file=", "", file_arg[[1L]])),
        winslash = "/",
        mustWork = TRUE
      )
    )
  }
  
  frames <- sys.frames()
  
  ofiles <- vapply(
    frames,
    function(x) {
      if (!is.null(x$ofile)) x$ofile else NA_character_
    },
    FUN.VALUE = character(1)
  )
  
  ofiles <- ofiles[!is.na(ofiles)]
  
  if (length(ofiles) > 0L) {
    
    return(
      normalizePath(
        dirname(tail(ofiles, 1L)),
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
  file.path(script_dir, ".."),
  winslash = "/",
  mustWork = TRUE
)

simulation_output_dir <- file.path(
  project_root,
  "outputs"
)

figure_output_dir <- file.path(
  project_root,
  "manuscript_outputs",
  "figures"
)

dir.create(
  figure_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


run_names <- c(
  "normal_fixed",
  "skew_normal_fixed",
  "log_normal_fixed",
  "normal_varying",
  "skew_normal_varying",
  "log_normal_varying"
)

replicate_files <- file.path(
  simulation_output_dir,
  run_names,
  "replicate_results.csv"
)

missing_files <- replicate_files[
  !file.exists(replicate_files)
]

if (length(missing_files) > 0L) {
  
  stop(
    "Missing replicate-level result file(s): ",
    paste(
      missing_files,
      collapse = ", "
    )
  )
}


simulation <- data.table::rbindlist(
  lapply(
    replicate_files,
    data.table::fread
  ),
  use.names = TRUE
)


method_levels <- c(
  "DiVE",
  "QE-RE",
  "QE-FE"
)

palette <- c(
  "DiVE" = "#A6D9FF",
  "QE-RE" = "#DEDEDE",
  "QE-FE" = "#CBCBCB"
)

figure_family <- "Times"


i2_from_ratio <- function(x) {
  
  out <- rep(
    NA_integer_,
    length(x)
  )
  
  out[abs(x - 0) < 1e-8] <- 0L
  
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


simulation[
  ,
  i2_pct := i2_from_ratio(
    heterogeneity_ratio
  )
]

if (anyNA(simulation$i2_pct)) {
  stop(
    "An unrecognized heterogeneity ratio was found."
  )
}


simulation[
  ,
  size_tag := ifelse(
    sample_size_mode == "fixed",
    "F",
    "V"
  )
]

simulation[
  ,
  row_label := sprintf(
    "N = %d, n = %d%s",
    num_studies,
    average_group_n,
    size_tag
  )
]

simulation[
  ,
  i2_label := paste0(
    "I\u00B2 = ",
    i2_pct,
    "%"
  )
]


row_levels_fixed <- c(
  "N = 10, n = 100F",
  "N = 10, n = 300F",
  "N = 30, n = 100F",
  "N = 30, n = 300F"
)

row_levels_varying <- c(
  "N = 10, n = 100V",
  "N = 10, n = 300V",
  "N = 30, n = 100V",
  "N = 30, n = 300V"
)

i2_levels <- paste0(
  "I\u00B2 = ",
  c(0, 25, 50, 75),
  "%"
)


make_relative_error_data <- function(
    data,
    error_type = c(
      "point",
      "variance"
    )) {
  
  error_type <- match.arg(
    error_type
  )
  
  if (error_type == "point") {
    
    out <- data.table::rbindlist(
      list(
        
        data[
          ,
          .(
            distribution,
            sample_size_mode,
            num_studies,
            average_group_n,
            row_label,
            i2_label,
            method = "DiVE",
            relative_error =
              100 *
              (dive_estimate - true_effect) /
              true_effect
          )
        ],
        
        data[
          ,
          .(
            distribution,
            sample_size_mode,
            num_studies,
            average_group_n,
            row_label,
            i2_label,
            method = "QE-RE",
            relative_error =
              100 *
              (qe_re_estimate - true_effect) /
              true_effect
          )
        ],
        
        data[
          ,
          .(
            distribution,
            sample_size_mode,
            num_studies,
            average_group_n,
            row_label,
            i2_label,
            method = "QE-FE",
            relative_error =
              100 *
              (qe_fe_estimate - true_effect) /
              true_effect
          )
        ]
      )
    )
    
  } else {
    
    out <- data.table::rbindlist(
      list(
        
        data[
          ,
          .(
            distribution,
            sample_size_mode,
            num_studies,
            average_group_n,
            row_label,
            i2_label,
            method = "DiVE",
            relative_error =
              100 *
              (dive_variance -
                 true_variance_dive) /
              true_variance_dive
          )
        ],
        
        data[
          ,
          .(
            distribution,
            sample_size_mode,
            num_studies,
            average_group_n,
            row_label,
            i2_label,
            method = "QE-RE",
            relative_error =
              100 *
              (qe_re_variance -
                 true_variance_qe_re) /
              true_variance_qe_re
          )
        ],
        
        data[
          ,
          .(
            distribution,
            sample_size_mode,
            num_studies,
            average_group_n,
            row_label,
            i2_label,
            method = "QE-FE",
            relative_error =
              100 *
              (qe_fe_variance -
                 true_variance_qe_fe) /
              true_variance_qe_fe
          )
        ]
      )
    )
  }
  
  out <- out[
    is.finite(relative_error)
  ]
  
  out[
    ,
    method := factor(
      method,
      levels = method_levels
    )
  ]
  
  out[
    ,
    i2_label := factor(
      i2_label,
      levels = i2_levels
    )
  ]
  
  out
}


point_error <- make_relative_error_data(
  simulation,
  "point"
)

variance_error <- make_relative_error_data(
  simulation,
  "variance"
)


plot_relative_error <- function(
    data,
    y_limits,
    row_levels) {
  
  plot_data <- data.table::copy(
    data
  )
  
  plot_data[
    ,
    row_label := factor(
      row_label,
      levels = row_levels
    )
  ]
  
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = method,
      y = relative_error,
      fill = method
    )
  ) +
    
    ggplot2::geom_boxplot(
      width = 0.7,
      coef = 1.5,
      outlier.shape = 1,
      outlier.size = 1,
      outlier.stroke = 0.3,
      colour = "black"
    ) +
    
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      colour = "black"
    ) +
    
    ggplot2::scale_x_discrete(
      limits = method_levels
    ) +
    
    ggplot2::scale_fill_manual(
      values = palette,
      breaks = method_levels
    ) +
    
    ggplot2::labs(
      y = "Relative Error (%)",
      x = NULL
    ) +
    
    ggplot2::coord_cartesian(
      ylim = y_limits
    ) +
    
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(
        n = 5
      )
    ) +
    
    ggplot2::facet_grid(
      rows = ggplot2::vars(
        row_label
      ),
      cols = ggplot2::vars(
        i2_label
      )
    ) +
    
    ggplot2::theme_bw(
      base_size = 12,
      base_family = figure_family
    ) +
    
    ggplot2::theme(
      legend.position = "none",
      
      panel.grid.major =
        ggplot2::element_blank(),
      
      panel.grid.minor =
        ggplot2::element_blank(),
      
      panel.background =
        ggplot2::element_rect(
          fill = "white",
          colour = NA
        ),
      
      plot.background =
        ggplot2::element_rect(
          fill = "white",
          colour = NA
        ),
      
      strip.background =
        ggplot2::element_rect(
          fill = "white",
          colour = "black"
        ),
      
      strip.text =
        ggplot2::element_text(
          family = figure_family,
          colour = "black"
        ),
      
      axis.text =
        ggplot2::element_text(
          family = figure_family,
          colour = "black"
        ),
      
      axis.title =
        ggplot2::element_text(
          family = figure_family,
          colour = "black"
        ),
      
      text =
        ggplot2::element_text(
          family = figure_family,
          colour = "black"
        ),
      
      panel.border =
        ggplot2::element_rect(
          colour = "black",
          fill = NA
        )
    )
}


save_plot_dual <- function(
    plot,
    stem,
    width,
    height) {
  
  grDevices::pdf(
    paste0(stem, ".pdf"),
    width = width,
    height = height,
    family = figure_family
  )
  
  print(plot)
  
  grDevices::dev.off()
  
  
  grDevices::postscript(
    paste0(stem, ".eps"),
    width = width,
    height = height,
    horizontal = FALSE,
    onefile = FALSE,
    paper = "special",
    family = figure_family
  )
  
  print(plot)
  
  grDevices::dev.off()
}


# Main Figure 2:
# log-normal, N = 30, varying average n = 100.

main_point_data <- point_error[
  distribution == "log_normal" &
    sample_size_mode == "varying" &
    num_studies == 30L &
    average_group_n == 100L
]

main_variance_data <- variance_error[
  distribution == "log_normal" &
    sample_size_mode == "varying" &
    num_studies == 30L &
    average_group_n == 100L
]


main_point <- plot_relative_error(
  main_point_data,
  y_limits = c(-90, 90),
  row_levels = row_levels_varying
)

main_variance <- plot_relative_error(
  main_variance_data,
  y_limits = c(-100, 230),
  row_levels = row_levels_varying
)


save_plot_dual(
  main_point,
  file.path(
    figure_output_dir,
    "figure2a_point_relative_error"
  ),
  width = 12,
  height = 4
)

save_plot_dual(
  main_variance,
  file.path(
    figure_output_dir,
    "figure2b_variance_relative_error"
  ),
  width = 12,
  height = 4
)


# Supplementary Figures S1-S12.

supplementary_map <- data.frame(
  
  figure = paste0(
    "S",
    1:12
  ),
  
  distribution = c(
    "normal",
    "normal",
    "skew_normal",
    "skew_normal",
    "log_normal",
    "log_normal",
    "normal",
    "normal",
    "skew_normal",
    "skew_normal",
    "log_normal",
    "log_normal"
  ),
  
  sample_size_mode = c(
    rep("fixed", 6),
    rep("varying", 6)
  ),
  
  error_type = rep(
    c(
      "point",
      "variance"
    ),
    6
  ),
  
  stringsAsFactors = FALSE
)


for (i in seq_len(
  nrow(supplementary_map)
)) {
  
  spec <- supplementary_map[
    i,
  ]
  
  source_data <- if (
    spec$error_type == "point"
  ) {
    point_error
  } else {
    variance_error
  }
  
  y_limits <- if (
    spec$error_type == "point"
  ) {
    c(-90, 90)
  } else {
    c(-100, 230)
  }
  
  row_levels <- if (
    spec$sample_size_mode == "fixed"
  ) {
    row_levels_fixed
  } else {
    row_levels_varying
  }
  
  figure_data <- source_data[
    distribution ==
      spec$distribution &
      sample_size_mode ==
      spec$sample_size_mode
  ]
  
  p <- plot_relative_error(
    figure_data,
    y_limits = y_limits,
    row_levels = row_levels
  )
  
  stem <- file.path(
    figure_output_dir,
    paste0(
      "supplementary_figure_",
      spec$figure
    )
  )
  
  save_plot_dual(
    p,
    stem,
    width = 12,
    height = 15
  )
}


message(
  "Relative-error figures written to: ",
  figure_output_dir
)