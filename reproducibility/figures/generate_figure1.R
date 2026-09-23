#!/usr/bin/env Rscript

# Generate the three distribution panels used in Figure 1.

if (!requireNamespace("sn", quietly = TRUE)) {
  stop("Missing required package: sn.")
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
  
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

script_dir <- get_script_dir()
project_root <- normalizePath(
  file.path(script_dir, ".."),
  winslash = "/",
  mustWork = TRUE
)

output_dir <- file.path(
  project_root,
  "manuscript_outputs",
  "figures"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

figure_family <- "Times"
median_color <- "darkred"

save_panel <- function(
    stem,
    x,
    density_g1,
    density_g2,
    median_g1,
    median_g2,
    x_limits) {
  
  for (device in c("pdf", "eps")) {
    
    output_file <- file.path(
      output_dir,
      paste0(stem, ".", device)
    )
    
    if (device == "pdf") {
      
      grDevices::pdf(
        output_file,
        width = 8,
        height = 5,
        family = figure_family
      )
      
    } else {
      
      grDevices::postscript(
        output_file,
        width = 8,
        height = 5,
        horizontal = FALSE,
        onefile = FALSE,
        paper = "special",
        family = figure_family
      )
    }
    
    graphics::par(
      family = figure_family,
      mar = c(3, 3, 1, 1),
      mgp = c(1.8, 0.6, 0),
      cex = 1.2,
      cex.axis = 1,
      cex.lab = 1
    )
    
    graphics::plot(
      x,
      density_g1,
      type = "l",
      lwd = 2,
      col = "black",
      xlab = "Outcome value",
      ylab = "Density",
      xlim = x_limits,
      ylim = c(
        0,
        max(density_g1, density_g2) * 1.1
      )
    )
    
    graphics::lines(
      x,
      density_g2,
      lty = 2,
      lwd = 2,
      col = "black"
    )
    
    graphics::abline(
      v = median_g1,
      col = median_color,
      lty = 1,
      lwd = 1.5
    )
    
    graphics::abline(
      v = median_g2,
      col = median_color,
      lty = 2,
      lwd = 1.5
    )
    
    grDevices::dev.off()
  }
}


# Normal panel.
# The shift of 2 is used only to display the two distribution shapes clearly.
# Simulation-specific normal shifts are generated in run_simulation.R.

normal_mean_g2 <- 5
normal_mean_g1 <- 7
normal_sd <- 1

x_normal <- seq(
  0,
  15,
  length.out = 1000
)

save_panel(
  stem = "figure1a_normal",
  x = x_normal,
  density_g1 = stats::dnorm(
    x_normal,
    mean = normal_mean_g1,
    sd = normal_sd
  ),
  density_g2 = stats::dnorm(
    x_normal,
    mean = normal_mean_g2,
    sd = normal_sd
  ),
  median_g1 = normal_mean_g1,
  median_g2 = normal_mean_g2,
  x_limits = c(0, 15)
)


# Skew-normal panel.

skew_g1 <- list(
  location = 5,
  scale = 5,
  shape = 5
)

skew_g2 <- list(
  location = 5,
  scale = 10,
  shape = 10
)

x_skew <- seq(
  0,
  40,
  length.out = 1000
)

skew_median_g1 <- sn::qsn(
  0.5,
  xi = skew_g1$location,
  omega = skew_g1$scale,
  alpha = skew_g1$shape
)

skew_median_g2 <- sn::qsn(
  0.5,
  xi = skew_g2$location,
  omega = skew_g2$scale,
  alpha = skew_g2$shape
)

save_panel(
  stem = "figure1b_skew_normal",
  x = x_skew,
  density_g1 = sn::dsn(
    x_skew,
    xi = skew_g1$location,
    omega = skew_g1$scale,
    alpha = skew_g1$shape
  ),
  density_g2 = sn::dsn(
    x_skew,
    xi = skew_g2$location,
    omega = skew_g2$scale,
    alpha = skew_g2$shape
  ),
  median_g1 = skew_median_g1,
  median_g2 = skew_median_g2,
  x_limits = c(0, 40)
)


# Log-normal panel.

lognormal_g1 <- list(
  meanlog = 2,
  sdlog = 1
)

lognormal_g2 <- list(
  meanlog = 3,
  sdlog = 2
)

x_log <- seq(
  0,
  50,
  length.out = 1000
)

save_panel(
  stem = "figure1c_log_normal",
  x = x_log,
  density_g1 = stats::dlnorm(
    x_log,
    meanlog = lognormal_g1$meanlog,
    sdlog = lognormal_g1$sdlog
  ),
  density_g2 = stats::dlnorm(
    x_log,
    meanlog = lognormal_g2$meanlog,
    sdlog = lognormal_g2$sdlog
  ),
  median_g1 = exp(lognormal_g1$meanlog),
  median_g2 = exp(lognormal_g2$meanlog),
  x_limits = c(0, 50)
)

message(
  "Figure 1 panels written to: ",
  output_dir
)