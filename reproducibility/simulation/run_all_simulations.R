#!/usr/bin/env Rscript

# Run the six manuscript simulation blocks as independent R processes and then
# summarize all completed outputs.

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


get_rscript <- function() {
  
  candidates <- c(
    file.path(
      R.home("bin"),
      "Rscript.exe"
    ),
    file.path(
      R.home("bin"),
      "Rscript"
    )
  )
  
  candidates <- candidates[
    file.exists(candidates)
  ]
  
  if (length(candidates) == 0L) {
    
    stop(
      paste(
        "Rscript executable could not be located",
        "for the current R installation."
      )
    )
  }
  
  normalizePath(
    candidates[[1L]],
    winslash = "/",
    mustWork = TRUE
  )
}


script_dir <- get_script_dir()

run_script <- file.path(
  script_dir,
  "run_simulation.R"
)

summary_script <- file.path(
  script_dir,
  "summarize_simulation.R"
)

rscript <- get_rscript()

runs <- expand.grid(
  distribution = c("normal", "skew_normal", "log_normal"),
  sample_size_mode = c("fixed", "varying"),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(runs))) {
  distribution <- runs$distribution[[i]]
  sample_size_mode <- runs$sample_size_mode[[i]]

  message("Starting: ", distribution, " / ", sample_size_mode)
  status <- system2(
    rscript,
    args = c(
      shQuote(run_script),
      shQuote(distribution),
      shQuote(sample_size_mode)
    ),
    wait = TRUE
  )

  if (!identical(status, 0L)) {
    stop(
      "Simulation failed for ",
      distribution,
      " / ",
      sample_size_mode,
      " (exit status ",
      status,
      ")."
    )
  }
}

message("All six simulation runs completed. Creating summaries.")
status <- system2(rscript, args = shQuote(summary_script), wait = TRUE)
if (!identical(status, 0L)) {
  stop("Simulation summarization failed (exit status ", status, ").")
}

message("All simulation and summary steps completed successfully.")
