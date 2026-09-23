#!/usr/bin/env Rscript

# Generate manuscript simulation tables and figures
# from completed simulation outputs.
#
# This script does not rerun the Monte Carlo simulation.

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

rscript <- get_rscript()


scripts <- c(
  
  file.path(
    script_dir,
    "tables",
    "generate_simulation_tables.R"
  ),
  
  file.path(
    script_dir,
    "figures",
    "generate_figure1.R"
  ),
  
  file.path(
    script_dir,
    "figures",
    "generate_relative_error_figures.R"
  )
)


missing_scripts <- scripts[
  !file.exists(scripts)
]


if (length(missing_scripts) > 0L) {
  
  stop(
    "Missing output script(s): ",
    paste(
      missing_scripts,
      collapse = ", "
    )
  )
}


for (script in scripts) {
  
  message(
    "Running: ",
    basename(script)
  )
  
  status <- system2(
    rscript,
    args = shQuote(script),
    wait = TRUE
  )
  
  if (!identical(
    status,
    0L
  )) {
    
    stop(
      "Output generation failed for ",
      basename(script),
      " (exit status ",
      status,
      ")."
    )
  }
}


message(
  "All manuscript simulation outputs were generated successfully."
)