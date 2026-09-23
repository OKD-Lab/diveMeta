#' Read example dataset shipped with the package
#'
#' @description
#' Read a package example dataset based on the Langhorne et al. early
#' supported discharge (ESD) trials that underlie the manuscript's real-data application.
#'
#' @param name Character string; currently only
#'   \code{"Langhorne_ESD_all.csv"} is available.
#'
#' @return A \code{data.frame} with one row per study and the columns
#'   \code{study_id}, \code{n_g1}, \code{median_g1}, \code{mean_g1},
#'   \code{n_g2}, \code{median_g2}, \code{mean_g2}.
#'
#' @examples
#' dat <- read_example("Langhorne_ESD_all.csv")
#' head(dat)
#'
#' @export
read_example <- function(name = c("Langhorne_ESD_all.csv")) {
  name <- match.arg(name)
  
  path <- system.file("extdata", name, package = "diveMeta")
  if (path == "") {
    stop("File not found in inst/extdata: ", name, call. = FALSE)
  }
  
  utils::read.csv(path, stringsAsFactors = FALSE)
}
