#' Read example datasets shipped with the package
#'
#' @param name character, file name in inst/extdata
#'   e.g. "oyelade_sdnn.csv" or "meling_grs_medians.csv"
#' @return data.frame
#' @export
read_example <- function(name = c("oyelade_sdnn.csv", "meling_grs_medians.csv")) {
  name <- match.arg(name)
  f <- system.file("extdata", name, package = "diveMeta", mustWork = TRUE)
  utils::read.csv(f, stringsAsFactors = FALSE)
}
