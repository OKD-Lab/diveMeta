#' Read example datasets shipped with the package
#'
#' @param name character, file name in inst/extdata.
#'   One of: "meling_grs_all.csv", "meling_grs_org.csv",
#'           "oyelade_sdnn_all.csv", "oyelade_sdnn_org.csv".
#' @return data.frame
#' @examples
#' read_example("oyelade_sdnn_org.csv")
#' read_example("meling_grs_org.csv")
#' @export
read_example <- function(name = c("meling_grs_all.csv", "meling_grs_org.csv",
                                  "oyelade_sdnn_all.csv", "oyelade_sdnn_org.csv")) {
  name <- match.arg(name)
  f <- system.file("extdata", name, package = "diveMeta", mustWork = TRUE)
  utils::read.csv(f, stringsAsFactors = FALSE)
}
