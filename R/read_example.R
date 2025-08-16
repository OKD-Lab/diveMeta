#' Read example datasets shipped with the package
#'
#' @param name character, file name in inst/extdata.
#'   Available:
#'   - "meling_grs_all.csv" : includes primary-study medians when available
#'   - "meling_grs_org.csv" : follows the original meta-analysis reporting
#'   - "oyelade_sdnn_all.csv": includes primary-study medians; shared control split 10/11
#'   - "oyelade_sdnn_org.csv": follows the original meta-analysis reporting
#' @return data.frame
#' @export
read_example <- function(name = c("meling_grs_all.csv",
                                  "meling_grs_org.csv",
                                  "oyelade_sdnn_all.csv",
                                  "oyelade_sdnn_org.csv")) {
  name <- match.arg(name)
  f <- system.file("extdata", name, package = "diveMeta", mustWork = TRUE)
  utils::read.csv(f, stringsAsFactors = FALSE)
}
