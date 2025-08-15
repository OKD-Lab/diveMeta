#' DiVE meta-analysis from a data.frame
#'
#' @param data data.frame with columns for group-level **central tendency**
#'   (median by default; mean as a proxy under symmetry), and sample sizes.
#'   Map them via `cols`, e.g., `cols = list(med_g1 = "ct_g1", ...)`.
#' @param cols named list mapping column names; the `med_*` slots may receive
#'   medians or means (as proxies) as central tendencies on a common scale.
#'
#' @inheritParams dive
#' @export

dive_df <- function(data,
                    cols = list(med_g1 = "med_g1", n_g1 = "n_g1",
                                med_g2 = "med_g2", n_g2 = "n_g2"),
                    direction = c("g1_minus_g2","g2_minus_g1"),
                    ci_type   = c("t","normal")) {
  direction <- match.arg(direction); ci_type <- match.arg(ci_type)
  need <- unlist(cols, use.names = FALSE)
  if (!all(need %in% names(data))) {
    stop("data is missing required columns: ",
         paste(setdiff(need, names(data)), collapse = ", "))
  }
  med_g1 <- data[[cols$med_g1]]
  n_g1   <- data[[cols$n_g1]]
  med_g2 <- data[[cols$med_g2]]
  n_g2   <- data[[cols$n_g2]]
  dive(med_g1, n_g1, med_g2, n_g2, direction = direction, ci_type = ci_type)
}

#' @export
print.dive <- function(x, ...) {
  fmt <- function(z) sprintf("%.2f", z)
  cat("DiVE (Direct Variance Estimation)\n")
  cat("  Estimate :", fmt(x$estimate), "\n")
  cat("  SE       :", fmt(x$se), "\n")
  cat("  95% CI   :", paste0(fmt(x$ci_low), " to ", fmt(x$ci_high)),
      " (", x$diagnostics$ci_type, ")\n", sep = "")
  cat("  Var_hat  :", fmt(x$var_hat), "\n")
  cat("Diagnostics:\n")
  cat("  K        :", x$diagnostics$n_studies, "\n")
  cat("  max w~   :", sprintf("%.4f", x$diagnostics$wmax), "\n")
  cat("  direction: ", x$diagnostics$direction, "\n", sep = "")
  invisible(x)
}

#' @export
summary.dive <- function(object, ...) {
  res <- list(
    estimate = object$estimate,
    se       = object$se,
    ci       = c(object$ci_low, object$ci_high),
    var_hat  = object$var_hat,
    weights  = object$weights,
    wtilde   = object$wtilde,
    diagnostics = object$diagnostics
  )
  class(res) <- "summary.dive"
  res
}
