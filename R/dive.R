#' DiVE meta-analysis using medians
#'
#' Pooled between-group difference using sample-size (IVW-style) weights and
#' a direct estimator of the variance; no within-study variances are required.
#'
#' @param med_g1 numeric, group 1 medians (length K)
#' @param n_g1   integer, group 1 sample sizes (length K, >0)
#' @param med_g2 numeric, group 2 medians (length K)
#' @param n_g2   integer, group 2 sample sizes (length K, >0)
#' @param direction character, \code{"g1_minus_g2"} (default) or \code{"g2_minus_g1"}
#' @param ci_type  character, \code{"t"} (default) or \code{"normal"}
#' @return An object of class \code{"dive"} with fields:
#'   \code{estimate}, \code{se}, \code{ci_low}, \code{ci_high},
#'   \code{var_hat}, \code{weights}, \code{wtilde}, and \code{diagnostics}.
#' @examples
#' med_g1 <- c(30,28,26); n_g1 <- c(50,60,55)
#' med_g2 <- c(25,27,24); n_g2 <- c(50,65,50)
#' fit <- dive(med_g1, n_g1, med_g2, n_g2)
#' print(fit)
#' @export
dive <- function(med_g1, n_g1, med_g2, n_g2,
                 direction = c("g1_minus_g2","g2_minus_g1"),
                 ci_type   = c("t","normal")) {

  direction <- match.arg(direction)
  ci_type   <- match.arg(ci_type)

  # ---- input checks ----
  if (any(is.na(med_g1) | is.na(med_g2) | is.na(n_g1) | is.na(n_g2))) {
    stop("Inputs contain NA. Remove or impute before calling dive().")
  }
  if (length(med_g1) != length(med_g2) ||
      length(n_g1)   != length(n_g2)   ||
      length(med_g1) != length(n_g1)) {
    stop("All input vectors must have equal length K.")
  }
  if (any(n_g1 <= 0 | n_g2 <= 0)) stop("All sample sizes must be > 0.")
  if (any(n_g1 != as.integer(n_g1) | n_g2 != as.integer(n_g2))) {
    warning("Sample sizes are coerced to integer.")
  }
  K <- length(med_g1)
  if (K < 2) stop("At least 2 studies are required.")

  # ---- effect per study ----
  Xi <- if (direction == "g1_minus_g2") med_g1 - med_g2 else med_g2 - med_g1

  # ---- weights ----
  w_i <- as.integer(n_g1 + n_g2)        # integer weights by total n_i
  W   <- sum(w_i)
  wtilde <- w_i / W

  # Theoretical requirement: max(wtilde) < 1/2
  wmax <- max(wtilde)
  if (wmax >= 0.5) {
    stop(sprintf("Violated DiVE requirement: max(wtilde)=%.4f >= 0.5. ", wmax),
         "Consider splitting large multi-arm studies or revising design.")
  }
  # safety: also forbid exactly 0.5 to avoid division by zero later
  if (any(abs(1 - 2*wtilde) < .Machine$double.eps^0.5)) {
    stop("Numerical instability: some 1 - 2*wtilde are ~0.")
  }

  # ---- pooled estimate ----
  mu_hat <- sum(wtilde * Xi)

  # ---- direct variance estimation ----
  # h_i = wtilde^2 / (1 - 2*wtilde)
  h_i <- (wtilde^2) / (1 - 2*wtilde)
  denom <- 1 + sum(h_i)
  var_hat <- sum(h_i * (Xi - mu_hat)^2) / denom
  if (!is.finite(var_hat) || var_hat < 0) {
    stop("Computed variance is not finite or negative; check inputs.")
  }
  se <- sqrt(var_hat)

  # ---- confidence interval ----
  if (ci_type == "t") {
    crit <- stats::qt(0.975, df = K - 1)
  } else {
    crit <- 1.96
  }
  ci_low  <- mu_hat - crit * se
  ci_high <- mu_hat + crit * se

  out <- list(
    estimate = mu_hat,
    se       = se,
    ci_low   = ci_low,
    ci_high  = ci_high,
    var_hat  = var_hat,
    weights  = w_i,
    wtilde   = wtilde,
    diagnostics = list(
      wmax      = wmax,
      n_studies = K,
      ci_type   = ci_type,
      direction = direction
    )
  )
  class(out) <- "dive"
  out
}
