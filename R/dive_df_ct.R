#' DiVE with automatic central tendencies (median first, else mean)
#'
#' @description
#' Convenience wrapper that builds per-study central tendencies and then calls
#' [dive_df()]. If a median is available it is used; otherwise the mean is used
#' as a proxy under approximate symmetry.
#'
#' @param data A data.frame containing at least \code{n_g1}, \code{n_g2} and
#'   one of \code{median_g1}/\code{mean_g1}, \code{median_g2}/\code{mean_g2}.
#' @param direction Character, passed to [dive_df()].
#' @param ci_type Character, passed to [dive_df()].
#' @param policy One of \code{c("median_first","median_only","mean_only")}.
#'   \code{"median_first"} uses median when available, otherwise mean.
#' @return An object of class \code{"dive"} (see [dive()]).
#' @examples
#' # dat <- read_example("Langhorne_ESD_all.csv")
#' # fit <- dive_df_ct(dat, direction = "g1_minus_g2", ci_type = "t")
#' # print(fit)
#' @export
dive_df_ct <- function(data,
                       direction = c("g1_minus_g2","g2_minus_g1"),
                       ci_type   = c("t","normal"),
                       policy    = c("median_first","median_only","mean_only")) {
  
  direction <- match.arg(direction)
  ci_type   <- match.arg(ci_type)
  policy    <- match.arg(policy)
  
  # ---- column presence checks ----
  need_n <- c("n_g1","n_g2")
  if (!all(need_n %in% names(data))) {
    stop("data must contain columns: ", paste(need_n, collapse = ", "))
  }
  
  # 安全に取り出し（無い列は NA ベクトルで代用）
  n <- nrow(data)
  get_or_na <- function(nm) if (nm %in% names(data)) data[[nm]] else rep(NA_real_, n)
  med_g1  <- get_or_na("median_g1")
  med_g2  <- get_or_na("median_g2")
  mean_g1 <- get_or_na("mean_g1")
  mean_g2 <- get_or_na("mean_g2")
  
  if (all(is.na(med_g1)) && all(is.na(mean_g1))) {
    stop("Provide at least one of median_g1 or mean_g1.")
  }
  if (all(is.na(med_g2)) && all(is.na(mean_g2))) {
    stop("Provide at least one of median_g2 or mean_g2.")
  }
  
  # ---- double-report diagnostic ----
  both_g1 <- !is.na(med_g1) & !is.na(mean_g1)
  both_g2 <- !is.na(med_g2) & !is.na(mean_g2)
  if (any(both_g1 | both_g2)) {
    which_rows <- which(both_g1 | both_g2)
    used <- switch(policy,
                   median_only  = "median (policy = median_only)",
                   mean_only    = "mean (policy = mean_only)",
                   median_first = "median when available, else mean (policy = median_first)"
    )
    warning("Some rows provide both mean and median; using ", used,
            ". Rows: ", paste(which_rows, collapse = ", "))
  }
  
  # ---- build central tendencies by policy ----
  pick <- function(med, mean) {
    if (policy == "median_only") {
      med
    } else if (policy == "mean_only") {
      mean
    } else { # "median_first"
      ifelse(!is.na(med), med, mean)
    }
  }
  ct_g1 <- pick(med_g1,  mean_g1)
  ct_g2 <- pick(med_g2,  mean_g2)
  
  # guard: 両群とも NA の行はエラー
  bad <- which(is.na(ct_g1) | is.na(ct_g2))
  if (length(bad) > 0) {
    stop("central tendencies missing in rows: ",
         paste(bad, collapse = ", "),
         ". Provide median_* or mean_* for both groups.")
  }
  
  tmp <- data
  tmp$ct_g1 <- ct_g1
  tmp$ct_g2 <- ct_g2
  
  # dive_df に渡す
  dive_df(tmp,
          cols = list(med_g1 = "ct_g1", n_g1 = "n_g1",
                      med_g2 = "ct_g2", n_g2 = "n_g2"),
          direction = direction, ci_type = ci_type)
}
