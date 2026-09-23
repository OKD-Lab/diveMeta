#!/usr/bin/env Rscript

# Reproduce the manuscript real-data application.

get_script_dir <- function() {
  cmd <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd, value = TRUE)
  if (length(file_arg) == 0L) {
    return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  }
  normalizePath(dirname(sub("^--file=", "", file_arg[[1L]])), winslash = "/", mustWork = TRUE)
}

script_dir <- get_script_dir()
repro_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
data_dir <- file.path(script_dir, "data")
out_dir <- file.path(repro_root, "outputs", "real_data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

required_packages <- c("diveMeta", "metamedian", "metafor", "readr", "tibble", "dplyr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]
if (length(missing_packages) > 0L) {
  stop("Missing required package(s): ", paste(missing_packages, collapse = ", "), ".")
}

suppressPackageStartupMessages({
  library(diveMeta)
  library(metamedian)
  library(metafor)
  library(readr)
  library(tibble)
  library(dplyr)
})

fmt_p <- function(p) {
  ifelse(is.na(p), NA_character_,
         ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p)))
}

fmt_ci <- function(lb, ub) {
  sprintf("[%.2f, %.2f]", lb, ub)
}

## =========================================================
## DiVE (manuscript-aligned: medians only, no mean fallback)
## =========================================================
dat_dive <- readr::read_csv(
  file.path(data_dir, "CD000443_DiVE.csv"),
  show_col_types = FALSE
)

dat_dive <- subset(
  dat_dive,
  !is.na(median_g1) & !is.na(median_g2) &
    !is.na(n_g1) & !is.na(n_g2)
)

fit_dive_z <- dive_df(
  dat_dive,
  cols = list(
    med_g1 = "median_g1", n_g1 = "n_g1",
    med_g2 = "median_g2", n_g2 = "n_g2"
  ),
  direction = "g1_minus_g2",
  ci_type = "normal"
)

fit_dive_t <- dive_df(
  dat_dive,
  cols = list(
    med_g1 = "median_g1", n_g1 = "n_g1",
    med_g2 = "median_g2", n_g2 = "n_g2"
  ),
  direction = "g1_minus_g2",
  ci_type = "t"
)

est_dive <- fit_dive_t$estimate
se_dive  <- fit_dive_t$se
N_dive   <- fit_dive_t$diagnostics$n_studies
ntotal_dive <- sum(dat_dive$n_g1 + dat_dive$n_g2)

z_stat_dive <- est_dive / se_dive
p_z_dive <- 2 * stats::pnorm(-abs(z_stat_dive))

t_stat_dive <- est_dive / se_dive
p_t_dive <- 2 * stats::pt(-abs(t_stat_dive), df = N_dive - 1)

wi_dive <- dat_dive$n_g1 + dat_dive$n_g2
wtilde_dive <- wi_dive / sum(wi_dive)

## =========================================
## QE--RE
## =========================================
dat_qe <- readr::read_csv(
  file.path(data_dir, "CD000443_QE_S2.csv"),
  show_col_types = FALSE
)

dat_qe <- subset(
  dat_qe,
  !is.na(n_g1) & !is.na(median_g1) & !is.na(q1_g1) & !is.na(q3_g1) &
    !is.na(n_g2) & !is.na(median_g2) & !is.na(q1_g2) & !is.na(q3_g2)
)

dat_qe_mm <- dat_qe |>
  dplyr::transmute(
    study_id = study_id,
    n.g1     = n_g1,
    q1.g1    = q1_g1,
    med.g1   = median_g1,
    q3.g1    = q3_g1,
    n.g2     = n_g2,
    q1.g2    = q1_g2,
    med.g2   = median_g2,
    q3.g2    = q3_g2
  )

res_qe_z <- metamedian(
  data          = dat_qe_mm,
  median_method = "qe",
  single.family = FALSE,
  loc.shift     = FALSE,
  cd_method     = "RE",
  pool_studies  = TRUE,
  method        = "DL"
)
pred_qe_z <- predict(res_qe_z)

res_qe_t <- metamedian(
  data          = dat_qe_mm,
  median_method = "qe",
  single.family = FALSE,
  loc.shift     = FALSE,
  cd_method     = "RE",
  pool_studies  = TRUE,
  method        = "DL",
  test          = "t"
)
pred_qe_t <- predict(res_qe_t)

est_qe <- as.numeric(pred_qe_t$pred)
se_qe  <- as.numeric(pred_qe_t$se)
N_qe   <- res_qe_t$k
ntotal_qe <- sum(dat_qe$n_g1 + dat_qe$n_g2)

z_stat_qe <- est_qe / se_qe
p_z_qe <- 2 * stats::pnorm(-abs(z_stat_qe))

t_stat_qe <- est_qe / se_qe
p_t_qe <- 2 * stats::pt(-abs(t_stat_qe), df = N_qe - 1)

wi_qe_re <- 1 / (res_qe_z$vi + res_qe_z$tau2)
wtilde_qe_re <- wi_qe_re / sum(wi_qe_re)

## ==========================================
## Table 3-style input/weight table
## ==========================================
qe_table3 <- tibble::tibble(
  study_id  = dat_qe$study_id,
  q1_g1     = dat_qe$q1_g1,
  q3_g1     = dat_qe$q3_g1,
  q1_g2     = dat_qe$q1_g2,
  q3_g2     = dat_qe$q3_g2,
  wtilde_qe = round(wtilde_qe_re, 3)
)

table3_review <- tibble::tibble(
  study_id  = dat_dive$study_id,
  n_g1      = dat_dive$n_g1,
  median_g1 = dat_dive$median_g1,
  n_g2      = dat_dive$n_g2,
  median_g2 = dat_dive$median_g2,
  wtilde_s  = round(wtilde_dive, 3)
) |>
  dplyr::left_join(qe_table3, by = "study_id") |>
  dplyr::select(
    study_id,
    n_g1, median_g1, q1_g1, q3_g1,
    n_g2, median_g2, q1_g2, q3_g2,
    wtilde_s, wtilde_qe
  )

readr::write_csv(table3_review, file.path(out_dir, "Table3_real_data_inputs.csv"))

## ==================================
## Table 4-style summary (numeric/raw)
## ==================================
table4_numeric <- tibble::tibble(
  Method      = c("DiVE", "QE--RE"),
  N           = c(N_dive, N_qe),
  ntotal      = c(ntotal_dive, ntotal_qe),
  Estimate    = c(est_dive, est_qe),
  z_ci_lb     = c(fit_dive_z$ci_low,  as.numeric(pred_qe_z$ci.lb)),
  z_ci_ub     = c(fit_dive_z$ci_high, as.numeric(pred_qe_z$ci.ub)),
  z_p_value   = c(p_z_dive, p_z_qe),
  t_ci_lb     = c(fit_dive_t$ci_low,  as.numeric(pred_qe_t$ci.lb)),
  t_ci_ub     = c(fit_dive_t$ci_high, as.numeric(pred_qe_t$ci.ub)),
  t_p_value   = c(p_t_dive, p_t_qe),
  SE          = c(se_dive, se_qe)
)

readr::write_csv(table4_numeric, file.path(out_dir, "Table4_real_data_summary_numeric.csv"))

## ===================================
## Table 4-style summary (manuscript-like)
## ===================================
table4_review <- tibble::tibble(
  Method      = c("DiVE", "QE--RE"),
  N           = c(N_dive, N_qe),
  ntotal      = c(ntotal_dive, ntotal_qe),
  Estimate    = sprintf("%.2f", c(est_dive, est_qe)),
  z_95_CI     = c(
    fmt_ci(fit_dive_z$ci_low, fit_dive_z$ci_high),
    fmt_ci(as.numeric(pred_qe_z$ci.lb), as.numeric(pred_qe_z$ci.ub))
  ),
  z_p_value   = fmt_p(c(p_z_dive, p_z_qe)),
  t_95_CI     = c(
    fmt_ci(fit_dive_t$ci_low, fit_dive_t$ci_high),
    fmt_ci(as.numeric(pred_qe_t$ci.lb), as.numeric(pred_qe_t$ci.ub))
  ),
  t_p_value   = fmt_p(c(p_t_dive, p_t_qe))
)

readr::write_csv(table4_review, file.path(out_dir, "Table4_real_data_summary.csv"))

sink(file.path(out_dir, "real_data_results_DiVE_and_QE_RE.txt"))
cat("Real-data application (Table 4 aligned)\n\n")

cat("[DiVE]\n")
cat(sprintf("N = %d, ntotal = %d\n", N_dive, ntotal_dive))
cat(sprintf("Estimate = %.6f\n", est_dive))
cat(sprintf("SE = %.6f\n", se_dive))
cat(sprintf("95%% CI (z) = [%.6f, %.6f]\n", fit_dive_z$ci_low, fit_dive_z$ci_high))
cat(sprintf("p-value (z) = %.6f\n", p_z_dive))
cat(sprintf("95%% CI (t) = [%.6f, %.6f]\n", fit_dive_t$ci_low, fit_dive_t$ci_high))
cat(sprintf("p-value (t) = %.6f\n\n", p_t_dive))

cat("[QE--RE]\n")
cat(sprintf("N = %d, ntotal = %d\n", N_qe, ntotal_qe))
cat(sprintf("Estimate = %.6f\n", est_qe))
cat(sprintf("SE = %.6f\n", se_qe))
cat(sprintf("95%% CI (z) = [%.6f, %.6f]\n", as.numeric(pred_qe_z$ci.lb), as.numeric(pred_qe_z$ci.ub)))
cat(sprintf("p-value (z) = %.6f\n", p_z_qe))
cat(sprintf("95%% CI (t) = [%.6f, %.6f]\n", as.numeric(pred_qe_t$ci.lb), as.numeric(pred_qe_t$ci.ub)))
cat(sprintf("p-value (t) = %.6f\n", p_t_qe))
sink()

message("Real-data reproduction completed: ", out_dir)
