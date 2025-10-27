# diveMeta

**DiVE (Direct Variance Estimation)** for meta-analysis using **medians**.

This package implements DiVE for pooling study-level differences when only medians and sample sizes are available. The method returns a pooled difference, a directly estimated variance, and confidence intervals without requiring within-study variances.

> This repository is intended for **method demonstration (Example)**, not for
> making clinical claims. The code is deterministic: the same inputs yield the
> same outputs.

---

## Installation

```r
# install.packages("remotes")  # if needed
remotes::install_github("OKD-Lab/diveMeta")
```


## Minimal example

```r
library(diveMeta)
dat <- read_example("meling_grs_all.csv")   # or "oyelade_sdnn_all.csv"
fit <- dive_df(
  transform(dat,
            ct_g1 = ifelse(!is.na(median_g1), median_g1, mean_g1),
            ct_g2 = ifelse(!is.na(median_g2), median_g2, mean_g2)),
  cols = list(med_g1="ct_g1", n_g1="n_g1", med_g2="ct_g2", n_g2="n_g2"),
  direction = "g1_minus_g2", ci_type = "t")
print(fit)    # rounded display; internal values are not rounded
summary(fit)
```


## One-liner: automatic central tendencies

If your dataset contains both `median_*` and `mean_*` columns, `dive_df_ct()` builds
per-study central tendencies (use median if available; otherwise mean) and calls
`dive_df()` internally.

```r
library(diveMeta)
dat <- read_example("meling_grs_all.csv")   # or "oyelade_sdnn_all.csv"
fit <- dive_df_ct(dat, direction = "g1_minus_g2", ci_type = "t")  # median-first policy
print(fit); summary(fit)
```


## Mixed reporting: pooling means (as proxies) with medians

When both mean- and median-reported studies exist, create a **central tendency** per group: use the median if available; otherwise fall back to the mean (proxy under symmetry).
*Tip: the demo files include both means and medians; adapt the column names as needed for your own data.*

```r
library(dplyr)
dat <- read_example("oyelade_sdnn_all.csv")   # or your own dataset

# Suppose your data has columns: median_g1, mean_g1, n_g1, median_g2, mean_g2, n_g2
# Build central tendencies (median preferred; mean as proxy)
dat_ct <- dat %>%
  mutate(
    ct_g1 = ifelse(!is.na(median_g1), median_g1, mean_g1),
    ct_g2 = ifelse(!is.na(median_g2), median_g2, mean_g2)
  )

# Pool with DiVE (map ct_* into the 'med_*' slots)
fit <- dive_df(
  dat_ct,
  cols = list(med_g1 = "ct_g1", n_g1 = "n_g1", med_g2 = "ct_g2", n_g2 = "n_g2"),
  direction = "g1_minus_g2",
  ci_type = "t"
)
print(fit); summary(fit)
```
Alternatively, use `dive_df_ct()` to construct central tendencies internally and run DiVE in one line.


## Available example datasets

- `meling_grs_all.csv`  — includes primary-study medians when available
- `meling_grs_org.csv`  — follows the original meta-analysis reporting
- `oyelade_sdnn_all.csv` — includes primary-study medians; shared control split 10/11
- `oyelade_sdnn_org.csv` — follows the original meta-analysis reporting

Use `read_example("<file>.csv")` to load; then build central tendencies per group
(median preferred; mean as proxy).


## Output fields

- `estimate`: pooled difference (default: g1 - g2)
- `se`, `ci_low`, `ci_high`: standard error and 95% CI from direct variance estimation
- `var_hat`: directly estimated variance
- `weights`, `wtilde`: integer weights (n_g1 + n_g2) and normalized weights
- `diagnostics`: list with `n_studies`, `wmax`, `ci_type`, `direction`

> **Column mapping.** If your column names differ, use `cols = list(med_g1=..., n_g1=..., med_g2=..., n_g2=...)` in `dive_df()` to map them explicitly.


## Defaults

- **CI**: t-interval with **df = K - 1** (set `ci_type = "normal"` to use the normal critical value).
- **Direction**: default is `g1_minus_g2` (set `direction = "g2_minus_g1"` to flip the sign).


## Requirements and constraints

- Weights: for each study i, `w_i = n_g1 + n_g2` (total sample size of that study); normalized weights are `w_i / sum_j w_j`.
- Theoretical requirement: **max(w~_i) < 0.5**. The function stops otherwise. Multi-arm trials with a shared control should split the control `n` across comparisons before calling `dive()`.


## Why not a forest plot?

DiVE estimates the *pooled* difference and its variance directly from study-level contrasts of group-level central tendencies (median, or mean-as-proxy under symmetry) and sample sizes. It does **not** recover **within-study** sampling variances, so per-study confidence intervals (CIs) are **not defined** without introducing additional modeling or imputation assumptions outside the scope of the method. A classic forest plot therefore does not apply.

For transparency, we visualize results as:
1) per-study group differences shown as **points only** (no CI) on a common scale,
2) a **single pooled DiVE estimate with its 95% CI** (one line/diamond).

This display matches DiVE’s estimand and avoids implying per-study precision
that the method does not estimate.

If a reviewer requests a forest-style figure, we can optionally:
(a) overlay the pooled 95% CI across the per-study points (still no per-study CIs), or
(b) provide a subset forest plot only for studies that report sufficient data to compute within-study SEs (e.g., mean+SD), clearly labeled as a subset analysis separate from DiVE’s main display.


## Data and scales

We target a **common location shift** δ between groups. Under **approximate distributional symmetry** (e.g., near-normal), the **mean** is a reasonable proxy for the **median**. Therefore, we treat **mean-reported** and **median-reported** studies as measuring the **same estimand** (the location shift δ) and **pool them** with DiVE on a common scale.

For transparency, figures show per-study points labeled by reporting type (mean vs median), and a single pooled DiVE estimate with CI.


## Shared control (multi-arm) handling

If a single control arm is reused to form two contrasts, split the control sample size across the comparisons before running dive(), while keeping the reported central tendencies unchanged. This avoids double-counting in DiVE’s sample-size weights.

Note for the shipped examples. The CSVs included in this repository list Nagasako 2009 as a single row (control n_g2 = 21) and therefore no split is applied in the packaged data. If you prefer the “split-control” representation (e.g., 10 and 11), create two rows in your working dataset as follows:

```r
# example: split a shared control (n_g2 = 21) into 10 and 11
library(dplyr)

split_control <- function(df, study_id, g2_n1 = 10, g2_n2 = 11) {
  i <- which(df$study_id == study_id)
  stopifnot(length(i) == 1L, df$n_g2[i] == (g2_n1 + g2_n2))
  r1 <- df[i, ]; r2 <- df[i, ]
  r1$study_id <- paste0(study_id, "_A"); r1$n_g2 <- g2_n1
  r2$study_id <- paste0(study_id, "_B"); r2$n_g2 <- g2_n2
  bind_rows(df[-i, ], r1, r2)
}

# usage:
# dat <- read_example("oyelade_sdnn_all.csv")
# dat_split <- split_control(dat, "Nagasako2009", g2_n1 = 10, g2_n2 = 11)
# fit <- dive_df_ct(dat_split, direction = "g1_minus_g2", ci_type = "t")
```


## Assumptions (for interpretation)

- **Location-shift estimand**: all studies target the same location shift δ between groups.
- **Approximate symmetry**: means can proxy medians under near-symmetric outcome distributions.
- **Common scale**: transform outcomes to a common, interpretable scale before pooling.
- **Shared controls**: split the control `n` across comparisons in multi-arm designs.
- **DiVE requirement**: `max(wtilde) < 0.5` is enforced at runtime.


## Reproducibility

- No randomness is used; results are fully reproducible.
- R >= 3.6 (recommend R >= 4.1).
- Minimal dependencies (`stats`).


## How to cite

After acceptance, please cite the journal article and this package:

```r
citation("diveMeta")
```


## License

MIT (see `LICENSE` and `LICENSE.md`). © 2025 Tadahisa Okuda
