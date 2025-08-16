# diveMeta

**DiVE (Direct Variance Estimation)** for meta-analysis using **medians**.

This package implements DiVE for pooling study-level effects when only medians and sample sizes are available. The method returns a pooled effect, a directly estimated variance, and confidence intervals **without** requiring within-study variances.

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

dat <- read_example("oyelade_sdnn.csv")
# required columns: study_id, med_g1, n_g1, med_g2, n_g2

fit <- dive_df(dat, direction = "g1_minus_g2", ci_type = "t")
print(fit)    # rounded display; internal values are not rounded
summary(fit)
```

## Mixed reporting: pooling means (as proxies) with medians

When both mean- and median-reported studies exist, create a **central tendency** per group: use the median if available; otherwise fall back to the mean (proxy under symmetry).
*(Note: the demo file ships medians only; replace column names accordingly when your data have both means and medians.)*

```r
library(dplyr)
dat <- read_example("oyelade_sdnn.csv")   # or your own dataset

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

## Output fields

- `estimate`: pooled effect (default: g1 − g2)
- `se`, `ci_low`, `ci_high`: standard error and 95% CI from direct variance estimation
- `var_hat`: directly estimated variance
- `weights`, `wtilde`: integer weights (n_g1 + n_g2) and normalized weights
- `diagnostics`: list with `n_studies`, `wmax`, `ci_type`, `direction`

## Defaults

- **CI**: t-interval with **df = K − 1** (set `ci_type = "normal"` to use the normal critical value).
- **Direction**: default is `g1_minus_g2` (set `direction = "g2_minus_g1"` to flip the sign).

## Requirements and constraints

- Weights: `w_i = n_g1i + n_g2i`, normalized `w~_i = w_i / Σ w_i`
- Theoretical requirement: **max(w~_i) < 0.5**. The function stops otherwise. Multi-arm trials with a shared control should split the control `n` across comparisons before calling `dive()`.

## Why not a forest plot?

DiVE estimates the *pooled* effect and its variance directly from study-level contrasts of group-level central tendencies (median, or mean-as-proxy under symmetry) and sample sizes. It does **not** recover **within-study** sampling variances, so per-study confidence intervals (CIs) are **not defined** without introducing additional modeling or imputation assumptions outside the scope of the method. A classic forest plot therefore does not apply.

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

When a single control arm is shared across two treatment contrasts, we split the control sample size evenly across the comparisons while keeping the reported central tendencies unchanged. Concretely, for **Nagasako 2009 (Child A/B)** we set **n_g2 = 10** for Child A and **n_g2 = 11** for Child B (median_g2 = 79 for both), so that 10 + 11 = 21 equals the original control size. This prevents double-counting in DiVE’s weights and keeps the analysis fully reproducible.

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
