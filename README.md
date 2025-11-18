# diveMeta

**DiVE (Direct Variance Estimation)** for meta-analysis using **medians**.

This package implements DiVE for pooling study-level differences when only central tendencies (medians and/or means) and sample sizes are available.
The method returns a pooled difference, a directly estimated variance, and confidence intervals without requiring within-study variances.

> This repository is intended for **method demonstration (Example)**, not for making clinical claims. The code is deterministic: the same inputs yield the same outputs.

---


## Installation

```r
# install.packages("remotes")  # if needed
remotes::install_github("OKD-Lab/diveMeta")
```


## Minimal example (Langhorne ESD data)

The package ships one example dataset, Langhorne_ESD_all.csv, derived from a published meta-analysis of early supported discharge (ESD) versus conventional care after acute stroke. The outcome is the length of initial hospital stay (days).

```r
library(diveMeta)

# Load example data
dat <- read_example("Langhorne_ESD_all.csv")

# Keep studies with at least one central tendency per group
dat_use <- subset(
  dat,
  (!is.na(median_g1) | !is.na(mean_g1)) &
  (!is.na(median_g2) | !is.na(mean_g2))
)

# One-line DiVE with automatic central tendencies
fit <- dive_df_ct(
  dat_use,
  direction = "g1_minus_g2",  # ESD (g1) minus conventional care (g2)
  ci_type   = "t"
)

print(fit)    # rounded display; internal values are not rounded
summary(fit)
```
Here, dive_df_ct() constructs per-study central tendencies (see below) and calls dive_df() internally.


## Central tendencies and mixed reporting

When both mean- and median-reported studies exist, DiVE works on a central tendency per group:
- Use the median if available.
- Otherwise, fall back to the mean (used as a proxy under approximate symmetry of the outcome distribution).

You can construct these central tendencies yourself:

```r
library(dplyr)
dat <- read_example("Langhorne_ESD_all.csv")

dat_use <- dat %>%
  # Require at least one central tendency per group
  filter((!is.na(median_g1) | !is.na(mean_g1)),
         (!is.na(median_g2) | !is.na(mean_g2))) %>%
  mutate(
    ct_g1 = ifelse(!is.na(median_g1), median_g1, mean_g1),
    ct_g2 = ifelse(!is.na(median_g2), median_g2, mean_g2)
  )

fit <- dive_df(
  dat_use,
  cols = list(med_g1 = "ct_g1", n_g1 = "n_g1",
              med_g2 = "ct_g2", n_g2 = "n_g2"),
  direction = "g1_minus_g2",
  ci_type   = "t"
)
print(fit); summary(fit)
```

Alternatively, you can let dive_df_ct() handle the central tendencies:

```r
fit <- dive_df_ct(
  dat,
  direction = "g1_minus_g2",
  ci_type   = "t",
  policy    = "median_first"   # default: median if available, else mean
)
```


## Available example dataset

All example data live in inst/extdata/ and can be loaded via read_example():

- Langhorne_ESD_all.csv — ESD vs conventional care after stroke; length of initial hospital stay (days); includes both medians and means where reported. Group 1 (*_g1) is ESD, group 2 (*_g2) is conventional care.

Use:

```r
dat <- read_example("Langhorne_ESD_all.csv")
```

and then build central tendencies per group (median preferred; mean as proxy), as shown above.


## Core functions

- dive() — core DiVE estimator given numeric vectors.
- dive_df() — DiVE from a data.frame with configurable column mapping.
- dive_df_ct() — convenience wrapper that builds central tendencies (median-first, or mean-only / median-only via policy) and calls dive_df().
- read_example() — load the shipped example dataset.


## Output fields

A call to dive() or dive_df() returns an object of class "dive" with:

- estimate : pooled difference (default: g1 − g2)
- se, ci_low, ci_high : standard error and 95% CI from direct variance estimation
- var_hat : directly estimated variance
- weights, wtilde : integer weights (n_g1 + n_g2) and normalized weights
- diagnostics : list with
　- n_studies : number of studies
　- wmax : max(wtilde)
　- ci_type : "t" or "normal"
　- direction : "g1_minus_g2" or "g2_minus_g1"

If your column names differ, use cols = list(med_g1 = ..., n_g1 = ..., med_g2 = ..., n_g2 = ...) in dive_df() to map them explicitly.


## Defaults

- Contrast direction
　- Default: direction = "g1_minus_g2" (here: ESD − conventional care).
　- Set direction = "g2_minus_g1" to flip the sign.
- Confidence interval
　- Default: ci_type = "t" → 95% CI uses the t critical value with df = K − 1.
　- Set ci_type = "normal" to use the standard-normal critical value.


## Requirements and constraints

DiVE is defined in terms of normalized total-sample weights:
- Raw weights: w_i = n_g1i + n_g2i
- Normalized: wtilde_i = w_i / Σ w_i

The theoretical requirement is:
- max(wtilde_i) < 0.5

If this condition is violated, dive() will stop with an error. In multi-arm settings with a shared control group, the usual remedy is to split the control sample size across comparisons (keeping central tendencies unchanged) before calling DiVE, to avoid double-counting and overly dominant weights.


## Why not a forest plot?

DiVE estimates the pooled difference and its variance directly from:

- study-level contrasts of group-level central tendencies (median, or mean-as-proxy under symmetry), and
- the corresponding sample sizes.

It does not recover within-study sampling variances.
Therefore, per-study confidence intervals (CIs) are not defined without introducing additional modeling or imputation assumptions outside the scope of the method.
A classic forest plot (per-study estimate + CI + pooled CI) is not appropriate.

For transparency, we recommend displays such as:

1.　per-study group differences shown as points only (no CI) on a common scale;

2.　a single pooled DiVE estimate with its 95% CI (line or diamond).

This matches DiVE’s estimand and avoids implying per-study precision that the method does not estimate.


## Data and scales

- DiVE targets a common location shift δ between two groups.
- Under approximate distributional symmetry (e.g., near-normal outcomes), the mean is a reasonable proxy for the median; combining medians and means as central tendencies is interpretable on this basis.
- For the Langhorne ESD example, the outcome is the length of stay in days, reported on a common and directly interpretable scale.


## Assumptions (for interpretation)

- Location-shift estimand: all included studies target the same underlying location shift δ between groups.
- Approximate symmetry: when means are used as proxies, outcome distributions are assumed not to be extremely skewed.
- Common scale: outcomes are on a common, interpretable scale before pooling.
- DiVE requirement: max(wtilde) < 0.5 is enforced at runtime.


## Reproducibility

- No randomness is used; results are fully deterministic and reproducible.
- Requires R ≥ 3.6 (R ≥ 4.1 recommended).
- Minimal dependencies (base R / stats).


## How to cite

After acceptance of the corresponding article, please cite both the journal paper and this package, for example via:

```r
citation("diveMeta")
```


## License

MIT (see LICENSE and LICENSE.md). © 2025 Tadahisa Okuda
