# Example datasets (inst/extdata)

## Common columns
- `study_id` : study label (may contain year)
- `mean_g1`, `median_g1`, `n_g1` : group 1 (intervention/patients)
- `mean_g2`, `median_g2`, `n_g2` : group 2 (control/healthy)
- Units: GRS (dimensionless score), SDNN (ms)

## File variants
- `meling_grs_all.csv`  : includes primary-study medians when available.
- `meling_grs_org.csv`  : follows the original meta-analysis reporting.
- `oyelade_sdnn_all.csv`: includes primary-study medians; shared control split 10/11.
- `oyelade_sdnn_org.csv`: follows the original meta-analysis reporting.

## Notes
- **Central tendency** used in analyses: median when available; otherwise mean as a proxy under approximate symmetry (common location-shift estimand δ).
- **Shared control (Oyelade, Nagasako 2009)**: control size 21 split as n_g2=10 (Child A) and 11 (Child B); medians unchanged (median_g2=79).
- No within-study variances are provided/used; DiVE does not estimate per-study CIs.
