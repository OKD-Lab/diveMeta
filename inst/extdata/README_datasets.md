# Example dataset (inst/extdata)

## Columns

- `study_id`  : study label (may contain year)
- `n_g1`      : sample size in group 1 (ESD)
- `median_g1`, `mean_g1` : central tendencies for group 1
- `n_g2`      : sample size in group 2 (conventional care)
- `median_g2`, `mean_g2` : central tendencies for group 2

Units: length of initial hospital stay in days.


## File

- `Langhorne_ESD_all.csv` : early supported discharge (ESD) vs conventional care after acute stroke; derived from the collection in Langhorne et al.
  Central tendencies are those reported in the primary studies (medians and/or means).


## Notes

- Analyses use a central tendency per group: median when available; otherwise mean as a proxy under approximate symmetry.
- No within-study variances are provided or used; DiVE does not estimate per-study confidence intervals.
- Group 1 (g1) is consistently the ESD/intervention arm; group 2 (g2) is the
  conventional care/control arm.
