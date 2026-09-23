# Manuscript reproducibility materials

This directory contains the code, summary-level data, and reference outputs used to reproduce the analyses reported in:

**A Direct Variance Estimation (DiVE) for Meta-Analysis of Median Differences**

The public materials are organized to match the manuscript analyses without including author-side QC or validation scripts.

## Contents

- `simulation/` — Monte Carlo simulation code.
- `figures/` — code for Figure 1, Figure 2, and Supplementary Figures S1--S12.
- `tables/` — code for Table 2 and Supplementary Tables S2--S24.
- `make_manuscript_outputs.R` — runs the table and figure generation scripts after the simulations are complete.
- `real_data_application/` — curated summary-level inputs and the reproduction script for the real-data application.
- `reference_outputs/` — validated final outputs for checking reproduced simulation summaries, manuscript tables, and real-data results.
- `environment/` — the recorded session information from the final real-data environment validation.

## Analysis environment

The final manuscript simulation was validated with:

- R 4.6.1
- metamedian 1.2.2
- estmeansd 1.0.1
- metafor 5.2.1
- gtools 3.9.5
- sn 2.1.3
- pwr 1.3.0

The final simulation run did not record a complete `sessionInfo()` output; the versions above are the validated reference environment used for the final package-direct QE analysis. The exact session information recorded during the final real-data environment validation is provided in `environment/sessionInfo_final_real_data_validation.txt`.

Additional packages used by the manuscript-output and real-data reproduction scripts include `data.table`, `ggplot2`, `scales`, `readr`, `tibble`, and `dplyr`. The `diveMeta` package in this repository is version 0.1.0.

## Reproducing the simulation study

Install the required R packages, then run from the repository root:

```sh
Rscript reproducibility/simulation/run_all_simulations.R
```

This runs all six combinations of outcome distribution (`normal`, `skew_normal`, `log_normal`) and sample-size pattern (`fixed`, `varying`) using 1,000 replicates per design setting. The simulation seed and manuscript settings are fixed in the scripts.

The replicate-level and summary outputs are written under `reproducibility/outputs/`. The validated final global summary is provided at `reference_outputs/simulation/summary_results_all.csv` for comparison.

## Generating manuscript tables and figures

After the simulations finish, run:

```sh
Rscript reproducibility/make_manuscript_outputs.R
```

The generated files are written under `reproducibility/manuscript_outputs/`. Validated final CSVs for Table 2 and Supplementary Tables S2--S24 are provided under `reference_outputs/manuscript_tables/`.

## Reproducing the real-data application

Install the package from the repository root, for example with:

```sh
R CMD INSTALL .
```

Then run:

```sh
Rscript reproducibility/real_data_application/reproduce_real_data_application.R
```

The script uses the two summary-level CSV files under `real_data_application/data/` and writes reproduced results under `reproducibility/outputs/real_data/`. Validated final numeric results and Table 3 inputs/weights are provided under `reference_outputs/real_data/` for comparison.

The real-data inputs were transcribed from published reports and the Cochrane review; no individual participant data are included.

## Reference outputs

Files under `reference_outputs/` are validation targets, not additional analysis inputs. They preserve the final manuscript-aligned results against which a fresh reproduction can be checked. Internal author-side QC files are intentionally not included in the public materials.

## Notes

- The manuscript analyses use study-level median differences throughout; the package's optional mean-fallback convenience functionality was not used for the manuscript analyses.
- QE is implemented with the maintained `metamedian` package using the median + IQR configuration and the standard four-family candidate set (normal, log-normal, Weibull, gamma), with DerSimonian--Laird random-effects pooling for QE--RE.
