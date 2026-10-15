# Replication materials

**Procedural Invariance in Environmental Valuation: Are Preferences Stable Across Choice-Set Sizes?**

Anonymous materials accompanying the submitted manuscript. The repository contains the replication data, 46 R scripts and the supplied descriptive outputs. It contains no manuscript PDF, author details or local R command histories.

## Contents

| Location | Purpose |
|---|---|
| `PrefMatWideFormatFullBiogeme.txt` | Tab-separated replication data; see `DATA_README.md` |
| `descriptives/` | Tables 2–4: demographic summaries, choice shares and serial SQ choices |
| `estimations/MNL_TableA1/` | MNL models, Table A1 |
| `estimations/RPL_TableA2/` | Correlated RPL models with negative log-normal cost, Table A2 |
| `estimations/LCM_TableA3/` | LCM models with demographic membership covariates, Table A3 |
| `WTP_Table5/` | MNL, RPL and LCM WTP calculations, Table 5 |
| `simulations/MNL_Table7_PanelA/` | Binary MNL benchmark predictions, Table 7 Panel A |
| `simulations/RPL_Table7_PanelB/` | Binary RPL benchmark predictions, Table 7 Panel B |
| `simulations/LCM_Table7_PanelC/` | Binary LCM benchmark predictions, Table 7 Panel C |
| `SCRIPT_INDEX.csv` | Every script, manuscript mapping, approximate runtime and dependencies |
| `RUNNING.md` | Exact commands for each script |

The MNL simulation folder is labelled Table 7 to match the submitted manuscript. Only the supplied descriptive CSVs are included as precomputed results; estimation, WTP and response-simulation outputs are generated when their scripts are run. Table 6 can be calculated from respondent-level `blockNG` shares by treatment (see `DATA_README.md`).

## Requirements

The reference environment is R 4.6.1. Package versions available for this release are listed below; they document the environment, not minimum version requirements. Numerical optimiser results may differ slightly across R/package versions, platforms and core counts.

| Package | Reference version |
|---|---|
| apollo | 0.3.7 |
| bgw | 0.1.4 |
| MASS | 7.3.65 |
| evd | 2.3.7.1 |
| dplyr | 1.2.1 |
| ggplot2 | 4.0.3 |
| patchwork | 1.3.2 |
| plotrix | 3.8.14 |

Install the required packages in R before running the scripts:

```r
install.packages(c("apollo", "bgw", "MASS", "evd", "dplyr",
                   "ggplot2", "patchwork", "plotrix"))
```

This installs the versions available from the configured R repository; it does not pin the reference versions. Descriptives and RPL WTP use base R only. Other scripts list their own dependencies in their headers.

## Running the scripts

Download the complete repository so the relative directory structure is preserved. The repository root is the folder containing this README and the data file; its name is arbitrary.

For descriptives, run from the repository root:

```sh
Rscript descriptives/descriptives_tables_2_3_4.R
```

For estimation, RPL/LCM WTP and response simulations, change into the script's folder first. For example:

```sh
cd estimations/MNL_TableA1
Rscript MNL_2alt.R
```

In RStudio, set the working directory to that script's folder before sourcing it. Scripts may clear the workspace, so use a fresh R session. MNL WTP and descriptives must be launched with `Rscript`; these locate their inputs relative to their script files. See `RUNNING.md` for commands for all 46 scripts, each starting from the repository root.

### Execution order and dependencies

- Descriptives run independently.
- Each estimation script fits one treatment. MNL and RPL estimation scripts request four cores; LCM scripts use one core.
- MNL WTP scripts re-estimate the matching MNL model in a child process on one core and use its robust covariance matrix. They do not require an earlier estimation run.
- LCM WTP scripts re-estimate the matching LCM themselves.
- RPL WTP scripts use the embedded estimates, marginal standard errors and Cholesky elements. They do not load newly fitted RPL models automatically.
- Response simulations use embedded binary benchmark parameters. They do not load newly fitted model objects automatically.

Run one script at a time. MNL WTP also writes to the corresponding estimation output folder. Running overlapping jobs could overwrite results. No master script starts all simulations automatically.

## Approximate runtimes

| Script group | Approximate runtime per script |
|---|---|
| Descriptives | Under 1 minute |
| MNL estimation | 10–30 seconds |
| LCM estimation / WTP | About 10 seconds to 3 minutes, depending on treatment |
| RPL estimation, 2Alt / 3Alt / 4Alt / 5Alt / 6Alt | About 30 / 60 / 90 / 180 / 210 minutes |
| MNL WTP, including estimation | 15–60 seconds |
| RPL WTP | 3–30 seconds |
| Response simulations, 2Alt–3Alt | Broad planning estimate: 0.5–4 hours |
| Response simulations, 4Alt–6Alt | Broad planning estimate: 1–6 hours |

Estimation and WTP estimates draw on existing recorded runs; some historical RPL runs were considerably faster. Response-simulation estimates are unbenchmarked planning allowances based on 10,000 replications and nested row operations, not measured durations or guaranteed upper bounds. Hardware, memory and software versions can substantially change execution times. The LCM response scripts preallocate large error arrays and require several GB of free memory; avoid running them concurrently. No estimation or response simulation was rerun when preparing this release.

## Interpretation and replication conventions

- MNL WTP: negative ratio of fitted coefficients, with percentile intervals from 100,000 joint normal draws using the robust sampling covariance.
- RPL WTP: median of 1,000 simulated population medians, each based on 10,000 individual draws. The Cholesky factor models individual preference correlations; independent outer parameter draws omit sampling covariances between estimated distribution parameters.
- LCM WTP: demographic-membership-weighted expected WTP. Percentile ranges describe variation across respondents, not estimation uncertainty. The original scripts calculate interpolated quantiles over repeated task rows; that convention is retained here. Deduplicating respondents can change some endpoints.
- Response simulations: the fitted binary benchmark is fixed, its SQ constant is retained and all policy constants are zero. The reported 99% intervals are conditional predictive intervals (0.5th and 99.5th percentiles).
- Files written by `write.table(..., sep=" ")` are space-delimited despite their `.csv` extension. Read them with `read.table(..., header=TRUE)`. The descriptives and MNL covariance WTP outputs written by `write.csv` are comma-delimited.

Headers and documentation have been standardised without changing executable analysis expressions, fitted starting values, random seeds or simulation counts. Unused survey fields have been removed from the public data copy; retained values, row order and respondent IDs are unchanged. The descriptives were checked against the supplied outputs. Parsing and static checks do not substitute for rerunning every model or simulation.
