# Treatment capture and mortality: linked claims + EHR (synthetic data)

## How to run
1. Install packages once: `install.packages(c("tidyverse","binom","rmarkdown","knitr"))`
2. Open the project folder in R and run `source("run_all.R")` (or `Rscript run_all.R`).
3. Results appear in `output/` (tables, plots, `data_issues.csv`, `report.html`, `session_info.txt`).

Input files in `data/` are never modified.

## Files
plan.md | functions.R | tests.R (automated checks) | analysis.R | report.Rmd | run_all.R

## Automated checks (tests.R)
1. Crosswalk is one-to-one; unmatched IDs are detected.
2. Treatment on day 365 counts, day 366 does not.
3. Each patient is counted once despite repeats and records in both sources; PSA is not treatment.
4. Observation is truncated at death and cutoff; death on day 365 still reaches day 365.

## Environment
R version and package versions: R version 4.6.1 (2026-06-24 ucrt)`. Random seed: 1 (no random steps).

## Time spent
About 4 hours.

## AI disclosure
Tool: ChatGPT. Used for: assist with syntax check, report structuring, code review, and refinement of scientific language.