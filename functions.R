# functions.R - small reusable pieces, used by analysis.R and tests.R
library(tidyverse)
library(binom)

# Day 0 = index date, day 365 = index + 365. Both ends included (366 dates).
in_window <- function(date, index_date) {
  !is.na(date) & !is.na(index_date) & date >= index_date & date <= index_date + 365
}

# Effective end of observation = earliest of recorded end, death date, cutoff
eff_end <- function(rec_end, death, cutoff = as.Date("2023-12-31")) {
  if_else(is.na(rec_end), as.Date(NA), pmin(rec_end, death, cutoff, na.rm = TRUE))
}

# Crosswalk must have no missing values and no repeated IDs in any column
is_one_to_one <- function(link) {
  !any(is.na(link)) &&
    !anyDuplicated(link$patient_id) &&
    !anyDuplicated(link$claims_id) &&
    !anyDuplicated(link$ehr_id)
}

# One row per patient x treatment modality, for treatment events in days 0-365.
#   events:   <id_col>, event_date, code
#   code_map: source, code, event_type, modality
#   id_map:   <id_col>, patient_id, index_date   (one row per source ID)
treated_patients <- function(events, code_map, src, id_map, id_col) {
  treatment_codes <- code_map |>
    filter(source == src, event_type == "treatment") |>
    select(code, modality)

  events |>
    inner_join(treatment_codes, by = "code") |>                 # keeps treatment codes only (PSA drops out)
    inner_join(id_map, by = id_col, relationship = "many-to-one") |>
    filter(in_window(event_date, index_date)) |>
    distinct(patient_id, modality)                              # repeats collapse to one row
}

# Wilson 95% interval for a proportion, returned in percent
wilson <- function(x, n) {
  b <- binom.confint(x, n, conf.level = 0.95, methods = "wilson")
  tibble(pct = 100 * b$mean, lo = 100 * b$lower, hi = 100 * b$upper)
}

# One table row: view name, number treated, n, percent and CI
prop_row <- function(view, x, n) {
  bind_cols(tibble(view = view, treated = x, n = n), wilson(x, n))
}
