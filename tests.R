# tests.R - automated checks. stopifnot() stops with an error if anything is wrong.
source("functions.R")

# ---- Test 1: crosswalk is one-to-one, unmatched IDs are detected ----
good <- tibble(patient_id = c("P1", "P2"), claims_id = c("C9", "C3"), ehr_id = c("E5", "E1"))
dup_claims <- tibble(patient_id = c("P1", "P2"), claims_id = c("C9", "C9"), ehr_id = c("E5", "E1"))
stopifnot(is_one_to_one(good))
stopifnot(!is_one_to_one(dup_claims))
stopifnot(identical(setdiff(c("C9", "C7"), good$claims_id), "C7"))   # C7 is unmatched

# ---- Test 2: day 365 counts, day 366 does not (2020 is a leap year) ----
idx <- as.Date("2020-02-04")
stopifnot(idx + 365 == as.Date("2021-02-03"))
stopifnot(in_window(as.Date("2021-02-03"), idx))      # day 365 -> counts
stopifnot(!in_window(as.Date("2021-02-04"), idx))     # day 366 -> does not
stopifnot(in_window(idx, idx))                        # day 0 -> counts
stopifnot(!in_window(idx - 1, idx))                   # day -1 -> does not

# ---- Test 3: each patient is counted once ----
code_map <- tibble(source = c("claims", "claims", "ehr", "ehr"),
                   code = c("C_ADT", "C_PSA", "E_ADT", "E_PSA"),
                   event_type = c("treatment", "other", "treatment", "other"),
                   modality = c("systemic", NA, "systemic", NA))
cl_map  <- tibble(claims_id = c("C1", "C2"), patient_id = c("P1", "P2"), index_date = idx)
ehr_map <- tibble(ehr_id = c("E1", "E2"), patient_id = c("P1", "P2"), index_date = idx)
cl_ev <- tibble(claims_id = c("C1", "C1", "C1", "C2"),                 # P1: repeated ADT; P2: PSA only
                event_date = as.Date(c("2020-03-01", "2020-03-01", "2020-06-01", "2020-03-01")),
                code = c("C_ADT", "C_ADT", "C_ADT", "C_PSA"))
ehr_ev <- tibble(ehr_id = "E1", event_date = as.Date("2020-03-01"), code = "E_ADT")   # P1 also in EHR

cl_t  <- treated_patients(cl_ev,  code_map, "claims", cl_map,  "claims_id")
ehr_t <- treated_patients(ehr_ev, code_map, "ehr",    ehr_map, "ehr_id")
stopifnot(nrow(cl_t) == 1)                                   # 3 repeated rows -> 1
linked <- union(cl_t$patient_id, ehr_t$patient_id)
stopifnot(identical(linked, "P1"))                           # P1 once; P2's PSA is not treatment

# ---- Test 4: observation is cut at death and at the cutoff ----
stopifnot(eff_end(as.Date("2023-06-01"), as.Date("2021-01-01")) == as.Date("2021-01-01"))  # death first
stopifnot(eff_end(as.Date("2023-06-01"), as.Date(NA)) == as.Date("2023-06-01"))            # no death
stopifnot(eff_end(as.Date("2025-01-01"), as.Date(NA)) == as.Date("2023-12-31"))            # capped at cutoff
# a patient who dies exactly on day 365 still reaches day 365
stopifnot(eff_end(as.Date("2023-06-01"), idx + 365) >= idx + 365)

message("All tests passed.")