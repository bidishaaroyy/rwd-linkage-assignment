# analysis.R - the whole analysis, top to end
library(tidyverse)
library(binom)
source("functions.R")
dir.create("output", showWarnings = FALSE)
cutoff <- as.Date("2023-12-31")

# =====================================================================
# 1. LOAD (everything as text first, so bad values are not hidden)
# =====================================================================
read_text <- function(f) {
  read_csv(file.path("data", f), col_types = cols(.default = col_character()),
           show_col_types = FALSE)
}
patients_raw <- read_text("patients.csv")
link         <- read_text("patient_linkage.csv")
cl_pat_raw   <- read_text("claims_patients.csv")
ehr_pat_raw  <- read_text("ehr_patients.csv")
cl_ev_raw    <- read_text("claims_events.csv")
ehr_ev_raw   <- read_text("ehr_events.csv")
code_map     <- read_text("code_map.csv")

# =====================================================================
# 2. ISSUE LOG (one summary row per problem type, with up to 5 example IDs)
# =====================================================================
issues <- tibble()
log_issue <- function(file, ids, column, values, issue, action, reason, impact) {
  if (length(ids) == 0) return(invisible())
  issues <<- bind_rows(issues, tibble(
    file = file,
    record_id = paste(head(ids, 5), collapse = ";"),
    column = column,
    original_value = paste(head(values, 5), collapse = ";"),
    issue = paste0(issue, " (n=", length(ids), ")"),
    action = action, reason = reason, likely_impact = impact))
}

# ---- 2a. Duplicate IDs ----
check_dups <- function(df, file, col) {
  d <- df[[col]][duplicated(df[[col]])]
  log_issue(file, d, col, d, "Duplicate ID", "Stop and review",
            "Duplicates would multiply rows in joins", "Inflated counts")
}
check_dups(patients_raw, "patients.csv", "patient_id")
check_dups(link, "patient_linkage.csv", "patient_id")
check_dups(link, "patient_linkage.csv", "claims_id")
check_dups(link, "patient_linkage.csv", "ehr_id")
check_dups(cl_pat_raw, "claims_patients.csv", "claims_id")
check_dups(ehr_pat_raw, "ehr_patients.csv", "ehr_id")
if (!is_one_to_one(link)) {
  write_csv(issues, "output/data_issues.csv")
  stop("Crosswalk is not one-to-one - see output/data_issues.csv")
}
stopifnot(!anyDuplicated(code_map[c("source", "code")]))

# ---- 2b. Unmatched IDs ----
check_unmatched <- function(ids, reference, file, col, ref_name) {
  u <- setdiff(unique(ids), reference)
  log_issue(file, u, col, u, paste("ID not found in", ref_name),
            "Not joined; patient fails eligibility or event ignored",
            "No valid link (no guessing allowed)", "Some records dropped")
}
check_unmatched(patients_raw$patient_id, link$patient_id, "patients.csv", "patient_id", "crosswalk")
check_unmatched(link$claims_id, cl_pat_raw$claims_id, "patient_linkage.csv", "claims_id", "claims_patients")
check_unmatched(link$ehr_id, ehr_pat_raw$ehr_id, "patient_linkage.csv", "ehr_id", "ehr_patients")
check_unmatched(cl_ev_raw$claims_id, cl_pat_raw$claims_id, "claims_events.csv", "claims_id", "claims_patients")
check_unmatched(ehr_ev_raw$ehr_id, ehr_pat_raw$ehr_id, "ehr_events.csv", "ehr_id", "ehr_patients")

# ---- 2c. Dates: parse, and log anything that fails ----
check_date <- function(df, file, id_col, col, required) {
  parsed <- ymd(df[[col]], quiet = TRUE)
  bad <- if (required) is.na(parsed) else (!is.na(df[[col]]) & is.na(parsed))
  log_issue(file, df[[id_col]][bad], col, df[[col]][bad],
            "Missing or unparseable date", "Set to NA (not guessed)",
            "NA fails date rules, so patient is excluded or event ignored", "Records lost")
  parsed
}

patients <- patients_raw |>
  mutate(index_date = check_date(patients_raw, "patients.csv", "patient_id", "index_date", TRUE),
         death_date = check_date(patients_raw, "patients.csv", "patient_id", "death_date", FALSE),
         age = suppressWarnings(as.integer(age_at_index))) |>
  select(patient_id, index_date, age, death_date)

cl_pat <- cl_pat_raw |>
  mutate(cl_start = check_date(cl_pat_raw, "claims_patients.csv", "claims_id", "observation_start", TRUE),
         cl_end   = check_date(cl_pat_raw, "claims_patients.csv", "claims_id", "observation_end", TRUE),
         part_d   = suppressWarnings(as.integer(part_d)),
         part_d   = if_else(part_d %in% c(0L, 1L), part_d, NA_integer_)) |>
  select(claims_id, cl_start, cl_end, part_d)

ehr_pat <- ehr_pat_raw |>
  mutate(ehr_start = check_date(ehr_pat_raw, "ehr_patients.csv", "ehr_id", "observation_start", TRUE),
         ehr_end   = check_date(ehr_pat_raw, "ehr_patients.csv", "ehr_id", "observation_end", TRUE),
         psa       = suppressWarnings(as.numeric(baseline_psa_ng_ml)),
         grade     = suppressWarnings(as.integer(grade_group))) |>
  select(ehr_id, ehr_start, ehr_end, psa, grade)

cl_ev  <- cl_ev_raw  |> mutate(event_date = check_date(cl_ev_raw,  "claims_events.csv", "claims_id", "event_date", TRUE))
ehr_ev <- ehr_ev_raw |> mutate(event_date = check_date(ehr_ev_raw, "ehr_events.csv",    "ehr_id",    "event_date", TRUE))

# ---- 2d. Value and logic checks ----
bad_age <- which(is.na(patients$age) | patients$age < 0 | patients$age > 110)
log_issue("patients.csv", patients$patient_id[bad_age], "age_at_index", patients_raw$age_at_index[bad_age],
          "Missing or implausible age", "Left as is; fails age rule if NA", "Not guessing", "Excluded at step 1")

i <- which(!is.na(patients$death_date) & patients$death_date < patients$index_date)
log_issue("patients.csv", patients$patient_id[i], "death_date", as.character(patients$death_date[i]),
          "Death before index date", "Kept; truncation then fails the look-back step", "Cannot tell which date is wrong", "Patient excluded")
i <- which(!is.na(patients$death_date) & patients$death_date > cutoff)
log_issue("patients.csv", patients$patient_id[i], "death_date", as.character(patients$death_date[i]),
          "Death after data cutoff", "Kept; effective end capped at cutoff", "Registry only complete to cutoff", "Negligible")

i <- which(cl_pat$cl_start > cl_pat$cl_end)
log_issue("claims_patients.csv", cl_pat$claims_id[i], "observation_start", as.character(cl_pat$cl_start[i]),
          "Start after end", "Kept; fails eligibility rules naturally", "Invalid span", "Patient excluded")
i <- which(ehr_pat$ehr_start > ehr_pat$ehr_end)
log_issue("ehr_patients.csv", ehr_pat$ehr_id[i], "observation_start", as.character(ehr_pat$ehr_start[i]),
          "Start after end", "Kept; fails eligibility rules naturally", "Invalid span", "Patient excluded")

i <- which(!is.na(cl_pat_raw$part_d) & !cl_pat_raw$part_d %in% c("0", "1"))
log_issue("claims_patients.csv", cl_pat_raw$claims_id[i], "part_d", cl_pat_raw$part_d[i],
          "part_d is not 0/1", "Set to NA", "Cannot interpret", "Part D split only")

i <- which(!is.na(ehr_pat$psa) & ehr_pat$psa <= 0)
log_issue("ehr_patients.csv", ehr_pat$ehr_id[i], "baseline_psa_ng_ml", ehr_pat_raw$baseline_psa_ng_ml[i],
          "PSA zero or negative", "Set to NA (not imputed)", "PSA must be positive", "Counted as missing")
ehr_pat$psa[which(!is.na(ehr_pat$psa) & ehr_pat$psa <= 0)] <- NA

i <- which(!is.na(ehr_pat_raw$grade_group) & !ehr_pat_raw$grade_group %in% as.character(1:5))
log_issue("ehr_patients.csv", ehr_pat$ehr_id[i], "grade_group", ehr_pat_raw$grade_group[i],
          "Grade not 1-5", "Set to NA (shown as Missing)", "Invalid value", "Counted as missing")
ehr_pat$grade[i] <- NA

# expected blanks: summarised, not an error
log_issue("patients.csv", patients$patient_id[is.na(patients$death_date)], "death_date", "",
          "Blank death date (expected)", "Treated as alive at 2023-12-31", "Complete registry assumed", "None")
log_issue("ehr_patients.csv", ehr_pat$ehr_id[is.na(ehr_pat$psa)], "baseline_psa_ng_ml", "",
          "Missing PSA", "Left missing, not imputed", "Per brief", "Reported in data-fitness section")
log_issue("ehr_patients.csv", ehr_pat$ehr_id[is.na(ehr_pat$grade)], "grade_group", "",
          "Missing grade", "Own 'Missing' category", "Per brief", "Reported in data-fitness section")

# ---- 2e. Event checks ----
log_issue("claims_events.csv", which(duplicated(cl_ev_raw)), "(row number)", "",
          "Exact duplicate event rows", "Kept; collapsed to one flag per patient", "Repeats are expected", "None")
log_issue("ehr_events.csv", which(duplicated(ehr_ev_raw)), "(row number)", "",
          "Exact duplicate event rows", "Kept; collapsed to one flag per patient", "Repeats are expected", "None")

for (s in c("claims", "ehr")) {
  ev <- if (s == "claims") cl_ev else ehr_ev
  id <- if (s == "claims") "claims_id" else "ehr_id"
  unmapped <- anti_join(ev, filter(code_map, source == s), by = "code")
  log_issue(paste0(s, "_events.csv"), unmapped[[id]], "code", unmapped$code,
            "Event code not in code_map", "Not counted as treatment", "No mapping; not guessing", "Possible undercount")

  x <- ev |>
    inner_join(select(link, patient_id, all_of(id)), by = id) |>
    inner_join(select(patients, patient_id, death_date), by = "patient_id")
  late <- filter(x, !is.na(death_date), event_date > death_date)
  log_issue(paste0(s, "_events.csv"), late[[id]], "event_date", as.character(late$event_date),
            "Event after death date", "Kept in file; cannot enter capture-cohort flags (see note below)",
            "Implausible", "None for capture cohort")
  after <- filter(x, event_date > cutoff)
  log_issue(paste0(s, "_events.csv"), after[[id]], "event_date", as.character(after$event_date),
            "Event after data cutoff", "Kept in file; cannot enter capture-cohort flags (see note below)",
            "Implausible", "None for capture cohort")
}
# NOTE: capture-cohort patients are observed (and alive) through day 365, so any event
# in days 0-365 automatically lies inside both observation spans and before death.

write_csv(issues, "output/data_issues.csv")

# =====================================================================
# 3. LINK AND BUILD COHORTS
# =====================================================================
base <- patients |>
  left_join(link, by = "patient_id") |>
  left_join(cl_pat, by = "claims_id") |>
  left_join(ehr_pat, by = "ehr_id") |>
  mutate(cl_eff  = eff_end(cl_end,  death_date),     # truncated at death and cutoff
         ehr_eff = eff_end(ehr_end, death_date),
         day365  = index_date + 365)
stopifnot(nrow(base) == nrow(patients))              # joins did not multiply rows

step1    <- filter(base, age >= 66)
baseline <- filter(step1, cl_start <= index_date - 365, ehr_start <= index_date - 365,
                   cl_eff >= index_date, ehr_eff >= index_date)
capture  <- filter(baseline, cl_eff >= day365, ehr_eff >= day365)
stopifnot(all(baseline$day365 <= cutoff))            # registry horizon check

flow <- tibble(step = c("Roster", "1. Age >= 66", "2. Look-back (baseline cohort, used for mortality)",
                        "3. Observed through day 365 (capture cohort, used for treatment)"),
               remaining = c(nrow(base), nrow(step1), nrow(baseline), nrow(capture))) |>
  mutate(excluded = coalesce(lag(remaining) - remaining, 0L))
write_csv(flow, "output/table1_cohort_flow.csv")
print(flow)

# =====================================================================
# 4. TREATMENT CAPTURE (capture cohort)
# =====================================================================
cl_map  <- select(base, claims_id, patient_id, index_date)
ehr_map <- select(base, ehr_id, patient_id, index_date)
cl_t  <- treated_patients(cl_ev,  code_map, "claims", cl_map,  "claims_id")
ehr_t <- treated_patients(ehr_ev, code_map, "ehr",    ehr_map, "ehr_id")

cap <- capture |>
  mutate(claims = patient_id %in% cl_t$patient_id,
         ehr    = patient_id %in% ehr_t$patient_id,
         linked = claims | ehr)
n <- nrow(cap)

table2 <- bind_rows(prop_row("Claims", sum(cap$claims), n),
                    prop_row("EHR", sum(cap$ehr), n),
                    prop_row("Linked (either)", sum(cap$linked), n)) |>
  mutate(view = factor(view, levels = c("Claims", "EHR", "Linked (either)")))
write_csv(table2, "output/table2_capture.csv")
print(table2)

# Agreement: four groups that add up to n
agreement <- cap |>
  mutate(group = case_when(claims & ehr ~ "Both sources",
                           claims ~ "Claims only",
                           ehr ~ "EHR only",
                           TRUE ~ "Neither"),
         group = factor(group, levels = c("Both sources", "Claims only", "EHR only", "Neither"))) |>
  count(group, .drop = FALSE)
stopifnot(sum(agreement$n) == n)
write_csv(agreement, "output/table_agreement.csv")
print(agreement)

# Linked minus claims (percentage points) should equal the EHR-only share
diff_pp <- table2$pct[3] - table2$pct[1]
stopifnot(isTRUE(all.equal(diff_pp, 100 * agreement$n[3] / n)))
write_csv(tibble(linked_minus_claims_pp = diff_pp), "output/capture_difference.csv")

# ---- Plot A ----
plotA <- ggplot(table2, aes(x = view, y = pct, ymin = lo, ymax = hi)) +
  geom_pointrange(linewidth = 0.9) +
  geom_text(aes(label = sprintf("%.1f%% (%d/%d)", pct, treated, n)), nudge_x = 0.25, size = 3.6) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(title = "Treatment recorded in days 0-365 after diagnosis",
       subtitle = paste0("Capture cohort, n = ", n, ". Points = percent treated; bars = Wilson 95% CI"),
       x = NULL, y = "% of patients with at least one treatment recorded",
       caption = "PSA tests are not counted as treatment. Intervals show sampling variation only.") +
  theme_minimal(base_size = 12)
ggsave("output/plotA_capture.png", plotA, width = 7.5, height = 5, dpi = 300)

# =====================================================================
# 5. TREATMENT PATTERNS (linked records, capture cohort)
# =====================================================================
linked_t <- bind_rows(cl_t, ehr_t) |> distinct(patient_id, modality)

per_patient <- linked_t |>
  group_by(patient_id) |>
  summarise(n_modalities = n_distinct(modality), first_modality = first(modality))

levels_pat <- c("No recorded treatment", "Prostatectomy only", "Radiation only",
                "Systemic therapy only", "Multiple modalities")
patterns <- cap |>
  select(patient_id) |>
  left_join(per_patient, by = "patient_id") |>
  mutate(n_modalities = coalesce(n_modalities, 0L),
         group = case_when(n_modalities == 0 ~ "No recorded treatment",
                           n_modalities >= 2 ~ "Multiple modalities",
                           first_modality == "prostatectomy" ~ "Prostatectomy only",
                           first_modality == "radiation" ~ "Radiation only",
                           first_modality == "systemic" ~ "Systemic therapy only"),
         group = factor(group, levels = levels_pat)) |>
  count(group, .drop = FALSE) |>
  mutate(pct = 100 * n / !!n)
stopifnot(sum(patterns$n) == n)
write_csv(patterns, "output/table_patterns.csv")
print(patterns)

plotB <- ggplot(patterns, aes(x = fct_rev(group), y = n)) +
  geom_col(fill = "#2c7fb8", width = 0.7) +
  geom_text(aes(label = sprintf("%d (%.1f%%)", n, pct)), hjust = -0.1, size = 3.6) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(title = "Initial treatment pattern, days 0-365 (linked claims + EHR)",
       subtitle = paste0("Capture cohort, n = ", n, ". Groups are mutually exclusive and add up to ", n),
       x = NULL, y = "Number of patients",
       caption = "Observed combinations only; not sequence, switching, lines of therapy or intent.") +
  theme_minimal(base_size = 12)
ggsave("output/plotB_patterns.png", plotB, width = 8.5, height = 4.8, dpi = 300)

# =====================================================================
# 6. 365-DAY MORTALITY (baseline cohort, NOT the capture cohort)
# =====================================================================
baseline <- baseline |> mutate(died = in_window(death_date, index_date))
mortality <- bind_cols(
  tibble(endpoint = "All-cause death, days 0-365 (registry)", denominator = nrow(baseline), deaths = sum(baseline$died)),
  wilson(sum(baseline$died), nrow(baseline)))

# Illustration of the WRONG denominator (capture cohort drops early deaths by design)
cap_died <- in_window(capture$death_date, capture$index_date)
mortality_wrong <- bind_cols(
  tibble(endpoint = "Same endpoint in capture cohort (biased, illustration only)",
         denominator = nrow(capture), deaths = sum(cap_died)),
  wilson(sum(cap_died), nrow(capture)))
write_csv(bind_rows(mortality, mortality_wrong), "output/table3_mortality.csv")
print(bind_rows(mortality, mortality_wrong))

# =====================================================================
# 7. DATA FITNESS (capture cohort)
# =====================================================================
fitness <- tibble(n_capture = n,
                  psa_median = median(cap$psa, na.rm = TRUE),
                  psa_missing_n = sum(is.na(cap$psa)),
                  psa_missing_pct = 100 * mean(is.na(cap$psa)),
                  grade_missing_n = sum(is.na(cap$grade)),
                  grade_missing_pct = 100 * mean(is.na(cap$grade)))
write_csv(fitness, "output/table_fitness.csv")
print(fitness)

# =====================================================================
# 8. Extension: does Part D explain claims missing systemic therapy?
#    Denominator: capture-cohort patients with systemic therapy in the EHR.
#    Outcome: is systemic therapy ALSO recorded in claims?
# =====================================================================
ehr_systemic <- filter(ehr_t, modality == "systemic")$patient_id
cl_systemic  <- filter(cl_t,  modality == "systemic")$patient_id

ext <- cap |>
  filter(patient_id %in% ehr_systemic) |>
  mutate(in_claims = patient_id %in% cl_systemic,
         part_d = factor(part_d, levels = c(1, 0), labels = c("Part D = 1", "Part D = 0"))) |>
  group_by(part_d) |>
  summarise(x = sum(in_claims), total = n(), .groups = "drop")
ext <- bind_cols(ext, wilson(ext$x, ext$total))
write_csv(ext, "output/table_ext_partd.csv")
print(ext)

plotC <- ggplot(ext, aes(x = part_d, y = pct, ymin = lo, ymax = hi)) +
  geom_pointrange(linewidth = 0.9) +
  geom_text(aes(label = paste0(x, "/", total)), nudge_x = 0.2) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(title = "Is EHR-recorded systemic therapy also seen in claims?",
       subtitle = "Capture-cohort patients with systemic therapy in the EHR; Wilson 95% CI",
       x = NULL, y = "% also recorded in claims") +
  theme_minimal(base_size = 12)
ggsave("output/plotC_extension.png", plotC, width = 7, height = 4.5, dpi = 300)