suppressPackageStartupMessages({
  library(dplyr)
  library(knitr)
  library(kableExtra)
  library(ggplot2)
  library(ISOweek)
  library(tidyr)
  library(lubridate)
  library(scales)
  library(RColorBrewer)
  library(htmltools)
  library(tibble)
  library(forcats)
  library(rlang)
})

# -------------------------------------------------------------------
# Paths & URLs
# -------------------------------------------------------------------
file_path <- "data/Otevrena-data-NR-26-30-COVID-19-prehled-populace-2024-01.csv"
file_url  <- "https://data.mzcr.cz/data/distribuce/402/Otevrena-data-NR-26-30-COVID-19-prehled-populace-2024-01.csv"

# New header applied to the CZ file after download
cz_new_header <- paste0(
  '"ID","Infection","Gender","YearOfBirth","DateOfPositivity","DateOfResult","Recovered","Death","Symptom","TestType","Date_First_Dose","Date_Second_Dose","Date_Third_Dose","Date_Fourth_Dose","Date_Fifth_Dose","Date_Sixth_Dose","Date_Seventh_Dose","VaccinationProductCode_First_Dose","VaccinationProductCode_Second_Dose","VaccinationProductCode_Third_Dose","VaccinationProductCode_Fourth_Dose","VaccinationProductCode_Fifth_Dose","VaccinationProductCode_Sixth_Dose","VaccinationProductCode_Seventh_Dose","PrimaryReasonForHospitalizationCOVID","binary_Hospitalization","min_Hospitalization","days_Hospitalization","max_Hospitalization","binary_ICU","min_ICU","days_ICU","max_ICU","binary_StandardWard","min_StandardWard","days_StandardWard","max_StandardWard","binary_Oxygen","min_Oxygen","days_Oxygen","max_Oxygen","binary_HFNO","min_HFNO","days_HFNO","max_HFNO","binary_MechanicalVentilation_ECMO","min_MechanicalVentilation_ECMO","days_MechanicalVentilation_ECMO","max_MechanicalVentilation_ECMO","Mutation","DateOfDeathInHealthcareFacility","Long_COVID","DCCI"'
)

dir.create("data", showWarnings = FALSE, recursive = TRUE)

# -------------------------------------------------------------------
# Ensure the CZ data file is present (download if missing) + replace header
# -------------------------------------------------------------------
if (!file.exists(file_path)) {
  options(timeout = 1200)
  download.file(file_url, file_path, mode = "wb")
  cat("CZ data file downloaded from source.\n")

  # Replace the first line with the provided header
  cz_lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  if (length(cz_lines) > 0) {
    cz_lines[1] <- cz_new_header
    writeLines(cz_lines, file_path, useBytes = TRUE)
    cat("CZ data file header replaced.\n")
  } else {
    stop("Error on download; cannot replace header.")
  }
} else {
  cat("CZ data file found locally.\n")
}

# -------------------------------------------------------------------
# Read the CZ CSV into a dataframe
# -------------------------------------------------------------------
df <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)

# -------------------------------------------------------------------
# Rows discarded when going from df -> df_base (Infection not NA and != 1)
# -------------------------------------------------------------------
df_discarded <- df %>%
  dplyr::filter(!is.na(Infection) & Infection != 1)

# Write discarded rows
dir.create("data", showWarnings = FALSE, recursive = TRUE)
write.csv(df_discarded, "data/mzcr_multiple_infections.csv", row.names = FALSE, na = "")

# Show how Infection values distribute in the discarded set
discard_summary <- df_discarded %>%
  dplyr::count(Infection, name = "n") %>%
  dplyr::arrange(desc(n))
print(discard_summary)

cat("Discarded rows count (Infection not NA and != 1):", nrow(df_discarded), "\n")

# -------------------------------------------------------------------
# Filter: keep only Infection == 1 or NA
# -------------------------------------------------------------------
df_base <- df %>%
  filter(is.na(Infection) | Infection == 1)

# ---------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------
to_iso_monday_flex <- function(x) {
  x0 <- ifelse(is.na(x), NA_character_, trimws(x))
  x0[!nzchar(x0)] <- NA_character_
  x1 <- toupper(x0)
  rx <- "^\\s*(\\d{4})[-\\s]?W?(\\d{1,2})(?:[-\\s]?([1-7]))?\\s*$"
  ok <- grepl(rx, x1)
  out <- rep(NA_character_, length(x1))
  if (any(ok)) {
    m <- regmatches(x1, regexec(rx, x1))
    for (i in which(ok)) {
      p  <- m[[i]]
      yr <- suppressWarnings(as.integer(p[2]))
      wk <- suppressWarnings(as.integer(p[3]))
      d  <- if (length(p) >= 4 && nchar(p[4])) suppressWarnings(as.integer(p[4])) else 1L
      if (!is.na(yr) && !is.na(wk) && wk >= 1L && wk <= 53L && d >= 1L && d <= 7L) {
        out[i] <- sprintf("%04d-W%02d-%d", yr, wk, d)
      }
    }
  }
  ISOweek::ISOweek2date(out)
}

# -------------------------------------------------------------------
# Handle YearOfBirth ranges and apply filtering
# -------------------------------------------------------------------
df_aug <- df_base %>%
  mutate(
    gender = as.character(Gender),
    year_of_birth_start = suppressWarnings(as.integer(sub("-.*", "", YearOfBirth))),
    year_of_birth_end = suppressWarnings(as.integer(sub(".*-", "", YearOfBirth))),
    has_first_dose = !is.na(Date_First_Dose) & nzchar(trimws(Date_First_Dose)),
    has_death = !is.na(DateOfDeathInHealthcareFacility) & nzchar(trimws(DateOfDeathInHealthcareFacility)),
    week_date_of_birth   = to_iso_monday_flex(YearOfBirth),
    week_date_of_positivity = to_iso_monday_flex(DateOfPositivity),
    death_from_death_field   = to_iso_monday_flex(Death),
    death_from_facility_field = to_iso_monday_flex(DateOfDeathInHealthcareFacility)
  )
print(df_aug)

# NA in one field & not in the other
n_na_mismatch <- sum(
  (is.na(df_aug$death_from_death_field) & !is.na(df_aug$death_from_facility_field)) |
  (!is.na(df_aug$death_from_death_field) & is.na(df_aug$death_from_facility_field))
)

# Both filled but different dates
n_both_filled_diff <- sum(
  !is.na(df_aug$death_from_death_field) &
  !is.na(df_aug$death_from_facility_field) &
  df_aug$death_from_death_field != df_aug$death_from_facility_field
)

# “Fitting” data:
#    (a) both filled and same date
n_both_filled_same <- sum(
  !is.na(df_aug$death_from_death_field) &
  !is.na(df_aug$death_from_facility_field) &
  df_aug$death_from_death_field == df_aug$death_from_facility_field
)
#    (b) both NA
n_both_na <- sum(
  is.na(df_aug$death_from_death_field) &
  is.na(df_aug$death_from_facility_field)
)

cat(
  "Death fields diagnostics:\n",
  " - NA in one & not the other:", n_na_mismatch, "\n",
  " - Both filled but different:", n_both_filled_diff, "\n",
  " - Both filled & same (fitting):", n_both_filled_same, "\n",
  " - Both NA:", n_both_na, "\n"
)

# -------------------------------------------------------------------
# Offsets when both fields are filled but different
# -------------------------------------------------------------------
offset_cases <- df_aug %>%
  dplyr::filter(
    !is.na(death_from_death_field),
    !is.na(death_from_facility_field)
  ) %>%
  dplyr::mutate(offset_days = as.integer(death_from_facility_field - death_from_death_field)) %>%
  dplyr::filter(offset_days != 0)
print(offset_cases)
write.csv(offset_cases, "data/mzcr_offset_cases.csv", row.names = FALSE, na = "")

if (nrow(offset_cases) > 0) {
  mean_signed <- mean(offset_cases$offset_days, na.rm = TRUE)
  q_signed <- quantile(offset_cases$offset_days, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  
  mean_abs <- mean(abs(offset_cases$offset_days), na.rm = TRUE)
  q_abs <- quantile(abs(offset_cases$offset_days), probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  
  cat("Offset between death fields (in days; facility - death) for rows with both filled and different:\n",
      " - Count:", nrow(offset_cases), "\n",
      " - Mean (signed):", round(mean_signed, 2), "\n",
      " - Quartiles (signed) [min, Q1, median, Q3, max]:",
      paste(round(q_signed, 2), collapse = ", "), "\n",
      " - Mean (absolute):", round(mean_abs, 2), "\n",
      " - Quartiles (absolute) [min, Q1, median, Q3, max]:",
      paste(round(q_abs, 2), collapse = ", "), "\n", sep = "")
} else {
  cat("Offset summary: no rows where both death fields are filled and different.\n")
}

# -------------------------------------------------------------------
# Breakdown of "NA in one & not the other"
# -------------------------------------------------------------------
only_facility <- df_aug %>%
  dplyr::filter(is.na(death_from_death_field) & !is.na(death_from_facility_field))

only_death <- df_aug %>%
  dplyr::filter(!is.na(death_from_death_field) & is.na(death_from_facility_field))

n_only_facility <- nrow(only_facility)
n_only_death    <- nrow(only_death)

# optional percentages within the mismatch bucket
pct_fac <- if (n_na_mismatch > 0) 100 * n_only_facility / n_na_mismatch else 0
pct_dea <- if (n_na_mismatch > 0) 100 * n_only_death    / n_na_mismatch else 0

cat(
  "   · of NA-mismatch: facility-only (DateOfDeathInHealthcareFacility present, Death missing): ",
  n_only_facility, sprintf(" (%.1f%%)\n", pct_fac),
  "   · of NA-mismatch: death-only (Death present, DateOfDeathInHealthcareFacility missing): ",
  n_only_death,    sprintf(" (%.1f%%)\n", pct_dea),
  sep = ""
)

# ---------------------------------------------------------------
# Resolve week_date_of_death by policy (±7 -> earliest, else latest)
# ---------------------------------------------------------------

SMALL_OFFSET_DAYS <- 7  # change here if you want a different tolerance

df_aug_resolved <- df_aug %>%
  dplyr::mutate(
    # offset only when both dates are present
    offset_days = dplyr::if_else(
      !is.na(death_from_death_field) & !is.na(death_from_facility_field),
      as.integer(death_from_facility_field - death_from_death_field),
      NA_integer_
    ),
    earliest_two = pmin(death_from_death_field, death_from_facility_field, na.rm = TRUE),
    latest_two   = pmax(death_from_death_field, death_from_facility_field, na.rm = TRUE),

    week_date_of_death = dplyr::case_when(
      is.na(death_from_death_field) & is.na(death_from_facility_field) ~ as.Date(NA),
      xor(is.na(death_from_death_field), is.na(death_from_facility_field)) ~
        dplyr::coalesce(death_from_death_field, death_from_facility_field),
      abs(offset_days) <= SMALL_OFFSET_DAYS ~ earliest_two,
      TRUE                                  ~ latest_two
    ),

    death_resolution_policy = dplyr::case_when(
      is.na(death_from_death_field) & is.na(death_from_facility_field) ~ "no_date",
      xor(is.na(death_from_death_field), is.na(death_from_facility_field)) ~ "single_source",
      abs(offset_days) <= SMALL_OFFSET_DAYS ~ "earliest_due_to_week_conflict",
      TRUE                                  ~ "latest_due_to_large_offset"
    ),
    age_at_death = dplyr::if_else(
      !is.na(year_of_birth_end) & !is.na(week_date_of_death),
      lubridate::year(week_date_of_death) - year_of_birth_end,
      NA_integer_
    )
  )

# -------------------------------------------------------------------
# 
# -------------------------------------------------------------------
# Parse dose dates
df_aug_resolved <- df_aug_resolved %>%
  dplyr::mutate(
    date_first_dose_parsed  = to_iso_monday_flex(Date_First_Dose),
    date_second_dose_parsed = to_iso_monday_flex(Date_Second_Dose),
    date_third_dose_parsed  = to_iso_monday_flex(Date_Third_Dose)
  )

# Rows where first dose is after the resolved death date
dose_after_death <- df_aug_resolved %>%
  dplyr::filter(!is.na(week_date_of_death), !is.na(date_first_dose_parsed)) %>%
  dplyr::mutate(delta_days_first_vs_death = as.integer(date_first_dose_parsed - week_date_of_death)) %>%
  dplyr::filter(delta_days_first_vs_death > 0L)

cat("Subjects with Date_First_Dose AFTER week_date_of_death: ",
    nrow(dose_after_death), "\n", sep = "")

# Print exactly the fields requested for analysis
dose_after_death_view <- dose_after_death %>%
  dplyr::select(ID, Date_First_Dose, Date_Second_Dose, Date_Third_Dose, DateOfDeathInHealthcareFacility, Death, week_date_of_death,
                delta_days_first_vs_death) %>%
  tibble::as_tibble() %>%
  dplyr::arrange(delta_days_first_vs_death)

print(dose_after_death_view, n = Inf, width = Inf)

# -------------------------------------------------------------------
# Resolve vaccination-after-death conflicts:
# shift week_date_of_death -> Monday of ISO week 1 in (year(latest dose) + 1)
# -------------------------------------------------------------------

# Parse remaining doses (4..7) and compute latest across all 1..7 doses
df_aug_resolved <- df_aug_resolved %>%
  dplyr::mutate(
    date_fourth_dose_parsed  = to_iso_monday_flex(Date_Fourth_Dose),
    date_fifth_dose_parsed   = to_iso_monday_flex(Date_Fifth_Dose),
    date_sixth_dose_parsed   = to_iso_monday_flex(Date_Sixth_Dose),
    date_seventh_dose_parsed = to_iso_monday_flex(Date_Seventh_Dose)
  )

# latest over all doses — work numerically to handle all-NA rows cleanly
latest_dose_num <- pmax(
  as.numeric(df_aug_resolved$date_first_dose_parsed),
  as.numeric(df_aug_resolved$date_second_dose_parsed),
  as.numeric(df_aug_resolved$date_third_dose_parsed),
  as.numeric(df_aug_resolved$date_fourth_dose_parsed),
  as.numeric(df_aug_resolved$date_fifth_dose_parsed),
  as.numeric(df_aug_resolved$date_sixth_dose_parsed),
  as.numeric(df_aug_resolved$date_seventh_dose_parsed),
  na.rm = TRUE
)
latest_dose_num[!is.finite(latest_dose_num)] <- NA_real_
latest_dose_parsed <- as.Date(latest_dose_num, origin = "1970-01-01")

df_aug_resolved <- df_aug_resolved %>%
  dplyr::mutate(latest_dose_parsed = latest_dose_parsed)

# Detect conflicts: any dose after the current week_date_of_death
vax_conflicts <- df_aug_resolved %>%
  dplyr::filter(!is.na(week_date_of_death), !is.na(latest_dose_parsed),
                latest_dose_parsed > week_date_of_death) %>%
  dplyr::mutate(
    delta_days_latest_vs_death = as.integer(latest_dose_parsed - week_date_of_death),
    week_date_of_death_before  = week_date_of_death,
    week_date_of_death_new     = ISOweek::ISOweek2date(sprintf("%04d-W01-1",
                                   lubridate::year(latest_dose_parsed) + 1L)),
    vax_resolution_policy      = "shifted_due_to_vax_after_death"
  )

cat("Vaccination-after-death conflicts (any dose > death): ",
    nrow(vax_conflicts), "\n", sep = "")

# Save detailed trail for these corrections
vax_after_death_resolutions <- vax_conflicts %>%
  dplyr::select(
    ID,
    Date_First_Dose, Date_Second_Dose, Date_Third_Dose,
    Date_Fourth_Dose, Date_Fifth_Dose, Date_Sixth_Dose, Date_Seventh_Dose,
    latest_dose_parsed,
    Death, DateOfDeathInHealthcareFacility,
    week_date_of_death_before, week_date_of_death_new,
    delta_days_latest_vs_death, vax_resolution_policy
  )
write.csv(vax_after_death_resolutions,
          "data/mzcr_vax_after_death_resolutions.csv",
          row.names = FALSE, na = "")
cat("Wrote: data/mzcr_vax_after_death_resolutions.csv\n")

# Apply change into the main frame + recompute age_at_death
df_aug_resolved <- df_aug_resolved %>%
  dplyr::left_join(vax_conflicts %>% dplyr::select(ID, week_date_of_death_new),
                   by = "ID") %>%
  dplyr::mutate(
    week_date_of_death = dplyr::coalesce(week_date_of_death_new, week_date_of_death),
    death_resolution_policy_final = dplyr::if_else(!is.na(week_date_of_death_new),
                                                   "shifted_due_to_vax_after_death",
                                                   death_resolution_policy),
    age_at_death = dplyr::if_else(
      !is.na(year_of_birth_end) & !is.na(week_date_of_death),
      lubridate::year(week_date_of_death) - year_of_birth_end,
      NA_integer_
    )
  ) %>%
  dplyr::select(-week_date_of_death_new)

# Summary of resolution policies
policy_summary <- df_aug_resolved %>%
  dplyr::count(death_resolution_policy, name = "n") %>%
  dplyr::arrange(dplyr::desc(n))
print(policy_summary)

cat("Resolution counts — earliest_due_to_week_conflict: ",
    policy_summary$n[policy_summary$death_resolution_policy == "earliest_due_to_week_conflict"] %||% 0,
    " | latest_due_to_large_offset: ",
    policy_summary$n[policy_summary$death_resolution_policy == "latest_due_to_large_offset"] %||% 0,
    " | single_source: ",
    policy_summary$n[policy_summary$death_resolution_policy == "single_source"] %||% 0,
    " | no_date: ",
    policy_summary$n[policy_summary$death_resolution_policy == "no_date"] %||% 0, "\n", sep = "")

# Export conflicting cases with decision taken
resolved_conflicts <- df_aug_resolved %>%
  dplyr::filter(!is.na(offset_days) & offset_days != 0) %>%
  dplyr::select(ID,
                death_from_death_field, death_from_facility_field,
                offset_days, week_date_of_death, death_resolution_policy)
write.csv(resolved_conflicts, "data/mzcr_death_resolution_cases.csv", row.names = FALSE, na = "")

# -------------------------------------------------------------------
# Missingness for YearOfBirth and Gender (NA and NA-or-blank)
# -------------------------------------------------------------------
is_missing <- function(x) {
  x_chr <- as.character(x)
  is.na(x_chr) | !nzchar(trimws(x_chr))
}

report_missing <- function(d, name) {
  n_all <- nrow(d)
  
  # derive numeric year if not already present
  yob_num <- if ("year_of_birth_end" %in% names(d)) {
    d$year_of_birth_end
  } else {
    suppressWarnings(as.integer(sub("-.*", "", d$YearOfBirth)))
  }
  
  na_yob_num  <- sum(is.na(yob_num))
  na_gender   <- sum(is.na(d$Gender))
  
  cat(sprintf(
    "[%s] rows=%d\n  - YearOfBirth (non-parsing -> NA): %d (%.2f%%)\n  - Gender: NA=%d (%.2f%%)\n",
    name, n_all,
    na_yob_num, if (n_all) 100*na_yob_num/n_all else 0,
    na_gender,  if (n_all) 100*na_gender/n_all else 0
  ))
}

report_missing(df_base,          "df_base (<= first infection)")

# -------------------------------------------------------------------
# Write the post-filtered augmented data
# -------------------------------------------------------------------
write.csv(df_aug_resolved, "data/mzcr_no_or_first_infection.csv", row.names = FALSE, na = "")
cat("Wrote:\n - data/mzcr_multiple_infections.csv (discarded rows)\n - data/mzcr_no_or_first_infection.csv (kept + augmented rows)\n")

# ================================================================
# 0) Extra parsed dates & quick flags
# ================================================================
df_aug_resolved <- df_aug_resolved %>%
  mutate(
    date_result_parsed   = to_iso_monday_flex(DateOfResult),
    date_recovered_parsed= to_iso_monday_flex(Recovered),
    
    has_pos = !is.na(week_date_of_positivity),
    has_any_vax = coalesce(has_first_dose, FALSE) |
      !is.na(date_second_dose_parsed) |
      !is.na(date_third_dose_parsed)  |
      !is.na(date_fourth_dose_parsed) |
      !is.na(date_fifth_dose_parsed)  |
      !is.na(date_sixth_dose_parsed)  |
      !is.na(date_seventh_dose_parsed),
    
    has_any_hosp = (binary_Hospitalization == 1L) |
      (binary_ICU == 1L) |
      (binary_StandardWard == 1L) |
      (binary_Oxygen == 1L) |
      (binary_HFNO == 1L) |
      (binary_MechanicalVentilation_ECMO == 1L),
    
    has_any_death = !is.na(death_from_death_field) | !is.na(death_from_facility_field),
    has_dcci = !is.na(DCCI) & nzchar(trimws(DCCI)),
    yob_missing = is.na(year_of_birth_end)
  )

# ================================================================
# 1) Provenance classification (ISIN / NRHZS-only / LPZ-only / etc.)
#    -> Quantifies the "no infection & no vaccination" population
# ================================================================
df_aug_resolved <- df_aug_resolved %>%
  mutate(
    provenance = dplyr::case_when(
      has_pos & has_any_vax                       ~ "ISIN+VAX",
      has_pos & !has_any_vax                      ~ "ISIN_only",
      !has_pos & has_any_vax                      ~ "Vaccination_only",
      !has_pos & !has_any_vax & has_dcci          ~ "NRHZS_only",
      !has_pos & !has_any_vax & !has_dcci & has_any_death ~ "LPZ_only",
      TRUE                                        ~ "Unknown_origin"
    )
  )

prov_breakdown <- df_aug_resolved %>%
  count(provenance, name = "n") %>% arrange(desc(n))
print(prov_breakdown)

prov_yob_missing <- df_aug_resolved %>%
  group_by(provenance, yob_missing) %>%
  summarise(n = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = yob_missing, values_from = n, values_fill = 0) %>%
  rename(yob_missing_FALSE = `FALSE`, yob_missing_TRUE = `TRUE`)
print(prov_yob_missing)

write.csv(prov_breakdown,     "data/mzcr_provenance_counts.csv", row.names = FALSE)
write.csv(prov_yob_missing,   "data/mzcr_provenance_yob_missing.csv", row.names = FALSE)

# ================================================================
# 2) Explicit "no infection & no vaccination" cohort anatomy
# ================================================================
no_inf_no_vax <- df_aug_resolved %>%
  filter((is.na(Infection) | Infection == 0L),
         !has_pos, !has_any_vax)

no_inf_no_vax_summary <- no_inf_no_vax %>%
  summarise(
    rows = n(),
    with_dcci = sum(has_dcci),
    with_long_covid = sum(!is.na(Long_COVID) & nzchar(trimws(Long_COVID))),
    with_any_hosp = sum(has_any_hosp),
    with_any_death = sum(has_any_death),
    yob_missing = sum(yob_missing)
  )
print(no_inf_no_vax_summary)

write.csv(no_inf_no_vax %>% select(ID, DCCI, Long_COVID, has_any_hosp, has_any_death, YearOfBirth),
          "data/mzcr_no_inf_no_vax_details.csv", row.names = FALSE)

# ================================================================
# 3) Contradictions & coherence checks
# ================================================================

# 3a) Infection==0 but positivity/vax/hospitalization present
contradictions_inf0 <- df_aug_resolved %>%
  filter(!is.na(Infection) & Infection == 0L & (has_pos | has_any_vax | has_any_hosp))
cat("Contradictions (Infection==0 with pos/vax/hosp): ", nrow(contradictions_inf0), "\n")
write.csv(contradictions_inf0, "data/mzcr_contradictions_inf0.csv", row.names = FALSE)

# 3b) Positivity vs Result date order
pos_vs_result <- df_aug_resolved %>%
  filter(!is.na(week_date_of_positivity), !is.na(date_result_parsed)) %>%
  mutate(delta_days = as.integer(date_result_parsed - week_date_of_positivity))
summary_pos_vs_result <- pos_vs_result %>%
  summarise(n = n(),
            neg = sum(delta_days < 0),
            gt_14 = sum(delta_days > 14),
            min = min(delta_days, na.rm = TRUE),
            q50 = median(delta_days, na.rm = TRUE),
            max = max(delta_days, na.rm = TRUE))
print(summary_pos_vs_result)
write.csv(pos_vs_result %>% filter(delta_days < 0 | delta_days > 14),
          "data/mzcr_pos_vs_result_outliers.csv", row.names = FALSE)

# 3c) Hospitalization flags vs dates/durations
hosp_incoh <- df_aug_resolved %>%
  filter(
    (binary_Hospitalization == 1L & is.na(min_Hospitalization)) |
      (binary_ICU == 1L           & is.na(min_ICU)) |
      (binary_StandardWard == 1L  & is.na(min_StandardWard)) |
      (binary_Oxygen == 1L        & is.na(min_Oxygen)) |
      (binary_HFNO == 1L          & is.na(min_HFNO)) |
      (binary_MechanicalVentilation_ECMO == 1L & is.na(min_MechanicalVentilation_ECMO)) |
      (days_Hospitalization < 0 | days_ICU < 0 | days_StandardWard < 0 |
         days_Oxygen < 0 | days_HFNO < 0 | days_MechanicalVentilation_ECMO < 0)
  )
cat("Hospitalization inconsistencies: ", nrow(hosp_incoh), "\n")
write.csv(hosp_incoh, "data/mzcr_hospitalization_inconsistencies.csv", row.names = FALSE)

# 3d) ICU implies hospitalization (sanity)
icu_wo_hosp <- df_aug_resolved %>%
  filter(binary_ICU == 1L & (is.na(binary_Hospitalization) | binary_Hospitalization == 0L))
cat("ICU without hospitalization flag: ", nrow(icu_wo_hosp), "\n")
write.csv(icu_wo_hosp, "data/mzcr_icu_without_hosp.csv", row.names = FALSE)

# 3e) Recovered before positivity or after death (if both present)
recovery_incoh <- df_aug_resolved %>%
  filter(!is.na(date_recovered_parsed)) %>%
  filter( (!is.na(week_date_of_positivity) & date_recovered_parsed < week_date_of_positivity) |
            (!is.na(week_date_of_death)      & date_recovered_parsed > week_date_of_death) )
cat("Recovery date inconsistencies: ", nrow(recovery_incoh), "\n")
write.csv(recovery_incoh, "data/mzcr_recovery_inconsistencies.csv", row.names = FALSE)

# ================================================================
# 4) Vaccination chronology anomalies
#    - Later dose present but earlier dose missing
#    - Non-monotonic dose dates
# ================================================================
dose_cols <- c("date_first_dose_parsed","date_second_dose_parsed","date_third_dose_parsed",
               "date_fourth_dose_parsed","date_fifth_dose_parsed","date_sixth_dose_parsed",
               "date_seventh_dose_parsed")

later_no_earlier <- df_aug_resolved %>%
  mutate(
    d1 = !is.na(date_first_dose_parsed),
    d2 = !is.na(date_second_dose_parsed),
    d3 = !is.na(date_third_dose_parsed),
    d4 = !is.na(date_fourth_dose_parsed),
    d5 = !is.na(date_fifth_dose_parsed),
    d6 = !is.na(date_sixth_dose_parsed),
    d7 = !is.na(date_seventh_dose_parsed),
    gap12 = d2 & !d1,
    gap23 = d3 & !d2,
    gap34 = d4 & !d3,
    gap45 = d5 & !d4,
    gap56 = d6 & !d5,
    gap67 = d7 & !d6
  ) %>%
  filter(gap12 | gap23 | gap34 | gap45 | gap56 | gap67)
cat("Later dose present while prior missing: ", nrow(later_no_earlier), "\n")
write.csv(later_no_earlier %>% select(ID, starts_with("Date_")),
          "data/mzcr_vax_gaps.csv", row.names = FALSE)

# --- Define parsed dose columns (must exist in df_aug_resolved) ---
dose_parsed_cols <- c(
  "date_first_dose_parsed","date_second_dose_parsed","date_third_dose_parsed",
  "date_fourth_dose_parsed","date_fifth_dose_parsed","date_sixth_dose_parsed",
  "date_seventh_dose_parsed"
)

# If you haven't already created has_any_vax, do it now (fast + vectorized)
if (!"has_any_vax" %in% names(df_aug_resolved)) {
  df_aug_resolved <- df_aug_resolved %>%
    dplyr::mutate(
      has_any_vax = Reduce(`|`, lapply(dose_parsed_cols, function(x) !is.na(.data[[x]])))
    )
}

# Work only on rows that have any vaccination at all to cut size early
idx_vax <- which(df_aug_resolved$has_any_vax %in% TRUE)
if (length(idx_vax)) {
  # Build numeric matrix of dose dates (Date -> numeric days since epoch)
  D <- do.call(
    cbind,
    lapply(dose_parsed_cols, function(x) as.numeric(df_aug_resolved[[x]][idx_vax]))
  )
  # D is (n_vax x 7); NA stays NA
  
  # Compute pairwise “inversions”: later dose j earlier than dose i (j > i)
  combs <- utils::combn(ncol(D), 2)  # all i<j pairs
  any_bad <- rep(FALSE, nrow(D))
  bad_cnt <- integer(nrow(D))        # optional: number of inversions per row
  
  for (k in seq_len(ncol(combs))) {
    i <- combs[1, k]; j <- combs[2, k]
    # Compare only where both dates exist
    bad <- is.finite(D[, i]) & is.finite(D[, j]) & (D[, j] < D[, i])
    any_bad <- any_bad | bad
    bad_cnt <- bad_cnt + bad
  }
  
  non_mono_mask_global <- rep(FALSE, nrow(df_aug_resolved))
  non_mono_mask_global[idx_vax] <- any_bad
  
  non_monotonic_vax <- df_aug_resolved[non_mono_mask_global, , drop = FALSE]
  non_monotonic_vax$monotonic_inversion_count <- bad_cnt[any_bad]
  
  cat("Non-monotonic vaccination date order: ",
      sum(non_mono_mask_global), "\n", sep = "")
  
  # Save a compact view
  out <- dplyr::select(
    non_monotonic_vax,
    ID, dplyr::starts_with("Date_"), monotonic_inversion_count
  )
  write.csv(out, "data/mzcr_vax_non_monotonic.csv", row.names = FALSE)
} else {
  cat("Non-monotonic vaccination date order: 0\n")
}
# ---------------------------------------------------------------------------


# ================================================================
# 5) Long-COVID without recorded infection/positivity
#    (legit in claims data; quantify it explicitly)
# ================================================================
longcovid_no_pos <- df_aug_resolved %>%
  filter(!is.na(Long_COVID) & nzchar(trimws(Long_COVID)) &
           !has_pos & (is.na(Infection) | Infection == 0L))

cat("Long-COVID entries with no positivity/infection: ", nrow(longcovid_no_pos), "\n")
write.csv(longcovid_no_pos %>% select(ID, Long_COVID, DCCI, YearOfBirth, Death, DateOfDeathInHealthcareFacility),
          "data/mzcr_longcovid_without_positivity.csv", row.names = FALSE)

# ================================================================
# 6) Unknown YOB deep dive by provenance, death, and gender
# ================================================================
yob_missing_detail <- df_aug_resolved %>%
  group_by(provenance, has_any_death, Gender) %>%
  summarise(
    n = n(),
    yob_missing = sum(yob_missing),
    pct_yob_missing = 100 * yob_missing / n,
    .groups = "drop"
  ) %>%
  arrange(desc(pct_yob_missing))
print(yob_missing_detail)
write.csv(yob_missing_detail, "data/mzcr_yob_missing_detail.csv", row.names = FALSE)

# ================================================================
# 7) ID-level duplicates after your Infection<=1 filter
# ================================================================
dup_ids <- df_base %>% count(ID, name = "rows_per_id") %>% filter(rows_per_id > 1)
cat("IDs with >1 row after filter: ", nrow(dup_ids), "\n")
write.csv(dup_ids, "data/mzcr_ids_with_multiple_rows_after_filter.csv", row.names = FALSE)

# (Optional) peek differences for a few IDs
if (nrow(dup_ids) > 0) {
  sample_ids <- head(dup_ids$ID, 50)
  write.csv(df_base %>% filter(ID %in% sample_ids) %>% arrange(ID),
            "data/mzcr_multirow_id_samples.csv", row.names = FALSE)
}

# ================================================================
# 8) Extra quick tests
#    - YearOfBirth token shape
#    - Mutations present w/o positivity
# ================================================================
yob_shape <- df_base %>%
  mutate(shape = dplyr::case_when(
    is.na(YearOfBirth) | !nzchar(trimws(YearOfBirth)) ~ "blank_or_na",
    grepl("^\\d{4}-\\d{4}$", YearOfBirth) ~ "five_year_band",
    YearOfBirth == "—" ~ "dash_unknown",
    TRUE ~ "other"
  )) %>%
  count(shape, name = "n") %>% arrange(desc(n))
print(yob_shape)

mutation_wo_pos <- df_aug_resolved %>%
  filter(!is.na(Mutation) & nzchar(trimws(Mutation)) & !has_pos)
cat("Rows with Mutation set but no positivity: ", nrow(mutation_wo_pos), "\n")
write.csv(mutation_wo_pos %>% select(ID, Mutation, DateOfPositivity, TestType),
          "data/mzcr_mutation_without_positivity.csv", row.names = FALSE)

by_week <- df_aug_resolved %>%
  filter(has_any_death) %>%
  mutate(week = ISOweek::ISOweek(week_date_of_death)) %>%
  group_by(week, provenance) %>%
  summarise(n = n(),
            yob_missing = sum(yob_missing),
            .groups = "drop") %>%
  group_by(week) %>%
  mutate(pct_missing = 100 * yob_missing / n) %>%
  ungroup()

# Which provenance drives missingness each week?
driver <- by_week %>%
  group_by(week) %>%
  slice_max(order_by = pct_missing, n = 1, with_ties = FALSE) %>%
  ungroup()
print(driver %>% count(provenance, name = "weeks_as_driver") %>% arrange(desc(weeks_as_driver)))

