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
