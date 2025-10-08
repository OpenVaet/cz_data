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
print(df)

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
df_aug <- df %>%
  mutate(
    week_date_of_positivity_result   = to_iso_monday_flex(DateOfResult),
    week_date_of_positivity = to_iso_monday_flex(DateOfPositivity)
  )
print(df_aug)

# ===============================================================
# Compare weekly totals: daily dataset vs record-level dataset
# ===============================================================

# --- Ensure the daily dataset is present (download if needed) ---
daily_file_path <- "data/nakazeni_vyleceni_umrti_testy.csv"
daily_url       <- "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakazeni-vyleceni-umrti-testy.csv"
daily_header    <- paste0(
  'date,',
  'cumulativeInfected,',
  'cumulativeRecovered,',
  'cumulativeDeaths,',
  'cumulativePcrTests,',
  'cumulativeAntigenTests,',
  'dailyInfected,',
  'dailyRecovered,',
  'dailyDeaths,',
  'dailyPcrTests,',
  'dailyAntigenTests,',
  'cumulativePrimaryInfections,',
  'cumulativeReinfections,',
  'dailyPrimaryInfections,',
  'dailyReinfections'
)

if (!file.exists(daily_file_path)) {
  options(timeout = 1200)
  download.file(daily_url, daily_file_path, mode = "wb")
  message("Daily CZ data file downloaded.")
  dl <- readLines(daily_file_path, warn = FALSE, encoding = "UTF-8")
  stopifnot(length(dl) > 0)
  dl[1] <- daily_header
  writeLines(dl, daily_file_path, useBytes = TRUE)
  message("Daily CZ header replaced.")
}

# --- Read & aggregate the daily dataset to ISO Monday weeks ---
df_daily <- read.csv(daily_file_path, header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(date = lubridate::ymd(date))

# Prefer primary+reinfections when available; otherwise fallback to dailyInfected
daily_pri <- suppressWarnings(as.numeric(df_daily$dailyPrimaryInfections))
daily_rei <- suppressWarnings(as.numeric(df_daily$dailyReinfections))
daily_inf <- suppressWarnings(as.numeric(df_daily$dailyInfected))
sum_pri_rei <- ifelse(is.na(daily_pri) & is.na(daily_rei),
                      NA_real_,
                      rowSums(cbind(daily_pri, daily_rei), na.rm = TRUE))
daily_total <- ifelse(is.na(sum_pri_rei), daily_inf, sum_pri_rei)

weekly_daily <- df_daily %>%
  mutate(
    daily_total_cases = daily_total,
    week_date = lubridate::floor_date(date, unit = "week", week_start = 1) # ISO Monday
  ) %>%
  group_by(week_date) %>%
  summarise(aggregatedWeeklyCases = sum(daily_total_cases, na.rm = TRUE), .groups = "drop") %>%
  arrange(week_date)

weekly_records <- df_aug %>%
  mutate(
    case_date     = week_date_of_positivity,
    week_date     = lubridate::floor_date(case_date, unit = "week", week_start = 1)
  ) %>%
  filter(!is.na(week_date)) %>%
  count(week_date, name = "weeklyInfections") %>%
  arrange(week_date)

print(weekly_records)

# --- Join & inspect ---
cmp <- weekly_daily %>%
  full_join(weekly_records, by = "week_date") %>%
  arrange(week_date) %>%
  mutate(diff = aggregatedWeeklyCases - weeklyInfections)

print(head(cmp, 10))
print(tail(cmp, 10))

# Quick table of largest absolute gaps
cmp %>%
  mutate(abs_diff = abs(diff)) %>%
  arrange(desc(abs_diff)) %>%
  slice_head(n = 10) %>%
  knitr::kable(format = "simple", digits = 0,
               col.names = c("Week (Mon)", "Daily→Weekly", "Record-level", "Diff", "Abs diff")) %>%
  print()

# --- Plot: bars (daily aggregated) vs line (record-level) ---
cmp_long <- cmp %>%
  select(
    week_date,
    `Aggregated from daily data` = aggregatedWeeklyCases,
    `Record-level dataset (rows = infections)` = weeklyInfections
  ) %>%
  tidyr::pivot_longer(-week_date, names_to = "series", values_to = "cases")

ggplot(cmp_long, aes(x = week_date, y = cases, group = series)) +
  geom_col(data = dplyr::filter(cmp_long, series == "Aggregated from daily data"),
           aes(y = cases), alpha = 0.6) +
  geom_line(data = dplyr::filter(cmp_long, series == "Record-level dataset (rows = infections)"),
            linewidth = 1) +
  scale_x_date(
    breaks = seq(min(cmp_long$week_date, na.rm = TRUE),
                 max(cmp_long$week_date, na.rm = TRUE),
                 by = "8 weeks"),
    labels = function(x) strftime(x, "%G-%V")
  ) +
  scale_y_continuous(labels = scales::label_number(big.mark = " ")) +
  labs(
    title = "Czechia — Weekly COVID-19 Cases: Daily Aggregation vs Record-level Dataset",
    x = "ISO Week (YYYY-WW, Monday)",
    y = "Cases",
    caption = "Bars: daily dataset aggregated to ISO weeks. Line: count of rows in record-level dataset by ISO week (DateOfResult preferred)."
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())
