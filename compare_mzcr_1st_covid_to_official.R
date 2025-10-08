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

# ============================================================================
# Load and Prepare Data
# ============================================================================
file_path <- "data/mzcr_no_or_first_infection.csv"

if (!file.exists(file_path)) {
  stop("Please run download_and_data_integrity.R first to generate the data file.")
}

df <- read.csv(file_path, stringsAsFactors = FALSE)
cat("Raw data dimensions:", nrow(df), "rows x", ncol(df), "columns\n")

# -------------------------------------------------------------------
# Filter: keep only Infection == 1 or NA
# -------------------------------------------------------------------
df_base <- df %>%
  filter(Infection == 1)

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

weekly_daily <- df_daily %>%
  mutate(
    daily_pri_cases = daily_pri,
    week_date = lubridate::floor_date(date, unit = "week", week_start = 1) # ISO Monday
  ) %>%
  group_by(week_date) %>%
  summarise(aggregatedWeeklyCases = sum(daily_pri_cases, na.rm = TRUE), .groups = "drop") %>%
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
    title = "Czechia — Weekly COVID-19 Primary Cases: Daily Aggregation vs Record-level Dataset",
    x = "ISO Week (YYYY-WW, Monday)",
    y = "Cases",
    caption = "Bars: daily dataset aggregated to ISO weeks. Line: count of rows in record-level dataset by ISO week (DateOfResult preferred)."
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())
