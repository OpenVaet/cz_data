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
file_path <- "data/nakazeni_vyleceni_umrti_testy.csv"
file_url  <- "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakazeni-vyleceni-umrti-testy.csv"

# New header applied to the CZ file after download
cz_new_header <- paste0(
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

# Parse dates
df <- df %>%
  mutate(date = as.Date(date))

# Prefer sum of primary+reinfections when available; otherwise fall back to dailyInfected
daily_pri <- suppressWarnings(as.numeric(df$dailyPrimaryInfections))
daily_rei <- suppressWarnings(as.numeric(df$dailyReinfections))
daily_inf <- suppressWarnings(as.numeric(df$dailyInfected))
sum_pri_rei <- ifelse(is.na(daily_pri) & is.na(daily_rei),
                      NA_real_,
                      rowSums(cbind(daily_pri, daily_rei), na.rm = TRUE))
daily_total <- ifelse(is.na(sum_pri_rei), daily_inf, sum_pri_rei)

# Compute ISO week label and Monday-of-week date
df_weekly <- df %>%
  mutate(
    daily_total_cases = daily_total,
    iso_week = ISOweek::ISOweek(date),                       # "YYYY-Www"
    week_date = ISOweek::ISOweek2date(paste0(iso_week, "-1")) # Monday date for the ISO week
  ) %>%
  group_by(iso_week, week_date) %>%
  summarise(aggregatedWeeklyCases = sum(daily_total_cases, na.rm = TRUE), .groups = "drop") %>%
  arrange(week_date)

# ---------------------------------------------------------------
# Load official weekly dataset (same as your first script)
# ---------------------------------------------------------------
covid_file <- "data/nakazeni_reinfekce_tyden.csv"
covid_url  <- "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakazeni-reinfekce-tyden.csv"

covid_new_header <- paste0(
  '"id","week_from","week_to","week","total_new_cases","primary_infections","first_reinfections","second_reinfections","third_and_subsequent_reinfections"'
)

if (!file.exists(covid_file)) {
  options(timeout = 1200)
  download.file(covid_url, covid_file, mode = "wb")
  covid_lines <- readLines(covid_file, warn = FALSE, encoding = "UTF-8")
  if (length(covid_lines) > 0) {
    covid_lines[1] <- covid_new_header
    writeLines(covid_lines, covid_file, useBytes = TRUE)
  } else {
    stop("Error on download; cannot replace header for weekly dataset.")
  }
}

# --- Official weekly dataset, aligned to ISO Monday ---
covid_weekly <- read.csv(covid_file, header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(
    total_new_cases = suppressWarnings(as.numeric(total_new_cases)),
    # Align to ISO week Monday regardless of what 'week_from' day is
    week_from = as.Date(week_from),
    iso_week_full = ISOweek::ISOweek(week_from),             # e.g. "2020-W10-7"
    iso_week_ymw = sub("-[1-7]$", "", iso_week_full),        # -> "2020-W10"
    week_date = ISOweek::ISOweek2date(paste0(iso_week_ymw, "-1"))  # Monday of that ISO week
  ) %>%
  transmute(
    week_date,
    reportedWeeklyCases = total_new_cases
  ) %>%
  arrange(week_date)

# --- Join by Monday-of-week only ---
cmp <- covid_weekly %>%
  full_join(df_weekly, by = "week_date") %>%
  arrange(week_date)

# Long format for a clear legend
cmp_long <- cmp %>%
  select(
    week_date,
    `Official weekly dataset` = reportedWeeklyCases,
    `Aggregated from daily data` = aggregatedWeeklyCases
  ) %>%
  tidyr::pivot_longer(-week_date, names_to = "series", values_to = "cases")

# Plot
ggplot(cmp_long, aes(x = week_date, y = cases, group = series)) +
  geom_col(data = dplyr::filter(cmp_long, series == "Official weekly dataset"),
           aes(y = cases), alpha = 0.6) +
  geom_line(data = dplyr::filter(cmp_long, series == "Aggregated from daily data"),
            linewidth = 1) +
  scale_x_date(
    breaks = seq(min(cmp_long$week_date, na.rm = TRUE),
                 max(cmp_long$week_date, na.rm = TRUE),
                 by = "8 weeks"),
    labels = function(x) strftime(x, "%G-%V")
  ) +
  scale_y_continuous(labels = scales::label_number(big.mark = " ")) +
  labs(
    title = "Czechia - COVID-19 Cases: Weekly vs Aggregated Daily",
    x = "ISO Week (YYYY-WW, Monday)",
    y = "Cases",
    caption = "Bars: official weekly dataset (nakazeni_reinfekce_tyden). Line: daily data aggregated to ISO weeks."
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())
