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
# Ensure the official daily dataset is present (download if missing)
# -------------------------------------------------------------------
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
  cat("Official daily file downloaded.\n")
  dl <- readLines(daily_file_path, warn = FALSE, encoding = "UTF-8")
  if (length(dl) > 0) {
    dl[1] <- daily_header
    writeLines(dl, daily_file_path, useBytes = TRUE)
    cat("Official daily file header replaced.\n")
  } else {
    stop("Error on download; cannot replace header (daily).")
  }
} else {
  cat("Official daily file found locally.\n")
}

# ============================================================================
# Load and Prepare Data
# ============================================================================
file_path <- "data/mzcr_no_or_first_infection.csv"

if (!file.exists(file_path)) {
  stop("Please run download_and_data_integrity.R first to generate the data file.")
}

data_raw <- read.csv(file_path, stringsAsFactors = FALSE)
cat("Raw data dimensions:", nrow(data_raw), "rows x", ncol(data_raw), "columns\n")

print(data_raw)

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

# Prepare data
data <- data_raw %>%
  mutate(
    year_of_birth = as.integer(year_of_birth_end),
    date_death = to_iso_monday_flex(Death)
  ) %>%
  filter(!is.na(year_of_birth), year_of_birth >= 1900, year_of_birth <= 2024) %>%
  select(ID, year_of_birth, date_death)

# ============================================================================
# Build weekly series (ISO weeks, Monday-start) and compare
# ============================================================================

# --- 1) Weekly deaths from the official daily file (sum of dailyDeaths) ----
daily_df <- read.csv(daily_file_path, stringsAsFactors = FALSE)

weekly_official <- daily_df %>%
  mutate(
    date = as.Date(date),
    iso_week = ISOweek::ISOweek(date),
    week_start = ISOweek::ISOweek2date(paste0(iso_week, "-1")) # Monday of ISO week
  ) %>%
  group_by(week_start) %>%
  summarise(deaths_official = sum(dailyDeaths, na.rm = TRUE), .groups = "drop") %>%
  arrange(week_start)

# --- 2) Weekly deaths from the second file (count of date_death) -----------
weekly_from_file <- data %>%
  filter(!is.na(date_death)) %>%
  mutate(
    iso_week = ISOweek::ISOweek(date_death),
    week_start = ISOweek::ISOweek2date(paste0(iso_week, "-1"))
  ) %>%
  count(week_start, name = "deaths_file") %>%
  arrange(week_start)

# --- 3) Align timelines and fill gaps with zeros ----------------------------
wk_min <- min(c(min(weekly_official$week_start, na.rm = TRUE),
                min(weekly_from_file$week_start, na.rm = TRUE)))
wk_max <- max(c(max(weekly_official$week_start, na.rm = TRUE),
                max(weekly_from_file$week_start, na.rm = TRUE)))

all_weeks <- tibble(week_start = seq.Date(wk_min, wk_max, by = "week"))

weekly_combined <- all_weeks %>%
  left_join(weekly_official, by = "week_start") %>%
  left_join(weekly_from_file, by = "week_start") %>%
  replace_na(list(deaths_official = 0, deaths_file = 0))

# --- 4) Long format for ggplot ---------------------------------------------
weekly_long <- weekly_combined %>%
  pivot_longer(
    cols = c(deaths_official, deaths_file),
    names_to = "series",
    values_to = "deaths"
  ) %>%
  mutate(series = recode(
    series,
    deaths_official = "Official (sum of dailyDeaths)",
    deaths_file     = "Weekly (from date_death)"
  ))

# --- 5) Plot with x breaks every 8 weeks -----------------------------------
# Columns for official aggregate, line for file-based weekly deaths
ggplot(weekly_combined, aes(x = week_start)) +
  geom_col(aes(y = deaths_official, fill = "Official (sum of daily deaths)"), alpha = 0.6) +
  geom_line(aes(y = deaths_file, color = "Weekly (from MZCR)"), linewidth = 1) +
  scale_x_date(date_breaks = "8 weeks", date_labels = "%G-W%V") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_discrete(name = NULL) +
  scale_color_discrete(name = NULL) +
  labs(
    title = "Weekly COVID-19 deaths (ISO weeks, Mon–Sun)",
    subtitle = "Bars: Official (sum of daily deaths) | Line: Weekly (from MZCR)",
    x = "ISO week (Monday start)",
    y = "Deaths"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )
