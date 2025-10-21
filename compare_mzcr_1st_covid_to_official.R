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

# --- Prepare cumulative % of sample (primary infections) --------------------
n_total <- nrow(df)  # original sample size

cmp_pct <- cmp %>%
  arrange(week_date) %>%
  mutate(
    weeklyInfections = tidyr::replace_na(weeklyInfections, 0L),
    cum_primary      = cumsum(weeklyInfections),
    pct_of_sample    = 100 * cum_primary / n_total
  )

# Scaling: map % to the left y-axis (counts) so we can draw a secondary axis
y_max_counts <- max(cmp_long$cases, na.rm = TRUE)
max_pct      <- max(cmp_pct$pct_of_sample, na.rm = TRUE)
coef <- if (is.finite(y_max_counts) && is.finite(max_pct) && max_pct > 0) y_max_counts / max_pct else 1

pct_line <- cmp_pct %>%
  transmute(week_date, pct_scaled = pct_of_sample * coef)

# --- Plot: bars (daily aggregated), line (record-level), dashed % line ------
ggplot(cmp_long, aes(x = week_date, y = cases, group = series)) +
  geom_col(
    data = dplyr::filter(cmp_long, series == "Aggregated from daily data"),
    aes(y = cases, fill = "Aggregated from daily data"),
    alpha = 0.6
  ) +
  geom_line(
    data = dplyr::filter(cmp_long, series == "Record-level dataset (rows = infections)"),
    aes(y = cases, color = "Record-level dataset (rows = infections)"),
    linewidth = 1
  ) +
  # dashed cumulative % of sample (right axis)
  geom_line(
    data = pct_line,
    aes(x = week_date,
        y = pct_scaled,
        color = "% of sample with ≥1 infection (cumulative)",
        group = 1),
    linewidth = 0.9,
    linetype = "dashed",
    inherit.aes = FALSE
  ) +
  scale_x_date(
    breaks = seq(min(cmp_long$week_date, na.rm = TRUE),
                 max(cmp_long$week_date, na.rm = TRUE),
                 by = "8 weeks"),
    labels = function(x) strftime(x, "%G-%V")
  ) +
  scale_y_continuous(
    labels = scales::label_number(big.mark = " "),
    sec.axis = sec_axis(~ . / coef, name = "% of sample (cumulative)")
  ) +
  scale_fill_discrete(name = NULL) +
  scale_color_discrete(name = NULL) +
  labs(
    title   = "Czechia - Weekly COVID-19 Primary Cases: Daily Aggregation vs Record-level Dataset",
    x       = "ISO Week (YYYY-WW, Monday)",
    y       = "Cases",
    caption = paste0(
      "Bars: daily dataset aggregated to ISO weeks. Line: count of rows in record-level dataset by ISO week. ",
      "Dashed line (right axis): cumulative % of the population (n=", format(n_total, big.mark = " "), ") with ≥1 infection."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  ) +
  guides(
    color = guide_legend(nrow = 1),
    fill  = guide_legend(nrow = 1)
  )

# =============================================================================
# Export weekly series for reuse (CSV + RDS)
# =============================================================================
out_dir <- "out"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Wide format: one row per week with both series and handy extras
weekly_out_wide <- cmp %>%
  arrange(week_date) %>%
  mutate(
    iso_week       = strftime(week_date, "%G-W%V"),
    weeklyInfections = tidyr::replace_na(weeklyInfections, 0L),
    cum_primary    = cumsum(weeklyInfections),
    pct_of_sample  = 100 * cum_primary / n_total
  ) %>%
  transmute(
    week_date      = as.Date(week_date),
    iso_week,
    aggregated_daily = aggregatedWeeklyCases,
    record_level     = weeklyInfections,
    diff             = aggregated_daily - record_level,
    cum_primary,
    pct_of_sample
  )

# Long (tidy) format: perfect for ggplot/Altair/etc.
weekly_out_long <- cmp_long %>%
  arrange(week_date, series) %>%
  mutate(
    iso_week = strftime(week_date, "%G-W%V")
  ) %>%
  select(week_date, iso_week, series, cases)

# Write files
write.csv(weekly_out_wide, file.path(out_dir, "weekly_covid_cases_wide.csv"), row.names = FALSE)
write.csv(weekly_out_long, file.path(out_dir, "weekly_covid_cases_long.csv"), row.names = FALSE)

# Also save as a single RDS bundle
saveRDS(
  list(
    weekly_wide = weekly_out_wide,
    weekly_long = weekly_out_long
  ),
  file.path(out_dir, "weekly_covid_cases.rds")
)

message("Saved: ",
        file.path(out_dir, "weekly_covid_cases_wide.csv"), " ; ",
        file.path(out_dir, "weekly_covid_cases_long.csv"), " ; ",
        file.path(out_dir, "weekly_covid_cases.rds"))
