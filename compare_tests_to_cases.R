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
file_path <- "data/nakazeni-hospitalizace-testy.csv"
file_url  <- "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakazeni-hospitalizace-testy.csv"

# New header applied to the CZ file after download (your “tests/cases per day” table)
cz_new_header <- paste0(
  '"id","date","ageCategory","testIndication","completedVaccination","reinfections","performedTests","confirmedCases","newHospitalizations","newIcuAdmissions"'
)

dir.create("data", showWarnings = FALSE, recursive = TRUE)

# -------------------------------------------------------------------
# Ensure the CZ Tests data file is present (download if missing) + replace header
# -------------------------------------------------------------------
if (!file.exists(file_path)) {
  options(timeout = 1200)
  download.file(file_url, file_path, mode = "wb")
  cat("CZ Tests data file downloaded from source.\n")

  cz_lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  if (length(cz_lines) > 0) {
    cz_lines[1] <- cz_new_header
    writeLines(cz_lines, file_path, useBytes = TRUE)
    cat("CZ Tests data file header replaced.\n")
  } else {
    stop("Error on download; cannot replace header.")
  }
} else {
  cat("CZ Tests data file found locally.\n")
}

# -------------------------------------------------------------------
# Read the CZ “tests/cases” CSV into a dataframe & aggregate to daily
# -------------------------------------------------------------------
to_num <- function(x) suppressWarnings(as.numeric(x))

df_tests <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(
    date = ymd(date),
    performedTests  = to_num(performedTests),
    confirmedCases  = to_num(confirmedCases)
  ) %>%
  filter(!is.na(date)) %>%
  group_by(date) %>%
  summarise(
    our_daily_tests = sum(performedTests, na.rm = TRUE),
    our_daily_cases = sum(confirmedCases, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(date)

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

# -------------------------------------------------------------------
# Read official daily, build totals comparable to our file
# -------------------------------------------------------------------
df_daily <- read.csv(daily_file_path, header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(
    date = ymd(date),
    pcr  = to_num(dailyPcrTests),
    ag   = to_num(dailyAntigenTests),
    dailyInf = to_num(dailyInfected),
    pri  = to_num(dailyPrimaryInfections),
    rei  = to_num(dailyReinfections)
  )

# Use primary+reinfections when available; else fallback to dailyInfected
sum_pri_rei <- ifelse(is.na(df_daily$pri) & is.na(df_daily$rei),
                      NA_real_,
                      rowSums(cbind(df_daily$pri, df_daily$rei), na.rm = TRUE))
daily_total_cases <- ifelse(is.na(sum_pri_rei), df_daily$dailyInf, sum_pri_rei)
daily_total_tests <- rowSums(cbind(df_daily$pcr, df_daily$ag), na.rm = TRUE)

df_daily <- df_daily %>%
  transmute(
    date,
    official_daily_tests = daily_total_tests,
    official_daily_cases = daily_total_cases
  ) %>%
  filter(!is.na(date)) %>%
  arrange(date)

# -------------------------------------------------------------------
# Compare (join) and basic checks
# -------------------------------------------------------------------
cmp <- full_join(df_tests, df_daily, by = "date") %>%
  arrange(date) %>%
  mutate(
    our_positivity       = ifelse(our_daily_tests > 0, our_daily_cases / our_daily_tests, NA_real_),
    official_positivity  = ifelse(official_daily_tests > 0, official_daily_cases / official_daily_tests, NA_real_),
    delta_tests          = our_daily_tests - official_daily_tests,
    delta_cases          = our_daily_cases - official_daily_cases
  )

# Quick look at largest gaps
cmp %>%
  mutate(abs_gap_tests = abs(delta_tests)) %>%
  arrange(desc(abs_gap_tests)) %>%
  slice_head(n = 10) %>%
  knitr::kable(format = "simple", digits = 0,
               col.names = c("Date","Our tests","Our cases","Official tests","Official cases",
                             "Our pos.","Official pos.","Δ tests","Δ cases","|Δ tests|")) %>%
  print()

# -------------------------------------------------------------------
# Weekly aggregation + 4-week moving average + plot
# -------------------------------------------------------------------

# Build weekly aggregates from the official daily totals
weekly <- df_daily %>%
  mutate(week = floor_date(date, unit = "week", week_start = 1)) %>%  # ISO week start (Mon)
  group_by(week) %>%
  summarise(
    weekly_tests = sum(official_daily_tests, na.rm = TRUE),
    weekly_cases = sum(official_daily_cases, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(week) %>%
  # 4-week moving average with window (t-2, t-1, t, t+1)
  mutate(
    cases_ma4 = (lag(weekly_cases, 2) + lag(weekly_cases, 1) + weekly_cases + lead(weekly_cases, 1)) / 4,
    positivity = ifelse(weekly_tests > 0, weekly_cases / weekly_tests, NA_real_)
  )

# For the secondary axis: scale positivity (0–1) to the tests scale
scale_mult <- max(weekly$weekly_tests, na.rm = TRUE)

ggplot(weekly, aes(x = week)) +
  # Columns: tests performed
  geom_col(aes(y = weekly_tests), alpha = 0.6) +
  # Solid line: 4-week moving average of cases
  geom_line(aes(y = cases_ma4), linewidth = 1) +
  # Dashed line: positivity rate on secondary axis (scaled)
  geom_line(aes(y = positivity * scale_mult), linewidth = 1, linetype = "dashed") +
  scale_y_continuous(
    name = "Weekly tests / cases (MA4)",
    labels = label_number(big.mark = " "),
    sec.axis = sec_axis(~ . / scale_mult, name = "Positivity (weekly)", labels = percent_format(accuracy = 1))
  ) +
  scale_x_date(
    date_breaks = "8 weeks",
    date_labels = "W%V\n%G"  # ISO week + ISO year
  ) +
  labs(
    title = "Czechia — Weekly Tests (columns), Cases MA(4) (line) & Positivity (dashed)",
    x = "ISO week (8-week ticks)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    panel.grid.minor = element_blank()
  )
