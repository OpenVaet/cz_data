# ===================================================================
# ASMR (15+) — Eurostat weekly deaths vs. OUR series using Eurostat pop
# (feeds from the new long table: asmr_esp2013_age_stratified_with_imputation.csv)
# ===================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(ISOweek)
library(ggplot2)
library(readr)
library(stringr)
library(scales)

# -------------------------------------------------------------------
# Paths & URLs — Eurostat weekly deaths
# -------------------------------------------------------------------
dir.create("data", showWarnings = FALSE, recursive = TRUE)

eu_file <- "data/demo_r_mwk_05_linear_2_0.csv"
eu_url  <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/demo_r_mwk_05/1.0?compress=false&format=csvdata&formatVersion=2.0&lang=en&labels=both"

# ===================================================================
# Ensure Eurostat weekly deaths file (download if missing)
# ===================================================================
if (!file.exists(eu_file)) {
  options(timeout = 1200)
  download.file(eu_url, eu_file, mode = "wb")
  cat("Eurostat weekly deaths by age group file downloaded.\n")
} else {
  cat("Eurostat weekly deaths by age group file found locally.\n")
}

eu <- read.csv(eu_file, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

# Normalize headers (Eurostat CSV "labels=both")
eu2 <- eu %>%
  rename(
    freq      = `freq: Time frequency`,
    sex       = `sex: Sex`,
    age_group = `age: Age class`,
    unit      = `unit: Unit of measure`,
    geo       = `geo: Geopolitical entity (reporting)`,
    time      = `TIME_PERIOD: Time`,
    value     = `OBS_VALUE: Observation value`
  ) %>%
  mutate(
    geo_code = sub(":.*$", "", geo),
    age_code = sub(":.*$", "", age_group),
    sex_code = sub(":.*$", "", sex)
  ) %>%
  filter(geo_code == "CZ") %>%
  mutate(value = suppressWarnings(as.numeric(value))) %>%
  filter(!is.na(value), age_code != "TOTAL")

# -----------------------------
# Keep sex total; tidy ages (95+ OPEN) and 15+
# -----------------------------
eu_w <- eu2 %>%
  filter(sex_code == "T") %>%
  mutate(
    year = as.integer(substr(time, 1, 4)),
    week = as.integer(substr(time, 7, 8)),
    age_start = dplyr::case_when(
      age_code == "Y_LT5" ~ 0L,
      grepl("^Y(\\d+)-(\\d+)$", age_code) ~ as.integer(sub("^Y(\\d+)-.*$", "\\1", age_code)),
      age_code == "Y_GE95" ~ 95L,
      TRUE ~ NA_integer_
    ),
    age_end = dplyr::case_when(
      age_code == "Y_LT5" ~ 4L,
      grepl("^Y(\\d+)-(\\d+)$", age_code) ~ as.integer(sub("^Y\\d+-(\\d+)$", "\\1", age_code)),
      age_code == "Y_GE95" ~ 999L,
      TRUE ~ NA_integer_
    ),
    iso_date = ISOweek::ISOweek2date(sprintf("%04d-W%02d-1", year, week))
  ) %>%
  filter(!is.na(age_start), !is.na(age_end)) %>%
  select(geo_code, year, week, iso_date, age_start, age_end, deaths = value) %>%
  filter(age_end >= 15)

# --------------------------------------------------------
# Eurostat annual population by single ages (CZ) -> 5y bands with 95+ OPEN
# --------------------------------------------------------
pop_file <- "data/demo_pjan_linear_2_0.csv"
pop_url  <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/demo_pjan/1.0?compress=false&format=csvdata&formatVersion=2.0&lang=en&labels=name"

if (!file.exists(pop_file)) {
  options(timeout = 1200)
  download.file(pop_url, pop_file, mode = "wb")
  cat("Eurostat yearly population by single ages file downloaded.\n")
} else {
  cat("Eurostat yearly population by single ages file found locally.\n")
}

pop_raw <- read.csv(pop_file, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

pop2 <- pop_raw %>%
  rename(time = `TIME_PERIOD`, value = `OBS_VALUE`) %>%
  mutate(geo_code = sub(":.*$", "", geo)) %>%
  filter(geo_code == "CZ") %>%
  mutate(value = suppressWarnings(as.numeric(value))) %>%
  filter(!is.na(value), age != "TOTAL") %>%
  mutate(
    year = as.integer(time),
    age  = suppressWarnings(as.integer(sub("^Y", "", age)))
  ) %>%
  filter(!is.na(age), age >= 15) %>%
  rename(pop = value)

pop_bands <- pop2 %>%
  mutate(
    age_start = pmin(floor(age / 5) * 5, 95L),
    age_end   = if_else(age_start < 95L, age_start + 4L, 999L)
  ) %>%
  group_by(year, age_start, age_end) %>%
  summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")

# --------------------------------------------------------
# ESP 2013 weights (15+ only)
# --------------------------------------------------------
esp2013 <- tibble::tibble(
  age_start = c(0, 1, seq(5, 90, by = 5), 95),
  age_end   = c(0, 4, seq(9, 94, by = 5), 999),
  esp_pop   = c(1000, 4000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000,
                7000, 7000, 6500, 6000, 5500, 5000, 4000, 2500, 1500, 800, 200)
) %>% filter(age_end >= 15)

esp15_total <- sum(esp2013$esp_pop)
weeks_per_year <- 52.1775  # Eurostat convention

# --------------------------------------------------------
# Eurostat-style weekly ASMR from EUROSTAT deaths (reference line)
# --------------------------------------------------------
eu_rates_15p <- eu_w %>%
  left_join(pop_bands, by = c("year", "age_start", "age_end")) %>%
  group_by(age_start, age_end) %>%
  tidyr::fill(pop, .direction = "downup") %>%
  ungroup() %>%
  mutate(
    person_weeks  = pop / weeks_per_year,
    rate_per_100k = if_else(person_weeks > 0, deaths / person_weeks * 1e5, NA_real_)
  ) %>%
  filter(!is.na(rate_per_100k)) %>%
  left_join(esp2013, by = c("age_start", "age_end"))

asmr15_eu <- eu_rates_15p %>%
  group_by(year, week, iso_date) %>%
  summarise(
    asmr_15plus_eurostat = sum(rate_per_100k * esp_pop) / esp15_total,
    .groups = "drop"
  ) %>%
  arrange(iso_date)

# --------------------------------------------------------
# OUR weekly ASMR from the new long table + Eurostat POP (by week ISO-year)
# (this is the only source we now rely on from our pipeline)
# --------------------------------------------------------
cube_file <- "data/asmr_esp2013_age_stratified_with_imputation.csv"
if (!file.exists(cube_file)) {
  stop("Missing age-stratified long table. Generate it with the main script first.")
}

cube <- read_csv(cube_file, show_col_types = FALSE) %>%
  # ensure types
  mutate(
    date = as.Date(date),
    week = as.integer(week),
    age_start = as.integer(age_start),
    age_end   = as.integer(age_end)
  )

# Sum deaths over vaccination/positivity to match Eurostat 'sex=total'
our_deaths_15p <- cube %>%
  filter(age_start >= 15) %>%
  group_by(week, date, age_start, age_end) %>%
  summarise(deaths = sum(n_deaths, na.rm = TRUE), .groups = "drop") %>%
  mutate(iso_year = floor(week / 100L))

# Join Eurostat annual population by ISO year, compute rates, ESP-standardize
our_rates_15p <- our_deaths_15p %>%
  left_join(esp2013, by = c("age_start", "age_end")) %>%
  left_join(pop_bands, by = c("iso_year" = "year", "age_start", "age_end")) %>%
  group_by(age_start, age_end) %>%
  tidyr::fill(pop, .direction = "downup") %>%
  ungroup() %>%
  mutate(
    person_weeks  = pop / weeks_per_year,
    rate_per_100k = if_else(person_weeks > 0, deaths / person_weeks * 1e5, NA_real_)
  ) %>%
  filter(!is.na(rate_per_100k))

asmr15_our <- our_rates_15p %>%
  group_by(week, date) %>%
  summarise(
    asmr_15plus_record_eupop = sum(rate_per_100k * esp_pop) / esp15_total,
    .groups = "drop"
  ) %>%
  arrange(date) %>%
  mutate(iso_date = as.Date(date))

# Save our series if you want to reuse quickly
write_csv(asmr15_our, "data/asmr_esp2013_15plus_weekly_from_longtable_eurostat_pop.csv")
cat("Saved: data/asmr_esp2013_15plus_weekly_from_longtable_eurostat_pop.csv\n")

# --------------------------------------------------------
# Join & compare
# --------------------------------------------------------
cmp <- asmr15_eu %>%
  inner_join(asmr15_our %>% select(iso_date, asmr_15plus_record_eupop),
             by = "iso_date") %>%
  arrange(iso_date)

cat("Joined weeks:", nrow(cmp), " | ",
    format(min(cmp$iso_date), "%Y-%m-%d"), " to ", format(max(cmp$iso_date), "%Y-%m-%d"), "\n")

if (nrow(cmp) > 2) {
  cat(sprintf("Correlation: %.4f\n",
              suppressWarnings(cor(cmp$asmr_15plus_record_eupop,
                                   cmp$asmr_15plus_eurostat, use = "complete.obs"))))
}

# --------------------------------------------------------
# Plot comparison
# --------------------------------------------------------
ggplot(cmp, aes(x = iso_date)) +
  geom_line(aes(y = asmr_15plus_record_eupop), linewidth = 1.1, color = "red") +
  geom_line(aes(y = asmr_15plus_eurostat), linewidth = 1.1, linetype = "dashed", color = "steelblue") +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months",
               expand = expansion(mult = c(0.01, 0.03))) +
  labs(
    title = "ASMR (ESP 2013) — Ages 15+ — Czechia",
    subtitle = "Eurostat annual population (solid red) vs MZCR weekly deaths + Eurostat annual population (dashed blue)",
    x = "Week (Monday)",
    y = "ASMR per 100,000 per week",
    caption = "Eurostat population applied by ISO week-year (person-weeks = population / 52.1775). 95+ open age bin."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
