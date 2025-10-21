# ===================================================================
# ASMR (15+) — Eurostat weekly deaths vs. OUR Eurostat-method output
# (feeds from mzcr_no_or_first_infection_with_imputation.csv)
# Files produced are suffixed with "_imputed"
# ===================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(ISOweek)
library(ggplot2)
library(readr)
library(stringr)
library(scales)

dir.create("data", showWarnings = FALSE, recursive = TRUE)

# -------------------------------------------------------------------
# Eurostat weekly deaths (demo_r_mwk_05)  — reference
# -------------------------------------------------------------------
eu_file <- "data/demo_r_mwk_05_linear_2_0.csv"
eu_url  <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/demo_r_mwk_05/1.0?compress=false&format=csvdata&formatVersion=2.0&lang=en&labels=both"

if (!file.exists(eu_file)) {
  options(timeout = 1200)
  download.file(eu_url, eu_file, mode = "wb")
  cat("Eurostat weekly deaths by age group file downloaded.\n")
} else {
  cat("Eurostat weekly deaths by age group file found locally.\n")
}
eu <- read.csv(eu_file, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

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

# -------------------------------------------------------------------
# Eurostat annual population (demo_pjan) -> 5y bands with 95+ OPEN
# -------------------------------------------------------------------
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

# -------------------------------------------------------------------
# ESP 2013 weights (15+ only)
# -------------------------------------------------------------------
esp2013 <- tibble::tibble(
  age_start = c(0, 1, seq(5, 90, by = 5), 95),
  age_end   = c(0, 4, seq(9, 94, by = 5), 999),
  esp_pop   = c(1000, 4000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000,
                7000, 7000, 6500, 6000, 5500, 5000, 4000, 2500, 1500, 800, 200)
) %>% filter(age_end >= 15)

esp15_total    <- sum(esp2013$esp_pop)
weeks_per_year <- 52.1775

# -------------------------------------------------------------------
# Eurostat weekly ASMR (reference)
# -------------------------------------------------------------------
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

# -------------------------------------------------------------------
# Our Eurostat-method ASMR (15+) — computed from the IMPUTED master
#   data/mzcr_no_or_first_infection_with_imputation.csv
#   cached to: data/asmr_esp2013_15plus_weekly_from_imputed_eurostat_pop.csv
# -------------------------------------------------------------------
imputed_file   <- "data/mzcr_no_or_first_infection_with_imputation.csv"
cached_imputed <- "data/asmr_esp2013_15plus_weekly_from_imputed_eurostat_pop.csv"

get_our_series_imputed <- function() {
  if (file.exists(cached_imputed)) {
    cat("Using cached our-series (imputed):", cached_imputed, "\n")
    return(read_csv(cached_imputed, show_col_types = FALSE) %>%
             mutate(iso_date = as.Date(iso_date)) %>%
             transmute(iso_date, asmr_15plus_record_imputed = asmr_15plus_record_imputed))
  }
  if (!file.exists(imputed_file)) {
    stop("Missing imputed master: ", imputed_file,
         "\nPlease run step 5 to build it first.")
  }
  cat("Cached our-series (imputed) not found; rebuilding from master...\n")

  # Read only what we need (much lighter): death date + age
  # death rows only = non-empty week_date_of_death
  dat <- read_csv(
    imputed_file,
    col_types = cols(
      week_date_of_death = col_character(),
      age = col_integer()
    ),
    col_select = c(week_date_of_death, age)
  ) %>%
    filter(!is.na(week_date_of_death), week_date_of_death != "")

  dat <- dat %>%
    mutate(
      death_date = as.Date(week_date_of_death),
      iso_year   = isoyear(death_date),
      iso_week   = isoweek(death_date),
      iso_date   = ISOweek::ISOweek2date(sprintf("%04d-W%02d-1", iso_year, iso_week)),
      age        = pmax(0L, as.integer(age)),
      age_start  = pmin(floor(age / 5) * 5, 95L),
      age_end    = if_else(age_start < 95L, age_start + 4L, 999L)
    ) %>%
    filter(age_end >= 15)

  our_weekly_15p <- dat %>%
    count(iso_year, iso_week, iso_date, age_start, age_end, name = "deaths")

  # Attach population and ESP weights, compute weekly age-specific rates
  our_rates <- our_weekly_15p %>%
    left_join(pop_bands, by = c("iso_year" = "year", "age_start", "age_end")) %>%
    group_by(age_start, age_end) %>%
    tidyr::fill(pop, .direction = "downup") %>%
    ungroup() %>%
    left_join(esp2013, by = c("age_start", "age_end")) %>%
    mutate(
      person_weeks  = pop / weeks_per_year,
      rate_per_100k = if_else(person_weeks > 0, deaths / person_weeks * 1e5, NA_real_)
    ) %>%
    filter(!is.na(rate_per_100k))

  our_asmr <- our_rates %>%
    group_by(iso_year, iso_week, iso_date) %>%
    summarise(
      asmr_15plus_record_imputed = sum(rate_per_100k * esp_pop) / esp15_total,
      .groups = "drop"
    ) %>%
    arrange(iso_date) %>%
    select(iso_date, asmr_15plus_record_imputed)

  # cache
  write_csv(our_asmr, cached_imputed)
  our_asmr
}

asmr15_record <- get_our_series_imputed()

# -------------------------------------------------------------------
# Join & compare (save comparison with _imputed suffix as well)
# -------------------------------------------------------------------
cmp_imputed <- asmr15_eu %>%
  inner_join(asmr15_record, by = "iso_date") %>%
  arrange(iso_date)

write_csv(cmp_imputed, "data/asmr_15plus_comparison_imputed.csv")

cat("Joined weeks:", nrow(cmp_imputed), " | ",
    format(min(cmp_imputed$iso_date), "%Y-%m-%d"), " to ", format(max(cmp_imputed$iso_date), "%Y-%m-%d"), "\n")

if (nrow(cmp_imputed) > 2) {
  cat(sprintf("Correlation (imputed): %.4f\n",
              suppressWarnings(cor(cmp_imputed$asmr_15plus_record_imputed,
                                   cmp_imputed$asmr_15plus_eurostat, use = "complete.obs"))))
}

# -------------------------------------------------------------------
# Limit to 2020-W10 through last ISO week of 2023
# -------------------------------------------------------------------
start_date <- ISOweek::ISOweek2date("2020-W10-1")
end_date   <- max(cmp_imputed$iso_date[lubridate::year(cmp_imputed$iso_date) == 2023], na.rm = TRUE)

cmp_win <- cmp_imputed %>%
  dplyr::filter(iso_date >= start_date, iso_date <= end_date)

cat("Windowed weeks:", nrow(cmp_win), " | ",
    format(min(cmp_win$iso_date), "%Y-%m-%d"), " to ", format(max(cmp_win$iso_date), "%Y-%m-%d"), "\n")

# -------------------------------------------------------------------
# Recompute correlation + legend-ready long data for the window
# -------------------------------------------------------------------
r_val <- suppressWarnings(cor(cmp_win$asmr_15plus_record_imputed,
                              cmp_win$asmr_15plus_eurostat,
                              use = "complete.obs"))
r_txt <- if (is.finite(r_val)) sprintf(" — correlation r = %.3f (n = %d weeks)", r_val, nrow(cmp_win)) else ""

cmp_long <- cmp_win %>%
  dplyr::transmute(
    iso_date,
    `Our (Eurostat-method, imputed)` = asmr_15plus_record_imputed,
    `Eurostat (weekly deaths + annual pop)` = asmr_15plus_eurostat
  ) %>%
  tidyr::pivot_longer(-iso_date, names_to = "series", values_to = "asmr") %>%
  dplyr::mutate(series = factor(
    series,
    levels = c("Our (Eurostat-method, imputed)", "Eurostat (weekly deaths + annual pop)")
  ))

# Color/linetype mappings
col_vals <- c(
  "Our (Eurostat-method, imputed)" = "#D55E00",
  "Eurostat (weekly deaths + annual pop)" = "#0072B2"
)
lty_vals <- c(
  "Our (Eurostat-method, imputed)" = "solid",
  "Eurostat (weekly deaths + annual pop)" = "longdash"
)

# -------------------------------------------------------------------
# Plot (windowed)
# -------------------------------------------------------------------
ggplot(cmp_long, aes(x = iso_date, y = asmr,
                     color = series, linetype = series)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = col_vals, name = "Series") +
  scale_linetype_manual(values = lty_vals, name = "Series") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_x_date(
    date_labels = "%b %Y", date_breaks = "3 months",
    limits = c(start_date, end_date),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  labs(
    title = "ASMR (ESP 2013) — Ages 15+ — Czechia",
    subtitle = paste0(
      "Our Eurostat-method (imputed) vs. Eurostat weekly deaths + annual population",
      " — window: 2020-W10 to last week of 2023", r_txt
    ),
    x = "Week (Monday)",
    y = "ASMR per 100,000 per week",
    caption = "Our series computed from mzcr_no_or_first_infection_with_imputation.csv. Eurostat: demo_r_mwk_05 (deaths), demo_pjan (annual population; person-weeks = pop / 52.1775). 95+ open age bin."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top",
    legend.justification = "center",
    legend.box = "horizontal",
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.25),
    panel.grid.major.y = element_line(linewidth = 0.25),
    panel.border = element_rect(color = "grey85", fill = NA, linewidth = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    plot.caption = element_text(hjust = 0)
  )

# =============================================================================
# Export + print weekly ASMR (15+) — full overlap + 2020-W10..2023 window
# =============================================================================
out_dir <- "out"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- Full overlap (all joined weeks in cmp_imputed) -------------------------
asmr_full_wide <- cmp_imputed %>%
  arrange(iso_date) %>%
  transmute(
    iso_week        = strftime(iso_date, "%G-W%V"),
    iso_date,
    asmr_eurostat   = asmr_15plus_eurostat,
    asmr_our_imputed= asmr_15plus_record_imputed,
    diff            = asmr_our_imputed - asmr_eurostat,
    ratio           = if_else(asmr_15plus_eurostat > 0,
                              asmr_our_imputed / asmr_15plus_eurostat,
                              NA_real_)
  )

asmr_full_long <- asmr_full_wide %>%
  select(iso_date, iso_week, asmr_eurostat, asmr_our_imputed) %>%
  tidyr::pivot_longer(
    cols = c(asmr_eurostat, asmr_our_imputed),
    names_to = "series",
    values_to = "asmr_per_100k_week"
  ) %>%
  mutate(series = dplyr::recode(
    series,
    asmr_eurostat    = "Eurostat (weekly deaths + annual pop)",
    asmr_our_imputed = "Our (Eurostat-method, imputed)"
  ))

# ---- Window (2020-W10 .. last ISO week of 2023) -----------------------------
asmr_win_wide <- cmp_win %>%
  arrange(iso_date) %>%
  transmute(
    iso_week        = strftime(iso_date, "%G-W%V"),
    iso_date,
    asmr_eurostat   = asmr_15plus_eurostat,
    asmr_our_imputed= asmr_15plus_record_imputed,
    diff            = asmr_our_imputed - asmr_eurostat,
    ratio           = if_else(asmr_15plus_eurostat > 0,
                              asmr_our_imputed / asmr_15plus_eurostat,
                              NA_real_)
  )

asmr_win_long <- asmr_win_wide %>%
  select(iso_date, iso_week, asmr_eurostat, asmr_our_imputed) %>%
  tidyr::pivot_longer(
    cols = c(asmr_eurostat, asmr_our_imputed),
    names_to = "series",
    values_to = "asmr_per_100k_week"
  ) %>%
  mutate(series = dplyr::recode(
    series,
    asmr_eurostat    = "Eurostat (weekly deaths + annual pop)",
    asmr_our_imputed = "Our (Eurostat-method, imputed)"
  ))

# ---- Write files ------------------------------------------------------------
write.csv(asmr_full_wide, file.path(out_dir, "asmr_weekly_15plus_wide_imputed.csv"), row.names = FALSE)
write.csv(asmr_full_long, file.path(out_dir, "asmr_weekly_15plus_long_imputed.csv"), row.names = FALSE)
write.csv(asmr_win_wide,  file.path(out_dir, "asmr_weekly_15plus_wide_window_imputed.csv"), row.names = FALSE)
write.csv(asmr_win_long,  file.path(out_dir, "asmr_weekly_15plus_long_window_imputed.csv"), row.names = FALSE)

# RDS bundle
saveRDS(
  list(
    full_wide   = asmr_full_wide,
    full_long   = asmr_full_long,
    window_wide = asmr_win_wide,
    window_long = asmr_win_long
  ),
  file.path(out_dir, "asmr_weekly_15plus_imputed.rds")
)

# ---- Console preview --------------------------------------------------------
message("Weekly ASMR (15+) — preview of last 12 weeks (full overlap):")
print(
  asmr_full_wide %>%
    dplyr::transmute(
      iso_date, iso_week,
      eurostat   = round(asmr_eurostat, 2),
      our_imputed= round(asmr_our_imputed, 2),
      diff       = round(diff, 2),
      ratio      = round(ratio, 3)
    ) %>%
    dplyr::slice_tail(n = 12)
)

if (nrow(asmr_win_wide) > 0) {
  message("Window ",
          format(min(asmr_win_wide$iso_date), "%Y-%m-%d"), " to ",
          format(max(asmr_win_wide$iso_date), "%Y-%m-%d"),
          " — correlation r = ",
          sprintf("%.3f", suppressWarnings(cor(asmr_win_wide$asmr_our_imputed,
                                               asmr_win_wide$asmr_eurostat,
                                               use = "complete.obs"))))
}

message("Saved: ",
        file.path(out_dir, "asmr_weekly_15plus_wide_imputed.csv"), " ; ",
        file.path(out_dir, "asmr_weekly_15plus_long_imputed.csv"), " ; ",
        file.path(out_dir, "asmr_weekly_15plus_wide_window_imputed.csv"), " ; ",
        file.path(out_dir, "asmr_weekly_15plus_long_window_imputed.csv"), " ; ",
        file.path(out_dir, "asmr_weekly_15plus_imputed.rds"))
