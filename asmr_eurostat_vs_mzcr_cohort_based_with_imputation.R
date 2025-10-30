# Compare ASMR (15+) from the first script's output vs reference
# - Uses Eurostat weekly deaths (demo_r_mwk_05) but denominators are the
#   week-specific reference population (total_unvaccinated + total_vaccinated)
#   from outputs/weekly_death_rates_with_imputation.csv
# - Aligns to ESP2013 with 90+ OPEN bin to match the first script
# - Joins by ISO week (Monday date), computes correlation, exports CSVs, and plots

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ISOweek)
  library(ggplot2)
  library(readr)
  library(stringr)
  library(scales)
})

dir.create("data", showWarnings = FALSE, recursive = TRUE)
dir.create("out",  showWarnings = FALSE, recursive = TRUE)

# -------------------------------------------------------------------
# 1) Read ASMR produced by the FIRST script
#    (outputs/weekly_asmr_with_imputation.csv)
#    Columns: year,week,asmr_unvaccinated,asmr_vaccinated,asmr_total
# -------------------------------------------------------------------
first_out_file <- "outputs/weekly_asmr_with_imputation.csv"
if (!file.exists(first_out_file)) {
  stop("Missing file from first script: ", first_out_file,
       "\nRun the first script to generate it.")
}

asmr_first <- read_csv(first_out_file, show_col_types = FALSE) %>%
  mutate(
    year = as.integer(year),
    week = as.integer(week),
    iso_date = ISOweek::ISOweek2date(sprintf("%04d-W%02d-1", year, week))
  ) %>%
  transmute(iso_date, asmr_15plus_first = as.numeric(asmr_total)) %>%
  arrange(iso_date)

# -------------------------------------------------------------------
# 2) Eurostat weekly deaths (demo_r_mwk_05) — reference deaths
# -------------------------------------------------------------------
eu_file <- "data/demo_r_mwk_05_linear_2_0.csv"
eu_url  <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/demo_r_mwk_05/1.0?compress=false&format=csvdata&formatVersion=2.0&lang=en&labels=both"

if (!file.exists(eu_file)) {
  options(timeout = 1200)
  download.file(eu_url, eu_file, mode = "wb")
  message("Eurostat weekly deaths by age group file downloaded.")
} else {
  message("Eurostat weekly deaths by age group file found locally.")
}

eu <- read.csv(eu_file, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

# Re-map columns + Czechia only, numeric deaths, exclude TOTAL age
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

# Keep Total sex; derive year/week + numeric age bands (Eurostat is 95+ open)
eu_w <- eu2 %>%
  filter(sex_code == "T") %>%
  mutate(
    year = as.integer(substr(time, 1, 4)),
    week = as.integer(substr(time, 7, 8)),
    is_band = stringr::str_detect(age_code, "^Y\\d+-\\d+$"),
    age_start = dplyr::case_when(
      age_code == "Y_LT5" ~ 0L,
      age_code == "Y_GE95" ~ 95L,
      is_band ~ readr::parse_integer(stringr::str_extract(age_code, "(?<=^Y)\\d+")),
      TRUE ~ NA_integer_
    ),
    age_end = dplyr::case_when(
      age_code == "Y_LT5" ~ 4L,
      age_code == "Y_GE95" ~ 999L,
      is_band ~ readr::parse_integer(stringr::str_extract(age_code, "(?<=-)\\d+")),
      TRUE ~ NA_integer_
    ),
    iso_date = ISOweek::ISOweek2date(sprintf("%04d-W%02d-1", year, week))
  ) %>%
  filter(!is.na(age_start), !is.na(age_end)) %>%
  select(year, week, iso_date, age_start, age_end, deaths = value) %>%
  filter(age_end >= 15)

# -------------------------------------------------------------------
# 3) WEEKLY reference population from first script output
#     (total_unvaccinated + total_vaccinated) by week & 5y age band
# -------------------------------------------------------------------
weekly_pop_file <- "outputs/weekly_death_rates_with_imputation.csv"
if (!file.exists(weekly_pop_file)) {
  stop("Missing weekly reference population file: ", weekly_pop_file,
       "\nGenerate it with the first script.")
}

ref_pop_weekly <- readr::read_csv(weekly_pop_file, show_col_types = FALSE) %>%
  mutate(
    year = as.integer(year),
    week = as.integer(week),
    age_group = as.character(age_group),
    # "15-19", "20-24", ..., "90+"
    age_start = readr::parse_integer(stringr::str_extract(age_group, "^[0-9]+")),
    age_end   = dplyr::case_when(
      stringr::str_detect(age_group, "\\+$") ~ 999L,
      TRUE ~ readr::parse_integer(stringr::str_extract(age_group, "(?<=-)[0-9]+"))
    ),
    pop_week = dplyr::coalesce(as.numeric(total_unvaccinated), 0) +
               dplyr::coalesce(as.numeric(total_vaccinated),   0),
    iso_date = ISOweek::ISOweek2date(sprintf("%04d-W%02d-1", year, week))
  ) %>%
  filter(!is.na(age_start), !is.na(age_end), age_end >= 15) %>%
  group_by(year, week, iso_date, age_start, age_end) %>%
  summarise(pop_week = sum(pop_week, na.rm = TRUE), .groups = "drop")

# Collapse to 90+ open (to match ESP + first script)
ref_pop_weekly_90p <- ref_pop_weekly %>%
  mutate(
    age_start_90p = if_else(age_start >= 90, 90L, age_start),
    age_end_90p   = if_else(age_start >= 90, 999L, age_end)
  ) %>%
  group_by(year, week, iso_date, age_start = age_start_90p, age_end = age_end_90p) %>%
  summarise(pop_week = sum(pop_week, na.rm = TRUE), .groups = "drop")

# -------------------------------------------------------------------
# 4) ESP2013 — match FIRST script (90+ OPEN) and 15+
#     (first script used 85–89 and 90+, with 90+ weight = 1000)
# -------------------------------------------------------------------
esp2013_90p <- tibble::tibble(
  age_start = c(15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90),
  age_end   = c(19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,999),
  esp_pop   = c(5500,6000,6000,6500,7000,7000,7000,7000,6500,6000,5500,5000,4000,2500,1500,1000)
)
esp_total_90p <- sum(esp2013_90p$esp_pop)

weeks_per_year <- 52.1775  # for annualising weekly rates

# -------------------------------------------------------------------
# 5) Aggregate Eurostat deaths to 90+ OPEN; compute weekly age-specific rates
#     using the WEEKLY reference population as denominator
# -------------------------------------------------------------------
# (a) Collapse Eurostat deaths into 90+ open
eu_w_90p <- eu_w %>%
  mutate(
    age_start_90p = if_else(age_start >= 90, 90L, age_start),
    age_end_90p   = if_else(age_start >= 90, 999L, age_end)
  ) %>%
  group_by(year, week, iso_date, age_start = age_start_90p, age_end = age_end_90p) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE), .groups = "drop")

# (b) Join WEEKLY population and compute annualised per-100k rates
eu_rates_refpop_15p_90p <- eu_w_90p %>%
  left_join(ref_pop_weekly_90p, by = c("year","week","iso_date","age_start","age_end")) %>%
  mutate(
    rate_per_100k = if_else(pop_week > 0, deaths / pop_week * weeks_per_year * 1e5, NA_real_)
  ) %>%
  filter(!is.na(rate_per_100k)) %>%
  left_join(esp2013_90p, by = c("age_start","age_end"))

asmr15_eu_refpop_90p <- eu_rates_refpop_15p_90p %>%
  group_by(year, week, iso_date) %>%
  summarise(
    asmr_15plus_eurostat_refpop_esp90p = sum(rate_per_100k * esp_pop) / esp_total_90p,
    .groups = "drop"
  ) %>%
  arrange(iso_date)

# -------------------------------------------------------------------
# 6) Join the two series + export
# -------------------------------------------------------------------
cmp <- asmr15_eu_refpop_90p %>%
  inner_join(asmr_first, by = "iso_date") %>%
  arrange(iso_date) %>%
  mutate(
    diff  = asmr_15plus_first - asmr_15plus_eurostat_refpop_esp90p,
    ratio = if_else(asmr_15plus_eurostat_refpop_esp90p > 0,
                    asmr_15plus_first / asmr_15plus_eurostat_refpop_esp90p,
                    NA_real_)
  )

write_csv(cmp, "out/asmr_15plus_comparison_first_vs_eurostat_refpop_esp90p.csv")

# Console preview
message("Joined weeks: ", nrow(cmp),
        " | ", format(min(cmp$iso_date), "%Y-%m-%d"),
        " to ", format(max(cmp$iso_date), "%Y-%m-%d"))
if (nrow(cmp) > 2) {
  message(sprintf("Correlation (full overlap): r = %.4f",
                  suppressWarnings(cor(cmp$asmr_15plus_first,
                                       cmp$asmr_15plus_eurostat_refpop_esp90p,
                                       use = "complete.obs"))))
}

# -------------------------------------------------------------------
# 7) Window to 2020-W10 .. last ISO week of 2023 (to match your plot)
# -------------------------------------------------------------------
start_date <- ISOweek::ISOweek2date("2020-W10-1")
end_date   <- max(cmp$iso_date[lubridate::year(cmp$iso_date) == 2023], na.rm = TRUE)

cmp_win <- cmp %>% filter(iso_date >= start_date, iso_date <= end_date)

if (nrow(cmp_win) > 2) {
  r_val <- suppressWarnings(cor(
    cmp_win$asmr_15plus_first,
    cmp_win$asmr_15plus_eurostat_refpop_esp90p,
    use = "complete.obs"
  ))
  message(sprintf(
    "Window %s to %s — r = %.3f (n = %d weeks)",
    format(min(cmp_win$iso_date), "%Y-%m-%d"),
    format(max(cmp_win$iso_date), "%Y-%m-%d"),
    r_val,
    nrow(cmp_win)
  ))
}

# -------------------------------------------------------------------
# 8) Long/wide exports + plot
# -------------------------------------------------------------------
asmr_full_wide <- cmp %>%
  transmute(
    iso_week      = strftime(iso_date, "%G-W%V"),
    iso_date,
    asmr_eurostat_refpop = asmr_15plus_eurostat_refpop_esp90p,
    asmr_first    = asmr_15plus_first,
    diff, ratio
  )
asmr_full_long <- asmr_full_wide %>%
  select(iso_date, iso_week, asmr_eurostat_refpop, asmr_first) %>%
  pivot_longer(c(asmr_eurostat_refpop, asmr_first),
               names_to = "series", values_to = "asmr_per_100k_year") %>%
  mutate(series = recode(series,
                         asmr_eurostat_refpop = "Eurostat deaths + weekly ref pop (ESP2013 90+)",
                         asmr_first           = "First script total (ESP2013 90+)") )

asmr_win_wide <- cmp_win %>%
  transmute(
    iso_week      = strftime(iso_date, "%G-W%V"),
    iso_date,
    asmr_eurostat_refpop = asmr_15plus_eurostat_refpop_esp90p,
    asmr_first    = asmr_15plus_first,
    diff          = asmr_first - asmr_eurostat_refpop,
    ratio         = if_else(asmr_eurostat_refpop > 0, asmr_first/asmr_eurostat_refpop, NA_real_)
  )
asmr_win_long <- asmr_win_wide %>%
  select(iso_date, iso_week, asmr_eurostat_refpop, asmr_first) %>%
  pivot_longer(c(asmr_eurostat_refpop, asmr_first),
               names_to = "series", values_to = "asmr_per_100k_year") %>%
  mutate(series = recode(series,
                         asmr_eurostat_refpop = "Eurostat deaths + weekly ref pop (ESP2013 90+)",
                         asmr_first           = "First script total (ESP2013 90+)") )

write.csv(asmr_full_wide, "out/asmr_weekly_15plus_wide_first_vs_eurostat_refpop_esp90p.csv", row.names = FALSE)
write.csv(asmr_full_long, "out/asmr_weekly_15plus_long_first_vs_eurostat_refpop_esp90p.csv", row.names = FALSE)
write.csv(asmr_win_wide,  "out/asmr_weekly_15plus_wide_window_first_vs_eurostat_refpop_esp90p.csv", row.names = FALSE)
write.csv(asmr_win_long,  "out/asmr_weekly_15plus_long_window_first_vs_eurostat_refpop_esp90p.csv", row.names = FALSE)

# Plot (windowed)
col_vals <- c(
  "First script total (ESP2013 90+)" = "#D55E00",
  "Eurostat deaths + weekly ref pop (ESP2013 90+)" = "#0072B2"
)
lty_vals <- c(
  "First script total (ESP2013 90+)" = "solid",
  "Eurostat deaths + weekly ref pop (ESP2013 90+)" = "longdash"
)

g <- ggplot(asmr_win_long, aes(x = iso_date, y = asmr_per_100k_year,
                                color = series, linetype = series)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = col_vals, name = "Series") +
  scale_linetype_manual(values = lty_vals, name = "Series") +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0))) +
  coord_cartesian(ylim = c(0, 2100)) +
  scale_x_date(
    date_labels = "%b %Y", date_breaks = "3 months",
    limits = c(start_date, end_date),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  labs(
    title = "ASMR (ESP 2013, 90+ open) — Ages 15+ — Czechia",
    subtitle = paste0(
      "First script total vs. Eurostat weekly deaths + weekly reference population",
      " — window: 2020-W10 to last week of 2023"
    ),
    x = "Week (Monday)",
    y = "ASMR per 100,000 (annualised)",
    caption = paste(
      "Deaths: Eurostat demo_r_mwk_05.",
      "Population: weekly reference (total_unvaccinated + total_vaccinated) from outputs/weekly_death_rates_with_imputation.csv.",
      "Annualisation: weeks_per_year = 52.1775."
    )
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
g

# Save plot
plot_file <- "out/asmr_15plus_first_vs_eurostat_refpop_esp90p_window.png"
ggsave(plot_file, g, width = 10.5, height = 6.2, dpi = 120)

# Console preview of last 12 joined weeks (full overlap)
print(
  asmr_full_wide %>%
    transmute(
      iso_date, iso_week,
      eurostat_refpop = round(asmr_eurostat_refpop, 2),
      first_total     = round(asmr_first, 2),
      diff            = round(diff, 2),
      ratio           = round(ratio, 3)
    ) %>%
    slice_tail(n = 12)
)

message("Saved:",
        "\n  out/asmr_15plus_comparison_first_vs_eurostat_refpop_esp90p.csv",
        "\n  out/asmr_weekly_15plus_wide_first_vs_eurostat_refpop_esp90p.csv",
        "\n  out/asmr_weekly_15plus_long_first_vs_eurostat_refpop_esp90p.csv",
        "\n  out/asmr_weekly_15plus_wide_window_first_vs_eurostat_refpop_esp90p.csv",
        "\n  out/asmr_weekly_15plus_long_window_first_vs_eurostat_refpop_esp90p.csv",
        "\n  ", plot_file)
