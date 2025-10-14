# ================================================================
# Combined plot: MZCR (blue) over Eurostat (red), super-imposed
# Age buckets: "Unknown","0–15","15-24","25–49","50–59","60–69","70–79","80+"
# ================================================================
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
})

# -------------------------------------------------------------------
# Path
# -------------------------------------------------------------------
file_path <- "data/mzcr_no_or_first_infection.csv"

# -------------------------------------------------------------------
# Ensure the CZ data file is present
# -------------------------------------------------------------------
if (!file.exists(file_path)) {
  stop("Error; run 'download_and_data_integrity.R' first.")
} else {
  cat("CZ enhanced data file found locally.\n")
}

# -------------------------------------------------------------------
# Read the CZ CSV into a dataframe
# -------------------------------------------------------------------
df <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)

# Small normalization used later (factors for the 2×2 table)
df_aug <- df %>%
  mutate(
    gender = as.character(gender),
    yob_known_factor = factor(!is.na(year_of_birth_end),
                              levels = c(FALSE, TRUE),
                              labels = c("Unknown","Known")),
    has_death_factor = factor(has_death,
                              levels = c(FALSE, TRUE),
                              labels = c("No","Yes"))
  )

# -------------------------------------------------------------------
# Weekly deaths by age group
# -------------------------------------------------------------------
age_levels <- c("Unknown", "0–14", "15–24", "25–49", "50–59", "60–69", "70–79", "80+")

weekly_mzcr <- df %>%
  # rely on precomputed has_death + week_date_of_death
  filter(has_death) %>%
  mutate(
    week_start = as.Date(week_date_of_death),                 # already Monday of ISO week in the enhanced data
    iso_year   = suppressWarnings(as.integer(format(week_start, "%G"))),

    # Age grouping; use provided age_at_death and fall back to Unknown if YOB missing
    age_group = case_when(
      is.na(year_of_birth_end) ~ "Unknown",
      age_at_death <= 14 ~ "0–14",
      age_at_death <= 24 ~ "15–24",
      age_at_death <= 49 ~ "25–49",
      age_at_death <= 59 ~ "50–59",
      age_at_death <= 69 ~ "60–69",
      age_at_death <= 79 ~ "70–79",
      age_at_death > 79  ~ "80+",
      TRUE ~ "Unknown"
    )

  ) %>%
  filter(!is.na(week_start)) %>%
  # keep plausible ages when known
  filter(is.na(year_of_birth_end) | (age_at_death >= 0 & age_at_death <= 110)) %>%
  mutate(age_group = factor(age_group, levels = age_levels)) %>%
  group_by(week_start, age_group) %>%
  summarise(deaths = n(), .groups = "drop") %>%
  complete(week_start, age_group, fill = list(deaths = 0)) %>%
  arrange(week_start, age_group) %>%
  mutate(iso_year = as.integer(format(week_start, "%G"))) %>%
  filter(iso_year >= 2020, iso_year <= 2024) %>%
  select(-iso_year)

# ---------------------------
# Eurostat (aggregate) prep
# ---------------------------
eu_file <- "data/demo_r_mwk_05_linear_2_0.csv"
eu_url  <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/demo_r_mwk_05/1.0?compress=false&format=csvdata&formatVersion=2.0&lang=en&labels=both"

if (!file.exists(eu_file)) {
  options(timeout = 1200)
  download.file(eu_url, eu_file, mode = "wb")
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

eu2_cz <- if (any(eu2$sex_code == "T")) {
  eu2 %>% filter(sex_code == "T") %>% select(time, age_code, value)
} else {
  eu2 %>% group_by(time, age_code) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
}

# Compute week_start
eu_w <- eu2_cz %>%
  mutate(
    time       = trimws(time),
    yy         = sub("^([0-9]{4}).*$", "\\1", time),
    ww         = suppressWarnings(as.integer(sub("^.*-W?([0-9]{1,2})$", "\\1", time))),
    time_norm  = ifelse(grepl("^\\d{4}-W?\\d{1,2}$", time) & !is.na(ww),
                        sprintf("%s-W%02d", yy, ww), NA_character_),
    week_start = suppressWarnings(ISOweek::ISOweek2date(paste0(time_norm, "-1")))
  ) %>%
  filter(!is.na(week_start))

# Rebin Eurostat to custom age buckets: 0–14, 15–24, 25–49, 50–59, 60–69, 70–79, 80+, Unknown
eu_rebinned <- eu_w %>%
  mutate(
    age_group = case_when(
      age_code %in% c("Y_LT5", "Y5-9", "Y10-14")                 ~ "0–14",
      age_code %in% c("Y15-19", "Y20-24")                         ~ "15–24",
      age_code %in% c("Y25-29", "Y30-34", "Y35-39", "Y40-44", "Y45-49") ~ "25–49",
      age_code %in% c("Y50-54", "Y55-59")                         ~ "50–59",
      age_code %in% c("Y60-64", "Y65-69")                         ~ "60–69",
      age_code %in% c("Y70-74", "Y75-79")                         ~ "70–79",
      age_code %in% c("Y80-84", "Y85-89", "Y_GE90")               ~ "80+",
      age_code == "UNK"                                           ~ "Unknown",
      TRUE                                                        ~ NA_character_
    )
  ) %>%
  filter(!is.na(age_group)) %>%
  select(week_start, age_group, value)

weekly_eu <- eu_rebinned %>%
  group_by(week_start, age_group) %>%
  summarise(deaths = sum(value, na.rm = TRUE), .groups = "drop") %>%
  complete(week_start, age_group, fill = list(deaths = 0)) %>%
  mutate(
    age_group = factor(age_group, levels = age_levels),
    iso_year = as.integer(format(week_start, "%G"))
  ) %>%
  filter(iso_year >= 2020, iso_year <= 2024) %>%
  select(-iso_year)


# ================================================================
# Global deaths comparison (total across all age groups)
# Same style as download_and_data_integrity.R
# ================================================================

# Aggregate MZCR deaths across all age groups
mzcr_total <- weekly_mzcr %>%
  group_by(week_start) %>%
  summarise(Deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    source = "Mzcr.cz",
    iso_year = as.integer(format(week_start, "%G")),
    iso_week = as.integer(format(week_start, "%V"))
  ) %>%
  rename(WeekDate = week_start, Year = iso_year, Week = iso_week) %>%
  select(Year, Week, WeekDate, Deaths, source)

# Aggregate Eurostat deaths across all age groups
eurostat_total <- weekly_eu %>%
  group_by(week_start) %>%
  summarise(Deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    source = "Eurostat",
    iso_year = as.integer(format(week_start, "%G")),
    iso_week = as.integer(format(week_start, "%V"))
  ) %>%
  rename(WeekDate = week_start, Year = iso_year, Week = iso_week) %>%
  select(Year, Week, WeekDate, Deaths, source)

# Combine for plotting
plot_df_total <- bind_rows(mzcr_total, eurostat_total) %>%
  arrange(source, WeekDate) %>%
  group_by(source) %>%
  mutate(
    point_idx = row_number(),
    show_label = point_idx %% 10 == 0
  ) %>%
  ungroup()

# Create the comparison plot
gg_total <- ggplot(plot_df_total, aes(x = WeekDate, y = Deaths, color = source, linetype = source)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8, alpha = 0.7) +
  geom_text(
    data = subset(plot_df_total, show_label),
    aes(label = Deaths),
    vjust = -0.6,
    size = 4.5,
    show.legend = FALSE
  ) +
  scale_linetype_manual(
    values = c("Eurostat" = "dashed", "Mzcr.cz" = "solid"),
    guide = guide_legend(override.aes = list(linewidth = 1.3))
  ) +
  scale_color_manual(
    values = c("Eurostat" = "#1f77b4", "Mzcr.cz" = "#d62728")
  ) +
  scale_x_date(date_breaks = "5 weeks", date_labels = "%G-W%V") +
  labs(
    title = "Deaths by ISO Year-Week: Eurostat vs. MZCR",
    subtitle = "Dashed = Eurostat weekly deaths · Solid = MZCR deaths in healthcare facilities",
    x = "ISO Week",
    y = "Deaths",
    color = "Dataset",
    linetype = "Dataset"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 14),
    panel.grid.minor = element_blank()
  )

print(gg_total)

cat("\n=== Global deaths comparison plot created ===\n\n")

# ---------------------------
# Palettes (same lightness ranks across sources)
# ---------------------------
# Unknown darkest; younger → older lighter; 80+ lightest
pal8_blues <- brewer.pal(8, "Blues")  # light → dark
fill_blues <- c(
  "Unknown" = pal8_blues[8],
  "0–14"    = pal8_blues[7],
  "15–24"   = pal8_blues[6],
  "25–49"   = pal8_blues[5],
  "50–59"   = pal8_blues[4],
  "60–69"   = pal8_blues[3],
  "70–79"   = pal8_blues[2],
  "80+"     = pal8_blues[1]
)


pal8_reds <- brewer.pal(8, "Reds")    # light → dark
fill_reds <- c(
  "Unknown" = pal8_reds[8],
  "0–14"    = pal8_reds[7],
  "15–24"   = pal8_reds[6],
  "25–49"   = pal8_reds[5],
  "50–59"   = pal8_reds[4],
  "60–69"   = pal8_reds[3],
  "70–79"   = pal8_reds[2],
  "80+"     = pal8_reds[1]
)

# ---------------------------
# Super-imposed plot (two independent fill scales)
# ---------------------------
ggplot() +
  # MZCR (blue)
  geom_area(
    data = weekly_mzcr,
    aes(x = week_start, y = deaths, fill = age_group),
    position = position_stack(reverse = TRUE),
    alpha = 0.65
  ) +
  scale_fill_manual(
    values = fill_blues, name = "Age group (MZCR)",
    guide = guide_legend(reverse = TRUE)
  ) +
  ggnewscale::new_scale_fill() +
  # Eurostat (red)
  geom_area(
    data = weekly_eu,
    aes(x = week_start, y = deaths, fill = age_group),
    position = position_stack(reverse = TRUE),
    alpha = 0.40
  ) +
  scale_fill_manual(
    values = fill_reds, name = "Age group (Eurostat)",
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_x_date(
    breaks = breaks_width("5 weeks"),
    labels = function(d) ISOweek::ISOweek(d),
    expand = expansion(mult = c(0.005, 0.01))
  ) +
  labs(
    title = "Weekly deaths by age group — MZCR (Blue) vs Eurostat (Red)",
    subtitle = "Czech Republic — ISO weeks (2020–2024); 'Unknown' = missing year of birth/age\nAreas are stacked within source and overlaid across sources",
    x = NULL, y = "Deaths (weekly total)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
    axis.text.y  = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.text  = element_text(size = 11),
    plot.title   = element_text(face = "bold", size = 22),
    plot.subtitle= element_text(size = 13)
  )





# ================================================================
# Per–age-group comparisons: MZCR (blue) vs Eurostat (red)
# ================================================================

# Restrict scope to 2020-W10 .. 2024-W20 (weeks anchored to Monday)
range_start <- ISOweek::ISOweek2date("2020-W10-1")
range_end   <- ISOweek::ISOweek2date("2024-W20-1")

weekly_mzcr <- weekly_mzcr %>%
  dplyr::filter(dplyr::between(week_start, range_start, range_end))

weekly_eu <- weekly_eu %>%
  dplyr::filter(dplyr::between(week_start, range_start, range_end))

# Combine both sources, keeping shared age levels and 2020–2024 truncation already applied
age_levels_combined <- c("Unknown","0–14","15–24","25–49","50–59","60–69","70–79","80+")
combined_by_age <- bind_rows(
  weekly_mzcr %>% select(week_start, age_group, deaths) %>% mutate(source = "MZCR"),
  weekly_eu   %>% select(week_start, age_group, deaths) %>% mutate(source = "Eurostat")
) %>%
  mutate(
    age_group = factor(age_group, levels = age_levels_combined),
    source    = factor(source, levels = c("MZCR","Eurostat"))
  ) %>%
  arrange(age_group, week_start, source) %>%
  dplyr::filter(dplyr::between(week_start, range_start, range_end))

# Consistent colors (dark blue vs dark red)
col_mzcr <- RColorBrewer::brewer.pal(9, "Blues")[8]   # dark blue
col_eu   <- RColorBrewer::brewer.pal(9, "Reds")[8]    # dark red

# ---------- 1) Faceted overview (one panel per age group) ----------
p_facets <- ggplot(combined_by_age, aes(x = week_start, y = deaths, colour = source)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c(MZCR = col_mzcr, Eurostat = col_eu), name = "Source") +
  scale_x_date(
    breaks = scales::breaks_width("5 weeks"),
    labels = function(d) ISOweek::ISOweek(d),
    expand = expansion(mult = c(0.005, 0.01)),
    limits = c(range_start, range_end)
  ) +
  facet_wrap(~ age_group, ncol = 2, scales = "free_y") +   # free_y to keep small groups readable
  labs(
    title    = "Weekly deaths by age group — MZCR (blue) vs Eurostat (red)",
    subtitle = "Czech Republic — ISO weeks (2020–2024)",
    x = NULL, y = "Deaths (weekly total)"
  ) +
  theme_minimal(base_size = 17) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1, size = 11),
    axis.text.y  = element_text(size = 11),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 11),
    plot.title   = element_text(face = "bold", size = 20),
    plot.subtitle= element_text(size = 13)
  )
print(p_facets)

# ---------- 2) Separate plot per age group (saved to disk) ----------
dir.create("plots/age_groups", showWarnings = FALSE, recursive = TRUE)

plot_age_group <- function(grp) {
  df_sub <- dplyr::filter(combined_by_age, age_group == grp)
  ggplot(df_sub, aes(x = week_start, y = deaths, colour = source)) +
    geom_line(linewidth = 1.0) +
    scale_color_manual(values = c(MZCR = col_mzcr, Eurostat = col_eu), name = "Source") +
    scale_x_date(
      breaks = scales::breaks_width("5 weeks"),
      labels = function(d) ISOweek::ISOweek(d),
      expand = expansion(mult = c(0.005, 0.01))
    ) +
    labs(
      title    = paste0("Weekly deaths — ", grp),
      subtitle = "Czech Republic — ISO weeks (2020–2024)",
      x = NULL, y = "Deaths (weekly total)"
    ) +
    theme_minimal(base_size = 17) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1, size = 11),
      axis.text.y  = element_text(size = 11),
      legend.position = "top",
      legend.title = element_text(size = 12),
      legend.text  = element_text(size = 11),
      plot.title   = element_text(face = "bold", size = 20),
      plot.subtitle= element_text(size = 13)
    )
}

for (grp in levels(combined_by_age$age_group)) {
  p <- plot_age_group(grp)
  # save a reasonably sized PNG; tweak as you wish
  ggsave(
    filename = file.path("plots/age_groups",
                         paste0("weekly_deaths_", gsub("[^0-9A-Za-z\\+]+", "_", grp), ".png")),
    plot = p, width = 11, height = 6, dpi = 150
  )
}

# ================================================================
# Weekly "missing deaths" by age group + week-level diagnostics
# Missing = pmax(Eurostat - MZCR, 0)
# Output (single write): data/weekly_missing_deaths_among_age_groups.csv
# ================================================================

# Use the same canonical labels everywhere
age_levels_combined <- c("Unknown","0–14","15–24","25–49","50–59","60–69","70–79","80+")

baseline_start <- ISOweek::ISOweek2date("2020-W13-1")   # <-- new lower bound

missing_by_age <- combined_by_age %>%
  dplyr::filter(week_start >= baseline_start) %>%        # <-- apply W13 filter up-front
  tidyr::pivot_wider(
    id_cols    = c(week_start, age_group),
    names_from = source,
    values_from = deaths,
    values_fill = 0
  ) %>%
  dplyr::mutate(
    age_group = factor(age_group, levels = age_levels_combined),
    missing_deaths = pmax(Eurostat - MZCR, 0)
  ) %>%
  dplyr::group_by(week_start) %>%
  dplyr::mutate(
    mzcr_total_week     = sum(MZCR, na.rm = TRUE),
    eurostat_total_week = sum(Eurostat, na.rm = TRUE),
    total_missing_deaths_week = sum(missing_deaths, na.rm = TRUE),
    # Share of the *week's missing* that sits in each age group
    pct_missing_share = dplyr::if_else(
      total_missing_deaths_week > 0,
      100 * missing_deaths / total_missing_deaths_week,
      NA_real_
    ),
    # Share of Eurostat weekly deaths that are "missing" overall
    pct_missing_of_total_week = dplyr::if_else(
      eurostat_total_week > 0,
      100 * total_missing_deaths_week / eurostat_total_week,
      NA_real_
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    iso_year = as.integer(format(week_start, "%G")),
    iso_week = as.integer(format(week_start, "%V")),
    imputation_eligible = week_start <= as.Date("2023-12-31")
  ) %>%
  dplyr::arrange(week_start, age_group) %>%
  dplyr::select(
    week_start, iso_year, iso_week, imputation_eligible,
    age_group,
    MZCR, Eurostat,
    mzcr_total_week, eurostat_total_week,
    missing_deaths, total_missing_deaths_week,
    pct_missing_share, pct_missing_of_total_week
  )

# Single write
write.csv(missing_by_age, "data/weekly_missing_deaths_among_age_groups.csv", row.names = FALSE)
cat("Wrote: data/weekly_missing_deaths_among_age_groups.csv\n")

# ------------------------------------------------
# Totals from 2020-W13 through 2023-12-31 (exclude 2024+)
# ------------------------------------------------
baseline_start <- ISOweek::ISOweek2date("2020-W13-1")
cutoff         <- as.Date("2023-12-31")

# Sum of weekly "missing deaths" (Eurostat - MZCR, zero-truncated), no age double count
missing_totals_2020W13_2023 <- missing_by_age %>%
  dplyr::filter(week_start >= baseline_start, week_start <= cutoff) %>%
  dplyr::distinct(week_start, total_missing_deaths_week) %>%  # one row per week
  dplyr::summarise(
    total_missing_deaths = sum(total_missing_deaths_week, na.rm = TRUE),
    n_weeks              = dplyr::n()
  )

# Eurostat & MZCR totals in the same window (aggregate per week first)
eurostat_total_2020W13_2023 <- weekly_eu %>%
  dplyr::filter(week_start >= baseline_start, week_start <= cutoff) %>%
  dplyr::group_by(week_start) %>%
  dplyr::summarise(total = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
  dplyr::summarise(total_deaths = sum(total, na.rm = TRUE)) %>%
  dplyr::pull(total_deaths)

mzcr_total_2020W13_2023 <- weekly_mzcr %>%
  dplyr::filter(week_start >= baseline_start, week_start <= cutoff) %>%
  dplyr::group_by(week_start) %>%
  dplyr::summarise(total = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
  dplyr::summarise(total_deaths = sum(total, na.rm = TRUE)) %>%
  dplyr::pull(total_deaths)

share_missing_vs_eurostat <- ifelse(eurostat_total_2020W13_2023 > 0,
                                    100 * missing_totals_2020W13_2023$total_missing_deaths / eurostat_total_2020W13_2023,
                                    NA_real_
)

cat("\n=== Missing-deaths summary (2020-W13 → 2023-12-31) ===\n")
cat(sprintf("Weeks considered: %d\n", missing_totals_2020W13_2023$n_weeks))
cat(sprintf("Eurostat total deaths: %s\n", format(eurostat_total_2020W13_2023, big.mark=",")))
cat(sprintf("MZCR total deaths:    %s\n", format(mzcr_total_2020W13_2023, big.mark=",")))
cat(sprintf("Missing deaths (Eurostat - MZCR, zero-truncated): %s\n",
            format(missing_totals_2020W13_2023$total_missing_deaths, big.mark=",")))
cat(sprintf("Missing as %% of Eurostat: %.2f%%\n\n", share_missing_vs_eurostat))

# Optional: write a compact one-line CSV with these totals
summary_row <- tibble::tibble(
  from_week          = "2020-W13",
  through_date       = cutoff,
  weeks              = missing_totals_2020W13_2023$n_weeks,
  eurostat_total     = eurostat_total_2020W13_2023,
  mzcr_total         = mzcr_total_2020W13_2023,
  missing_zero_trunc = missing_totals_2020W13_2023$total_missing_deaths,
  missing_pct_eurostat = share_missing_vs_eurostat
)
write.csv(summary_row, "data/weekly_missing_deaths_summary_2020W13_2023.csv", row.names = FALSE)
cat("Wrote: data/weekly_missing_deaths_summary_2020W13_2023.csv\n")
