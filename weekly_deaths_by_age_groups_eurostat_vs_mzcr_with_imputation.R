# ================================================================
# Weekly Deaths by Age Groups: MZCR (POST-IMPUTATION) vs Eurostat
# Stacked area plot with superimposed sources
# Source of MZCR: outputs/optimal_imputed_final.csv (our imputation)
# Buckets shown: 15–24, 25–49, 50–59, 60–69, 70–79, 80+   (no Unknown, no <15)
# ================================================================

# Install ggnewscale if needed
if (!require("ggnewscale", quietly = TRUE)) {
  install.packages("ggnewscale")
}

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(lubridate)
  library(scales)
  library(ISOweek)
  library(RColorBrewer)
  library(ggnewscale)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

cat("=== Loading POST-IMPUTATION Data (our output) ===\n")

# ---- READ OUR IMPUTED OUTPUT ----
file_path <- "outputs/optimal_imputed_final.csv"
if (!file.exists(file_path)) {
  stop("Error: Run 'imputation_extended.R' first to create outputs/optimal_imputed_final.csv")
}

df <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
cat(sprintf("Loaded %d records from %s\n", nrow(df), file_path))

# Build ISO week start date (Monday) from death_year/death_week
df <- df %>%
  mutate(
    death_year = as.integer(death_year),
    death_week = as.integer(death_week),
    week_start = ISOweek::ISOweek2date(sprintf("%04d-W%02d-1", death_year, death_week)),
    imputed_yob = suppressWarnings(as.integer(imputed_yob))
  )

# ---------------- MZCR weekly aggregation (no Unknown, no <15) ----------------
# Age buckets to display
age_levels <- c("15-24","25–49","50–59","60–69","70–79","80+")

weekly_mzcr <- df %>%
  # Use imputed_yob for everyone (for originals, it's equal to orig_yob)
  filter(!is.na(week_start), !is.na(imputed_yob)) %>%
  mutate(
    age_at_death = death_year - imputed_yob
  ) %>%
  # Keep plausible ages and only >=15
  filter(age_at_death >= 15, age_at_death <= 110) %>%
  mutate(
    iso_year = as.integer(format(week_start, "%G")),
    age_group = case_when(
      age_at_death <= 24 ~ "15-24",
      age_at_death <= 49 ~ "25–49",
      age_at_death <= 59 ~ "50–59",
      age_at_death <= 69 ~ "60–69",
      age_at_death <= 79 ~ "70–79",
      TRUE              ~ "80+"
    ),
    age_group = factor(age_group, levels = age_levels)
  ) %>%
  filter(iso_year >= 2020, iso_year <= 2024) %>%
  group_by(week_start, age_group) %>%
  summarise(deaths = n(), .groups = "drop") %>%
  tidyr::complete(week_start, age_group, fill = list(deaths = 0)) %>%
  arrange(week_start, age_group)

total_deaths_mzcr <- sum(weekly_mzcr$deaths)
cat(sprintf("MZCR totals (2020–2024, ≥15 only): %s\n\n", format(total_deaths_mzcr, big.mark=",")))

# ---------------- Eurostat weekly aggregation to same buckets ----------------
cat("=== Loading Eurostat Data ===\n")

eu_file <- "data/demo_r_mwk_05_linear_2_0.csv"
eu_url <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/demo_r_mwk_05/1.0?compress=false&format=csvdata&formatVersion=2.0&lang=en&labels=both"
if (!file.exists(eu_file)) {
  cat("Downloading Eurostat data...\n")
  options(timeout = 1200)
  download.file(eu_url, eu_file, mode = "wb")
}
eu <- read.csv(eu_file, stringsAsFactors = FALSE, check.names = FALSE)

eu2 <- eu %>%
  rename(
    age_group = `age: Age class`,
    geo = `geo: Geopolitical entity (reporting)`,
    sex = `sex: Sex`,
    time = `TIME_PERIOD: Time`,
    value = `OBS_VALUE: Observation value`
  ) %>%
  mutate(
    geo_code = sub(":.*$", "", geo),
    age_code = sub(":.*$", "", age_group),
    sex_code = sub(":.*$", "", sex),
    value = suppressWarnings(as.numeric(value))
  ) %>%
  filter(geo_code == "CZ", !is.na(value), age_code != "TOTAL")

# Aggregate across sexes (if no T row, sum M+F)
eu2_cz <- if (any(eu2$sex_code == "T")) {
  eu2 %>% filter(sex_code == "T") %>% select(time, age_code, value)
} else {
  eu2 %>% group_by(time, age_code) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
}

# Week start date
eu_w <- eu2_cz %>%
  mutate(
    time = trimws(time),
    yy = sub("^([0-9]{4}).*$", "\\1", time),
    ww = suppressWarnings(as.integer(sub("^.*-W?([0-9]{1,2})$", "\\1", time))),
    time_norm = ifelse(grepl("^\\d{4}-W?\\d{1,2}$", time) & !is.na(ww),
                       sprintf("%s-W%02d", yy, ww), NA_character_),
    week_start = suppressWarnings(ISOweek::ISOweek2date(paste0(time_norm, "-1")))
  ) %>%
  filter(!is.na(week_start))

# Rebin to the SAME 6 buckets; assign ALL Y15-19 into 15–24; drop <15 and Unknown
eu_main <- eu_w %>%
  mutate(
    age_group = case_when(
      age_code %in% c("Y15-19","Y20-24") ~ "15-24",
      age_code %in% c("Y25-29","Y30-34","Y35-39","Y40-44","Y45-49") ~ "25–49",
      age_code %in% c("Y50-54","Y55-59") ~ "50–59",
      age_code %in% c("Y60-64","Y65-69") ~ "60–69",
      age_code %in% c("Y70-74","Y75-79") ~ "70–79",
      age_code %in% c("Y80-84","Y85-89","Y_GE90") ~ "80+",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(age_group)) %>%
  select(week_start, age_group, value)

weekly_eu <- eu_main %>%
  group_by(week_start, age_group) %>%
  summarise(deaths = sum(value, na.rm = TRUE), .groups = "drop") %>%
  tidyr::complete(week_start, age_group, fill = list(deaths = 0)) %>%
  mutate(
    age_group = factor(age_group, levels = age_levels),
    iso_year = as.integer(format(week_start, "%G"))
  ) %>%
  filter(iso_year >= 2020, iso_year <= 2024) %>%
  select(-iso_year)

cat("Processed Eurostat data (≥15 only, no Unknown)\n\n")

# -------------------------------------------------------------------
# Create stacked area plot (superimposed)
# -------------------------------------------------------------------
cat("=== Creating Stacked Area Visualization ===\n")

# Palettes (6 shades)
pal6_blues <- brewer.pal(6, "Blues")  # light → dark
fill_blues <- c(
  "15-24" = pal6_blues[6],
  "25–49" = pal6_blues[5],
  "50–59" = pal6_blues[4],
  "60–69" = pal6_blues[3],
  "70–79" = pal6_blues[2],
  "80+"   = pal6_blues[1]
)

pal6_reds <- brewer.pal(6, "Reds")    # light → dark
fill_reds <- c(
  "15-24" = pal6_reds[6],
  "25–49" = pal6_reds[5],
  "50–59" = pal6_reds[4],
  "60–69" = pal6_reds[3],
  "70–79" = pal6_reds[2],
  "80+"   = pal6_reds[1]
)

p_stacked <- ggplot() +
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
    subtitle = "Czech Republic — ISO weeks 2020–2024; age groups shown: 15–24, 25–49, 50–59, 60–69, 70–79, 80+",
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

print(p_stacked)

dir.create("plots/post_imputation", showWarnings = FALSE, recursive = TRUE)
ggsave("plots/post_imputation/weekly_deaths_by_age_stacked.png", p_stacked, 
       width = 14, height = 9, dpi = 150)

cat("\n✓ Stacked area plot saved: plots/post_imputation/weekly_deaths_by_age_stacked.png\n")

# ================================================================
# Per–age-group comparisons: MZCR (blue) vs Eurostat (red)
# ================================================================
cat("\n=== Creating Faceted Comparison Plot ===\n")

# Restrict scope to 2020-W10 .. 2024-W20 (weeks anchored to Monday)
range_start <- ISOweek::ISOweek2date("2020-W10-1")
range_end   <- ISOweek::ISOweek2date("2024-W20-1")

weekly_mzcr_filtered <- weekly_mzcr %>%
  dplyr::filter(dplyr::between(week_start, range_start, range_end))

weekly_eu_filtered <- weekly_eu %>%
  dplyr::filter(dplyr::between(week_start, range_start, range_end))

# Combine both sources, keeping shared age levels
age_levels_combined <- age_levels
combined_by_age <- bind_rows(
  weekly_mzcr_filtered %>% select(week_start, age_group, deaths) %>% mutate(source = "MZCR"),
  weekly_eu_filtered   %>% select(week_start, age_group, deaths) %>% mutate(source = "Eurostat")
) %>%
  mutate(
    age_group = factor(age_group, levels = age_levels_combined),
    source    = factor(source, levels = c("MZCR","Eurostat"))
  ) %>%
  arrange(age_group, week_start, source)

# Consistent colors (dark blue vs dark red)
col_mzcr <- RColorBrewer::brewer.pal(9, "Blues")[8]   # dark blue
col_eu   <- RColorBrewer::brewer.pal(9, "Reds")[8]    # dark red

p_facets <- ggplot(combined_by_age, aes(x = week_start, y = deaths, colour = source)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c(MZCR = col_mzcr, Eurostat = col_eu), name = "Source") +
  scale_x_date(
    breaks = scales::breaks_width("5 weeks"),
    labels = function(d) ISOweek::ISOweek(d),
    expand = expansion(mult = c(0.005, 0.01)),
    limits = c(range_start, range_end)
  ) +
  facet_wrap(~ age_group, ncol = 2, scales = "free_y") +
  labs(
    title    = "Weekly deaths by age group — MZCR (blue) vs Eurostat (red)",
    subtitle = "Czech Republic — ISO weeks (2020–2024) — groups ≥15 only",
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

ggsave("plots/post_imputation/weekly_deaths_by_age_faceted.png", p_facets, 
       width = 14, height = 10, dpi = 150)

cat("\n✓ Faceted plot saved: plots/post_imputation/weekly_deaths_by_age_faceted.png\n")

# -------------------------------------------------------------------
# Summary statistics
# -------------------------------------------------------------------
cat("\n=== Summary Statistics (≥15 only) ===\n")

summary_stats <- weekly_mzcr %>%
  group_by(age_group) %>%
  summarise(total_deaths = sum(deaths), .groups = "drop") %>%
  mutate(percentage = 100 * total_deaths / sum(total_deaths)) %>%
  arrange(age_group)

cat("\nMZCR Deaths by Age Group (2020–2024, ≥15):\n")
print(summary_stats, digits = 2)

eurostat_stats <- weekly_eu %>%
  group_by(age_group) %>%
  summarise(total_deaths = sum(deaths), .groups = "drop") %>%
  mutate(percentage = 100 * total_deaths / sum(total_deaths)) %>%
  arrange(age_group)

cat("\nEurostat Deaths by Age Group (2020–2024, ≥15):\n")
print(eurostat_stats, digits = 2)
