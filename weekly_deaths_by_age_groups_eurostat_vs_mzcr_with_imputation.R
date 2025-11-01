# ================================================================
# Weekly Deaths by Age Groups (15+): Before vs After vs Eurostat
# Faceted line plot with 3 sources
# Inputs:
#   - outputs/deaths_non_imputed.csv  ("Before Model")
#   - outputs/deaths_imputed.csv      ("After Model")
# Filters:
#   - drop sex == "U"
#   - drop missing year_of_birth_end
# Buckets: 15–24, 25–49, 50–59, 60–69, 70–79, 80+
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(lubridate)
  library(scales)
  library(ISOweek)
  library(RColorBrewer)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

cat("=== Loading BEFORE/AFTER data ===\n")
file_before <- "outputs/deaths_non_imputed.csv"
file_after  <- "outputs/deaths_imputed.csv"

stopifnot(file.exists(file_before))
stopifnot(file.exists(file_after))

df_before <- read.csv(file_before, stringsAsFactors = FALSE, check.names = FALSE)
df_after  <- read.csv(file_after,  stringsAsFactors = FALSE, check.names = FALSE)

# ---------------- Helper: weekly aggregation 15+ into 6 buckets ----------------
age_levels <- c("15-24","25–49","50–59","60–69","70–79","80+")

require_columns <- function(df, needed) {
  missing <- setdiff(needed, names(df))
  if (length(missing)) {
    stop(sprintf("Missing required columns: %s", paste(missing, collapse = ", ")))
  }
}

prepare_weekly <- function(df, label) {
  require_columns(df, c("death_year","death_week","year_of_birth_end"))
  # Coerce basics
  df <- df %>%
    mutate(
      death_year = as.integer(death_year),
      death_week = as.integer(death_week),
      week_start = ISOweek::ISOweek2date(sprintf("%04d-W%02d-1", death_year, death_week)),
      year_of_birth_end = as.integer(suppressWarnings(year_of_birth_end))
    )

  # Filter sex != "U" if 'sex' exists; otherwise no-op
  if ("sex" %in% names(df)) {
    df <- df %>% filter(!is.na(sex), sex != "U")
  }

  weekly <- df %>%
    # drop missing YOB and invalid week_start
    filter(!is.na(week_start), !is.na(year_of_birth_end)) %>%
    mutate(
      age_at_death = death_year - year_of_birth_end
    ) %>%
    # ages 15..110
    filter(age_at_death >= 15, age_at_death <= 110) %>%
    mutate(
      iso_year = as.integer(format(week_start, "%G")),
      age_group = case_when(
        age_at_death <= 24 ~ "15-24",
        age_at_death <= 49 ~ "25–49",
        age_at_death <= 59 ~ "50–59",
        age_at_death <= 69 ~ "60–69",
        age_at_death <= 79 ~ "70–79",
        TRUE               ~ "80+"
      ),
      age_group = factor(age_group, levels = age_levels)
    ) %>%
    # scope 2020–2023 to match Eurostat section below
    filter(iso_year >= 2020, iso_year < 2024) %>%
    group_by(week_start, age_group) %>%
    summarise(deaths = n(), .groups = "drop") %>%
    tidyr::complete(week_start, age_group, fill = list(deaths = 0)) %>%
    arrange(week_start, age_group) %>%
    mutate(source = label)

  total <- sum(weekly$deaths)
  cat(sprintf("%s totals (2020–2023, ≥15): %s\n", label, format(total, big.mark=",")))
  weekly
}

weekly_before <- prepare_weekly(df_before, "Before Model")
weekly_after  <- prepare_weekly(df_after,  "After Model")

cat("\n=== Loading Eurostat Data ===\n")

eu_file <- "data/demo_r_mwk_05_linear_2_0.csv"
eu_url  <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/demo_r_mwk_05/1.0?compress=false&format=csvdata&formatVersion=2.0&lang=en&labels=both"
if (!file.exists(eu_file)) {
  dir.create("data", showWarnings = FALSE, recursive = TRUE)
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

# Aggregate across sexes (prefer T; else sum M+F)
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

# Re-bin to same 6 buckets; drop <15 and unknown
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
  filter(iso_year >= 2020, iso_year < 2024) %>%
  select(-iso_year) %>%
  mutate(source = "Eurostat")

cat("Processed Eurostat data (≥15 only)\n\n")


# -------------------------------------------------------------------
# Faceted comparison with bars for "After Model" + lines for others
# -------------------------------------------------------------------
cat("=== Creating Faceted 3-line Comparison Plot ===\n")

range_start <- ISOweek::ISOweek2date("2020-W10-1")
range_end   <- ISOweek::ISOweek2date("2023-W52-1")

weekly_before_f <- weekly_before %>% filter(between(week_start, range_start, range_end))
weekly_after_f  <- weekly_after  %>% filter(between(week_start, range_start, range_end))
weekly_eu_f     <- weekly_eu     %>% filter(between(week_start, range_start, range_end))

# Split data for geom_col (After) and geom_line (Before + Eurostat)
bars_df  <- weekly_after_f  %>% mutate(source = "After Model")
lines_df <- bind_rows(
  weekly_before_f %>% mutate(source = "Before Model"),
  weekly_eu_f     %>% mutate(source = "Eurostat")
)

# Colors / fill
col_before <- RColorBrewer::brewer.pal(9, "Greys")[8]   # dark grey (line)
col_eu     <- RColorBrewer::brewer.pal(9, "Reds")[8]    # dark red  (line)
fill_after <- RColorBrewer::brewer.pal(9, "Blues")[4]   # light blue (bars)
col_after  <- RColorBrewer::brewer.pal(9, "Blues")[7]   # outline for bars (optional)

p_facets <- ggplot() +
  # Bars for After Model (weekly width ~ 5.5 days)
  geom_col(
    data = bars_df,
    aes(x = week_start, y = deaths, fill = source, colour = source),
    width = 5.5, alpha = 0.35
  ) +
  # Lines for Before + Eurostat on top
  geom_line(
    data = lines_df,
    aes(x = week_start, y = deaths, colour = source),
    linewidth = 0.9
  ) +
  scale_fill_manual(
    values = c("After Model" = fill_after),
    guide = "none"   # <- hide the fill legend
  ) +
  scale_color_manual(
    name = "Source",
    values = c("Before Model" = col_before,
               "After Model"  = col_after,   # outlines the bars
               "Eurostat"     = col_eu),
    breaks = c("Before Model","After Model","Eurostat")
  ) +
  scale_x_date(
    breaks = scales::breaks_width("5 weeks"),
    labels = function(d) ISOweek::ISOweek(d),
    expand = expansion(mult = c(0.005, 0.01)),
    limits = c(range_start, range_end)
  ) +
  facet_wrap(~ age_group, ncol = 2, scales = "free_y") +
  labs(
    title    = "Weekly deaths by age group — Before vs After Model vs Eurostat",
    subtitle = "Czech Republic — ISO weeks (2020–2023) — groups ≥15 only\nAfter Model uses columns; Before/Eurostat use lines",
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

dir.create("plots/model_comparison", showWarnings = FALSE, recursive = TRUE)
ggsave("plots/model_comparison/weekly_deaths_by_age_faceted_bars_after.png", p_facets,
       width = 14, height = 10, dpi = 150)

cat("\n✓ Plot saved: plots/model_comparison/weekly_deaths_by_age_faceted_bars_after.png\n")


# -------------------------------------------------------------------
# Optional: summary stats per source (≥15 only, 2020–2023)
# -------------------------------------------------------------------
summarise_source <- function(x, name) {
  x %>%
    group_by(age_group) %>%
    summarise(total_deaths = sum(deaths), .groups = "drop") %>%
    mutate(percentage = 100 * total_deaths / sum(total_deaths),
           source = name) %>%
    arrange(age_group)
}

cat("\n=== Summary Statistics (≥15 only, 2020–2023) ===\n")
summary_all <- bind_rows(
  summarise_source(weekly_before_f, "Before Model"),
  summarise_source(weekly_after_f,  "After Model"),
  summarise_source(weekly_eu_f,     "Eurostat")
)

print(summary_all, digits = 2)
