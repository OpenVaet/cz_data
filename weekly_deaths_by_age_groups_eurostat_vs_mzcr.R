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

# ================================================================
# GLOBAL weekly error % (Eurostat − MZCR), 2020-W10 .. 2023-12-31
# ================================================================

library(tidyr)

global_weekly_error <- plot_df_total %>%
  dplyr::select(WeekDate, source, Deaths) %>%
  tidyr::pivot_wider(names_from = source, values_from = Deaths) %>%
  # Treat missing weeks as 0 so the denominator is "total observed in both datasets"
  dplyr::mutate(
    Eurostat = dplyr::coalesce(Eurostat, 0),
    `Mzcr.cz` = dplyr::coalesce(`Mzcr.cz`, 0),
    iso_year = as.integer(format(WeekDate, "%G")),
    iso_week = as.integer(format(WeekDate, "%V"))
  ) %>%
  dplyr::filter(
    (iso_year > 2020 | (iso_year == 2020 & iso_week >= 10)) &
      iso_year <= 2023
  ) %>%
  dplyr::arrange(WeekDate) %>%
  dplyr::mutate(
    diff      = Eurostat - `Mzcr.cz`,
    diff_pos  = pmax(diff, 0),
    diff_neg  = pmin(diff, 0),
    diff_abs  = abs(diff)
  )

sum_pos  <- sum(global_weekly_error$diff_pos, na.rm = TRUE)
sum_neg  <- sum(global_weekly_error$diff_neg, na.rm = TRUE)  # negative
sum_abs  <- sum(global_weekly_error$diff_abs, na.rm = TRUE)
denom    <- sum(global_weekly_error$Eurostat + global_weekly_error$`Mzcr.cz`, na.rm = TRUE)
pct_err  <- ifelse(denom > 0, 100 * sum_abs / denom, NA_real_)
n_weeks  <- nrow(global_weekly_error)

n_weeks <- as.integer(nrow(global_weekly_error))

# Render a compact table
global_tbl <- tibble::tibble(
  Metric = c(
    "Weeks considered",
    "Sum of weekly positives (≥0)",
    "Sum of weekly negatives (<0)",
    "Sum of absolute weekly diffs",
    "Total deaths (Eurostat + MZCR)",
    "Total % of weekly error"
  ),
  Value = c(
    n_weeks,
    sum_pos,
    sum_neg,
    sum_abs,
    denom,
    pct_err
  )
)
global_tbl %>%
  dplyr::transmute(
    Metric,
    Value = dplyr::case_when(
      Metric == "Total % of weekly error" ~ sprintf("%.2f%%", Value),
      TRUE ~ formatC(Value, format = "f", digits = 0, big.mark = ",")
    )
  ) %>%
  kable(
    format = "html",
    caption = "Global (all ages) — Total % of weekly error (2020-W10 to 2023-12-31)",
    align = c("l","r")
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped","condensed"))

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

# Restrict scope to 2020-W10 .. 2024-W1 (weeks anchored to Monday)
range_start <- ISOweek::ISOweek2date("2020-W10-1")
range_end   <- ISOweek::ISOweek2date("2024-W01-1")

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

# ================================================================
# Focused comparison: 0–4 vs 0–14 (Eurostat vs MZCR)
# ================================================================

cat("\n\n================================================================\n")
cat("FOCUSED COMPARISON — 0–4 vs 0–14 (Eurostat vs MZCR)\n")
cat("================================================================\n\n")

# Guard: color constants if not defined earlier
if (!exists("col_mzcr")) col_mzcr <- RColorBrewer::brewer.pal(9, "Blues")[8]
if (!exists("col_eu"))   col_eu   <- RColorBrewer::brewer.pal(9, "Reds")[8]

# Window (match rest of analysis)
if (!exists("baseline_start")) baseline_start <- ISOweek::ISOweek2date("2020-W13-1")
if (!exists("cutoff"))         cutoff         <- as.Date("2023-12-31")

dir.create("plots/age_groups", showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------
# Build weekly series for 0–4 (from raw) and 0–14 (from prior aggregates)
# ------------------------------------------------

# --- MZCR 0–4 (from raw df; require known YOB & plausible age) ---
weekly_mzcr_0_4 <- df %>%
  dplyr::filter(has_death, !is.na(week_date_of_death)) %>%
  dplyr::mutate(
    week_start = as.Date(week_date_of_death),
    age_ok     = !is.na(year_of_birth_end) & age_at_death >= 0 & age_at_death <= 4
  ) %>%
  dplyr::filter(age_ok) %>%
  dplyr::group_by(week_start) %>%
  dplyr::summarise(deaths = dplyr::n(), .groups = "drop") %>%
  tidyr::complete(week_start, fill = list(deaths = 0)) %>%
  dplyr::mutate(iso_year = as.integer(format(week_start, "%G"))) %>%
  dplyr::filter(iso_year >= 2020, iso_year <= 2024) %>%
  dplyr::select(-iso_year)

# --- Eurostat 0–4 (Y_LT5 directly from eu_w) ---
weekly_eu_0_4 <- eu_w %>%
  dplyr::filter(age_code == "Y_LT5") %>%
  dplyr::group_by(week_start) %>%
  dplyr::summarise(deaths = sum(value, na.rm = TRUE), .groups = "drop") %>%
  tidyr::complete(week_start, fill = list(deaths = 0)) %>%
  dplyr::mutate(iso_year = as.integer(format(week_start, "%G"))) %>%
  dplyr::filter(iso_year >= 2020, iso_year <= 2024) %>%
  dplyr::select(-iso_year)

# --- MZCR 0–14 & Eurostat 0–14 from earlier rebinned aggregates ---
weekly_mzcr_0_14 <- weekly_mzcr %>%
  dplyr::filter(age_group == "0–14") %>%
  dplyr::select(week_start, deaths)

weekly_eu_0_14 <- weekly_eu %>%
  dplyr::filter(age_group == "0–14") %>%
  dplyr::select(week_start, deaths)

# ------------------------------------------------
# Harmonize, compute weekly differences (Eurostat − MZCR)
# ------------------------------------------------
mk_comp <- function(mzcr_df, eu_df, bucket_label) {
  dplyr::full_join(
    mzcr_df %>% dplyr::rename(MZCR = deaths),
    eu_df   %>% dplyr::rename(Eurostat = deaths),
    by = "week_start"
  ) %>%
    dplyr::mutate(
      Eurostat = dplyr::coalesce(Eurostat, 0),
      MZCR     = dplyr::coalesce(MZCR, 0),
      iso_year = as.integer(format(week_start, "%G")),
      iso_week = as.integer(format(week_start, "%V"))
    ) %>%
    dplyr::filter(dplyr::between(week_start, baseline_start, cutoff)) %>%
    dplyr::mutate(
      diff     = Eurostat - MZCR,
      diff_pos = pmax(diff, 0),
      diff_abs = abs(diff),
      bucket   = bucket_label
    ) %>%
    dplyr::arrange(week_start)
}

comp_0_4  <- mk_comp(weekly_mzcr_0_4,  weekly_eu_0_4,  "0–4")
comp_0_14 <- mk_comp(weekly_mzcr_0_14, weekly_eu_0_14, "0–14")

comp_both <- dplyr::bind_rows(comp_0_4, comp_0_14)

# ------------------------------------------------
# Plots
# ------------------------------------------------

# Lines per source, faceted by bucket
p_lines <- comp_both %>%
  tidyr::pivot_longer(c(Eurostat, MZCR), names_to = "source", values_to = "deaths") %>%
  ggplot2::ggplot(ggplot2::aes(x = week_start, y = deaths, colour = source)) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::scale_color_manual(values = c(Eurostat = col_eu, MZCR = col_mzcr), name = "Source") +
  ggplot2::scale_x_date(
    breaks = scales::breaks_width("5 weeks"),
    labels = function(d) ISOweek::ISOweek(d),
    expand = ggplot2::expansion(mult = c(0.005, 0.01))
  ) +
  ggplot2::facet_wrap(~ bucket, ncol = 1, scales = "free_y") +
  ggplot2::labs(
    title = "Weekly deaths — Eurostat vs MZCR",
    subtitle = "Buckets 0–4 and 0–14; Czech Republic — ISO weeks (2020-W13 to 2023-12-31)",
    x = NULL, y = "Deaths (weekly total)"
  ) +
  ggplot2::theme_minimal(base_size = 16) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "top",
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
  )

ggplot2::ggsave("plots/age_groups/eu_vs_mzcr_0_4_and_0_14.png", p_lines, width = 12, height = 8, dpi = 150, bg = "white")
print(p_lines)
cat("Saved: plots/age_groups/eu_vs_mzcr_0_4_and_0_14.png\n")

# Weekly difference (Eurostat − MZCR); zero baseline line
p_diff <- comp_both %>%
  ggplot2::ggplot(ggplot2::aes(x = week_start, y = diff, colour = bucket)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "gray50") +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::scale_color_manual(values = c("0–4" = col_eu, "0–14" = col_mzcr), name = "Bucket") +
  ggplot2::scale_x_date(
    breaks = scales::breaks_width("5 weeks"),
    labels = function(d) ISOweek::ISOweek(d),
    expand = ggplot2::expansion(mult = c(0.005, 0.01))
  ) +
  ggplot2::labs(
    title = "Weekly difference (Eurostat − MZCR)",
    subtitle = "Comparison of buckets 0–4 vs 0–14 (2020-W13 to 2023-12-31)",
    x = NULL, y = "Deaths (Eurostat − MZCR)"
  ) +
  ggplot2::theme_minimal(base_size = 16) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "top",
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
  )

ggplot2::ggsave("plots/age_groups/diff_eu_minus_mzcr_0_4_vs_0_14.png", p_diff, width = 12, height = 6.5, dpi = 150, bg = "white")
print(p_diff)
cat("Saved: plots/age_groups/diff_eu_minus_mzcr_0_4_vs_0_14.png\n")

# ------------------------------------------------
# Summaries & exports
# ------------------------------------------------
summarise_bucket <- function(comp_df) {
  denom <- sum(comp_df$Eurostat + comp_df$MZCR, na.rm = TRUE)
  data.frame(
    bucket                = unique(comp_df$bucket),
    weeks                 = nrow(comp_df),
    eurostat_total        = sum(comp_df$Eurostat, na.rm = TRUE),
    mzcr_total            = sum(comp_df$MZCR,     na.rm = TRUE),
    total_diff_pos        = sum(comp_df$diff_pos, na.rm = TRUE),   # zero-truncated "missing"
    sum_abs_diffs         = sum(comp_df$diff_abs, na.rm = TRUE),
    total_pct_weekly_err  = ifelse(denom > 0, 100 * sum(comp_df$diff_abs, na.rm = TRUE) / denom, NA_real_),
    stringsAsFactors = FALSE
  )
}

sum_0_4  <- summarise_bucket(comp_0_4)
sum_0_14 <- summarise_bucket(comp_0_14)

summary_both <- dplyr::bind_rows(sum_0_4, sum_0_14)

# Extra: how much of the 0–14 absolute weekly difference is from the 0–4 bucket
share_abs_0_4_in_0_14 <- ifelse(sum_0_14$sum_abs_diffs > 0,
                                100 * sum_0_4$sum_abs_diffs / sum_0_14$sum_abs_diffs, NA_real_)
cat(sprintf("\nShare of |weekly diff| in 0–14 that comes from 0–4: %.2f%%\n\n", share_abs_0_4_in_0_14))

# Pretty table
summary_both %>%
  dplyr::transmute(
    Bucket = bucket,
    Weeks = weeks,
    `Eurostat total` = scales::comma(eurostat_total),
    `MZCR total`     = scales::comma(mzcr_total),
    `Missing (Σ max(E−M,0))` = scales::comma(total_diff_pos),
    `Σ|E−M|` = scales::comma(sum_abs_diffs),
    `Total % of weekly error` = sprintf("%.2f%%", total_pct_weekly_err)
  ) %>%
  kable(format = "html",
        caption = "0–4 vs 0–14 — Eurostat/MZCR comparison (2020-W13 → 2023-12-31)",
        align = c("l","r","r","r","r","r","r")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped","condensed"))

# Raw CSVs
readr::write_csv(summary_both, "data/age_0_4_vs_0_14_summary.csv")
readr::write_csv(
  comp_both %>%
    dplyr::mutate(
      iso_year = as.integer(format(week_start, "%G")),
      iso_week = as.integer(format(week_start, "%V"))
    ) %>%
    dplyr::select(week_start, iso_year, iso_week, bucket, Eurostat, MZCR, diff, diff_pos, diff_abs),
  "data/age_0_4_vs_0_14_weekly.csv"
)

cat("Wrote: data/age_0_4_vs_0_14_summary.csv\n")
cat("Wrote: data/age_0_4_vs_0_14_weekly.csv\n")

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

# Write a compact one-line CSV with these totals
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

# ================================================================
# BY AGE GROUP: Total % of weekly error (Eurostat − MZCR)
# Window: 2020-W13 .. 2023-12-31
# ================================================================

baseline_start <- ISOweek::ISOweek2date("2020-W13-1")
cutoff         <- as.Date("2023-12-31")
age_levels_combined <- c("Unknown","0–14","15–24","25–49","50–59","60–69","70–79","80+")

age_weekly_error <- combined_by_age %>%
  dplyr::filter(week_start >= baseline_start, week_start <= cutoff) %>%
  dplyr::mutate(age_group = factor(age_group, levels = age_levels_combined)) %>%
  tidyr::pivot_wider(
    id_cols     = c(week_start, age_group),
    names_from  = source,
    values_from = deaths,
    values_fill = 0
  ) %>%
  dplyr::mutate(
    diff     = Eurostat - MZCR,
    diff_pos = pmax(diff, 0),
    diff_neg = pmin(diff, 0),
    diff_abs = abs(diff)
  ) %>%
  dplyr::group_by(age_group) %>%
  dplyr::summarise(
    sum_pos        = sum(diff_pos, na.rm = TRUE),
    sum_neg        = sum(diff_neg, na.rm = TRUE),   # negative
    sum_abs        = sum(diff_abs, na.rm = TRUE),
    eurostat_total = sum(Eurostat, na.rm = TRUE),
    mzcr_total     = sum(MZCR,     na.rm = TRUE),
    denom_both     = eurostat_total + mzcr_total,
    pct_weekly_err = dplyr::if_else(denom_both > 0, 100 * sum_abs / denom_both, NA_real_),
    .groups = "drop"
  ) %>%
  dplyr::arrange(factor(age_group, levels = age_levels_combined))

# --- CSV with the percentages ---
write.csv(age_weekly_error, "data/age_group_weekly_error_pct_2020W13_2023.csv", row.names = FALSE)
cat("Wrote: data/age_group_weekly_error_pct_2020W13_2023.csv\n")

# --- Table (pretty) ---
age_weekly_error %>%
  dplyr::transmute(
    `Age group`               = age_group,
    `Eurostat total`          = eurostat_total,
    `MZCR total`              = mzcr_total,
    `Positive diffs`          = sum_pos,
    `Negative diffs`          = sum_neg,
    `Sum of abs. negative & positive weekly diffs`   = sum_abs,
    `Total % of weekly error` = pct_weekly_err
  ) %>%
  dplyr::mutate(
    `Eurostat total`          = scales::comma(`Eurostat total`),
    `MZCR total`              = scales::comma(`MZCR total`),
    `Positive diffs`          = scales::comma(`Positive diffs`),
    `Negative diffs`          = scales::comma(`Negative diffs`),
    `Sum of abs. negative & positive weekly diffs`   = scales::comma(`Sum of abs. negative & positive weekly diffs`),
    `Total % of weekly error` = sprintf("%.2f%%", `Total % of weekly error`)
  ) %>%
  kable(format = "html",
        caption = "By age group — Total % of weekly error (2020-W13 to 2023-12-31)",
        align = c("l","r","r","r","r")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped","condensed"))

# --- Plot (bar per age group) ---
gg_age_err <- age_weekly_error %>%
  ggplot(aes(x = age_group, y = pct_weekly_err, fill = age_group)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.2f%%", pct_weekly_err)),
            vjust = -0.3, size = 4) +
  labs(
    title = "Eurostat − MZCR: Total % of weekly error by age group",
    subtitle = "Σ|Eurostat − MZCR| divided by Σ(Eurostat + MZCR), 2020-W13 to 2023-12-31",
    x = NULL, y = "% of total deaths in both datasets"
  ) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none", panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggsave("plots/age_groups/weekly_error_pct_by_age.png", gg_age_err, width = 12, height = 6, dpi = 150)
print(gg_age_err)
cat("Saved: plots/age_groups/weekly_error_pct_by_age.png\n")

# ================================================================
# UNKNOWN DEATHS COMPENSATION ANALYSIS FOR 15+ AGE GROUP
# ================================================================

cat("\n\n================================================================\n")
cat("UNKNOWN DEATHS COMPENSATION ANALYSIS FOR 15+ AGE GROUP\n")
cat("================================================================\n\n")

# Function to create the unknown compensation visualization (15+)
create_unknown_compensation_plot <- function(combined_data) {
  
  cat("Preparing data for Unknown compensation analysis (15+)...\n")

  # Step 0: separator hard-coded to 2021 week 10 (Monday of ISO week)
  separator_week <- ISOweek::ISOweek2date("2021-W10-1")

  # Step 1: Use Unknown deaths from MZCR as-is
  unknown_adjusted <- combined_data %>%
    dplyr::filter(age_group == "Unknown", source == "MZCR") %>%
    dplyr::select(week_start, unknown_deaths = deaths) %>%
    dplyr::arrange(week_start) %>%
    dplyr::mutate(
      available_unknown = pmax(0, unknown_deaths)  # effectively just unknown_deaths
    ) %>%
    dplyr::select(week_start, unknown_deaths, available_unknown)
  
  # Step 2: Calculate missing deaths in 15+ group
  # (sum of 15–24, 25–49, 50–59, 60–69, 70–79, 80+)
  missing_15plus <- combined_data %>%
    dplyr::filter(age_group %in% c("15–24","25–49","50–59","60–69","70–79","80+")) %>%
    dplyr::group_by(week_start, source) %>%
    dplyr::summarise(deaths_15plus = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
      id_cols = week_start,
      names_from = source,
      values_from = deaths_15plus,
      values_fill = 0
    ) %>%
    dplyr::mutate(
      missing_15plus = pmax(0, Eurostat - MZCR)  # Missing deaths in 15+ (Eurostat > MZCR)
    ) %>%
    dplyr::select(week_start, Eurostat_15plus = Eurostat, MZCR_15plus = MZCR, missing_15plus)
  
  # Step 3: Combine and calculate compensation
  compensation_data <- unknown_adjusted %>%
    dplyr::left_join(missing_15plus, by = "week_start") %>%
    dplyr::mutate(
      missing_15plus = tidyr::replace_na(missing_15plus, 0),
      # Calculate compensation
      can_compensate = pmin(available_unknown, missing_15plus),
      compensation_status = dplyr::case_when(
        missing_15plus == 0 ~ "No deficit",
        available_unknown >= missing_15plus ~ "Full",
        available_unknown > 0 & available_unknown < missing_15plus ~ "Partial",
        TRUE ~ "None"
      ),
      uncompensated = pmax(0, missing_15plus - available_unknown),
      excess_unknown = pmax(0, available_unknown - missing_15plus),
      period = ifelse(week_start < separator_week, "before", "after")
    ) %>%
    dplyr::filter(!is.na(week_start)) %>%
    dplyr::arrange(week_start)
  
  # Summary statistics
  total_weeks <- nrow(compensation_data)
  weeks_full_comp     <- sum(compensation_data$compensation_status == "Full", na.rm = TRUE)
  weeks_partial_comp  <- sum(compensation_data$compensation_status == "Partial", na.rm = TRUE)
  weeks_no_comp       <- sum(compensation_data$compensation_status == "None", na.rm = TRUE)
  weeks_no_deficit    <- sum(compensation_data$compensation_status == "No deficit", na.rm = TRUE)
  weeks_excess        <- sum(compensation_data$excess_unknown > 0, na.rm = TRUE)
  
  total_missing_15plus   <- sum(compensation_data$missing_15plus, na.rm = TRUE)
  total_compensated      <- sum(compensation_data$can_compensate, na.rm = TRUE)
  total_uncompensated    <- sum(compensation_data$uncompensated, na.rm = TRUE)
  total_excess_unknown   <- sum(compensation_data$excess_unknown, na.rm = TRUE)
  
  compensation_rate <- ifelse(total_missing_15plus > 0, 
                              100 * total_compensated / total_missing_15plus, 0)
  
  # Prepare data for plotting
  plot_data <- compensation_data %>%
    dplyr::mutate(
      # For stacked bar chart
      compensated_full = ifelse(compensation_status == "Full",    missing_15plus, 0),
      compensated_partial = ifelse(compensation_status == "Partial", can_compensate, 0),
      uncompensated_partial = ifelse(compensation_status == "Partial", uncompensated, 0),
      uncompensated_none = ifelse(compensation_status == "None",  missing_15plus, 0)
    )
  
  # Background shading for "No deficit" weeks (7-day window centered on each week)
  shade_df <- plot_data %>%
    dplyr::filter(compensation_status == "No deficit") %>%
    dplyr::transmute(
      xmin = week_start - 3.5,
      xmax = week_start + 3.5
    )

  # Main plot
  p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = week_start)) +
    ggplot2::geom_rect(
      data = shade_df,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = Inf),
      fill = "gray95", alpha = 0.5,
      inherit.aes = FALSE
    ) +
    # Stacked bars for compensation status
    ggplot2::geom_bar(ggplot2::aes(y = compensated_full), stat = "identity",
                      fill = "#51CF66", alpha = 0.8, width = 7) +
    ggplot2::geom_bar(ggplot2::aes(y = compensated_partial), stat = "identity",
                      fill = "#FFB84D", alpha = 0.8, width = 7) +
    ggplot2::geom_bar(ggplot2::aes(y = compensated_partial + uncompensated_partial), stat = "identity",
                      fill = "#FF6B6B", alpha = 0.5, width = 7) +
    # Lines
    ggplot2::geom_line(ggplot2::aes(y = available_unknown), color = "#495057", linewidth = 1, alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(y = available_unknown), color = "#495057", size = 0.8, alpha = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = missing_15plus), color = "#C92A2A", linewidth = 1, linetype = "dashed") +
    # Vertical separator at 2021-W10 (hard-coded)
    ggplot2::geom_vline(xintercept = as.numeric(separator_week),
                        linetype = "dashed", color = "black", linewidth = 0.6) +
    ggplot2::annotate("text",
                      x = separator_week, y = 0, label = "Week 10, 2021",
                      angle = 90, hjust = -0.2, vjust = -0.5, size = 3, color = "gray40") +
    ggplot2::scale_x_date(
      date_labels = "%b %Y",
      date_breaks = "3 months",
      expand = ggplot2::expansion(mult = c(0.01, 0.01))
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::labs(
      title = "Weekly Compensation of 15+ Missing Deaths Using Unknown Category",
      subtitle = "Using MZCR Unknown as-is | Green: Full compensation | Orange: Partial | Red: Uncompensated",
      x = "Week",
      y = "Number of Deaths",
      caption = sprintf(
        "Total missing in 15+: %s | Compensated: %s (%.1f%%) | Uncompensated: %s",
        format(round(total_missing_15plus), big.mark = ","),
        format(round(total_compensated),  big.mark = ","),
        compensation_rate,
        format(round(total_uncompensated), big.mark = ",")
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10, color = "gray40"),
      plot.caption  = ggplot2::element_text(hjust = 0.5, size = 9, color = "gray50"),
      legend.position = "top",
      axis.text.x   = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(15,15,15,15)
    )

  # Optional: manual line legend
  max_y <- max(c(plot_data$missing_15plus, plot_data$available_unknown), na.rm = TRUE)
  p1 <- p1 +
    ggplot2::annotate("segment", x = min(plot_data$week_start) + 30,
                      xend = min(plot_data$week_start) + 90,
                      y = max_y * 1.1, yend = max_y * 1.1,
                      color = "#495057", linewidth = 1) +
    ggplot2::annotate("text", x = min(plot_data$week_start) + 95, 
                      y = max_y * 1.1, label = "Available Unknown",
                      hjust = 0, size = 3, color = "#495057") +
    ggplot2::annotate("segment", x = min(plot_data$week_start) + 280,
                      xend = min(plot_data$week_start) + 340,
                      y = max_y * 1.1, yend = max_y * 1.1,
                      color = "#C92A2A", linewidth = 1, linetype = "dashed") +
    ggplot2::annotate("text", x = min(plot_data$week_start) + 345,
                      y = max_y * 1.1, label = "Missing 15+ deaths",
                      hjust = 0, size = 3, color = "#C92A2A")

  # Also keep the vertical line before the first 'Partial' week (left edge of that bar)
  first_partial_week <- plot_data %>%
    dplyr::filter(compensation_status == "Partial") %>%
    dplyr::summarise(first_week = min(week_start, na.rm = TRUE)) %>%
    dplyr::pull(first_week)

  if (!is.na(first_partial_week) && is.finite(as.numeric(first_partial_week))) {
    p1 <- p1 +
      ggplot2::geom_vline(
        xintercept = as.numeric(first_partial_week) - 3.5,
        linetype = "dashed",
        color = "black",
        linewidth = 0.6
      )
  }
  
  # Summary bar chart
  summary_df <- data.frame(
    Category = c("Fully\nCompensated", "Partially\nCompensated", "Not\nCompensated", "No Deficit", "Excess\nUnknown"),
    Weeks = c(weeks_full_comp, weeks_partial_comp, weeks_no_comp, weeks_no_deficit, weeks_excess),
    Color = c("#51CF66", "#FFB84D", "#FF6B6B", "#ADB5BD", "#74C0FC")
  )
  
  p2 <- ggplot2::ggplot(summary_df, ggplot2::aes(x = Category, y = Weeks, fill = Color)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = Weeks), vjust = -0.3, size = 4, fontface = "bold") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::labs(
      title = "Weekly Compensation Status Summary (15+)",
      subtitle = sprintf("Total weeks analyzed: %d", total_weeks),
      x = NULL,
      y = "Number of Weeks"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10, color = "gray50"),
      axis.text.x = ggplot2::element_text(size = 10),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  # Time series of compensation percentage
  p3 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = week_start)) +
    ggplot2::geom_area(ggplot2::aes(y = 100 * can_compensate / pmax(1, missing_15plus)), 
              fill = "#74C0FC", alpha = 0.3) +
    ggplot2::geom_line(ggplot2::aes(y = 100 * can_compensate / pmax(1, missing_15plus)), 
              color = "#339AF0", linewidth = 1) +
    ggplot2::geom_hline(yintercept = 100, linetype = "dashed", color = "gray50", alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 50,  linetype = "dotted", color = "gray50", alpha = 0.5) +
    ggplot2::scale_x_date(
      date_labels = "%b %Y",
      date_breaks = "3 months",
      expand = ggplot2::expansion(mult = c(0.01, 0.01))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),
      labels = function(x) paste0(x, "%"),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::labs(
      title = "Weekly Compensation Rate: Unknown Deaths vs 15+ Deficit",
      subtitle = "Percentage of 15+ missing deaths that can be compensated by Unknown deaths",
      x = "Week",
      y = "Compensation Rate"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10, color = "gray50"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  # Return results
  return(list(
    main_plot = p1,
    summary_plot = p2,
    rate_plot = p3,
    data = compensation_data,
    statistics = list(
      total_weeks = total_weeks,
      weeks_full = weeks_full_comp,
      weeks_partial = weeks_partial_comp,
      weeks_none = weeks_no_comp,
      weeks_no_deficit = weeks_no_deficit,
      weeks_excess = weeks_excess,
      total_missing = total_missing_15plus,
      total_compensated = total_compensated,
      total_uncompensated = total_uncompensated,
      total_excess = total_excess_unknown,
      compensation_rate = compensation_rate
    )
  ))
}

# ================================================================
# EXECUTE: Create the Unknown compensation visualization (15+)
# ================================================================

# Combine MZCR and Eurostat data if not already done
if (!exists("combined_by_age")) {
  combined_by_age <- dplyr::bind_rows(
    weekly_mzcr %>% dplyr::mutate(source = "MZCR"),
    weekly_eu   %>% dplyr::mutate(source = "Eurostat")
  ) %>% dplyr::filter(!is.na(deaths), deaths >= 0)
}

# Create output directory if it doesn't exist
if (!dir.exists("plots/compensation")) {
  dir.create("plots/compensation", recursive = TRUE)
}

# Generate the analysis
cat("\nGenerating Unknown compensation analysis for 15+ age group...\n")
result <- create_unknown_compensation_plot(combined_by_age)

# Save plots
ggplot2::ggsave("plots/compensation/unknown_15plus_main.png", 
       result$main_plot, 
       width = 16, height = 9, dpi = 150, bg = "white")
cat("  Main plot saved: plots/compensation/unknown_15plus_main.png\n")

ggplot2::ggsave("plots/compensation/unknown_15plus_summary.png", 
       result$summary_plot, 
       width = 10, height = 6, dpi = 150, bg = "white")
cat("  Summary plot saved: plots/compensation/unknown_15plus_summary.png\n")

ggplot2::ggsave("plots/compensation/unknown_15plus_rate.png", 
       result$rate_plot, 
       width = 14, height = 6, dpi = 150, bg = "white")
cat("  Rate plot saved: plots/compensation/unknown_15plus_rate.png\n")

# Display plots
print(result$main_plot)
print(result$summary_plot)
print(result$rate_plot)

# Print detailed statistics
cat("\n=== UNKNOWN COMPENSATION STATISTICS (15+) ===\n")
cat("---------------------------------------------\n")
cat(sprintf("Total weeks analyzed: %d\n", result$statistics$total_weeks))
cat(sprintf("\nWeekly breakdown:\n"))
cat(sprintf("  Fully compensated:     %d weeks (%.1f%%)\n", 
            result$statistics$weeks_full,
            100 * result$statistics$weeks_full / result$statistics$total_weeks))
cat(sprintf("  Partially compensated: %d weeks (%.1f%%)\n", 
            result$statistics$weeks_partial,
            100 * result$statistics$weeks_partial / result$statistics$total_weeks))
cat(sprintf("  Not compensated:       %d weeks (%.1f%%)\n", 
            result$statistics$weeks_none,
            100 * result$statistics$weeks_none / result$statistics$total_weeks))
cat(sprintf("  No deficit:            %d weeks (%.1f%%)\n", 
            result$statistics$weeks_no_deficit,
            100 * result$statistics$weeks_no_deficit / result$statistics$total_weeks))
cat(sprintf("  Excess unknown:        %d weeks\n", result$statistics$weeks_excess))

cat(sprintf("\nDeath totals:\n"))
cat(sprintf("  Total missing in 15+:        %s deaths\n", 
            format(round(result$statistics$total_missing), big.mark = ",")))
cat(sprintf("  Total compensated:           %s deaths (%.1f%%)\n", 
            format(round(result$statistics$total_compensated), big.mark = ","),
            result$statistics$compensation_rate))
cat(sprintf("  Total uncompensated:         %s deaths\n", 
            format(round(result$statistics$total_uncompensated), big.mark = ",")))
cat(sprintf("  Total excess unknown:        %s deaths\n", 
            format(round(result$statistics$total_excess), big.mark = ",")))

# Save detailed data
write.csv(result$data, 
          "data/unknown_compensation_15plus_weekly.csv", 
          row.names = FALSE)
cat("\nDetailed weekly data saved: data/unknown_compensation_15plus_weekly.csv\n")

# Create summary report
summary_report <- data.frame(
  Metric = c(
    "Total weeks analyzed",
    "Weeks with full compensation",
    "Weeks with partial compensation", 
    "Weeks with no compensation",
    "Weeks with no deficit",
    "Weeks with excess unknown deaths",
    "Total missing deaths in 15+",
    "Total deaths compensated",
    "Total deaths uncompensated",
    "Total excess unknown deaths",
    "Overall compensation rate"
  ),
  Value = c(
    result$statistics$total_weeks,
    result$statistics$weeks_full,
    result$statistics$weeks_partial,
    result$statistics$weeks_none,
    result$statistics$weeks_no_deficit,
    result$statistics$weeks_excess,
    round(result$statistics$total_missing),
    round(result$statistics$total_compensated),
    round(result$statistics$total_uncompensated),
    round(result$statistics$total_excess),
    sprintf("%.1f%%", result$statistics$compensation_rate)
  ),
  stringsAsFactors = FALSE
)

write.csv(summary_report, 
          "data/unknown_compensation_15plus_summary.csv", 
          row.names = FALSE)
cat("Summary report saved: data/unknown_compensation_15plus_summary.csv\n")

# Detailed breakdown by period
period_breakdown <- result$data %>%
  dplyr::mutate(
    year = as.integer(format(week_start, "%Y")),
    quarter = quarters(week_start)
  ) %>%
  dplyr::group_by(year, quarter) %>%
  dplyr::summarise(
    weeks = dplyr::n(),
    avg_missing_15plus     = mean(missing_15plus, na.rm = TRUE),
    avg_available_unknown  = mean(available_unknown, na.rm = TRUE),
    avg_compensated        = mean(can_compensate, na.rm = TRUE),
    total_compensated      = sum(can_compensate, na.rm = TRUE),
    total_uncompensated    = sum(uncompensated, na.rm = TRUE),
    compensation_rate      = 100 * sum(can_compensate, na.rm = TRUE) / 
                             pmax(1, sum(missing_15plus, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::arrange(year, quarter)

write.csv(period_breakdown, 
          "data/unknown_compensation_15plus_by_period.csv", 
          row.names = FALSE)
cat("Period breakdown saved: data/unknown_compensation_15plus_by_period.csv\n")

cat("\n=== ANALYSIS COMPLETED (15+) ===\n")
cat("All plots and data files have been generated successfully!\n")

# ================================================================
# ENHANCED PLOTS FOR AGE GROUPS 15-24 TO 60-69
# ================================================================

cat("\n\n================================================================\n")
cat("Creating Enhanced Age Group Plots with Deficit/Compensation Areas\n")
cat("================================================================\n\n")

# Function to create enhanced age group plot with signed inner-area fill only for contiguous runs
create_enhanced_age_plot <- function(combined_data,
                                     age_group_name,
                                     separator_week,
                                     min_run_weeks = 3,     # change to 2 if you want shorter runs filled
                                     show_separator = TRUE) {

  age_data <- combined_data %>%
    dplyr::filter(age_group == age_group_name) %>%
    tidyr::pivot_wider(
      id_cols = week_start,
      names_from = source,
      values_from = deaths,
      values_fill = 0
    ) %>%
    dplyr::arrange(week_start) %>%
    dplyr::mutate(
      diff = Eurostat - MZCR,
      sign = dplyr::case_when(
        diff > 0 ~ "pos",      # Eurostat > MZCR
        diff < 0 ~ "neg",      # MZCR > Eurostat
        TRUE     ~ "zero"
      ),
      # run id increments whenever the sign changes (zeros break runs)
      run_id = cumsum(dplyr::coalesce(sign != dplyr::lag(sign), FALSE))
    ) %>%
    dplyr::group_by(run_id) %>%
    dplyr::mutate(run_len = dplyr::n()) %>%
    dplyr::ungroup()

  # Keep only runs long enough to avoid 1-week triangles
  red_df   <- age_data %>% dplyr::filter(sign == "pos", run_len >= min_run_weeks)
  green_df <- age_data %>% dplyr::filter(sign == "neg", run_len >= min_run_weeks)

  # Build the plot
  p <- ggplot(age_data, aes(x = week_start)) +
    # Red: Eurostat above MZCR (filled per contiguous run)
    geom_ribbon(
      data = red_df,
      aes(ymin = MZCR, ymax = Eurostat, group = run_id),
      fill = "red", alpha = 0.25, na.rm = TRUE
    ) +
    # Green: MZCR above Eurostat (filled per contiguous run)
    geom_ribbon(
      data = green_df,
      aes(ymin = Eurostat, ymax = MZCR, group = run_id),
      fill = "green", alpha = 0.25, na.rm = TRUE
    ) +
    # Lines
    geom_line(aes(y = Eurostat, color = "Eurostat"), linewidth = 1.1) +
    geom_line(aes(y = MZCR,     color = "MZCR"),     linewidth = 1.1) +
    # Optional reference line for Week 10, 2021
    { if (show_separator)
        geom_vline(xintercept = as.numeric(separator_week),
                   linetype = "dashed", color = "black", linewidth = 0.8)
      else
        NULL } +
    { if (show_separator)
        annotate("text", x = separator_week, y = 0,
                 label = "Week 10, 2021", angle = 90,
                 hjust = 0, vjust = -0.5, size = 3, color = "gray40")
      else
        NULL } +
    # Styling
    scale_color_manual(
      name = "Data Source",
      values = c("Eurostat" = "#8B0000", "MZCR" = "#00008B"),
      labels = c("Eurostat", "MZCR")
    ) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", expand = c(0.01, 0.01)) +
    scale_y_continuous(labels = scales::comma, expand = c(0.02, 0)) +
    labs(
      title    = sprintf("Weekly Deaths Comparison: Age Group %s", age_group_name),
      subtitle = sprintf("Inner area filled for runs ≥ %d weeks: red if Eurostat > MZCR; green if MZCR > Eurostat", min_run_weeks),
      x = "Date", y = "Weekly Deaths"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray30"),
      legend.position = "top",
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.title = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.3, color = "gray90"),
      plot.margin = margin(10, 10, 10, 10)
    )

  list(plot = p, data = age_data)
}


# ================================================================
# EXECUTE: Create the enhanced plots
# ================================================================

# Combine MZCR and Eurostat data if not already done
if (!exists("combined_by_age")) {
  combined_by_age <- bind_rows(
    weekly_mzcr %>% mutate(source = "MZCR"),
    weekly_eu %>% mutate(source = "Eurostat")
  ) %>%
    filter(!is.na(deaths), deaths >= 0)
}

# Define the separator (Week 10, 2021)
separator_date <- ISOweek::ISOweek2date("2021-W10-1")

# Create output directory if it doesn't exist
if (!dir.exists("plots/age_groups")) {
  dir.create("plots/age_groups", recursive = TRUE)
}

# Generate plot for 15-24 age group
cat("Generating enhanced plot for Age Group 15-24...\n")
result_15_24 <- create_enhanced_age_plot(combined_by_age, "15–24", separator_date)
ggsave("plots/age_groups/enhanced_deaths_15_24.png", 
       result_15_24$plot, 
       width = 14, height = 8, dpi = 150, bg = "white")
print(result_15_24$plot)

# Generate plot for 25-49 age group
cat("Generating enhanced plot for Age Group 25-49...\n")
result_25_49 <- create_enhanced_age_plot(combined_by_age, "25–49", separator_date)
ggsave("plots/age_groups/enhanced_deaths_25_49.png", 
       result_25_49$plot, 
       width = 14, height = 8, dpi = 150, bg = "white")
print(result_25_49$plot)

# Generate plot for 50-59 age group
cat("Generating enhanced plot for Age Group 50-59...\n")
result_50_59 <- create_enhanced_age_plot(combined_by_age, "50–59", separator_date)
ggsave("plots/age_groups/enhanced_deaths_50_59.png", 
       result_50_59$plot, 
       width = 14, height = 8, dpi = 150, bg = "white")
print(result_50_59$plot)

# Generate plot for 60-69 age group
cat("Generating enhanced plot for Age Group 60-69...\n")
result_60_69 <- create_enhanced_age_plot(combined_by_age, "60–69", separator_date)
ggsave("plots/age_groups/enhanced_deaths_60_69.png",
       result_60_69$plot,
       width = 14, height = 8, dpi = 150, bg = "white")
print(result_60_69$plot)

# Generate plot for 70-79 age group
cat("Generating enhanced plot for Age Group 70-79...\n")
result_70_79 <- create_enhanced_age_plot(combined_by_age, "70–79", separator_date)
ggsave("plots/age_groups/enhanced_deaths_70_79.png",
       result_70_79$plot,
       width = 14, height = 8, dpi = 150, bg = "white")
print(result_70_79$plot)

# Generate plot for 80+ age group
cat("Generating enhanced plot for Age Group 80+...\n")
result_80_plus <- create_enhanced_age_plot(combined_by_age, "80+", separator_date)
ggsave("plots/age_groups/enhanced_deaths_80.png",
       result_80_plus$plot,
       width = 14, height = 8, dpi = 150, bg = "white")
print(result_80_plus$plot)


# ================================================================
# SUMMARY STATISTICS
# ================================================================

cat("\n=== ENHANCED PLOTS SUMMARY ===\n")
cat("-------------------------------\n")

fmt_int <- function(x) format(round(x), big.mark = ",", trim = TRUE)
fmt_pct <- function(x) ifelse(is.na(x), "NA", sprintf("%.1f%%", x))

# 15–24
cat("\nAge Group 15-24:\n")
cat(sprintf("  Total red area (Eurostat > MZCR):  %s deaths\n", fmt_int(result_15_24$total_red)))
cat(sprintf("  Total green area (MZCR > Eurostat): %s deaths\n", fmt_int(result_15_24$total_green)))
cat(sprintf("  Green as %% of red: %s\n", fmt_pct(result_15_24$pct_green_of_red)))

# 25–49
cat("\nAge Group 25-49:\n")
cat(sprintf("  Total red area (Eurostat > MZCR):  %s deaths\n", fmt_int(result_25_49$total_red)))
cat(sprintf("  Total green area (MZCR > Eurostat): %s deaths\n", fmt_int(result_25_49$total_green)))
cat(sprintf("  Green as %% of red: %s\n", fmt_pct(result_25_49$pct_green_of_red)))

# 50–59
cat("\nAge Group 50-59:\n")
cat(sprintf("  Total red area (Eurostat > MZCR):  %s deaths\n", fmt_int(result_50_59$total_red)))
cat(sprintf("  Total green area (MZCR > Eurostat): %s deaths\n", fmt_int(result_50_59$total_green)))
cat(sprintf("  Green as %% of red: %s\n", fmt_pct(result_50_59$pct_green_of_red)))

# 60–69
cat("\nAge Group 60-69:\n")
cat(sprintf("  Total red area (Eurostat > MZCR):  %s deaths\n", fmt_int(result_60_69$total_red)))
cat(sprintf("  Total green area (MZCR > Eurostat): %s deaths\n", fmt_int(result_60_69$total_green)))
cat(sprintf("  Green as %% of red: %s\n", fmt_pct(result_60_69$pct_green_of_red)))

# Create summary data frame
summary_enhanced <- data.frame(
  age_group         = c("15–24", "25–49", "50–59", "60–69"),
  total_red_area    = c(result_15_24$total_red,   result_25_49$total_red,   result_50_59$total_red,   result_60_69$total_red),
  total_green_area  = c(result_15_24$total_green, result_25_49$total_green, result_50_59$total_green, result_60_69$total_green),
  green_as_pct_red  = c(result_15_24$pct_green_of_red, result_25_49$pct_green_of_red, result_50_59$pct_green_of_red, result_60_69$pct_green_of_red),
  stringsAsFactors = FALSE
)

write.csv(summary_enhanced,
          "data/enhanced_age_groups_signed_areas_summary.csv",
          row.names = FALSE)
cat("\nSummary saved to: data/enhanced_age_groups_signed_areas_summary.csv\n")

