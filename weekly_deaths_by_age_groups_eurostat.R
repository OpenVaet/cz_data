library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(ISOweek)

# -------------------------------------------------------------------
# Paths & URLs
# -------------------------------------------------------------------
eu_file <- "data/demo_r_mwk_05_linear_2_0.csv"
eu_url  <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/demo_r_mwk_05/1.0?compress=false&format=csvdata&formatVersion=2.0&lang=en&labels=both"

dir.create("data", showWarnings = FALSE, recursive = TRUE)

# ===================================================================
# Ensures Eurostat data file is present (download if missing)
# ===================================================================
if (!file.exists(eu_file)) {
  options(timeout = 1200)
  download.file(eu_url, eu_file, mode = "wb")
  cat("Eurostat weekly deaths by age group file downloaded.\n")
} else {
  cat("Eurostat weekly deaths by age group file found locally.\n")
}

# Read
eu <- read.csv(eu_file, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

print(eu)

# The defaultview is WEEKLY and uses colon-labelled headers; normalize them
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
    geo_name = sub("^.*:\\s*", "", geo),
    age_code = sub(":.*$", "", age_group),
    age_name = sub("^.*:\\s*", "", age_group),
    sex_code = sub(":.*$", "", sex),
    sex_name = sub("^.*:\\s*", "", sex)
  ) %>%
  filter(geo_code == "CZ") %>%                             # Czechia only
  mutate(value = suppressWarnings(as.numeric(value))) %>%  # numeric deaths
  filter(!is.na(value)) %>%
  filter(age_code != "TOTAL")                              # <-- remove Total age

# Collapse across sex categories (prefer Total if present; otherwise sum M+F)
if (any(eu2$sex_code == "T")) {
  eu2_cz <- eu2 %>%
    filter(sex_code == "T") %>%
    select(time, age_code, age_name, value)
} else {
  eu2_cz <- eu2 %>%
    group_by(time, age_code, age_name) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
}

# --- Compute week_start (Monday of ISO week) ---
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

# --- Rebin to: 0–15, 15–24, 25–49, 50–59, 60–69, 70–79, 80+ (+ Unknown) ---
# Direct mappings (everything except the split band Y15-19)
eu_main <- eu_w %>%
  filter(age_code != "Y15-19") %>%
  mutate(
    age_group = case_when(
      age_code %in% c("Y_LT5","Y5-9","Y10-14") ~ "0–15",
      age_code == "Y20-24"                      ~ "15–24",
      age_code %in% c("Y25-29","Y30-34","Y35-39","Y40-44","Y45-49") ~ "25–49",
      age_code %in% c("Y50-54","Y55-59")        ~ "50–59",
      age_code %in% c("Y60-64","Y65-69")        ~ "60–69",
      age_code %in% c("Y70-74","Y75-79")        ~ "70–79",
      age_code %in% c("Y80-84","Y85-89","Y_GE90") ~ "80+",
      age_code == "UNK"                         ~ "Unknown",
      TRUE                                      ~ NA_character_
    )
  ) %>%
  filter(!is.na(age_group)) %>%
  select(week_start, age_group, value)

# Split Y15-19 across 0–15 (1/5) and 15–24 (4/5)
eu_split <- eu_w %>%
  filter(age_code == "Y15-19") %>%
  transmute(
    week_start,
    `0–15`  = value * 1/5,
    `15–24` = value * 4/5
  ) %>%
  pivot_longer(cols = c(`0–15`, `15–24`),
               names_to = "age_group", values_to = "value")

# Combine and aggregate
age_levels_custom <- c("Unknown","0–15","15–24","25–49","50–59","60–69","70–79","80+")
weekly_by_age_eu <- bind_rows(eu_main, eu_split) %>%
  group_by(week_start, age_group) %>%
  summarise(deaths = sum(value, na.rm = TRUE), .groups = "drop") %>%
  complete(week_start, age_group, fill = list(deaths = 0)) %>%
  mutate(age_group = factor(age_group, levels = age_levels_custom)) %>%
  arrange(week_start, age_group) %>%
  # Truncate to ISO years 2020–2024
  mutate(iso_year = as.integer(format(week_start, "%G"))) %>%
  filter(iso_year >= 2020, iso_year <= 2024) %>%
  select(-iso_year)

# --- Palette: Unknown darkest; younger → older gets lighter, 80+ lightest ---
pal_base <- brewer.pal(9, "Blues")             # light → dark
ramp <- colorRampPalette(pal_base)(length(age_levels_custom) - 1)
fill_vals_eu <- c(
  "Unknown" = pal_base[9],
  setNames(rev(ramp), age_levels_custom[-1])   # youngest dark → oldest light
)

# --- Plot (same style as your other chart) ---
ggplot(weekly_by_age_eu, aes(x = week_start, y = deaths, fill = age_group)) +
  geom_area(position = position_stack(reverse = TRUE)) +
  geom_line(
    aes(group = age_group),
    linewidth = 0.25, colour = "black",
    position   = position_stack(reverse = TRUE),
    show.legend = FALSE
  ) +
  scale_fill_manual(values = fill_vals_eu, name = "Age group") +
  scale_x_date(
    breaks = scales::breaks_width("5 weeks"),
    labels = function(d) ISOweek::ISOweek(d),
    expand  = expansion(mult = c(0.005, 0.01))
  ) +
  labs(
    title    = "Eurostat - Weekly deaths by age group",
    subtitle = "Czech Republic - ISO weeks",
    x = NULL, y = "Deaths (weekly total)"
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
    axis.text.y  = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12),
    plot.title   = element_text(face = "bold", size = 22),
    plot.subtitle= element_text(size = 14)
  )
