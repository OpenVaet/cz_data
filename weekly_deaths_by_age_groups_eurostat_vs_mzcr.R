# ================================================================
# Combined plot: MZCR (blue) over Eurostat (red), super-imposed
# Age buckets: "Unknown","0–15","15-24","25–49","50–59","60–69","70–79","80+"
# ================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(ISOweek)
library(lubridate)
library(scales)
library(RColorBrewer)
library(ggnewscale)   # allows two independent fill scales

dir.create("data", showWarnings = FALSE, recursive = TRUE)

# ---------------------------
# MZCR (individual-level) prep
# ---------------------------
file_path <- "data/Otevrena-data-NR-26-30-COVID-19-prehled-populace-2024-01.csv"
file_url  <- "https://data.mzcr.cz/data/distribuce/402/Otevrena-data-NR-26-30-COVID-19-prehled-populace-2024-01.csv"

cz_new_header <- paste0(
  '"ID","Infection","Gender","YearOfBirth","DateOfPositivity","DateOfResult","Recovered","Death","Symptom","TestType","Date_First_Dose","Date_Second_Dose","Date_Third_Dose","Date_Fourth_Dose","Date_Fifth_Dose","Date_Sixth_Dose","Date_Seventh_Dose","VaccinationProductCode_First_Dose","VaccinationProductCode_Second_Dose","VaccinationProductCode_Third_Dose","VaccinationProductCode_Fourth_Dose","VaccinationProductCode_Fifth_Dose","VaccinationProductCode_Sixth_Dose","VaccinationProductCode_Seventh_Dose","PrimaryReasonForHospitalizationCOVID","binary_Hospitalization","min_Hospitalization","days_Hospitalization","max_Hospitalization","binary_ICU","min_ICU","days_ICU","max_ICU","binary_StandardWard","min_StandardWard","days_StandardWard","max_StandardWard","binary_Oxygen","min_Oxygen","days_Oxygen","max_Oxygen","binary_HFNO","min_HFNO","days_HFNO","max_HFNO","binary_MechanicalVentilation_ECMO","min_MechanicalVentilation_ECMO","days_MechanicalVentilation_ECMO","max_MechanicalVentilation_ECMO","Mutation","DateOfDeathInHealthcareFacility","Long_COVID","DCCI"'
)

if (!file.exists(file_path)) {
  options(timeout = 1200)
  download.file(file_url, file_path, mode = "wb")
  cz_lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  if (length(cz_lines) > 0) {
    cz_lines[1] <- cz_new_header
    writeLines(cz_lines, file_path, useBytes = TRUE)
  } else stop("Error on download; cannot replace header.")
}

df <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)

# Keep Infection == 1 or NA (stable base)
df_base <- df %>% filter(is.na(Infection) | Infection == 1)

df_aug <- df_base %>%
  mutate(
    Gender = as.character(Gender),
    YearOfBirth_start = suppressWarnings(as.integer(sub("-.*", "", YearOfBirth))),
    has_death = !is.na(DateOfDeathInHealthcareFacility) & nzchar(trimws(DateOfDeathInHealthcareFacility))
  )

# --- Weekly deaths by custom age buckets (MZCR) ---
age_levels <- c("Unknown","0–15","15-24","25–49","50–59","60–69","70–79","80+")

weekly_mzcr <- df_aug %>%
  filter(has_death) %>%
  mutate(
    death_week_raw = na_if(trimws(DateOfDeathInHealthcareFacility), ""),
    iso_week = case_when(
      !is.na(death_week_raw) & grepl("^\\d{4}-W\\d{2}$", death_week_raw) ~ death_week_raw,
      !is.na(death_week_raw) & grepl("^\\d{4}-\\d{1,2}$", death_week_raw) ~ {
        yy <- sub("^([0-9]{4}).*$", "\\1", death_week_raw)
        ww <- suppressWarnings(as.integer(sub("^.*-([0-9]{1,2})$", "\\1", death_week_raw)))
        ifelse(!is.na(ww) & ww >= 1 & ww <= 53, sprintf("%s-W%02d", yy, ww), NA_character_)
      },
      TRUE ~ NA_character_
    ),
    week_start = suppressWarnings(ISOweek2date(ifelse(is.na(iso_week), NA, paste0(iso_week, "-1")))),
    iso_year   = suppressWarnings(as.integer(format(week_start, "%G"))),
    age_at_death = ifelse(is.na(YearOfBirth_start) | is.na(iso_year), NA_real_, iso_year - YearOfBirth_start),
    age_group = case_when(
      is.na(YearOfBirth_start) ~ "Unknown",
      !is.na(age_at_death) & age_at_death <= 15 ~ "0–15",
      age_at_death <= 24 ~ "15-24",
      age_at_death <= 49 ~ "25–49",
      age_at_death <= 59 ~ "50–59",
      age_at_death <= 69 ~ "60–69",
      age_at_death <= 79 ~ "70–79",
      age_at_death > 79  ~ "80+",
      TRUE ~ "Unknown"
    )
  ) %>%
  filter(!is.na(week_start)) %>%
  filter(is.na(YearOfBirth_start) | (age_at_death >= 0 & age_at_death <= 110)) %>%
  group_by(week_start, age_group) %>%
  summarise(deaths = n(), .groups = "drop") %>%
  complete(week_start, age_group, fill = list(deaths = 0)) %>%
  mutate(
    age_group = factor(age_group, levels = age_levels),
    source = "MZCR"
  ) %>%
  # Truncate to ISO years 2020–2024 for comparability
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

# Rebin Eurostat to custom buckets (split 15–19 => 1/5 to 0–15, 4/5 to 15-24)
eu_main <- eu_w %>%
  filter(age_code != "Y15-19") %>%
  mutate(
    age_group = case_when(
      age_code %in% c("Y_LT5","Y5-9","Y10-14") ~ "0–15",
      age_code == "Y20-24"                      ~ "15-24",
      age_code %in% c("Y25-29","Y30-34","Y35-39","Y40-44","Y45-49") ~ "25–49",
      age_code %in% c("Y50-54","Y55-59")        ~ "50–59",
      age_code %in% c("Y60-64","Y65-69")        ~ "60–69",
      age_code %in% c("Y70-74","Y75-79")        ~ "70–79",
      age_code %in% c("Y80-84","Y85-89","Y_GE90") ~ "80+",
      age_code == "UNK"                         ~ "Unknown",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(age_group)) %>%
  select(week_start, age_group, value)

eu_split <- eu_w %>%
  filter(age_code == "Y15-19") %>%
  transmute(
    week_start,
    `0–15`  = value * 1/5,
    `15-24` = value * 4/5
  ) %>%
  pivot_longer(cols = c(`0–15`, `15-24`), names_to = "age_group", values_to = "value")

weekly_eu <- bind_rows(eu_main, eu_split) %>%
  group_by(week_start, age_group) %>%
  summarise(deaths = sum(value, na.rm = TRUE), .groups = "drop") %>%
  complete(week_start, age_group, fill = list(deaths = 0)) %>%
  mutate(
    age_group = factor(age_group, levels = age_levels),
    source = "Eurostat"
  ) %>%
  # Truncate to ISO years 2020–2024
  mutate(iso_year = as.integer(format(week_start, "%G"))) %>%
  filter(iso_year >= 2020, iso_year <= 2024) %>%
  select(-iso_year)

# ---------------------------
# Palettes (same lightness ranks across sources)
# ---------------------------
# Unknown darkest; younger → older lighter; 80+ lightest
pal8_blues <- brewer.pal(8, "Blues")  # light → dark
fill_blues <- c(
  "Unknown" = pal8_blues[8],
  "0–15"    = pal8_blues[7],
  "15-24"   = pal8_blues[6],
  "25–49"   = pal8_blues[5],
  "50–59"   = pal8_blues[4],
  "60–69"   = pal8_blues[3],
  "70–79"   = pal8_blues[2],
  "80+"     = pal8_blues[1]
)

pal8_reds <- brewer.pal(8, "Reds")    # light → dark
fill_reds <- c(
  "Unknown" = pal8_reds[8],
  "0–15"    = pal8_reds[7],
  "15-24"   = pal8_reds[6],
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
    title = "Weekly COVID-19 deaths by age group — MZCR (Blue) vs Eurostat (Red)",
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
age_levels_combined <- c("Unknown","0–15","15-24","25–49","50–59","60–69","70–79","80+")
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
