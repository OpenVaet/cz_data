library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(ISOweek)

# -------------------------------------------------------------------
# Paths & URLs
# -------------------------------------------------------------------
file_path <- "data/Otevrena-data-NR-26-30-COVID-19-prehled-populace-2024-01.csv"
file_url  <- "https://data.mzcr.cz/data/distribuce/402/Otevrena-data-NR-26-30-COVID-19-prehled-populace-2024-01.csv"

eu_file <- "data/demo_r_mwk_ts_defaultview_linear_2_0.csv"
eu_url  <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/demo_r_mwk_ts/1.0?compress=false&format=csvdata&formatVersion=2.0&lang=en&labels=both"

# New header applied to the CZ file after download
cz_new_header <- paste0(
  '"ID","Infection","Gender","YearOfBirth","DateOfPositivity","DateOfResult","Recovered","Death","Symptom","TestType","Date_First_Dose","Date_Second_Dose","Date_Third_Dose","Date_Fourth_Dose","Date_Fifth_Dose","Date_Sixth_Dose","Date_Seventh_Dose","VaccinationProductCode_First_Dose","VaccinationProductCode_Second_Dose","VaccinationProductCode_Third_Dose","VaccinationProductCode_Fourth_Dose","VaccinationProductCode_Fifth_Dose","VaccinationProductCode_Sixth_Dose","VaccinationProductCode_Seventh_Dose","PrimaryReasonForHospitalizationCOVID","binary_Hospitalization","min_Hospitalization","days_Hospitalization","max_Hospitalization","binary_ICU","min_ICU","days_ICU","max_ICU","binary_StandardWard","min_StandardWard","days_StandardWard","max_StandardWard","binary_Oxygen","min_Oxygen","days_Oxygen","max_Oxygen","binary_HFNO","min_HFNO","days_HFNO","max_HFNO","binary_MechanicalVentilation_ECMO","min_MechanicalVentilation_ECMO","days_MechanicalVentilation_ECMO","max_MechanicalVentilation_ECMO","Mutation","DateOfDeathInHealthcareFacility","Long_COVID","DCCI"'
)

dir.create("data", showWarnings = FALSE, recursive = TRUE)

# -------------------------------------------------------------------
# Ensure the CZ data file is present (download if missing) + replace header
# -------------------------------------------------------------------
if (!file.exists(file_path)) {
  options(timeout = 1200)
  download.file(file_url, file_path, mode = "wb")
  cat("CZ data file downloaded from source.\n")

  # Replace the first line with the provided header
  cz_lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  if (length(cz_lines) > 0) {
    cz_lines[1] <- cz_new_header
    writeLines(cz_lines, file_path, useBytes = TRUE)
    cat("CZ data file header replaced.\n")
  } else {
    stop("Error on download; cannot replace header.")
  }
} else {
  cat("CZ data file found locally.\n")
}

# -------------------------------------------------------------------
# Read the CZ CSV into a dataframe
# -------------------------------------------------------------------
df <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)


# -------------------------------------------------------------------
# Filter once: keep only Infection == 1 or NA
# -------------------------------------------------------------------
df_base <- df %>%
  filter(is.na(Infection) | Infection == 1)

# -------------------------------------------------------------------
# Verify uniqueness of IDs
# -------------------------------------------------------------------
if (anyDuplicated(df_base$ID) > 0) {
  stop("Some IDs are duplicated in the dataset.")
} else {
  cat("All row IDs are unique.\n")
}

# -------------------------------------------------------------------
# Count rows before further filtering
# -------------------------------------------------------------------
rows_before <- nrow(df_base)
print(head(df_base))

# -------------------------------------------------------------------
# Handle YearOfBirth ranges and apply filtering
# -------------------------------------------------------------------
df_aug <- df_base %>%
  mutate(
    Gender = as.character(Gender),
    YearOfBirth_start = suppressWarnings(as.integer(sub("-.*", "", YearOfBirth))),
    has_first_dose = !is.na(Date_First_Dose) & nzchar(trimws(Date_First_Dose)),
    has_death = !is.na(DateOfDeathInHealthcareFacility) & nzchar(trimws(DateOfDeathInHealthcareFacility))
  )
print(df_aug)

keep_pred <- with(df_aug,
  !is.na(Gender) &
  Gender %in% c("1", "2") &
  !is.na(YearOfBirth_start) &
  YearOfBirth_start >= 1925
)

df_filtered <- df_aug %>% filter(keep_pred)
rows_after <- nrow(df_filtered)

# Among filtered-out rows: % with Date_First_Dose completed & % with DateOfDeathInHealthcareFacility
excluded <- df_aug %>% filter(!keep_pred)
excluded_n <- nrow(excluded)
pct_first_dose_in_excluded <- if (excluded_n > 0) 100 * mean(excluded$has_first_dose) else NA_real_
pct_death_in_excluded <- if (excluded_n > 0) 100 * mean(excluded$has_death) else NA_real_

pct_removed <- if (rows_before > 0) 100 * (rows_before - rows_after) / rows_before else NA_real_

cat(sprintf(
  "Rows before filtering: %d\nRows after filtering:  %d\nRemoved: %d (%.2f%%)\n%% with Date_First_Dose among removed rows: %.2f%%\n%% with Death among removed rows: %.2f%%\n",
  rows_before, rows_after, rows_before - rows_after, pct_removed, pct_first_dose_in_excluded, pct_death_in_excluded
))

# -------------------------------------------------------------------
# Recompute summary by YearOfBirth range and Gender
# -------------------------------------------------------------------
summary_year_gender_filtered <- df_filtered %>%
  group_by(YearOfBirth) %>%
  summarise(
    total        = n(),
    total_female = sum(Gender == "1"),
    total_male   = sum(Gender == "2"),
    .groups = "drop"
)
print(summary_year_gender_filtered)

# Formatted table of before/after stats
kable(
  data.frame(
    metric = c("rows_before", "rows_after", "rows_removed", "pct_removed", "pct_first_dose_in_excluded"),
    value  = c(rows_before, rows_after, rows_before - rows_after, round(pct_removed, 2), round(pct_first_dose_in_excluded, 2))
  ),
  col.names = c("Metric", "Value"),
  align = c("l", "r")
) %>%
  kable_styling(full_width = FALSE)

# ---------------------------
# Build MZCR weekly death counts (robust ISO-week parsing)
# ---------------------------
mzcr_weekly <- df_aug %>%
  filter(has_death) %>%
  mutate(
    death_week_raw = trimws(DateOfDeathInHealthcareFacility),

    # If already like "YYYY-Www", keep it; if "YYYY-w" or "YYYY-ww", convert to "YYYY-Www"
    already_iso = grepl("^\\d{4}-W\\d{2}$", death_week_raw),
    yy          = sub("^([0-9]{4}).*$", "\\1", death_week_raw),
    ww          = suppressWarnings(as.integer(sub("^.*-([0-9]{1,2})$", "\\1", death_week_raw))),
    iso_week    = ifelse(already_iso,
                         death_week_raw,
                         ifelse(grepl("^\\d{4}-\\d{1,2}$", death_week_raw) & !is.na(ww),
                                sprintf("%s-W%02d", yy, ww),
                                NA_character_)),

    # Monday of ISO week, ISO week & ISO year
    WeekDate = suppressWarnings(ISOweek::ISOweek2date(paste0(iso_week, "-1"))),
    Week     = as.integer(format(WeekDate, "%V")),  # ISO week number
    Year     = as.integer(format(WeekDate, "%G"))   # ISO year
  ) %>%
  filter(!is.na(WeekDate), Week >= 1, Week <= 53) %>%
  group_by(Year, Week, WeekDate) %>%
  summarise(Deaths = n(), .groups = "drop") %>%
  arrange(WeekDate) %>%
  mutate(source = "Mzcr.cz") %>%
  select(Year, Week, WeekDate, Deaths, source)

print(mzcr_weekly)

# ===================================================================
# Ensures Eurostat data file is present (download if missing)
# ===================================================================
if (!file.exists(eu_file)) {
  download.file(eu_url, eu_file, mode = "wb")
  cat("Eurostat weekly deaths file downloaded.\n")
} else {
  cat("Eurostat weekly deaths file found locally.\n")
}

# Read with original column names preserved
eu <- read.csv(eu_file, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

# The defaultview is WEEKLY and uses colon-labelled headers; normalize them
eu2 <- eu %>%
  rename(
    freq        = `freq: Time frequency`,
    sex         = `sex: Sex`,
    unit        = `unit: Unit of measure`,
    geo         = `geo: Geopolitical entity (reporting)`,
    time        = `TIME_PERIOD: Time`,
    value       = `OBS_VALUE: Observation value`
  ) %>%
  mutate(
    geo_code = sub(":.*$", "", geo),
    geo_name = sub("^.*:\\s*", "", geo)
  ) %>%
  # Keep Czechia only
  filter(geo_code == "CZ") %>%
  # Coerce numeric deaths (value can be character)
  mutate(value = suppressWarnings(as.numeric(value))) %>%
  filter(!is.na(value))

# Collapse across sex categories (use Total if present; otherwise sum)
if (any(grepl("^T\\b", eu2$sex))) {
  eu2_cz <- eu2 %>% filter(grepl("^T\\b", sex))
} else {
  eu2_cz <- eu2 %>%
    group_by(time) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
}

# Parse ISO week string from Eurostat
eu2_cz <- eu2_cz %>%
  mutate(
    time      = trimws(time),
    # Extract year + week, allow 1- or 2-digit weeks, optional 'W'
    yy        = sub("^([0-9]{4}).*$", "\\1", time),
    ww        = suppressWarnings(as.integer(sub("^.*-W?([0-9]{1,2})$", "\\1", time))),
    time_norm = ifelse(grepl("^\\d{4}-W?\\d{1,2}$", time) & !is.na(ww),
                       sprintf("%s-W%02d", yy, ww),
                       NA_character_),
    WeekDate  = suppressWarnings(ISOweek::ISOweek2date(paste0(time_norm, "-1"))),
    Week      = as.integer(format(WeekDate, "%V")),   # ISO week number
    Year      = as.integer(format(WeekDate, "%G")),   # ISO year
    Deaths    = value
  ) %>%
  filter(!is.na(WeekDate), Week >= 1, Week <= 53, Year >= 2020) %>%
  arrange(WeekDate) %>%
  mutate(source = "Eurostat") %>%
  select(Year, Week, WeekDate, Deaths, source)

print(eu2_cz)

# ---------------------------
# Long format for plotting
# ---------------------------
plot_df <- bind_rows(mzcr_weekly, eu2_cz) %>%
  arrange(source, WeekDate) %>%
  group_by(source) %>%
  mutate(
    point_idx = row_number(),
    show_label = point_idx %% 10 == 0
  ) %>%
  ungroup()
print(plot_df)

# ---------------------------
# Plot
# ---------------------------
gg <- ggplot(plot_df, aes(x = WeekDate, y = Deaths, color = source, linetype = source)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8, alpha = 0.7) +
  geom_text(
    data = subset(plot_df, show_label),
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

print(gg)
