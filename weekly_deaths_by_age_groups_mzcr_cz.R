library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(ISOweek)
library(tidyr)
library(lubridate)
library(scales)
library(RColorBrewer)

# -------------------------------------------------------------------
# Paths & URLs
# -------------------------------------------------------------------
file_path <- "data/Otevrena-data-NR-26-30-COVID-19-prehled-populace-2024-01.csv"
file_url  <- "https://data.mzcr.cz/data/distribuce/402/Otevrena-data-NR-26-30-COVID-19-prehled-populace-2024-01.csv"

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

# -------------------------------------------------------------------
# Weekly deaths by age group
# -------------------------------------------------------------------
weekly_by_age <- df_aug %>%
  filter(has_death) %>%
  mutate(
    death_week_raw = trimws(DateOfDeathInHealthcareFacility),
    death_week_raw = na_if(death_week_raw, ""),

    # normalize to canonical ISO week "YYYY-Www"
    iso_week = case_when(
      !is.na(death_week_raw) & grepl("^\\d{4}-W\\d{2}$", death_week_raw) ~ death_week_raw,
      !is.na(death_week_raw) & grepl("^\\d{4}-\\d{1,2}$", death_week_raw) ~ {
        yy <- sub("^([0-9]{4}).*$", "\\1", death_week_raw)
        ww <- suppressWarnings(as.integer(sub("^.*-([0-9]{1,2})$", "\\1", death_week_raw)))
        ifelse(!is.na(ww) & ww >= 1 & ww <= 53, sprintf("%s-W%02d", yy, ww), NA_character_)
      },
      TRUE ~ NA_character_
    ),

    # anchor date = Monday of ISO week (for plotting on a date axis)
    week_start = suppressWarnings(ISOweek2date(ifelse(is.na(iso_week), NA, paste0(iso_week, "-1")))),

    # ISO year of death (more robust than substr in case of NA)
    iso_year = suppressWarnings(as.integer(format(week_start, "%G"))),

    # Age handling:
    # - Unknown YearOfBirth -> NA age, will map to "Unknown" group
    # - Known YearOfBirth   -> compute age at death from ISO year
    age_at_death = ifelse(is.na(YearOfBirth_start) | is.na(iso_year),
                          NA_real_,
                          iso_year - YearOfBirth_start),

    # Build age groups incl. "Unknown"
    age_group = dplyr::case_when(
      is.na(YearOfBirth_start) ~ "Unknown",
      !is.na(age_at_death) & age_at_death <= 15 ~ "0–15",
      age_at_death <= 24 ~ "15-24",
      age_at_death <= 49 ~ "25–49",
      age_at_death <= 59 ~ "50–59",
      age_at_death <= 69 ~ "60–69",
      age_at_death <= 79 ~ "70–79",
      age_at_death > 79 ~ "80+",
      TRUE ~ "Unknown"
    )
  ) %>%
  filter(!is.na(week_start)) %>%
  # keep plausible ages when known
  filter(is.na(YearOfBirth_start) | (age_at_death >= 0 & age_at_death <= 110)) %>%
  mutate(
    age_group = factor(
      age_group,
      levels = c("Unknown","0–15","15-24","25–49","50–59","60–69","70–79","80+")
    )
  ) %>%
  group_by(week_start, age_group) %>%
  summarise(deaths = n(), .groups = "drop") %>%
  complete(week_start, age_group, fill = list(deaths = 0)) %>%
  arrange(week_start, age_group)

# -------------------------------------------------------------------
# Plot
# -------------------------------------------------------------------

age_levels <- c("Unknown","0–15","15-24","25–49","50–59","60–69","70–79","80+")
weekly_by_age <- weekly_by_age %>%
  mutate(age_group = factor(age_group, levels = age_levels))

# Build a soft-blue palette: dark for Unknown (bottom) → light for 80+ (top)
pal8 <- brewer.pal(8, "Blues")        # light → dark
fill_vals <- c(
  "Unknown" = pal8[8],  # darkest
  "0–15"    = pal8[7],
  "15-24"   = pal8[6],
  "25–49"   = pal8[5],
  "50–59"   = pal8[4],
  "60–69"   = pal8[3],
  "70–79"   = pal8[2],
  "80+"     = pal8[1]   # lightest
)

ggplot(weekly_by_age, aes(x = week_start, y = deaths, fill = age_group)) +
  geom_area(position = position_stack(reverse = TRUE)) +
  geom_line(
    aes(group = age_group),
    linewidth = 0.25, colour = "black",
    position = position_stack(reverse = TRUE),
    show.legend = FALSE
  ) +
  scale_fill_manual(values = fill_vals, name = "Age group") +
  scale_x_date(
    breaks = scales::breaks_width("5 weeks"),              # tick every 5 weeks
    labels = function(d) ISOweek::ISOweek(d),              # label as YYYY-Www
    expand = expansion(mult = c(0.005, 0.01))
  ) +
  labs(
    title = "MZCR.cz - Weekly COVID-19 deaths by age group (by date of death)",
    subtitle = "Czech Republic - ISO weeks; 'Unknown' = missing year of birth",
    x = NULL, y = "Deaths (weekly total)"
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal(base_size = 18) +                          # larger base font
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
    axis.text.y  = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12),
    plot.title   = element_text(face = "bold", size = 22),
    plot.subtitle= element_text(size = 14)
  )


