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
  library(forcats)
})

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

# --------------------------
# Weekly doses by product type (stacked plot)
# --------------------------
to_iso_monday_flex <- function(x) {
  x0 <- ifelse(is.na(x), NA_character_, trimws(x))
  x0[!nzchar(x0)] <- NA_character_
  x1 <- toupper(x0)

  # Accept: "YYYY-ww", "YYYY-Www", "YYYYWww", "YYYY ww", optionally with day
  rx <- "^\\s*(\\d{4})[-\\s]?W?(\\d{1,2})(?:[-\\s]?([1-7]))?\\s*$"
  ok <- grepl(rx, x1)
  out <- rep(NA_character_, length(x1))

  if (any(ok)) {
    m <- regmatches(x1, regexec(rx, x1))
    for (i in which(ok)) {
      p  <- m[[i]]
      yr <- suppressWarnings(as.integer(p[2]))
      wk <- suppressWarnings(as.integer(p[3]))
      d  <- if (length(p) >= 4 && nchar(p[4])) suppressWarnings(as.integer(p[4])) else 1L
      if (!is.na(yr) && !is.na(wk) && wk >= 1L && wk <= 53L && d >= 1L && d <= 7L) {
        out[i] <- sprintf("%04d-W%02d-%d", yr, wk, d)  # ISOweek2date needs "-d"
      }
    }
  }
  ISOweek::ISOweek2date(out)  # invalids remain NA
}

# --------------------------
# Rebuild long dose data & weekly counts
# --------------------------
dose_cols_pattern <- "^(Date|VaccinationProductCode)_(First|Second|Third|Fourth|Fifth|Sixth|Seventh)_Dose$"

doses_long <- df_aug %>%
  dplyr::select(dplyr::matches(dose_cols_pattern)) %>%
  tidyr::pivot_longer(
    cols = tidyselect::everything(),
    names_to = c(".value", "dose_label"),
    names_pattern = dose_cols_pattern
  ) %>%
  dplyr::mutate(
    week_date   = to_iso_monday_flex(Date),
    product_type = ifelse(is.na(VaccinationProductCode) | !nzchar(trimws(VaccinationProductCode)),
                          "Unknown", trimws(VaccinationProductCode)),
    dose_num     = dplyr::recode(dose_label,
                                 First = 1L, Second = 2L, Third = 3L, Fourth = 4L,
                                 Fifth = 5L, Sixth = 6L, Seventh = 7L, .default = NA_integer_)
  ) %>%
  dplyr::filter(!is.na(week_date))

weekly_type_counts <- doses_long %>%
  dplyr::count(week_date, product_type, name = "n")

# (Optional) check
message("Non-empty rows after parsing: ", nrow(weekly_type_counts))
print(weekly_type_counts %>% dplyr::arrange(week_date) %>% head(10))

# Keep top 7 + lump others into "Other" -> total 8 legend entries
type_levels_top <- weekly_type_counts %>%
  dplyr::group_by(product_type) %>%
  dplyr::summarise(total = sum(n), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(total)) %>%
  dplyr::slice(1:7) %>%
  dplyr::pull(product_type) %>%
  as.character()

# Lump the rest into "Other" and re-aggregate
weekly_type_counts_lumped <- weekly_type_counts %>%
  dplyr::mutate(product_type = forcats::fct_other(
    factor(product_type),                 # ensure factor input
    keep = type_levels_top,               # character vector
    other_level = "Other"
  )) %>%
  dplyr::group_by(week_date, product_type) %>%
  dplyr::summarise(n = sum(n), .groups = "drop")

# Plot
p_weekly <- ggplot(weekly_type_counts_lumped, aes(x = week_date, y = n, fill = product_type)) +
  geom_col() +
  scale_fill_brewer(palette = "Set2", name = "Vaccine / Type") +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_si("")),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "COVID-19 vaccine doses by ISO week — Czechia (MZCR)",
    subtitle = "Top vaccine types; others grouped as 'Other'",
    x = NULL, y = "Number of doses"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        panel.grid.major.x = element_blank())

print(p_weekly)


