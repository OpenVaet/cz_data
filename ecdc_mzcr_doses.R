suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ISOweek)
  library(readr)
  library(tidyr)
  library(scales)
})

# =========
# Helpers
# =========
to_iso_monday_flex <- function(x) {
  x0 <- ifelse(is.na(x), NA_character_, trimws(x))
  x0[!nzchar(x0)] <- NA_character_
  x1 <- toupper(x0)
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
        out[i] <- sprintf("%04d-W%02d-%d", yr, wk, d)
      }
    }
  }
  ISOweek::ISOweek2date(out)
}

dir.create("data", showWarnings = FALSE, recursive = TRUE)

# =========
# Load MZCR (individual-level) and compute weekly totals of administered doses
# =========
mzcr_path <- "data/Otevrena-data-NR-26-30-COVID-19-prehled-populace-2024-01.csv"
mzcr_url  <- "https://data.mzcr.cz/data/distribuce/402/Otevrena-data-NR-26-30-COVID-19-prehled-populace-2024-01.csv"

# Header fix used in your previous scripts
cz_new_header <- paste0(
  '"ID","Infection","Gender","YearOfBirth","DateOfPositivity","DateOfResult","Recovered","Death","Symptom","TestType","Date_First_Dose","Date_Second_Dose","Date_Third_Dose","Date_Fourth_Dose","Date_Fifth_Dose","Date_Sixth_Dose","Date_Seventh_Dose","VaccinationProductCode_First_Dose","VaccinationProductCode_Second_Dose","VaccinationProductCode_Third_Dose","VaccinationProductCode_Fourth_Dose","VaccinationProductCode_Fifth_Dose","VaccinationProductCode_Sixth_Dose","VaccinationProductCode_Seventh_Dose","PrimaryReasonForHospitalizationCOVID","binary_Hospitalization","min_Hospitalization","days_Hospitalization","max_Hospitalization","binary_ICU","min_ICU","days_ICU","max_ICU","binary_StandardWard","min_StandardWard","days_StandardWard","max_StandardWard","binary_Oxygen","min_Oxygen","days_Oxygen","max_Oxygen","binary_HFNO","min_HFNO","days_HFNO","max_HFNO","binary_MechanicalVentilation_ECMO","min_MechanicalVentilation_ECMO","days_MechanicalVentilation_ECMO","max_MechanicalVentilation_ECMO","Mutation","DateOfDeathInHealthcareFacility","Long_COVID","DCCI"'
)

if (!file.exists(mzcr_path)) {
  options(timeout = 1200)
  download.file(mzcr_url, mzcr_path, mode = "wb")
  cz_lines <- readLines(mzcr_path, warn = FALSE, encoding = "UTF-8")
  if (length(cz_lines) > 0) {
    cz_lines[1] <- cz_new_header
    writeLines(cz_lines, mzcr_path, useBytes = TRUE)
  } else stop("MZCR: download error; cannot replace header.")
}

mzcr <- read.csv(mzcr_path, header = TRUE, stringsAsFactors = FALSE)

# Filter once: keep only Infection == 1 or NA (as in your pipeline)
mzcr_base <- mzcr %>% filter(is.na(Infection) | Infection == 1)

# Long format over the 7 dose-date columns (type of vaccine not needed for totals)
date_cols <- paste0("Date_", c("First","Second","Third","Fourth","Fifth","Sixth","Seventh"), "_Dose")

mzcr_weekly <- mzcr_base %>%
  select(all_of(date_cols)) %>%
  pivot_longer(everything(), names_to = "dose", values_to = "date_str") %>%
  filter(!is.na(date_str) & nzchar(trimws(date_str))) %>%
  mutate(week_date = to_iso_monday_flex(date_str)) %>%
  filter(!is.na(week_date)) %>%
  count(week_date, name = "total_doses") %>%
  arrange(week_date) %>%
  mutate(source = "MZCR")

# =========
# Load ECDC (aggregated) and compute weekly totals across vaccines
# =========
ecdc_path <- "data/ecdc_doses_data.csv"
ecdc_url  <- "https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv"

if (!file.exists(ecdc_path)) {
  options(timeout = 1200)
  download.file(ecdc_url, ecdc_path, mode = "wb")
}

ecdc <- read.csv(ecdc_path, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

dose_cols_ecdc <- c("FirstDose","SecondDose",
                    "DoseAdditional1","DoseAdditional2","DoseAdditional3",
                    "DoseAdditional4","DoseAdditional5")

ecdc_weekly <- ecdc %>%
  filter(ReportingCountry == "CZ", TargetGroup == "ALL") %>%
  mutate(across(all_of(dose_cols_ecdc), ~ suppressWarnings(as.numeric(.)))) %>%
  mutate(weekly_row_total = rowSums(across(all_of(dose_cols_ecdc)), na.rm = TRUE),
         week_date = ISOweek::ISOweek2date(paste0(YearWeekISO, "-1"))) %>%
  group_by(week_date) %>%
  summarise(total_doses = sum(weekly_row_total, na.rm = TRUE), .groups = "drop") %>%
  arrange(week_date) %>%
  mutate(source = "ECDC")

# =========
# Combine & plot (superimposed areas: ECDC Blue, MZCR Red)
# =========
compare_df <- bind_rows(ecdc_weekly, mzcr_weekly)

# Keep overlapping period (optional, but often clearer)
min_common <- max(min(ecdc_weekly$week_date, na.rm = TRUE),
                  min(mzcr_weekly$week_date, na.rm = TRUE))
max_common <- min(max(ecdc_weekly$week_date, na.rm = TRUE),
                  max(mzcr_weekly$week_date, na.rm = TRUE))

compare_df_common <- compare_df %>%
  filter(week_date >= min_common, week_date <= max_common)

# Plot with two explicit area layers to ensure correct colors & overlap, dashed lines on top
p_compare <- ggplot() +
  # areas
  geom_area(data = compare_df_common %>% filter(source == "ECDC"),
            aes(x = week_date, y = total_doses),
            fill = "#1f77b4", alpha = 0.35) +
  geom_area(data = compare_df_common %>% filter(source == "MZCR"),
            aes(x = week_date, y = total_doses),
            fill = "#d62728", alpha = 0.35) +
  # dashed outlines on top of areas
  geom_line(data = compare_df_common %>% filter(source == "ECDC"),
            aes(x = week_date, y = total_doses),
            color = "#1f77b4", linewidth = 0.7, linetype = "dashed") +
  geom_line(data = compare_df_common %>% filter(source == "MZCR"),
            aes(x = week_date, y = total_doses),
            color = "#d62728", linewidth = 0.7, linetype = "dashed") +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si("")),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Weekly COVID-19 vaccine doses — ECDC (Blue) vs MZCR (Red)",
    subtitle = "Overlaid areas with dashed outlines; both series on the same ISO-week (Monday) axis",
    x = NULL, y = "Doses administered"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

print(p_compare)

# Save
dir.create("visual/compare_sources", recursive = TRUE, showWarnings = FALSE)
ggsave("visual/compare_sources/weekly_doses_ecdc_vs_mzcr.png",
       p_compare, width = 12, height = 6, dpi = 300, bg = "white")

# =============================================================================
# Export weekly doses series for reuse (CSV + RDS)
# =============================================================================
out_dir <- "out"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- Full timeline (union of weeks across sources) -------------------------
all_weeks_full <- tibble(
  week_date = seq.Date(
    min(compare_df$week_date, na.rm = TRUE),
    max(compare_df$week_date, na.rm = TRUE),
    by = "week"
  )
)

weekly_doses_wide <- all_weeks_full %>%
  left_join(
    compare_df %>%
      select(week_date, source, total_doses) %>%
      tidyr::pivot_wider(names_from = source, values_from = total_doses),
    by = "week_date"
  ) %>%
  replace_na(list(ECDC = 0, MZCR = 0)) %>%
  arrange(week_date) %>%
  mutate(
    iso_week = strftime(week_date, "%G-W%V"),
    diff     = ECDC - MZCR,
    cum_ecdc = cumsum(ECDC),
    cum_mzcr = cumsum(MZCR)
  ) %>%
  select(
    week_date, iso_week,
    total_doses_ecdc = ECDC,
    total_doses_mzcr = MZCR,
    diff, cum_ecdc, cum_mzcr
  )

weekly_doses_long <- compare_df %>%
  arrange(week_date, source) %>%
  mutate(iso_week = strftime(week_date, "%G-W%V")) %>%
  select(week_date, iso_week, source, total_doses)

# ---- Overlapping period only ----------------------------------------------
all_weeks_common <- tibble(week_date = seq.Date(min_common, max_common, by = "week"))

weekly_doses_common_wide <- all_weeks_common %>%
  left_join(
    compare_df_common %>%
      select(week_date, source, total_doses) %>%
      tidyr::pivot_wider(names_from = source, values_from = total_doses),
    by = "week_date"
  ) %>%
  replace_na(list(ECDC = 0, MZCR = 0)) %>%
  arrange(week_date) %>%
  mutate(
    iso_week = strftime(week_date, "%G-W%V"),
    diff     = ECDC - MZCR,
    cum_ecdc = cumsum(ECDC),
    cum_mzcr = cumsum(MZCR)
  ) %>%
  select(
    week_date, iso_week,
    total_doses_ecdc = ECDC,
    total_doses_mzcr = MZCR,
    diff, cum_ecdc, cum_mzcr
  )

weekly_doses_common_long <- compare_df_common %>%
  arrange(week_date, source) %>%
  mutate(iso_week = strftime(week_date, "%G-W%V")) %>%
  select(week_date, iso_week, source, total_doses)

# ---- Write files -----------------------------------------------------------
write.csv(weekly_doses_wide,        file.path(out_dir, "weekly_covid_doses_wide.csv"),        row.names = FALSE)
write.csv(weekly_doses_long,        file.path(out_dir, "weekly_covid_doses_long.csv"),        row.names = FALSE)
write.csv(weekly_doses_common_wide, file.path(out_dir, "weekly_covid_doses_common_wide.csv"), row.names = FALSE)
write.csv(weekly_doses_common_long, file.path(out_dir, "weekly_covid_doses_common_long.csv"), row.names = FALSE)

# RDS bundle (everything in one object)
saveRDS(
  list(
    weekly_wide         = weekly_doses_wide,
    weekly_long         = weekly_doses_long,
    weekly_common_wide  = weekly_doses_common_wide,
    weekly_common_long  = weekly_doses_common_long
  ),
  file.path(out_dir, "weekly_covid_doses.rds")
)

message("Saved: ",
        file.path(out_dir, "weekly_covid_doses_wide.csv"), " ; ",
        file.path(out_dir, "weekly_covid_doses_long.csv"), " ; ",
        file.path(out_dir, "weekly_covid_doses_common_wide.csv"), " ; ",
        file.path(out_dir, "weekly_covid_doses_common_long.csv"), " ; ",
        file.path(out_dir, "weekly_covid_doses.rds"))
