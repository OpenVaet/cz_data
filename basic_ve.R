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
print(df_filtered)

# -------------------------------------------------------------------
# Fast date conversion using lookup table
# -------------------------------------------------------------------
convert_yearweek_fast <- function(yearweek_vec) {
  n <- length(yearweek_vec)
  result <- rep(as.Date(NA), n)
  valid_idx <- !is.na(yearweek_vec) & nzchar(trimws(yearweek_vec))
  
  if (sum(valid_idx) == 0) return(result)
  
  valid_yw <- yearweek_vec[valid_idx]
  years <- as.integer(substr(valid_yw, 1, 4))
  weeks <- as.integer(substr(valid_yw, 6, 7))
  iso_strings <- paste0(years, "-W", sprintf("%02d", weeks), "-1")
  
  tryCatch({
    result[valid_idx] <- ISOweek::ISOweek2date(iso_strings)
  }, error = function(e) {
    warning("Some dates could not be converted: ", e$message)
  })
  
  return(result)
}

# Convert dates
df_analysis <- df_filtered %>%
  mutate(
    date_first_dose = convert_yearweek_fast(Date_First_Dose),
    date_death = convert_yearweek_fast(DateOfDeathInHealthcareFacility)
  )

# -------------------------------------------------------------------
# Setup analysis periods
# -------------------------------------------------------------------
start_date <- ISOweek::ISOweek2date("2020-W01-1")
end_date <- ISOweek::ISOweek2date("2024-W15-1")

weekly_dates <- seq(start_date, end_date, by = "week")
weekly_periods <- data.frame(
  week_start = weekly_dates[-length(weekly_dates)],
  week_end = weekly_dates[-1],
  year_week = substr(ISOweek::date2ISOweek(weekly_dates[-length(weekly_dates)]), 1, 8)
)

# -------------------------------------------------------------------
# Calculate person-time and deaths by week
# -------------------------------------------------------------------
calculate_person_time_vectorized <- function(df, periods) {
  df <- df %>%
    mutate(
      contrib_start = as.Date("2020-01-01"),
      contrib_end = if_else(is.na(date_death), as.Date("2024-04-15"), date_death)
    )
  
  results <- list()
  
  for (i in 1:nrow(periods)) {
    period_start <- periods$week_start[i]
    period_end <- periods$week_end[i]
    year_week <- periods$year_week[i]
    
    period_calc <- df %>%
      filter(contrib_end >= period_start) %>%
      mutate(
        period_days = pmax(0, as.numeric(pmin(contrib_end, period_end) - period_start)),
        
        days_unvacc = case_when(
          is.na(date_first_dose) ~ period_days,
          date_first_dose >= period_end ~ period_days,
          date_first_dose < period_start ~ 0,
          TRUE ~ pmax(0, as.numeric(date_first_dose - period_start))
        ),
        
        days_vacc = period_days - days_unvacc,
        
        died_in_period = !is.na(date_death) & 
                        date_death >= period_start & 
                        date_death < period_end,
        
        died_vaccinated = died_in_period & 
                         !is.na(date_first_dose) & 
                         date_first_dose <= date_death,
                         
        died_unvaccinated = died_in_period & !died_vaccinated
      )
    
    summary_data <- period_calc %>%
      summarise(
        year_week = year_week,
        person_days_unvaccinated = sum(days_unvacc, na.rm = TRUE),
        person_days_vaccinated = sum(days_vacc, na.rm = TRUE),
        deaths_unvaccinated = sum(died_unvaccinated, na.rm = TRUE),
        deaths_vaccinated = sum(died_vaccinated, na.rm = TRUE),
        .groups = 'drop'
      )
    
    results[[i]] <- summary_data
    
    if (i %% 20 == 0) cat("Processed week", i, "of", nrow(periods), "\r")
  }
  
  cat("\n")
  bind_rows(results)
}

cat("Calculating person-time and deaths by week...\n")
weekly_results <- calculate_person_time_vectorized(df_analysis, weekly_periods)

# -------------------------------------------------------------------
# Calculate rates and vaccine effectiveness
# -------------------------------------------------------------------
weekly_summary <- weekly_results %>%
  mutate(
    deaths_per_100k_unvaccinated = ifelse(person_days_unvaccinated > 0, 
                                          (deaths_unvaccinated / person_days_unvaccinated) * 100000, 0),
    deaths_per_100k_vaccinated = ifelse(person_days_vaccinated > 0, 
                                        (deaths_vaccinated / person_days_vaccinated) * 100000, 0),
    
    # Only calculate VE when there are deaths in both groups or at least vaccinated person-time
    ve = case_when(
      deaths_vaccinated == 0 & person_days_vaccinated == 0 ~ NA_real_,  # No vaccinated population yet
      deaths_per_100k_unvaccinated == 0 ~ NA_real_,  # Can't calculate if no unvaccinated deaths
      TRUE ~ (1 - (deaths_per_100k_vaccinated / deaths_per_100k_unvaccinated)) * 100
    )
  ) %>%
  filter(person_days_unvaccinated > 0 | person_days_vaccinated > 0)

# -------------------------------------------------------------------
# Prepare data for plotting
# -------------------------------------------------------------------
plot_data_unvax <- weekly_summary %>%
  select(year_week, deaths_per_100k = deaths_per_100k_unvaccinated) %>%
  mutate(group = "Unvaccinated")

plot_data_vax <- weekly_summary %>%
  select(year_week, deaths_per_100k = deaths_per_100k_vaccinated) %>%
  mutate(group = "Vaccinated")

plot_data <- rbind(plot_data_unvax, plot_data_vax) %>%
  left_join(weekly_summary %>% select(year_week, ve), by = "year_week")

# Scale factor for dual axis
max_deaths <- max(c(weekly_summary$deaths_per_100k_unvaccinated, 
                    weekly_summary$deaths_per_100k_vaccinated), na.rm = TRUE)
scale_factor <- max_deaths / 400  # Map -300 to 100 range

# -------------------------------------------------------------------
# Create final plot
# -------------------------------------------------------------------
p <- ggplot(plot_data, aes(x = year_week)) +
  geom_col(aes(y = deaths_per_100k, fill = group),
           position = position_dodge2(width = 0.9, padding = 0.1), 
           alpha = 0.8) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
  
  # Only plot VE where it's not NA (excludes early weeks with no vaccinated population)
  geom_line(data = weekly_summary %>% filter(!is.na(ve)),
            aes(x = year_week, y = (ve + 300) * scale_factor, group = 1), 
            color = "#e74c3c", size = 1.2) +
  geom_point(data = weekly_summary %>% filter(!is.na(ve)),
             aes(x = year_week, y = (ve + 300) * scale_factor), 
             color = "#e74c3c", size = 2) +
  
  scale_y_continuous(
    name = "Deaths per 100k Person-Days",
    sec.axis = sec_axis(~(./scale_factor) - 300, 
                        name = "Vaccine Effectiveness (%)",
                        breaks = seq(-300, 100, by = 50))
  ) +
  
  scale_fill_manual(values = c("Vaccinated" = "#69b3a2", "Unvaccinated" = "#404080"), 
                    name = "Group") +
  
  labs(x = "Year-Week", 
       title = "All Causes Deaths per 100k Person-Days and Raw Vaccine Effectiveness Over Time",
       subtitle = "Czech Republic, 2020-W01 to 2024-W15") +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5, size = 9, face = "italic")
  ) +
  
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 8)])

print(p)

# -------------------------------------------------------------------
# Summary statistics
# -------------------------------------------------------------------
cat("\n=== Summary Statistics ===\n")
cat("Total person-days unvaccinated:", sum(weekly_summary$person_days_unvaccinated), "\n")
cat("Total person-days vaccinated:", sum(weekly_summary$person_days_vaccinated), "\n")
cat("Total deaths unvaccinated:", sum(weekly_summary$deaths_unvaccinated), "\n")
cat("Total deaths vaccinated:", sum(weekly_summary$deaths_vaccinated), "\n")
cat("Overall death rate unvaccinated (per 100k person-days):", 
    sum(weekly_summary$deaths_unvaccinated) / sum(weekly_summary$person_days_unvaccinated) * 100000, "\n")
cat("Overall death rate vaccinated (per 100k person-days):", 
    sum(weekly_summary$deaths_vaccinated) / sum(weekly_summary$person_days_vaccinated) * 100000, "\n")
cat("Overall vaccine effectiveness:", 
    (1 - (sum(weekly_summary$deaths_vaccinated) / sum(weekly_summary$person_days_vaccinated)) / 
         (sum(weekly_summary$deaths_unvaccinated) / sum(weekly_summary$person_days_unvaccinated))) * 100, "%\n")