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

# Define age groups for standardization
df_analysis <- df_analysis %>%
  mutate(
    age_at_2020 = 2020 - YearOfBirth_start,
    age_group = cut(age_at_2020,
                    breaks = c(0, 40, 50, 60, 70, 80, Inf),
                    labels = c("0-39", "40-49", "50-59", "60-69", "70-79", "80+"),
                    right = FALSE)
  )

# WHO World Standard Population (per 100,000)
# Using simplified weights for demonstration
who_standard <- data.frame(
  age_group = c("0-39", "40-49", "50-59", "60-69", "70-79", "80+"),
  standard_weight = c(0.35, 0.20, 0.18, 0.15, 0.09, 0.03)  # Simplified weights
)

# -------------------------------------------------------------------
# Calculate age-specific person-time and deaths by week
# -------------------------------------------------------------------
calculate_age_specific_rates <- function(df, periods) {
  
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
    
    # Aggregate by age group
    age_summary <- period_calc %>%
      group_by(age_group) %>%
      summarise(
        person_days_unvaccinated = sum(days_unvacc, na.rm = TRUE),
        person_days_vaccinated = sum(days_vacc, na.rm = TRUE),
        deaths_unvaccinated = sum(died_unvaccinated, na.rm = TRUE),
        deaths_vaccinated = sum(died_vaccinated, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(year_week = year_week)
    
    results[[i]] <- age_summary
    
    if (i %% 20 == 0) cat("Processed week", i, "of", nrow(periods), "\r")
  }
  
  cat("\n")
  bind_rows(results)
}

cat("Calculating age-specific person-time and deaths by week...\n")
age_specific_results <- calculate_age_specific_rates(df_analysis, weekly_periods)

# -------------------------------------------------------------------
# Calculate ASMR for each week
# -------------------------------------------------------------------
calculate_weekly_asmr <- function(age_data, who_standard) {
  age_data %>%
    left_join(who_standard, by = "age_group") %>%
    mutate(
      # Age-specific rates per 100k person-days
      rate_unvaccinated = ifelse(person_days_unvaccinated > 0,
                                 (deaths_unvaccinated / person_days_unvaccinated) * 100000, 0),
      rate_vaccinated = ifelse(person_days_vaccinated > 0,
                              (deaths_vaccinated / person_days_vaccinated) * 100000, 0),
      
      # Weighted rates
      weighted_rate_unvaccinated = rate_unvaccinated * standard_weight,
      weighted_rate_vaccinated = rate_vaccinated * standard_weight
    ) %>%
    group_by(year_week) %>%
    summarise(
      # Sum of weighted rates = ASMR
      asmr_unvaccinated = sum(weighted_rate_unvaccinated, na.rm = TRUE),
      asmr_vaccinated = sum(weighted_rate_vaccinated, na.rm = TRUE),
      
      # Also keep raw counts for reference
      total_deaths_unvaccinated = sum(deaths_unvaccinated, na.rm = TRUE),
      total_deaths_vaccinated = sum(deaths_vaccinated, na.rm = TRUE),
      total_pd_unvaccinated = sum(person_days_unvaccinated, na.rm = TRUE),
      total_pd_vaccinated = sum(person_days_vaccinated, na.rm = TRUE),
      
      .groups = 'drop'
    ) %>%
    mutate(
      # Calculate VE based on ASMR
      ve_asmr = case_when(
        total_deaths_vaccinated == 0 & total_pd_vaccinated == 0 ~ NA_real_,
        asmr_unvaccinated == 0 ~ NA_real_,
        TRUE ~ (1 - (asmr_vaccinated / asmr_unvaccinated)) * 100
      )
    )
}

weekly_asmr <- calculate_weekly_asmr(age_specific_results, who_standard)

# -------------------------------------------------------------------
# Prepare data for ASMR plotting
# -------------------------------------------------------------------
plot_data_asmr_unvax <- weekly_asmr %>%
  select(year_week, asmr = asmr_unvaccinated) %>%
  mutate(group = "Unvaccinated")

plot_data_asmr_vax <- weekly_asmr %>%
  select(year_week, asmr = asmr_vaccinated) %>%
  mutate(group = "Vaccinated")

plot_data_asmr <- rbind(plot_data_asmr_unvax, plot_data_asmr_vax) %>%
  left_join(weekly_asmr %>% select(year_week, ve_asmr), by = "year_week")

# Scale factor for dual axis
max_asmr <- max(c(weekly_asmr$asmr_unvaccinated, weekly_asmr$asmr_vaccinated), na.rm = TRUE)
scale_factor_asmr <- max_asmr / 150  # Map -50 to 100 range (narrower for ASMR)

# -------------------------------------------------------------------
# Create ASMR plot
# -------------------------------------------------------------------
p_asmr <- ggplot(plot_data_asmr, aes(x = year_week)) +
  geom_col(aes(y = asmr, fill = group),
           position = position_dodge2(width = 0.9, padding = 0.1), 
           alpha = 0.8) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
  
  # Plot VE based on ASMR
  geom_line(data = weekly_asmr %>% filter(!is.na(ve_asmr)),
            aes(x = year_week, y = (ve_asmr + 50) * scale_factor_asmr, group = 1), 
            color = "#e74c3c", size = 1.2) +
  geom_point(data = weekly_asmr %>% filter(!is.na(ve_asmr)),
             aes(x = year_week, y = (ve_asmr + 50) * scale_factor_asmr), 
             color = "#e74c3c", size = 2) +
  
  scale_y_continuous(
    name = "Age-Standardized Mortality Rate (per 100k person-days)",
    sec.axis = sec_axis(~(./scale_factor_asmr) - 50, 
                        name = "Vaccine Effectiveness (%)",
                        breaks = seq(-50, 100, by = 25))
  ) +
  
  scale_fill_manual(values = c("Vaccinated" = "#69b3a2", "Unvaccinated" = "#404080"), 
                    name = "Group") +
  
  labs(x = "Year-Week", 
       title = "All Causes Age-Standardized Mortality Rates and Vaccine Effectiveness",
       subtitle = "Czech Republic, 2020-W01 to 2024-W15 (WHO Standard Population)") +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0.5, size = 9, face = "italic")
  ) +
  
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 8)])

print(p_asmr)

# -------------------------------------------------------------------
# Compare crude vs ASMR vaccine effectiveness
# -------------------------------------------------------------------
comparison_df <- weekly_asmr %>%
  left_join(weekly_summary %>% select(year_week, ve_crude = ve), by = "year_week") %>%
  select(year_week, ve_crude, ve_asmr) %>%
  filter(!is.na(ve_asmr) | !is.na(ve_crude)) %>%
  pivot_longer(cols = c(ve_crude, ve_asmr), names_to = "type", values_to = "ve") %>%
  mutate(type = ifelse(type == "ve_crude", "Crude VE", "Age-Standardized VE"))

p_comparison <- ggplot(comparison_df %>% filter(!is.na(ve)), aes(x = year_week, y = ve, color = type)) +
  geom_line(aes(group = type), size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 50, linetype = "dotted", color = "gray70") +
  scale_color_manual(values = c("Crude VE" = "#404080", "Age-Standardized VE" = "#69b3a2")) +
  labs(x = "Year-Week",
       y = "Vaccine Effectiveness (%)",
       title = "Comparison of Crude vs Age-Standardized Vaccine Effectiveness",
       subtitle = "Age standardization reduces bias from prioritizing elderly for vaccination",
       color = "Method") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 12)])

print(p_comparison)

# -------------------------------------------------------------------
# Summary statistics
# -------------------------------------------------------------------
cat("\n=== ASMR-Based Summary ===\n")

# Calculate overall ASMR VE
overall_asmr_unvax <- sum(weekly_asmr$total_deaths_unvaccinated) / sum(weekly_asmr$total_pd_unvaccinated) * 100000
overall_asmr_vax <- sum(weekly_asmr$total_deaths_vaccinated) / sum(weekly_asmr$total_pd_vaccinated) * 100000
overall_ve_asmr <- (1 - (overall_asmr_vax / overall_asmr_unvax)) * 100

cat("Overall ASMR unvaccinated:", overall_asmr_unvax, "\n")
cat("Overall ASMR vaccinated:", overall_asmr_vax, "\n")
cat("Overall Age-Standardized VE:", overall_ve_asmr, "%\n\n")

# Show improvement in early 2021
early_2021 <- weekly_asmr %>%
  filter(substr(year_week, 1, 7) %in% c("2021-W0", "2021-W1", "2021-W2")) %>%
  filter(!is.na(ve_asmr))

cat("=== Early 2021 VE Comparison ===\n")
cat("Weeks with negative crude VE:", sum(weekly_summary$ve < 0, na.rm = TRUE), "\n")
cat("Weeks with negative ASMR VE:", sum(weekly_asmr$ve_asmr < 0, na.rm = TRUE), "\n")
cat("Average ASMR VE in early 2021:", mean(early_2021$ve_asmr, na.rm = TRUE), "%\n")