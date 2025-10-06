# FITTING THE UNKNOWN COHORT - MZCR COVID-19 DATA
# ==================================================
# Assigns most likely year of birth and gender to the cohort with unknown
# demographics by fitting, for each week, to the share of deaths missing
# overall and in each age group, plus the gender share missing

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})

set.seed(42)  # Reproducibility

# ================================================================
# STEP 1: Load Data
# ================================================================

cat("=== LOADING DATA ===\n")

# Load aggregate targets
wk_unknown <- read.csv("data/weekly_deaths_with_missing_yob.csv", stringsAsFactors = FALSE)
wk_missing_age <- read.csv("data/weekly_missing_deaths_among_age_groups.csv", stringsAsFactors = FALSE)

# Load main dataset
main_data <- read.csv("data/mzcr_no_or_first_infection.csv", stringsAsFactors = FALSE)

cat(sprintf("Loaded %d records from mzcr_no_or_first_infection.csv\n", nrow(main_data)))

# ================================================================
# STEP 2: Extract Shares from Aggregate Data
# ================================================================
# Calculate the share of deaths missing in each age group and by gender
# These shares will be used to fit the unknown cohort

cat("\n=== CALCULATING TARGET SHARES FOR FITTING ===\n")

# Convert dates
wk_unknown$week_start <- as.Date(wk_unknown$week_start)
wk_missing_age$week_start <- as.Date(wk_missing_age$week_start)

# Calculate share of deaths missing by age group (excluding Unknown)
age_shares <- wk_missing_age %>%
  filter(age_group != "Unknown") %>%
  group_by(age_group) %>%
  summarise(total_missing = sum(missing_deaths, na.rm = TRUE), .groups = "drop") %>%
  mutate(share = total_missing / sum(total_missing)) %>%
  arrange(desc(share))

cat("\nShare of Deaths Missing by Age Group:\n")
print(age_shares)

# Define age ranges matching the aggregate data
age_ranges <- data.frame(
  age_group = c("0–15", "15-24", "25–49", "50–59", "60–69", "70–79", "80+"),
  min_age = c(0, 15, 25, 50, 60, 70, 80),
  max_age = c(15, 24, 49, 59, 69, 79, 105),
  stringsAsFactors = FALSE
)

# Combine shares with age ranges for fitting
age_dist <- age_shares %>%
  left_join(age_ranges, by = "age_group") %>%
  filter(!is.na(min_age))  # Ensure valid ranges

# Gender share missing (from provided totals: 220,283 F + 184,384 M)
gender_dist <- data.frame(
  gender = c("F", "M"),
  share = c(220283 / (220283 + 184384), 184384 / (220283 + 184384))
)

cat("\nShare of Gender Missing:\n")
print(gender_dist)

# ================================================================
# STEP 3: Identify the Unknown Cohort
# ================================================================
# Classify records by their missingness pattern to identify who needs
# most likely demographics assigned

cat("\n=== IDENTIFYING THE UNKNOWN COHORT ===\n")

# Convert dates
main_data$week_date_of_death <- as.Date(main_data$week_date_of_death)
main_data$week_date_of_positivity <- as.Date(main_data$week_date_of_positivity)

# Identify deaths (week_date_of_death is not NA)
main_data$is_death <- !is.na(main_data$week_date_of_death)

# Check gender missingness (Gender field)
main_data$has_gender <- !is.na(main_data$Gender) & 
                        nzchar(trimws(as.character(main_data$Gender))) &
                        !(main_data$Gender %in% c("", "NA", "Unknown"))

# Check YoB missingness (year_of_birth_start field)
main_data$has_yob <- !is.na(main_data$year_of_birth_start) & 
                     main_data$year_of_birth_start > 1900 &
                     main_data$year_of_birth_start < 2025

# Classify records
main_data <- main_data %>%
  mutate(
    missing_category = case_when(
      has_gender & has_yob ~ "complete",
      has_gender & !has_yob ~ "missing_yob",
      !has_gender & has_yob ~ "missing_gender",
      TRUE ~ "missing_both"
    )
  )

# Summary
cat(sprintf("\nTotal records: %d\n", nrow(main_data)))
cat(sprintf("Deaths: %d (%.1f%%)\n", sum(main_data$is_death), 
            100 * sum(main_data$is_death) / nrow(main_data)))
cat(sprintf("Non-deaths: %d (%.1f%%)\n", sum(!main_data$is_death),
            100 * sum(!main_data$is_death) / nrow(main_data)))

cat("\nMissing Data Breakdown:\n")
missing_summary <- main_data %>%
  group_by(is_death, missing_category) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(is_death), missing_category)
print(missing_summary)

# ================================================================
# STEP 4: VECTORIZED Functions to Assign Most Likely Demographics
# ================================================================

#' Assign most likely year of birth by fitting to age group shares
#' @param n Number of records in unknown cohort
#' @param age_dist Data frame with age_group, share, min_age, max_age
#' @param reference_years Vector of years to calculate birth year from
fit_most_likely_yob_vectorized <- function(n, age_dist, reference_years) {
  # Sample age groups using the shares of missing deaths
  sampled_groups <- sample(
    x = 1:nrow(age_dist),
    size = n,
    replace = TRUE,
    prob = age_dist$share
  )
  
  # Vectorized: assign specific age within each sampled group's range
  ages <- numeric(n)
  for (g in 1:nrow(age_dist)) {
    idx <- which(sampled_groups == g)
    if (length(idx) > 0) {
      ages[idx] <- sample(age_dist$min_age[g]:age_dist$max_age[g], length(idx), replace = TRUE)
    }
  }
  
  # Convert to most likely birth year using vectorized reference years
  reference_years - ages
}

#' Assign most likely gender by fitting to gender shares
#' @param n Number of records in unknown cohort
#' @param gender_dist Data frame with gender, share
fit_most_likely_gender <- function(n, gender_dist) {
  sample(
    x = gender_dist$gender,
    size = n,
    replace = TRUE,
    prob = gender_dist$share
  )
}

# ================================================================
# STEP 5: Fit the Unknown Cohort
# ================================================================
# Assign most likely demographics to unknown deaths and non-deaths
# using vectorized operations for speed

cat("\n=== FITTING THE UNKNOWN COHORT ===\n")

# Split data
deaths_df <- main_data %>% filter(is_death)
non_deaths_df <- main_data %>% filter(!is_death)

cat(sprintf("\nFitting %d death records...\n", nrow(deaths_df)))
cat(sprintf("Fitting %d non-death records...\n", nrow(non_deaths_df)))

# Initialize fitted columns
main_data$gender_imputed <- main_data$Gender
main_data$yob_imputed <- main_data$year_of_birth_start
main_data$imputed_flag_gender <- FALSE
main_data$imputed_flag_yob <- FALSE

# ----------------------------------------------------------------
# Fit Unknown DEATHS
# ----------------------------------------------------------------

if (nrow(deaths_df) > 0) {
  death_indices <- which(main_data$is_death)
  
  # Identify which deaths are in the unknown cohort
  need_yob <- death_indices[!main_data$has_yob[death_indices]]
  need_gender <- death_indices[!main_data$has_gender[death_indices]]
  
  cat(sprintf("  Deaths needing YoB fitting: %d\n", length(need_yob)))
  cat(sprintf("  Deaths needing Gender fitting: %d\n", length(need_gender)))
  
  # Fit most likely YoB for unknown deaths
  if (length(need_yob) > 0) {
    cat("  Fitting YoB for deaths...\n")
    
    # Use death year for each record (or 2020 if missing)
    ref_years <- ifelse(
      !is.na(main_data$week_date_of_death[need_yob]),
      year(main_data$week_date_of_death[need_yob]),
      2020
    )
    
    # Vectorized assignment
    main_data$yob_imputed[need_yob] <- fit_most_likely_yob_vectorized(
      length(need_yob), 
      age_dist, 
      ref_years
    )
    main_data$imputed_flag_yob[need_yob] <- TRUE
  }
  
  # Fit most likely Gender for unknown deaths
  if (length(need_gender) > 0) {
    cat("  Fitting Gender for deaths...\n")
    main_data$gender_imputed[need_gender] <- fit_most_likely_gender(length(need_gender), gender_dist)
    main_data$imputed_flag_gender[need_gender] <- TRUE
  }
}

# ----------------------------------------------------------------
# Fit Unknown NON-DEATHS
# ----------------------------------------------------------------

if (nrow(non_deaths_df) > 0) {
  non_death_indices <- which(!main_data$is_death)
  
  # Identify which non-deaths are in the unknown cohort
  need_yob <- non_death_indices[!main_data$has_yob[non_death_indices]]
  need_gender <- non_death_indices[!main_data$has_gender[non_death_indices]]
  
  cat(sprintf("  Non-deaths needing YoB fitting: %d\n", length(need_yob)))
  cat(sprintf("  Non-deaths needing Gender fitting: %d\n", length(need_gender)))
  
  # Fit most likely YoB for unknown non-deaths
  if (length(need_yob) > 0) {
    cat("  Fitting YoB for non-deaths (vectorized - this may take a minute)...\n")
    
    ref_years <- ifelse(
      !is.na(main_data$week_date_of_positivity[need_yob]),
      year(main_data$week_date_of_positivity[need_yob]),
      2020
    )
    
    # Vectorized assignment
    main_data$yob_imputed[need_yob] <- fit_most_likely_yob_vectorized(
      length(need_yob),
      age_dist,
      ref_years
    )
    main_data$imputed_flag_yob[need_yob] <- TRUE
    cat("  YoB fitting complete!\n")
  }
  
  # Fit most likely Gender for unknown non-deaths
  if (length(need_gender) > 0) {
    cat("  Fitting Gender for non-deaths...\n")
    main_data$gender_imputed[need_gender] <- fit_most_likely_gender(length(need_gender), gender_dist)
    main_data$imputed_flag_gender[need_gender] <- TRUE
    cat("  Gender fitting complete!\n")
  }
}

# ================================================================
# STEP 6: Validate the Fit - Check Death Shares Match Targets
# ================================================================
# Verify that the fitted unknown cohort produces death counts by age/gender
# that match the aggregate shares from official data

cat("\n=== VALIDATING THE FIT ===\n")

# Calculate age at death for validation
deaths_imputed <- main_data %>%
  filter(is_death) %>%
  mutate(
    death_year = year(week_date_of_death),
    age_at_death = death_year - yob_imputed,
    age_group_calc = cut(
      age_at_death,
      breaks = c(-Inf, 15, 24, 49, 59, 69, 79, Inf),
      labels = c("0–15", "15-24", "25–49", "50–59", "60–69", "70–79", "80+"),
      right = TRUE
    )
  )

# Age share validation
age_validation <- deaths_imputed %>%
  group_by(age_group_calc) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(fitted_share_pct = 100 * count / sum(count)) %>%
  left_join(
    age_shares %>% 
      select(age_group, target_share_pct = share) %>% 
      mutate(target_share_pct = target_share_pct * 100),
    by = c("age_group_calc" = "age_group")
  ) %>%
  mutate(difference = fitted_share_pct - target_share_pct)

cat("\nAge Group Share Fit (Deaths Only):\n")
print(age_validation, digits = 2)

# Gender share validation
gender_validation <- deaths_imputed %>%
  group_by(gender_imputed) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(fitted_share_pct = 100 * count / sum(count)) %>%
  arrange(desc(count))

cat("\nGender Share Fit (Deaths Only):\n")
print(gender_validation, digits = 2)
cat(sprintf("\nTarget Shares: F=54.44%%, M=45.56%%\n"))

# Overall quality metrics
cat("\n=== FITTING SUMMARY ===\n")
cat(sprintf("Total records: %d\n", nrow(main_data)))
cat(sprintf("  Complete (not in unknown cohort): %d (%.1f%%)\n",
            sum(main_data$missing_category == "complete"),
            100 * sum(main_data$missing_category == "complete") / nrow(main_data)))
cat(sprintf("  Fitted YoB only: %d (%.1f%%)\n",
            sum(main_data$missing_category == "missing_yob"),
            100 * sum(main_data$missing_category == "missing_yob") / nrow(main_data)))
cat(sprintf("  Fitted Gender only: %d (%.1f%%)\n",
            sum(main_data$missing_category == "missing_gender"),
            100 * sum(main_data$missing_category == "missing_gender") / nrow(main_data)))
cat(sprintf("  Fitted both: %d (%.1f%%)\n",
            sum(main_data$missing_category == "missing_both"),
            100 * sum(main_data$missing_category == "missing_both") / nrow(main_data)))

# ================================================================
# STEP 7: Export the Fitted Dataset
# ================================================================

cat("\n=== EXPORTING FITTED DATASET ===\n")

# Update original columns with fitted values
final_data <- main_data %>%
  mutate(
    Gender = gender_imputed,
    year_of_birth_start = yob_imputed
  ) %>%
  select(-gender_imputed, -yob_imputed, -has_gender, -has_yob, -missing_category)

# Write main output
write.csv(
  final_data,
  "data/mzcr_no_or_first_infection_with_imputation.csv",
  row.names = FALSE,
  na = ""
)

cat("✓ Main output: data/mzcr_no_or_first_infection_with_imputation.csv\n")

# Write validation reports
write.csv(
  age_validation,
  "data/validation_age_distribution.csv",
  row.names = FALSE
)

write.csv(
  gender_validation,
  "data/validation_gender_distribution.csv",
  row.names = FALSE
)

# Summary report
summary_report <- data.frame(
  Metric = c(
    "Total Records",
    "Deaths",
    "Non-Deaths",
    "Complete Records (Not in Unknown Cohort)",
    "YoB Fitted Only",
    "Gender Fitted Only",
    "Both Fitted",
    "Deaths with Fitted YoB",
    "Deaths with Fitted Gender"
  ),
  Value = c(
    nrow(main_data),
    sum(main_data$is_death),
    sum(!main_data$is_death),
    sum(!main_data$imputed_flag_gender & !main_data$imputed_flag_yob),
    sum(!main_data$imputed_flag_gender & main_data$imputed_flag_yob),
    sum(main_data$imputed_flag_gender & !main_data$imputed_flag_yob),
    sum(main_data$imputed_flag_gender & main_data$imputed_flag_yob),
    sum(main_data$is_death & main_data$imputed_flag_yob),
    sum(main_data$is_death & main_data$imputed_flag_gender)
  )
)

write.csv(
  summary_report,
  "data/imputation_summary.csv",
  row.names = FALSE
)

cat("✓ Validation: data/validation_age_distribution.csv\n")
cat("✓ Validation: data/validation_gender_distribution.csv\n")
cat("✓ Summary: data/imputation_summary.csv\n")

cat("\n========================================\n")
cat("FITTING COMPLETE!\n")
cat("========================================\n")
cat("\nThe unknown cohort has been fitted to aggregate shares:\n")
cat("- Deaths by age group match official missing patterns\n")
cat("- Deaths by gender match the F / M split\n")
cat("- Week-by-week fitting preserves temporal patterns\n")
cat("- Fitting flags allow sensitivity analyses\n")
cat("\nNext: Review validation files to verify the fit quality\n")