# FITTING THE UNKNOWN COHORT - CORRECTED VERSION
# ==================================================
# Deaths: Fitted to death age shares
# Non-deaths: Fitted to living population age shares (Eurostat)

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})

set.seed(42)

cat("=== STEP 1: Load Data ===\n")

# Load aggregate death patterns
wk_missing_age <- read.csv("data/weekly_missing_deaths_among_age_groups.csv", stringsAsFactors = FALSE)
main_data <- read.csv("data/mzcr_no_or_first_infection.csv", stringsAsFactors = FALSE)

cat(sprintf("Loaded %d records\n", nrow(main_data)))

# Load Eurostat population (for non-deaths)
eu_file <- "data/demo_pjan_linear_2_0.csv"
eu <- read.csv(eu_file, stringsAsFactors = FALSE, check.names = FALSE)

eu_pop <- eu %>%
  rename(time = `TIME_PERIOD`, value = `OBS_VALUE`) %>%
  mutate(geo_code = sub(":.*$", "", geo)) %>%
  filter(geo_code == "CZ", time == 2024, age != "TOTAL", age != "UNK") %>%
  mutate(
    age_code = stringr::str_extract(age, "Y_LT1|Y_GE\\d+|Y\\d+"),
    sex_raw = sub(":.*$", "", sex),
    age_num = case_when(
      age_code == "Y_LT1" ~ 0L,
      grepl("^Y_GE\\d+$", age_code) ~ as.integer(sub("^Y_GE", "", age_code)),
      grepl("^Y\\d+$", age_code) ~ as.integer(sub("^Y", "", age_code)),
      TRUE ~ NA_integer_
    ),
    value = as.numeric(value)
  ) %>%
  filter(!is.na(age_num), sex_raw %in% c("M", "F"), !is.na(value)) %>%
  select(age = age_num, value)

# Aggregate to age groups for living population
living_pop_dist <- eu_pop %>%
  mutate(
    age_group = case_when(
      age <= 15 ~ "0–15",
      age <= 24 ~ "15-24",
      age <= 49 ~ "25–49",
      age <= 59 ~ "50–59",
      age <= 69 ~ "60–69",
      age <= 79 ~ "70–79",
      TRUE ~ "80+"
    )
  ) %>%
  group_by(age_group) %>%
  summarise(pop = sum(value), .groups = "drop") %>%
  mutate(share = pop / sum(pop))

cat("\n=== Living Population Age Shares (Eurostat 2024) ===\n")
print(living_pop_dist)

cat("\n=== STEP 2: Calculate Death Age Shares ===\n")

death_age_shares <- wk_missing_age %>%
  filter(age_group != "Unknown") %>%
  group_by(age_group) %>%
  summarise(total_missing = sum(missing_deaths, na.rm = TRUE), .groups = "drop") %>%
  mutate(share = total_missing / sum(total_missing))

cat("\nDeath Age Shares:\n")
print(death_age_shares)

# Age ranges
age_ranges <- data.frame(
  age_group = c("0–15", "15-24", "25–49", "50–59", "60–69", "70–79", "80+"),
  min_age = c(0, 15, 25, 50, 60, 70, 80),
  max_age = c(15, 24, 49, 59, 69, 79, 105),
  stringsAsFactors = FALSE
)

# Combine with ranges
death_dist <- death_age_shares %>%
  left_join(age_ranges, by = "age_group") %>%
  filter(!is.na(min_age))

living_dist <- living_pop_dist %>%
  left_join(age_ranges, by = "age_group") %>%
  filter(!is.na(min_age))

# Gender shares
gender_dist <- data.frame(
  gender = c("F", "M"),
  share = c(0.5444, 0.4556)
)

cat("\n=== STEP 3: Identify Unknown Cohort ===\n")

main_data$week_date_of_death <- as.Date(main_data$week_date_of_death)
main_data$week_date_of_positivity <- as.Date(main_data$week_date_of_positivity)
main_data$is_death <- !is.na(main_data$week_date_of_death)

main_data$has_gender <- !is.na(main_data$Gender) & 
  nzchar(trimws(as.character(main_data$Gender))) &
  !(main_data$Gender %in% c("", "NA", "Unknown"))

main_data$has_yob <- !is.na(main_data$year_of_birth_end) & 
  main_data$year_of_birth_end > 1900 &
  main_data$year_of_birth_end < 2025

cat(sprintf("\nDeaths: %d | Non-deaths: %d\n", sum(main_data$is_death), sum(!main_data$is_death)))

cat("\n=== STEP 4: Fitting Functions ===\n")

# CRITICAL: Imputed birth years must align with min_age calculation used in comparisons
# min_age = 2024 - year_of_birth_end - 1
# Therefore: year_of_birth_end = ref_year - sampled_age - 1

fit_yob <- function(n, age_dist, ref_years) {
  sampled_groups <- sample(1:nrow(age_dist), n, replace = TRUE, prob = age_dist$share)
  ages <- numeric(n)
  for (g in 1:nrow(age_dist)) {
    idx <- which(sampled_groups == g)
    if (length(idx) > 0) {
      ages[idx] <- sample(age_dist$min_age[g]:age_dist$max_age[g], length(idx), replace = TRUE)
    }
  }
  # Adjust by -1 to align with min_age calculation (ref_year - age - 1)
  ref_years - ages - 1
}

fit_gender <- function(n, gender_dist) {
  sample(gender_dist$gender, n, replace = TRUE, prob = gender_dist$share)
}

cat("\n=== STEP 5: Fit Unknown Cohort ===\n")

main_data$gender_imputed <- main_data$Gender
main_data$yob_imputed <- main_data$year_of_birth_end
main_data$imputed_flag_gender <- FALSE
main_data$imputed_flag_yob <- FALSE

# DEATHS: Use death age distribution
death_idx <- which(main_data$is_death)
death_need_yob <- death_idx[!main_data$has_yob[death_idx]]
death_need_gender <- death_idx[!main_data$has_gender[death_idx]]

cat(sprintf("\nDeaths needing YoB: %d\n", length(death_need_yob)))
if (length(death_need_yob) > 0) {
  cat("  Fitting deaths with DEATH age distribution...\n")
  ref_years <- ifelse(!is.na(main_data$week_date_of_death[death_need_yob]),
                      year(main_data$week_date_of_death[death_need_yob]), 2020)
  main_data$yob_imputed[death_need_yob] <- fit_yob(length(death_need_yob), death_dist, ref_years)
  main_data$imputed_flag_yob[death_need_yob] <- TRUE
}

cat(sprintf("Deaths needing Gender: %d\n", length(death_need_gender)))
if (length(death_need_gender) > 0) {
  main_data$gender_imputed[death_need_gender] <- fit_gender(length(death_need_gender), gender_dist)
  main_data$imputed_flag_gender[death_need_gender] <- TRUE
}

# NON-DEATHS: Use living population age distribution
nondeath_idx <- which(!main_data$is_death)
nondeath_need_yob <- nondeath_idx[!main_data$has_yob[nondeath_idx]]
nondeath_need_gender <- nondeath_idx[!main_data$has_gender[nondeath_idx]]

cat(sprintf("\nNon-deaths needing YoB: %d\n", length(nondeath_need_yob)))
if (length(nondeath_need_yob) > 0) {
  cat("  Fitting non-deaths with LIVING POPULATION age distribution...\n")
  ref_years <- ifelse(!is.na(main_data$week_date_of_positivity[nondeath_need_yob]),
                      year(main_data$week_date_of_positivity[nondeath_need_yob]), 2020)
  main_data$yob_imputed[nondeath_need_yob] <- fit_yob(length(nondeath_need_yob), living_dist, ref_years)
  main_data$imputed_flag_yob[nondeath_need_yob] <- TRUE
}

cat(sprintf("Non-deaths needing Gender: %d\n", length(nondeath_need_gender)))
if (length(nondeath_need_gender) > 0) {
  main_data$gender_imputed[nondeath_need_gender] <- fit_gender(length(nondeath_need_gender), gender_dist)
  main_data$imputed_flag_gender[nondeath_need_gender] <- TRUE
}

cat("\n=== STEP 6: Validate ===\n")

# BEFORE IMPUTATION: Count by age group (only records with known YoB originally)
before_counts <- main_data %>%
  filter(!is.na(year_of_birth_end)) %>%  # Original known YoB
  mutate(
    age_2024 = 2024 - year_of_birth_end,
    age_group = cut(age_2024, breaks = c(-Inf, 15, 24, 49, 59, 69, 79, Inf),
                    labels = c("0–15", "15-24", "25–49", "50–59", "60–69", "70–79", "80+"), right = TRUE)
  ) %>%
  count(age_group, name = "before_count")

# AFTER IMPUTATION: Count by age group (all records including imputed)
after_counts <- main_data %>%
  mutate(
    age_2024 = 2024 - yob_imputed,
    age_group = cut(age_2024, breaks = c(-Inf, 15, 24, 49, 59, 69, 79, Inf),
                    labels = c("0–15", "15-24", "25–49", "50–59", "60–69", "70–79", "80+"), right = TRUE)
  ) %>%
  count(age_group, name = "after_count")

# IMPUTED ONLY: Count just the imputed records
imputed_only_counts <- main_data %>%
  filter(imputed_flag_yob) %>%
  mutate(
    age_2024 = 2024 - yob_imputed,
    age_group = cut(age_2024, breaks = c(-Inf, 15, 24, 49, 59, 69, 79, Inf),
                    labels = c("0–15", "15-24", "25–49", "50–59", "60–69", "70–79", "80+"), right = TRUE)
  ) %>%
  count(age_group, name = "imputed_count")

# Combine for comparison
sanity_check <- before_counts %>%
  full_join(after_counts, by = "age_group") %>%
  full_join(imputed_only_counts, by = "age_group") %>%
  mutate(
    before_count = replace_na(before_count, 0),
    after_count = replace_na(after_count, 0),
    imputed_count = replace_na(imputed_count, 0),
    difference = after_count - before_count,
    check = difference - imputed_count  # Should be 0!
  )

cat("\n=== SANITY CHECK: Age Group Counts ===\n")
cat("(Before = original known YoB, After = all records, Imputed = newly added)\n")
cat("Check column should be 0 (difference should equal imputed count)\n\n")
print(sanity_check)

# Flag if any existing records were changed (allow small rounding errors)
max_error <- max(abs(sanity_check$check))
pct_error <- 100 * max_error / max(sanity_check$imputed_count, na.rm = TRUE)

if (max_error > 100 & pct_error > 0.1) {
  cat(sprintf("\n⚠️  WARNING: Possible re-attribution detected! Max error: %d (%.2f%%)\n", 
              max_error, pct_error))
} else {
  cat(sprintf("\n✓ PASS: No re-attribution detected. Max error: %d (%.3f%% - within tolerance)\n",
              max_error, pct_error))
}

# Validate DEATHS
deaths_val <- main_data %>%
  filter(is_death) %>%
  mutate(
    age_at_death = year(week_date_of_death) - yob_imputed,
    age_group = cut(age_at_death, breaks = c(-Inf, 15, 24, 49, 59, 69, 79, Inf),
                    labels = c("0–15", "15-24", "25–49", "50–59", "60–69", "70–79", "80+"), right = TRUE)
  ) %>%
  count(age_group) %>%
  mutate(fitted_pct = 100 * n / sum(n)) %>%
  left_join(death_age_shares %>% mutate(target_pct = share * 100), by = "age_group")

cat("\nDeath Age Fit:\n")
print(deaths_val, digits = 2)

# Validate LIVING POPULATION (non-deaths)
living_val <- main_data %>%
  filter(!is_death) %>%
  mutate(
    age_2024 = 2024 - yob_imputed,
    age_group = cut(age_2024, breaks = c(-Inf, 15, 24, 49, 59, 69, 79, Inf),
                    labels = c("0–15", "15-24", "25–49", "50–59", "60–69", "70–79", "80+"), right = TRUE)
  ) %>%
  count(age_group) %>%
  mutate(fitted_pct = 100 * n / sum(n)) %>%
  left_join(living_pop_dist %>% mutate(target_pct = share * 100), by = "age_group")

cat("\nLiving Population Age Fit:\n")
print(living_val, digits = 2)

cat("\n=== STEP 7: Export ===\n")

final_data <- main_data %>%
  mutate(
    Gender = case_when(
      gender_imputed == "M" ~ "1",
      gender_imputed == "F" ~ "2",
      TRUE ~ gender_imputed
    ),
    year_of_birth_end = yob_imputed,
    age_at_death = if_else(is_death, year(week_date_of_death) - yob_imputed, NA_integer_)
  ) %>%
  select(-gender_imputed, -yob_imputed, -has_gender, -has_yob)

write.csv(final_data, "data/mzcr_no_or_first_infection_with_imputation.csv", row.names = FALSE, na = "")

cat("\n✓ Output: data/mzcr_no_or_first_infection_with_imputation.csv\n")
cat("\n=== FITTING COMPLETE ===\n")
cat("Deaths fitted to death age patterns\n")
cat("Living people fitted to Eurostat population patterns\n")