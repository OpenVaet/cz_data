library(dplyr)
library(tibble)
library(tidyr)
library(lubridate)
library(stringr)
library(parallel)
library(ISOweek)
if (!requireNamespace("slider", quietly = TRUE)) install.packages("slider")
library(slider)

# ============================================================================
# European Standard Population 2013 (ESP 2013)
# ============================================================================
esp2013 <- tibble(
  age_start = c(0, 1, seq(5, 90, by = 5), 95),
  age_end   = c(0, 4, seq(9, 94, by = 5), 999),
  esp_pop   = c(1000, 4000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000,
                7000, 7000, 6500, 6000, 5500, 5000, 4000, 2500, 1500, 800, 200)
) %>%
  mutate(
    age_group = paste0(age_start, "-", ifelse(age_end == 999, "95+", age_end)),
    age_midpoint = ifelse(age_end == 999, age_start + 2.5, (age_start + age_end) / 2)
  )

cat("ESP 2013 total population:", sum(esp2013$esp_pop), "\n")

# ============================================================================
# Helper Functions
# ============================================================================
yw2date <- function(yw) {
  year <- floor(yw / 100)
  week <- yw %% 100
  ISOweek::ISOweek2date(sprintf("%04d-W%02d-1", year, week))
}

assign_age_group <- function(age, esp_df) {
  sapply(age, function(a) {
    if (is.na(a)) return(NA_integer_)
    idx <- which(a >= esp_df$age_start & a <= esp_df$age_end)
    if (length(idx) == 0) return(NA_integer_)
    idx[1]
  })
}

# ============================================================================
# Parameters: BASELINE WEEK for inclusion (15+ only)
# ============================================================================
baseline_week <- 202013                      # 2020 - week 13
baseline_date <- yw2date(baseline_week)

# ============================================================================
# Load and Prepare Data  (EXCLUDE <15 at BASELINE)
# ============================================================================
file_path <- "data/mzcr_no_or_first_infection_with_imputation.csv"

if (!file.exists(file_path)) {
  stop("Please run download_and_data_integrity.R first to generate the data file.")
}

data_raw <- read.csv(file_path, stringsAsFactors = FALSE)
cat("Raw data dimensions:", nrow(data_raw), "rows x", ncol(data_raw), "columns\n")



data <- data_raw %>%
  mutate(
    year_of_birth = as.integer(year_of_birth_end),
    date_positivity = as.Date(week_date_of_positivity),
    date_death = as.Date(week_date_of_death),
    date_first_dose_char = trimws(as.character(Date_First_Dose)),
    dv1 = if_else(
      nzchar(date_first_dose_char) & !is.na(date_first_dose_char),
      suppressWarnings({
        if (grepl("^\\d{4}-W\\d{2}", date_first_dose_char[1])) {
          yr <- as.integer(substr(date_first_dose_char, 1, 4))
          wk <- as.integer(substr(date_first_dose_char, 7, 8))
          yr * 100 + wk
        } else {
          as.integer(gsub("-", "", date_first_dose_char))
        }
      }),
      NA_integer_
    )
  ) %>%
  filter(!is.na(year_of_birth), year_of_birth >= 1900, year_of_birth <= 2024) %>%
  # EXCLUDE <15 at BASELINE
  mutate(age_at_baseline = as.integer(lubridate::year(baseline_date) - year_of_birth)) %>%
  filter(age_at_baseline >= 15, age_at_baseline <= 120) %>%
  select(id, year_of_birth, dv1, date_positivity, date_death)

cat("Prepared data (15+ at baseline):\n")
cat("  - Rows:", nrow(data), "\n")
cat("  - Deaths:", sum(!is.na(data$date_death)), "\n")
cat("  - COVID+ cases:", sum(!is.na(data$date_positivity)), "\n")
cat("  - Vaccinated:", sum(!is.na(data$dv1)), "\n\n")

# ============================================================================
# Weekly Mortality Calculation  — export minimal long table
# One row per week × age_group × (is_vaccinated, is_positive)
# ============================================================================
calculate_age_strata_for_week <- function(week_code, data, esp_ref) {
  week_date <- yw2date(week_code)
  if (is.na(week_date) || !inherits(week_date, "Date")) {
    warning(paste("Invalid week date for week", week_code))
    return(NULL)
  }

  data_week <- data %>%
    mutate(
      age_at_week   = as.integer(lubridate::year(week_date) - year_of_birth),
      is_alive      = is.na(date_death) | date_death >= week_date,
      is_vaccinated = !is.na(dv1) & dv1 <= week_code,
      is_positive   = !is.na(date_positivity) & date_positivity <= week_date,
      died_this_week = !is.na(date_death) &
        date_death >= week_date &
        date_death < (week_date + 7)
    ) %>%
    filter(is_alive, age_at_week >= 15, age_at_week <= 120) %>%
    mutate(age_group_idx = assign_age_group(age_at_week, esp_ref))

  if (nrow(data_week) == 0) {
    warning(paste("No data for week", week_code))
    return(NULL)
  }

  # 4-way stratification (vaccination x positivity) by age
  counts_by_age_4way <- data_week %>%
    filter(!is.na(age_group_idx)) %>%
    group_by(age_group_idx, is_vaccinated, is_positive) %>%
    summarise(
      n_pop = n(),
      n_deaths = sum(died_this_week, na.rm = TRUE),
      .groups = "drop"
    )

  # Ensure every age/flag cell exists and join ESP weights
  fourway_rates <- counts_by_age_4way %>%
    full_join(
      expand.grid(
        age_group_idx = 1:nrow(esp_ref),
        is_vaccinated = c(TRUE, FALSE),
        is_positive   = c(TRUE, FALSE)
      ),
      by = c("age_group_idx", "is_vaccinated", "is_positive")
    ) %>%
    replace_na(list(n_pop = 0, n_deaths = 0)) %>%
    left_join(esp_ref %>% mutate(age_group_idx = row_number()), by = "age_group_idx")

  # Export minimal long table for downstream ASMR
  age_detail <- fourway_rates %>%
    mutate(
      week      = week_code,
      date      = week_date,
      age_group = paste0(age_start, "-", ifelse(age_end == 999, "95+", age_end))
    ) %>%
    transmute(
      week, date,
      age_group_idx,
      age_group, age_start, age_end,
      is_vaccinated, is_positive,
      n_pop,               # at-risk this week in this cell
      n_deaths,            # deaths in the 7-day window for this cell
      esp_pop              # ESP2013 weight for this age cell
    )

  age_detail
}

# ============================================================================
# Process All Weeks with Pause/Resume  — write only the long table
# ============================================================================
age_file <- "data/asmr_esp2013_age_stratified_incremental_with_imputation.csv"

# Generate all ISO week codes 2020-2024
all_week_codes <- c()
for (year in 2020:2024) {
  for (week in 1:53) {
    all_week_codes <- c(all_week_codes, year * 100 + week)
  }
}

# Check for existing long-table results to support resume
already_processed <- c()
if (file.exists(age_file)) {
  existing_age <- read.csv(age_file, stringsAsFactors = FALSE)
  already_processed <- sort(unique(existing_age$week))
  cat("Found existing age-stratified rows for", length(already_processed), "weeks\n")
} else {
  cat("No existing age-stratified file found - will process all weeks\n")
}

week_codes <- setdiff(all_week_codes, already_processed)
cat("Total weeks:", length(all_week_codes), "\n")
cat("Already completed:", length(already_processed), "\n")
cat("Remaining to process:", length(week_codes), "\n\n")

if (length(week_codes) == 0) {
  cat("All weeks already processed! Loading existing age-stratified results...\n")
  age_stratified <- read.csv(age_file, stringsAsFactors = FALSE) %>%
    mutate(date = as.Date(date))
} else {
  # Test first week if starting fresh: just show structure of the long table
  if (length(already_processed) == 0) {
    cat("=== Testing first week ===\n")
    test_age <- calculate_age_strata_for_week(202152, data, esp2013)
    if (!is.null(test_age)) {
      cat("Test age_detail structure:\n")
      print(str(test_age))
      cat("\nFirst few rows:\n")
      print(head(test_age))
      cat("\nTotal deaths in test week:", sum(test_age$n_deaths), "\n")
    } else {
      stop("Test failed")
    }
  }

  cat("=== Processing remaining weeks ===\n")
  batch_size <- 10
  n_batches <- ceiling(length(week_codes) / batch_size)

  for (batch_idx in 1:n_batches) {
    start_idx <- (batch_idx - 1) * batch_size + 1
    end_idx   <- min(batch_idx * batch_size, length(week_codes))
    batch_weeks <- week_codes[start_idx:end_idx]

    cat(sprintf("Batch %d/%d: Weeks %s to %s... ",
                batch_idx, n_batches, batch_weeks[1], batch_weeks[length(batch_weeks)]))

    n_cores <- max(1, detectCores() - 1)

    if (.Platform$OS.type == "windows") {
      cl <- makeCluster(n_cores)
      clusterExport(cl, c("data","esp2013","calculate_age_strata_for_week",
                          "yw2date","assign_age_group"),
                    envir = environment())
      clusterEvalQ(cl, {
        library(dplyr); library(tibble); library(tidyr);
        library(lubridate); library(ISOweek)
      })
      batch_results <- parLapply(
        cl, batch_weeks, calculate_age_strata_for_week,
        data = data, esp_ref = esp2013
      )
      stopCluster(cl)
    } else {
      batch_results <- mclapply(
        batch_weeks, calculate_age_strata_for_week,
        data = data, esp_ref = esp2013,
        mc.cores = n_cores
      )
    }

    batch_age <- bind_rows(batch_results)

    if (file.exists(age_file)) {
      write.table(batch_age, age_file, sep = ",", append = TRUE,
                  row.names = FALSE, col.names = FALSE)
    } else {
      write.csv(batch_age, age_file, row.names = FALSE)
    }

    cat(sprintf("✓ Deaths in batch: %d\n", sum(batch_age$n_deaths)))
  }

  cat("\nProcessing complete! Loading age-stratified results...\n")

  age_stratified_raw <- read.csv(age_file, stringsAsFactors = FALSE)
  age_stratified <- age_stratified_raw %>%
    mutate(date = as.Date(as.character(date)))
}

# ============================================================================
# Save Final Results (single canonical export)
# ============================================================================
age_stratified <- age_stratified %>%
  filter(!is.na(week)) %>%
  arrange(date, age_group_idx, is_vaccinated, is_positive)

write.csv(
  age_stratified,
  "data/asmr_esp2013_age_stratified_with_imputation.csv",
  row.names = FALSE
)

cat("\nFiles created:\n")
cat("  - asmr_esp2013_age_stratified_with_imputation.csv (",
    nrow(age_stratified), "rows)\n")

# =============================================================================
# Rebuild weekly summaries from the long table (age_stratified)
# =============================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(lubridate)

# Helper to compute ASMR from an age-level table with n_pop, n_deaths, esp_pop
asmr_from_age <- function(df_age) {
  # df_age must have: n_pop, n_deaths, esp_pop for a single (week, stratum)
  # compute age-specific rates, then ESP-weighted sum
  df_age %>%
    mutate(age_rate = if_else(n_pop > 0, (n_deaths / n_pop) * 100000, 0)) %>%
    summarise(asmr = sum(age_rate * esp_pop, na.rm = TRUE) / sum(esp_pop, na.rm = TRUE),
              .groups = "drop") %>%
    pull(asmr)
}

# 1) Core four-way tallies by week (vacc x pos), aggregated over age
fourway_week <- age_stratified %>%
  group_by(week, date, is_vaccinated, is_positive) %>%
  summarise(
    pop = sum(n_pop, na.rm = TRUE),
    deaths = sum(n_deaths, na.rm = TRUE),
    .groups = "drop"
  )

# Convenience wide layout for populations and deaths (vp, vn, sp, sn)
fourway_wide <- fourway_week %>%
  mutate(label = paste0(if_else(is_vaccinated, "v", "s"),
                        if_else(is_positive, "p", "n"))) %>%  # v=vac, s=unvac; p=pos, n=neg
  select(week, date, label, pop, deaths) %>%
  pivot_wider(
    names_from = label,
    values_from = c(pop, deaths),
    values_fill = 0
  )

# 2) Vaccination-only population (sums over positivity)
pop_vacc <- age_stratified %>%
  group_by(week, date, is_vaccinated) %>%
  summarise(pop = sum(n_pop, na.rm = TRUE), .groups = "drop") %>%
  mutate(label = if_else(is_vaccinated, "pop_vaccinated", "pop_unvaccinated")) %>%
  select(-is_vaccinated) %>%
  pivot_wider(names_from = label, values_from = pop, values_fill = 0)

# 3) Total population (sums over both dimensions)
pop_total <- age_stratified %>%
  group_by(week, date) %>%
  summarise(pop_total = sum(n_pop, na.rm = TRUE), .groups = "drop")

# 4) Compute ASMRs (overall, vaccinated-only, unvaccinated-only) per week
#    Do this by computing age-specific rates then ESP-weighted sums.

# Overall ASMR (aggregate across both flags, per age)
asmr_overall <- age_stratified %>%
  group_by(week, date, age_group_idx, esp_pop) %>%
  summarise(n_pop = sum(n_pop, na.rm = TRUE),
            n_deaths = sum(n_deaths, na.rm = TRUE),
            .groups = "drop_last") %>%
  group_split(week, date) %>%
  lapply(function(df) {
    tibble(
      week = df$week[1],
      date = df$date[1],
      asmr_total = asmr_from_age(df)
    )
  }) %>% bind_rows()

# Vaccinated-only ASMR (sum over positivity, restrict to vaccinated)
asmr_vacc <- age_stratified %>%
  filter(is_vaccinated) %>%
  group_by(week, date, age_group_idx, esp_pop) %>%
  summarise(n_pop = sum(n_pop, na.rm = TRUE),
            n_deaths = sum(n_deaths, na.rm = TRUE),
            .groups = "drop_last") %>%
  group_split(week, date) %>%
  lapply(function(df) {
    tibble(
      week = df$week[1],
      date = df$date[1],
      asmr_vaccinated = asmr_from_age(df)
    )
  }) %>% bind_rows()

# Unvaccinated-only ASMR (sum over positivity, restrict to unvaccinated)
asmr_unvacc <- age_stratified %>%
  filter(!is_vaccinated) %>%
  group_by(week, date, age_group_idx, esp_pop) %>%
  summarise(n_pop = sum(n_pop, na.rm = TRUE),
            n_deaths = sum(n_deaths, na.rm = TRUE),
            .groups = "drop_last") %>%
  group_split(week, date) %>%
  lapply(function(df) {
    tibble(
      week = df$week[1],
      date = df$date[1],
      asmr_unvaccinated = asmr_from_age(df)
    )
  }) %>% bind_rows()

# 5) Assemble the results table equivalent to the old "results"
results <- fourway_wide %>%
  left_join(pop_vacc, by = c("week","date")) %>%
  left_join(pop_total, by = c("week","date")) %>%
  left_join(asmr_overall, by = c("week","date")) %>%
  left_join(asmr_vacc, by = c("week","date")) %>%
  left_join(asmr_unvacc, by = c("week","date")) %>%
  # Rename four-way columns to match old naming (vp,vn,sp,sn)
  transmute(
    week, date,
    pop_vp = pop_vp, pop_vn = pop_vn, pop_sp = pop_sp, pop_sn = pop_sn,
    deaths_vp = deaths_vp, deaths_vn = deaths_vn, deaths_sp = deaths_sp, deaths_sn = deaths_sn,
    asmr_total, asmr_vaccinated, asmr_unvaccinated,
    pop_vaccinated, pop_unvaccinated, pop_total
  ) %>%
  arrange(date)

results_nonzero <- results %>% filter(asmr_total > 0)

# =============================================================================
# Sanity Check
# =============================================================================
# After calculating results, add validation
results_check <- results %>%
  mutate(
    vacc_proportion = if_else(pop_total > 0, pop_vaccinated / pop_total, NA_real_),
    unvacc_proportion = if_else(pop_total > 0, pop_unvaccinated / pop_total, NA_real_)
  )

# Plot to verify population dynamics
ggplot(results_check, aes(x = date)) +
  geom_line(aes(y = vacc_proportion, color = "Vaccinated")) +
  geom_line(aes(y = unvacc_proportion, color = "Unvaccinated")) +
  labs(title = "Population Proportions Over Time",
       y = "Proportion of Total Population",
       color = NULL) +
  theme_minimal(base_size = 14)

pop_summary <- results %>%
  group_by(year = year(date)) %>%
  summarise(
    min_unvacc_pop = min(pop_unvaccinated, na.rm = TRUE),
    max_unvacc_pop = max(pop_unvaccinated, na.rm = TRUE),
    mean_vacc_proportion = mean(pop_vaccinated / pop_total, na.rm = TRUE),
    .groups = "drop"
  )

# =============================================================================
# ggplot (dataset is already 15+, so no need to re-filter)
# =============================================================================
cat("\n=== Creating ggplot (15+ dynamic denominators) ===\n")

asmr_for_plot <- results_nonzero %>%
  select(date, asmr_total, asmr_vaccinated, asmr_unvaccinated) %>%
  rename(asmr_v = asmr_vaccinated, asmr_s = asmr_unvaccinated) %>%
  pivot_longer(cols = starts_with("asmr_"),
               names_to = "group",
               values_to = "asmr") %>%
  mutate(group = factor(group,
                        levels = c("asmr_total","asmr_v","asmr_s"),
                        labels = c("Overall (15+)","Vaccinated (15+)","Unvaccinated (15+)")))

# 5-week centered moving average (+2, current, -2)
asmr_for_plot_ma <- asmr_for_plot %>%
  arrange(group, date) %>%
  group_by(group) %>%
  mutate(
    asmr_ma5 = slide_dbl(asmr, mean, .before = 2, .after = 2, .complete = TRUE)
  ) %>%
  ungroup()

p <- ggplot(asmr_for_plot_ma, aes(x = date, y = asmr_ma5,
                                  color = group, linetype = group)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  scale_color_manual(values = c(
    "Overall (15+)"       = "black",
    "Vaccinated (15+)"    = "red",
    "Unvaccinated (15+)"  = "blue"
  )) +
  scale_linetype_manual(values = c(
    "Overall (15+)"       = "dashed",
    "Vaccinated (15+)"    = "solid",
    "Unvaccinated (15+)"  = "solid"
  )) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  labs(
    title = "All-Cause Mortality in Czechia (with imputation) — Ages 15+",
    subtitle = "Age-Standardized Mortality Rate (ESP 2013), 5-week centered moving average (+2, current, −2)",
    x = "Date", y = "ASMR per 100,000 per week",
    color = "Group", linetype = "Group",
    caption = "Data: Czech Ministry of Health | Rates standardized to ESP 2013; denominators update weekly based on actual population"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12),
    plot.caption = element_text(hjust = 0, color = "gray50", size = 9),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p

# Save figures
ggsave("CzMort_ASMR_ESP2013_15plus_MA5.png", p,
       width = 12, height = 7, dpi = 300, bg = "white")

ggsave("CzMort_ASMR_ESP2013_15plus_MA5_ggplot.png", p,
       width = 12, height = 7, dpi = 300, bg = "white")

cat("ggplot saved: CzMort_ASMR_ESP2013_15plus_MA5_ggplot.png\n")


# =============================================================================
# Also save a compact 15+ dataset (now fully populated from long table)
# =============================================================================
asmr_15plus <- results_nonzero %>%
  transmute(
    week, date,
    pop_v = pop_vaccinated,
    pop_s = pop_unvaccinated,
    deaths_v = deaths_vp + deaths_vn,  # vaccinated deaths (sum over positivity)
    deaths_s = deaths_sp + deaths_sn,  # unvaccinated deaths
    asmr_total,
    asmr_v = asmr_vaccinated,
    asmr_s = asmr_unvaccinated
  )

write.csv(asmr_15plus, "data/asmr_esp2013_15plus_with_imputation.csv", row.names = FALSE)
cat("Data saved: data/asmr_esp2013_15plus_with_imputation.csv\n")

# =============================================================================
# Summary Statistics
# =============================================================================
cat("\n=== Summary Statistics (15+) ===\n")
cat("Total weeks:", nrow(results), "\n")
cat("Weeks with deaths:", nrow(results_nonzero), "\n")
cat("Date range:", min(results_nonzero$date), "to", max(results_nonzero$date), "\n\n")

total_deaths <- sum(results_nonzero$deaths_vp) + sum(results_nonzero$deaths_vn) +
  sum(results_nonzero$deaths_sp) + sum(results_nonzero$deaths_sn)

cat("Total deaths:", total_deaths, "\n")
cat("  Vaccinated & COVID+:", sum(results_nonzero$deaths_vp), "\n")
cat("  Vaccinated & COVID-:", sum(results_nonzero$deaths_vn), "\n")
cat("  Unvaccinated & COVID+:", sum(results_nonzero$deaths_sp), "\n")
cat("  Unvaccinated & COVID-:", sum(results_nonzero$deaths_sn), "\n\n")

cat("Mean ASMR (per 100,000/week):\n")
cat("  Overall:", round(mean(results_nonzero$asmr_total), 2), "\n")
cat("  Vaccinated:", round(mean(results_nonzero$asmr_vaccinated), 2), "\n")
cat("  Unvaccinated:", round(mean(results_nonzero$asmr_unvaccinated), 2), "\n")
