library(dplyr)
library(tibble)
library(tidyr)
library(lubridate)
library(stringr)
library(parallel)
library(ISOweek)

# ============================================================================
# European Standard Population 2013 (ESP 2013)
# ============================================================================
esp2013 <- tibble(
  age_start = c(0, 1, seq(5, 90, by = 5), 95),
  age_end = c(0, 4, seq(9, 94, by = 5), 999),
  esp_pop = c(1000, 4000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000, 
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
# Load and Prepare Data
# ============================================================================
file_path <- "data/mzcr_no_or_first_infection.csv"

if (!file.exists(file_path)) {
  stop("Please run download_and_data_integrity.R first to generate the data file.")
}

data_raw <- read.csv(file_path, stringsAsFactors = FALSE)
cat("Raw data dimensions:", nrow(data_raw), "rows x", ncol(data_raw), "columns\n")

# Prepare data
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
  select(ID, year_of_birth, dv1, date_positivity, date_death)

cat("Prepared data:\n")
cat("  - Rows:", nrow(data), "\n")
cat("  - Deaths:", sum(!is.na(data$date_death)), "\n")
cat("  - COVID+ cases:", sum(!is.na(data$date_positivity)), "\n")
cat("  - Vaccinated:", sum(!is.na(data$dv1)), "\n\n")

# ============================================================================
# Weekly Mortality Calculation
# ============================================================================
calculate_asmr_for_week <- function(week_code, data, esp_ref) {
  week_date <- yw2date(week_code)
  if (is.na(week_date)) return(NULL)
  
  data_week <- data %>%
    mutate(
      age_at_week = as.integer(year(week_date) - year_of_birth),
      is_alive = is.na(date_death) | date_death >= week_date,  # FIXED: >= not >
      is_vaccinated = !is.na(dv1) & dv1 <= week_code,
      is_positive = !is.na(date_positivity) & date_positivity <= week_date,
      died_this_week = !is.na(date_death) & 
        date_death >= week_date & 
        date_death < week_date + 7
    ) %>%
    filter(is_alive, age_at_week >= 0, age_at_week <= 120) %>%
    mutate(age_group_idx = assign_age_group(age_at_week, esp_ref))
  
  counts_by_age <- data_week %>%
    filter(!is.na(age_group_idx)) %>%
    group_by(age_group_idx, is_vaccinated, is_positive) %>%
    summarise(n_pop = n(), n_deaths = sum(died_this_week), .groups = "drop")
  
  results_by_age <- esp_ref %>%
    mutate(age_group_idx = row_number()) %>%
    left_join(counts_by_age, by = "age_group_idx") %>%
    replace_na(list(n_pop = 0, n_deaths = 0, is_vaccinated = FALSE, is_positive = FALSE)) %>%
    mutate(mortality_rate = if_else(n_pop > 0, (n_deaths / n_pop) * 100000, 0))
  
  # Age-stratified detail for export
  age_detail <- results_by_age %>%
    mutate(week = week_code, date = week_date) %>%
    select(week, date, age_group, age_start, age_end, is_vaccinated, is_positive,
           n_pop, n_deaths, mortality_rate, esp_pop)
  
  # Calculate ASMR by subgroup
  calc_asmr <- function(df, vacc, pos) {
    df_sub <- df %>% filter(is_vaccinated == vacc, is_positive == pos)
    if (nrow(df_sub) == 0 || sum(df_sub$n_pop) == 0) {
      return(tibble(total_pop = 0, total_deaths = 0, asmr = 0))
    }
    tibble(
      total_pop = sum(df_sub$n_pop),
      total_deaths = sum(df_sub$n_deaths),
      asmr = sum(df_sub$mortality_rate * df_sub$esp_pop) / sum(df_sub$esp_pop)
    )
  }
  
  vp <- calc_asmr(results_by_age, TRUE, TRUE)
  vn <- calc_asmr(results_by_age, TRUE, FALSE)
  sp <- calc_asmr(results_by_age, FALSE, TRUE)
  sn <- calc_asmr(results_by_age, FALSE, FALSE)
  
  # Combined groups
  v_all <- results_by_age %>% filter(is_vaccinated == TRUE)
  s_all <- results_by_age %>% filter(is_vaccinated == FALSE)
  p_all <- results_by_age %>% filter(is_positive == TRUE)
  n_all <- results_by_age %>% filter(is_positive == FALSE)
  
  list(
    summary = tibble(
      week = week_code,
      date = week_date,
      pop_vp = vp$total_pop, pop_vn = vn$total_pop,
      pop_sp = sp$total_pop, pop_sn = sn$total_pop,
      deaths_vp = vp$total_deaths, deaths_vn = vn$total_deaths,
      deaths_sp = sp$total_deaths, deaths_sn = sn$total_deaths,
      asmr_vp = vp$asmr, asmr_vn = vn$asmr,
      asmr_sp = sp$asmr, asmr_sn = sn$asmr,
      asmr_vaccinated = sum(v_all$mortality_rate * v_all$esp_pop) / sum(v_all$esp_pop),
      asmr_unvaccinated = sum(s_all$mortality_rate * s_all$esp_pop) / sum(s_all$esp_pop),
      asmr_positive = sum(p_all$mortality_rate * p_all$esp_pop) / sum(p_all$esp_pop),
      asmr_negative = sum(n_all$mortality_rate * n_all$esp_pop) / sum(n_all$esp_pop),
      asmr_total = sum(results_by_age$mortality_rate * results_by_age$esp_pop) / sum(results_by_age$esp_pop)
    ),
    age_detail = age_detail
  )
}

# ============================================================================
# Process All Weeks with Pause/Resume
# ============================================================================
summary_file <- "data/asmr_esp2013_results_incremental.csv"
age_file <- "data/asmr_esp2013_age_stratified_incremental.csv"

# Generate all week codes
all_week_codes <- c()
for (year in 2020:2024) {
  for (week in 1:53) {
    all_week_codes <- c(all_week_codes, year * 100 + week)
  }
}

# Check for existing results
already_processed <- c()
if (file.exists(summary_file)) {
  existing_summary <- read.csv(summary_file, stringsAsFactors = FALSE)
  already_processed <- unique(existing_summary$week)
  cat("Found existing results with", length(already_processed), "weeks already processed\n")
} else {
  cat("No existing results found - will process all weeks\n")
}

week_codes <- setdiff(all_week_codes, already_processed)
cat("Total weeks:", length(all_week_codes), "\n")
cat("Already completed:", length(already_processed), "\n")
cat("Remaining to process:", length(week_codes), "\n\n")

if (length(week_codes) == 0) {
  cat("All weeks already processed! Loading existing results...\n")
  results <- read.csv(summary_file, stringsAsFactors = FALSE) %>%
    mutate(date = as.Date(date))
  age_stratified <- read.csv(age_file, stringsAsFactors = FALSE) %>%
    mutate(date = as.Date(date))
} else {
  # Test first week if starting fresh
  if (length(already_processed) == 0) {
    cat("=== Testing first week ===\n")
    test_result <- calculate_asmr_for_week(202152, data, esp2013)
    if (!is.null(test_result)) {
      cat("Test summary structure:\n")
      print(str(test_result$summary))
      cat("\nTest summary data:\n")
      print(test_result$summary)
      cat("\nDeath columns (5:8):\n")
      print(test_result$summary[,5:8])
      cat("\nSum of death columns:", sum(test_result$summary[,5:8]), "\n")
      cat("\nIndividual death counts:\n")
      cat("  deaths_vp:", test_result$summary$deaths_vp, "\n")
      cat("  deaths_vn:", test_result$summary$deaths_vn, "\n")
      cat("  deaths_sp:", test_result$summary$deaths_sp, "\n")
      cat("  deaths_sn:", test_result$summary$deaths_sn, "\n")
      cat("  TOTAL:", test_result$summary$deaths_vp + test_result$summary$deaths_vn + 
            test_result$summary$deaths_sp + test_result$summary$deaths_sn, "\n\n")
    } else {
      stop("Test failed")
    }
  }
  
  cat("=== Processing remaining weeks ===\n")
  batch_size <- 10
  n_batches <- ceiling(length(week_codes) / batch_size)
  
  for (batch_idx in 1:n_batches) {
    start_idx <- (batch_idx - 1) * batch_size + 1
    end_idx <- min(batch_idx * batch_size, length(week_codes))
    batch_weeks <- week_codes[start_idx:end_idx]
    
    cat(sprintf("Batch %d/%d: Weeks %s to %s... ", 
                batch_idx, n_batches, batch_weeks[1], batch_weeks[length(batch_weeks)]))
    
    # Parallel processing
    n_cores <- max(1, detectCores() - 1)
    
    if (.Platform$OS.type == "windows") {
      cl <- makeCluster(n_cores)
      clusterExport(cl, c("data", "esp2013", "calculate_asmr_for_week", 
                          "yw2date", "assign_age_group"), envir = environment())
      clusterEvalQ(cl, {
        library(dplyr); library(tibble); library(tidyr); library(lubridate); library(ISOweek)
      })
      batch_results <- parLapply(cl, batch_weeks, calculate_asmr_for_week,
                                 data = data, esp_ref = esp2013)
      stopCluster(cl)
    } else {
      batch_results <- mclapply(batch_weeks, calculate_asmr_for_week,
                                data = data, esp_ref = esp2013, mc.cores = n_cores)
    }
    
    # Extract and save
    batch_summary <- bind_rows(lapply(batch_results, function(x) if (!is.null(x)) x$summary else NULL))
    batch_age <- bind_rows(lapply(batch_results, function(x) if (!is.null(x)) x$age_detail else NULL))
    
    if (file.exists(summary_file)) {
      write.table(batch_summary, summary_file, sep = ",", append = TRUE, 
                  row.names = FALSE, col.names = FALSE)
      write.table(batch_age, age_file, sep = ",", append = TRUE, 
                  row.names = FALSE, col.names = FALSE)
    } else {
      write.csv(batch_summary, summary_file, row.names = FALSE)
      write.csv(batch_age, age_file, row.names = FALSE)
    }
    
    cat(sprintf("✓ Deaths: %d\n", sum(batch_summary$deaths_vp + batch_summary$deaths_vn + 
                                        batch_summary$deaths_sp + batch_summary$deaths_sn)))
  }
  
  cat("\nProcessing complete! Loading results...\n")
  results <- read.csv(summary_file, stringsAsFactors = FALSE) %>%
    mutate(date = as.Date(date))
  age_stratified <- read.csv(age_file, stringsAsFactors = FALSE) %>%
    mutate(date = as.Date(date))
}

# ============================================================================
# Save Final Results
# ============================================================================
results <- results %>% filter(!is.na(week)) %>% arrange(date)
results_nonzero <- results %>% filter(asmr_total > 0)

write.csv(results, "data/asmr_esp2013_results_all.csv", row.names = FALSE)
write.csv(results_nonzero, "data/asmr_esp2013_results.csv", row.names = FALSE)
write.csv(age_stratified, "data/asmr_esp2013_age_stratified.csv", row.names = FALSE)

age_stratified_nonzero <- age_stratified %>% filter(week %in% results_nonzero$week)
write.csv(age_stratified_nonzero, "data/asmr_esp2013_age_stratified_nonzero.csv", row.names = FALSE)

cat("\nFiles created:\n")
cat("  - asmr_esp2013_results.csv (", nrow(results_nonzero), "weeks)\n")
cat("  - asmr_esp2013_age_stratified.csv (", nrow(age_stratified), "rows)\n")

# ============================================================================
# Create Plots
# ============================================================================
w <- 800; h <- 600
plot_data <- results_nonzero

png("CzMort_ASMR_ESP2013.png", width = w, height = h)
par(mar = c(5, 5, 4, 2), cex = 1.2)
ymax <- max(plot_data$asmr_total, plot_data$asmr_vaccinated, plot_data$asmr_unvaccinated, na.rm = TRUE)
plot(plot_data$date, plot_data$asmr_total, ylim = c(0, ymax), type = "l", col = "black", lwd = 2,
     xlab = "Date", ylab = "ASMR per 100,000/week (ESP 2013)",
     main = "COVID-19 Mortality in Czechia (ESP 2013)")
lines(plot_data$date, plot_data$asmr_vaccinated, col = "red", lwd = 2)
lines(plot_data$date, plot_data$asmr_unvaccinated, col = "green", lwd = 2)
legend("topright", c("Overall", "Vaccinated", "Unvaccinated"),
       col = c("black", "red", "green"), lty = 1, lwd = 2, bg = "white")
dev.off()

png("CzMortUnvax_ASMR_ESP2013.png", width = w, height = h)
par(mar = c(5, 5, 4, 2), cex = 1.2)
ymax <- max(plot_data$asmr_unvaccinated, plot_data$asmr_sp, plot_data$asmr_sn, na.rm = TRUE)
plot(plot_data$date, plot_data$asmr_unvaccinated, ylim = c(0, ymax), type = "l", col = "black", lwd = 2,
     xlab = "Date", ylab = "ASMR per 100,000/week (ESP 2013)",
     main = "Unvaccinated Mortality in Czechia")
lines(plot_data$date, plot_data$asmr_sp, col = "red", lwd = 2)
lines(plot_data$date, plot_data$asmr_sn, col = "green", lwd = 2)
legend("topright", c("Unvaccinated overall", "Unvaccinated COVID+", "Unvaccinated COVID-"),
       col = c("black", "red", "green"), lty = 1, lwd = 2, bg = "white")
dev.off()

png("CzMortVax_ASMR_ESP2013.png", width = w, height = h)
par(mar = c(5, 5, 4, 2), cex = 1.2)
ymax <- max(plot_data$asmr_vaccinated, plot_data$asmr_vp, plot_data$asmr_vn, na.rm = TRUE)
plot(plot_data$date, plot_data$asmr_vaccinated, ylim = c(0, ymax), type = "l", col = "black", lwd = 2,
     xlab = "Date", ylab = "ASMR per 100,000/week (ESP 2013)",
     main = "Vaccinated Mortality in Czechia")
lines(plot_data$date, plot_data$asmr_vp, col = "red", lwd = 2)
lines(plot_data$date, plot_data$asmr_vn, col = "green", lwd = 2)
legend("topright", c("Vaccinated overall", "Vaccinated COVID+", "Vaccinated COVID-"),
       col = c("black", "red", "green"), lty = 1, lwd = 2, bg = "white")
dev.off()

png("CzMortPos_ASMR_ESP2013.png", width = w, height = h)
par(mar = c(5, 5, 4, 2), cex = 1.2)
ymax <- max(plot_data$asmr_positive, plot_data$asmr_vp, plot_data$asmr_sp, na.rm = TRUE)
plot(plot_data$date, plot_data$asmr_positive, ylim = c(0, ymax), type = "l", col = "black", lwd = 2,
     xlab = "Date", ylab = "ASMR per 100,000/week (ESP 2013)",
     main = "COVID+ Mortality in Czechia")
lines(plot_data$date, plot_data$asmr_vp, col = "red", lwd = 2)
lines(plot_data$date, plot_data$asmr_sp, col = "green", lwd = 2)
legend("topright", c("COVID+ overall", "Vaccinated COVID+", "Unvaccinated COVID+"),
       col = c("black", "red", "green"), lty = 1, lwd = 2, bg = "white")
dev.off()

png("CzMortNeg_ASMR_ESP2013.png", width = w, height = h)
par(mar = c(5, 5, 4, 2), cex = 1.2)
ymax <- max(plot_data$asmr_negative, plot_data$asmr_vn, plot_data$asmr_sn, na.rm = TRUE)
plot(plot_data$date, plot_data$asmr_negative, ylim = c(0, ymax), type = "l", col = "black", lwd = 2,
     xlab = "Date", ylab = "ASMR per 100,000/week (ESP 2013)",
     main = "COVID- Mortality in Czechia")
lines(plot_data$date, plot_data$asmr_vn, col = "red", lwd = 2)
lines(plot_data$date, plot_data$asmr_sn, col = "green", lwd = 2)
legend("topright", c("COVID- overall", "Vaccinated COVID-", "Unvaccinated COVID-"),
       col = c("black", "red", "green"), lty = 1, lwd = 2, bg = "white")
dev.off()

cat("\nPlots created (5 PNG files)\n")

# ============================================================================
# Additional ggplot: ASMR 15+ only
# ============================================================================
library(ggplot2)
library(scales)

cat("\n=== Creating ggplot for ages 15+ ===\n")

# Recalculate ASMR excluding under-15s
age_15plus <- age_stratified_nonzero %>%
  filter(age_start >= 15)

cat("Age groups included (15+):", length(unique(age_15plus$age_group)), "\n")

# Recalculate weekly ASMR for 15+ only
asmr_15plus <- age_15plus %>%
  group_by(week, date) %>%
  summarise(
    # Vaccinated
    pop_v = sum(n_pop[is_vaccinated == TRUE]),
    deaths_v = sum(n_deaths[is_vaccinated == TRUE]),
    asmr_v = sum(mortality_rate[is_vaccinated == TRUE] * esp_pop[is_vaccinated == TRUE]) / 
      sum(esp_pop[is_vaccinated == TRUE]),
    
    # Unvaccinated
    pop_s = sum(n_pop[is_vaccinated == FALSE]),
    deaths_s = sum(n_deaths[is_vaccinated == FALSE]),
    asmr_s = sum(mortality_rate[is_vaccinated == FALSE] * esp_pop[is_vaccinated == FALSE]) / 
      sum(esp_pop[is_vaccinated == FALSE]),
    
    # Overall
    pop_total = sum(n_pop),
    deaths_total = sum(n_deaths),
    asmr_total = sum(mortality_rate * esp_pop) / sum(esp_pop),
    
    .groups = "drop"
  ) %>%
  filter(asmr_total > 0)

cat("Weeks with data (15+ only):", nrow(asmr_15plus), "\n")
cat("Total deaths (15+ only):", sum(asmr_15plus$deaths_total), "\n")

# Reshape for ggplot
asmr_15plus_long <- asmr_15plus %>%
  select(date, asmr_total, asmr_v, asmr_s) %>%
  pivot_longer(cols = starts_with("asmr_"), 
               names_to = "group", 
               values_to = "asmr") %>%
  mutate(group = factor(group,
                        levels = c("asmr_total", "asmr_v", "asmr_s"),
                        labels = c("Overall (15+)", "Vaccinated (15+)", "Unvaccinated (15+)")))

# Create ggplot
p <- ggplot(asmr_15plus_long, aes(x = date, y = asmr, color = group)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("Overall (15+)" = "black", 
                                "Vaccinated (15+)" = "#D55E00", 
                                "Unvaccinated (15+)" = "#009E73")) +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  labs(
    title = "COVID-19 Mortality in Czechia - Ages 15+ Only",
    subtitle = "Age-Standardized Mortality Rate (ESP 2013)",
    x = "Date",
    y = "ASMR per 100,000 per week",
    color = "Group",
    caption = paste0("Data: Czech Ministry of Health | Analysis excludes ages 0-14\n",
                     "Total deaths (15+): ", format(sum(asmr_15plus$deaths_total), big.mark = ","))
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

ggsave("CzMort_ASMR_ESP2013_15plus_ggplot.png", p, 
       width = 12, height = 7, dpi = 300, bg = "white")

cat("ggplot saved: CzMort_ASMR_ESP2013_15plus_ggplot.png\n")

# Also save the 15+ data
write.csv(asmr_15plus, "data/asmr_esp2013_15plus.csv", row.names = FALSE)
cat("Data saved: data/asmr_esp2013_15plus.csv\n")

# ============================================================================
# Summary Statistics
# ============================================================================
cat("\n=== Summary Statistics ===\n")
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