#!/usr/bin/env Rscript
library(tidyverse)
library(lubridate)

# --------------------------------------------------
# INPUTS
# --------------------------------------------------
mzcr_imputed_data <- list()
population_stats <- list(
  population = list(),
  vaccinated = list(),
  deaths = list()
)
mzcr_origin_file <- "data/mzcr_no_or_first_infection_with_imputation.csv"

# =============================================================================
# ESP 2013
# =============================================================================
esp_age_start <- c(0, 1, 5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85, 90)
esp_age_end <- c(0, 4, 9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89, 999)
esp_pop <- c(
  1000,  # 0
  4000,  # 1-4
  5500,  # 5-9
  5500,  # 10-14
  5500,  # 15-19
  6000,  # 20-24
  6000,  # 25-29
  6500,  # 30-34
  7000,  # 35-39
  7000,  # 40-44
  7000,  # 45-49
  7000,  # 50-54
  6500,  # 55-59
  6000,  # 60-64
  5500,  # 65-69
  5000,  # 70-74
  4000,  # 75-79
  2500,  # 80-84
  1500,  # 85-89
  1000   # 90+
)

esp_total <- sum(esp_pop)
cat("ESP 2013 total population:", esp_total, "\n")

# Build helper structure
esp <- data.frame(
  idx = seq_along(esp_pop),
  age_start = esp_age_start,
  age_end = esp_age_end,
  esp_pop = esp_pop,
  stringsAsFactors = FALSE
) %>%
  mutate(
    age_group = ifelse(age_end == 999, 
                      paste0(age_start, "-999"),
                      paste0(age_start, "-", age_end)),
    age_mid = ifelse(age_end == 999,
                    age_start + 2.5,
                    (age_start + age_end) / 2.0)
  )

# Labels for per-age outputs
age_labels <- esp %>%
  mutate(label = ifelse(age_end == 999, "90p", paste0(age_start, "_", age_end))) %>%
  pull(label)

# Helper functions
ymd_to_iso_year_week <- function(ymd) {
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", ymd)) {
    stop(paste("Bad date:", ymd))
  }
  
  date_obj <- as.Date(ymd)
  iso_week <- as.integer(format(date_obj, "%V"))
  iso_wday <- as.integer(format(date_obj, "%u"))
  thu <- date_obj + (4 - iso_wday)
  iso_year <- as.integer(format(thu, "%Y"))
  
  return(c(iso_year, iso_week))
}

iso_week_monday_date <- function(iso_week_str) {
  if (!grepl("^\\s*(\\d{4})[- ]?W?(\\d{1,2})\\s*$", iso_week_str)) {
    stop("Expect ISO 'YYYY-WW' like 2015-25 or 2015-W25")
  }
  
  matches <- regmatches(iso_week_str, regexec("^\\s*(\\d{4})[- ]?W?(\\d{1,2})\\s*$", iso_week_str))[[1]]
  year <- as.integer(matches[2])
  week <- as.integer(matches[3])
  
  weeks_in_year <- iso_weeks_from_year(year)
  if (week < 1 || week > weeks_in_year) {
    stop(paste("Year", year, "has only", weeks_in_year, "ISO weeks"))
  }
  
  jan4 <- as.Date(paste0(year, "-01-04"))
  iso_dow <- as.integer(format(jan4, "%u"))
  week1_monday <- jan4 - (iso_dow - 1)
  monday <- week1_monday + (week - 1) * 7
  
  return(format(monday, "%Y-%m-%d"))
}

iso_weeks_from_year <- function(year) {
  dec28 <- as.Date(paste0(year, "-12-28"))
  return(as.integer(format(dec28, "%V")))
}

next_week <- function(y, w) {
  wiy <- iso_weeks_from_year(y)
  if (w < wiy) {
    return(c(y, w + 1))
  } else {
    return(c(y + 1, 1))
  }
}

# Load MZCR data
load_mzcr <- function() {
  total_rows <- 12125969
  cpt <- 0
  cur <- 0
  
  # Read CSV with appropriate encoding and separator detection
  first_line <- readLines(mzcr_origin_file, n = 1, encoding = "UTF-8")
  first_line <- gsub("^\xef\xbb\xbf", "", first_line)  # Remove BOM
  
  sep_char <- ","
  if (grepl(";", first_line) && !grepl(",", first_line)) {
    sep_char <- ";"
  } else if (grepl("\t", first_line) && !grepl(",|;", first_line)) {
    sep_char <- "\t"
  }
  
  data <- read.csv(mzcr_origin_file, 
                   sep = sep_char,
                   stringsAsFactors = FALSE,
                   encoding = "UTF-8")
  
  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    
    id <- row$id
    week_date_of_death <- row$week_date_of_death
    age_group <- row$age_group
    year_of_birth_end <- row$year_of_birth_end
    sex <- row$sex
    age_at_death <- row$age_at_death
    alive_on_jan_1st_2024 <- 1
    comp_death <- NULL
    
    if (!is.na(week_date_of_death) && week_date_of_death != "") {
      comp_death <- as.integer(gsub("\\D", "", week_date_of_death))
      if (comp_death <= 20240101) {
        alive_on_jan_1st_2024 <- 0
      } else {
        comp_death <- NULL
      }
    }
    
    cur <- cur + 1
    cpt <- cpt + 1
    if (cpt == 1000) {
      cpt <- 0
      cat("\rParsing MZCR - [", cur, "/", total_rows, "]", sep = "")
    }
    
    comp_dose <- NULL
    vaccinated_on_death <- 0
    date_first_dose <- row$Date_First_Dose
    
    if (!is.na(date_first_dose) && date_first_dose != "") {
      parts <- strsplit(date_first_dose, "-")[[1]]
      vax_year <- as.integer(parts[1])
      vax_week <- as.integer(parts[2])
      
      if (vax_year < 2024) {
        comp_dose <- as.integer(gsub("\\D", "", iso_week_monday_date(date_first_dose)))
        
        if (!is.null(comp_death)) {
          if (comp_death < comp_dose) {
            stop("Death before vaccination")
          }
          vaccinated_on_death <- 1
        }
        
        # Track vaccinations per week
        if (!vax_year %in% names(population_stats$vaccinated)) {
          population_stats$vaccinated[[as.character(vax_year)]] <<- list()
        }
        if (!vax_week %in% names(population_stats$vaccinated[[as.character(vax_year)]])) {
          population_stats$vaccinated[[as.character(vax_year)]][[as.character(vax_week)]] <<- list()
        }
        if (!age_group %in% names(population_stats$vaccinated[[as.character(vax_year)]][[as.character(vax_week)]])) {
          population_stats$vaccinated[[as.character(vax_year)]][[as.character(vax_week)]][[age_group]] <<- 0
        }
        population_stats$vaccinated[[as.character(vax_year)]][[as.character(vax_week)]][[age_group]] <<- 
          population_stats$vaccinated[[as.character(vax_year)]][[as.character(vax_week)]][[age_group]] + 1
      }
    }
    
    if (alive_on_jan_1st_2024) {
      if (!age_group %in% names(population_stats$population)) {
        population_stats$population[[age_group]] <<- list(total_population = 0, total_deaths = 0)
      }
      population_stats$population[[age_group]]$total_population <<- 
        population_stats$population[[age_group]]$total_population + 1
    } else {
      death_info <- ymd_to_iso_year_week(week_date_of_death)
      death_year <- death_info[1]
      death_week <- death_info[2]
      
      # Track deaths
      if (!death_year %in% names(population_stats$deaths)) {
        population_stats$deaths[[as.character(death_year)]] <<- list()
      }
      if (!death_week %in% names(population_stats$deaths[[as.character(death_year)]])) {
        population_stats$deaths[[as.character(death_year)]][[as.character(death_week)]] <<- list()
      }
      if (!age_group %in% names(population_stats$deaths[[as.character(death_year)]][[as.character(death_week)]])) {
        population_stats$deaths[[as.character(death_year)]][[as.character(death_week)]][[age_group]] <<- list()
      }
      if (!as.character(vaccinated_on_death) %in% names(population_stats$deaths[[as.character(death_year)]][[as.character(death_week)]][[age_group]])) {
        population_stats$deaths[[as.character(death_year)]][[as.character(death_week)]][[age_group]][[as.character(vaccinated_on_death)]] <<- 0
      }
      
      population_stats$deaths[[as.character(death_year)]][[as.character(death_week)]][[age_group]][[as.character(vaccinated_on_death)]] <<- 
        population_stats$deaths[[as.character(death_year)]][[as.character(death_week)]][[age_group]][[as.character(vaccinated_on_death)]] + 1
      
      if (!age_group %in% names(population_stats$population)) {
        population_stats$population[[age_group]] <<- list(total_population = 0, total_deaths = 0)
      }
      population_stats$population[[age_group]]$total_deaths <<- 
        population_stats$population[[age_group]]$total_deaths + 1
    }
    
    # if (cur >= 20000) break  # Uncomment for testing
  }
  
  cat("\rParsing MZCR - [", cur, "/", total_rows, "]\n", sep = "")
}

# Population at baseline
population_at_baseline <- function() {
  population_baseline <- list()
  
  for (age_group in sort(names(population_stats$population))) {
    total_deaths <- population_stats$population[[age_group]]$total_deaths
    total_population <- population_stats$population[[age_group]]$total_population
    
    population_baseline[[age_group]] <- list(
      total_unvaccinated = total_deaths + total_population,
      total_vaccinated = 0,
      deaths = list(
        unvaccinated = 0,
        vaccinated = 0
      )
    )
  }
  
  return(population_baseline)
}

# Main execution
load_mzcr()
population_baseline <- population_at_baseline()

# Find min/max year and week
min_year <- NULL
min_week <- NULL
max_year <- NULL
max_week <- NULL

for (y in names(population_stats$vaccinated)) {
  for (w in names(population_stats$vaccinated[[y]])) {
    y_num <- as.integer(y)
    w_num <- as.integer(w)
    if (is.null(min_year) || y_num < min_year || (y_num == min_year && w_num < min_week)) {
      min_year <- y_num
      min_week <- w_num
    }
    if (is.null(max_year) || y_num > max_year || (y_num == max_year && w_num > max_week)) {
      max_year <- y_num
      max_week <- w_num
    }
  }
}

for (y in names(population_stats$deaths)) {
  for (w in names(population_stats$deaths[[y]])) {
    y_num <- as.integer(y)
    w_num <- as.integer(w)
    if (is.null(min_year) || y_num < min_year || (y_num == min_year && w_num < min_week)) {
      min_year <- y_num
      min_week <- w_num
    }
    if (is.null(max_year) || y_num > max_year || (y_num == max_year && w_num > max_week)) {
      max_year <- y_num
      max_week <- w_num
    }
  }
}



# -------------------------------------------------------------------
# 3) Eurostat annual population (demo_pjan) — single years -> 5y bands
# -------------------------------------------------------------------
pop_file <- "data/demo_pjan_linear_2_0.csv"
pop_url  <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/demo_pjan/1.0?compress=false&format=csvdata&formatVersion=2.0&lang=en&labels=name"

if (!file.exists(pop_file)) {
  options(timeout = 1200)
  download.file(pop_url, pop_file, mode = "wb")
  message("Eurostat yearly population by single ages file downloaded.")
} else {
  message("Eurostat yearly population by single ages file found locally.")
}

pop_raw <- read.csv(pop_file, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

pop2 <- pop_raw %>%
  rename(time = `TIME_PERIOD`, value = `OBS_VALUE`) %>%
  mutate(geo_code = sub(":.*$", "", geo)) %>%
  filter(geo_code == "CZ") %>%
  mutate(value = suppressWarnings(as.numeric(value))) %>%
  filter(!is.na(value), age != "TOTAL") %>%
  mutate(
    year = as.integer(time),
    age  = suppressWarnings(as.integer(sub("^Y", "", age)))
  ) %>%
  filter(!is.na(age), age >= 15) %>%
  rename(pop = value)

pop_bands <- pop2 %>%
  mutate(
    age_start = pmin(floor(age / 5) * 5, 95L),
    age_end   = if_else(age_start < 95L, age_start + 4L, 999L)
  ) %>%
  group_by(year, age_start, age_end) %>%
  summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")

# -------------------------------------------------------------------
# 4) ESP2013 — match FIRST script (90+ OPEN) and 15+
#     (first script used 85–89 and 90+, with 90+ weight = 1000)
# -------------------------------------------------------------------
esp2013_90p <- tibble::tibble(
  age_start = c(15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90),
  age_end   = c(19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,999),
  esp_pop   = c(5500,6000,6000,6500,7000,7000,7000,7000,6500,6000,5500,5000,4000,2500,1500,1000)
)
esp_total_90p <- sum(esp2013_90p$esp_pop)

weeks_per_year <- 52.1775  # consistent with Eurostat-method script

# -------------------------------------------------------------------
# 5) Aggregate Eurostat to 90+ OPEN, compute weekly age-specific rates
# -------------------------------------------------------------------
# (a) Collapse Eurostat deaths into 90+ open
eu_w_90p <- eu_w %>%
  mutate(
    age_start_90p = if_else(age_start >= 90, 90L, age_start),
    age_end_90p   = if_else(age_start >= 90, 999L, age_end)
  ) %>%
  group_by(year, week, iso_date, age_start = age_start_90p, age_end = age_end_90p) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE), .groups = "drop")

# (b) Collapse annual population into 90+ open (matching age bands)
pop_bands_90p <- pop_bands %>%
  mutate(
    age_start_90p = if_else(age_start >= 90, 90L, age_start),
    age_end_90p   = if_else(age_start >= 90, 999L, age_end)
  ) %>%
  group_by(year, age_start = age_start_90p, age_end = age_end_90p) %>%
  summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")

# (c) Weekly Eurostat age-specific rates (per 100k, annualised)
eu_rates_15p_90p <- eu_w_90p %>%
  left_join(pop_bands_90p, by = c("year", "age_start", "age_end")) %>%
  group_by(age_start, age_end) %>%
  tidyr::fill(pop, .direction = "downup") %>%
  ungroup() %>%
  mutate(
    person_weeks  = pop / weeks_per_year,
    rate_per_100k = if_else(person_weeks > 0, deaths / person_weeks * 1e5, NA_real_)
  ) %>%
  filter(!is.na(rate_per_100k)) %>%
  left_join(esp2013_90p, by = c("age_start", "age_end"))

asmr15_eu_90p <- eu_rates_15p_90p %>%
  group_by(year, week, iso_date) %>%
  summarise(
    asmr_15plus_eurostat_esp90p = sum(rate_per_100k * esp_pop) / esp_total_90p,
    .groups = "drop"
  ) %>%
  arrange(iso_date)

weeks_per_year <- 52.1775

# Build: person_weeks_lookup[[year]][["15-19"]] = pop / 52.1775, etc.
person_weeks_lookup <- pop_bands_90p %>%
  dplyr::mutate(
    age_group = ifelse(age_end == 999L,
                       sprintf("%d-999", age_start),
                       sprintf("%d-%d",  age_start, age_end)),
    person_weeks = pop / weeks_per_year
  ) %>%
  dplyr::select(year, age_group, person_weeks) %>%
  split(.$year) %>%
  lapply(function(df) setNames(as.list(df$person_weeks), df$age_group))


# Process weekly data
out_file <- file("outputs/weekly_death_rates_deaths_with_imputation_eurostat_pop.csv", "w")
out_asmr_file <- file("outputs/weekly_asmr_deaths_with_imputation_eurostat_pop.csv", "w")

writeLines("year,week,age_group,esp_pop,total_unvaccinated,total_vaccinated,vaccinated_this_week,deaths_vaccinated,deaths_unvaccinated,rate_per_100000_unvaccinated,rate_per_100000_vaccinated,esp_rate_unvaccinated,esp_rate_vaccinated", out_file)
writeLines("year,week,asmr_unvaccinated,asmr_vaccinated,asmr_total", out_asmr_file)

year <- min_year
week <- min_week

while (!is.null(year) && (year < max_year || (year == max_year && week <= max_week))) {
  weekly_data <- list()
  
  for (i in seq_len(nrow(esp))) {
    age_group_data <- esp[i, ]
    age_group <- age_group_data$age_group
    from_age <- as.integer(strsplit(age_group, "-")[[1]][1])
    if (from_age < 15) next
    
    esp_pop <- age_group_data$esp_pop
    total_unvaccinated <- population_baseline[[age_group]]$total_unvaccinated
    total_vaccinated   <- population_baseline[[age_group]]$total_vaccinated
    
    # Vaccinations this week
    vaccinated_this_week <- 0
    if (as.character(year) %in% names(population_stats$vaccinated) &&
        as.character(week) %in% names(population_stats$vaccinated[[as.character(year)]]) &&
        age_group %in% names(population_stats$vaccinated[[as.character(year)]][[as.character(week)]])) {
      vaccinated_this_week <- population_stats$vaccinated[[as.character(year)]][[as.character(week)]][[age_group]]
    }
    
    # Adjust baseline (for vax/unvax series only)
    total_vaccinated   <- total_vaccinated + vaccinated_this_week
    total_unvaccinated <- total_unvaccinated - vaccinated_this_week
    
    # Deaths
    deaths_vaccinated <- 0
    deaths_unvaccinated <- 0
    if (as.character(year) %in% names(population_stats$deaths) &&
        as.character(week) %in% names(population_stats$deaths[[as.character(year)]]) &&
        age_group %in% names(population_stats$deaths[[as.character(year)]][[as.character(week)]])) {
      if ("1" %in% names(population_stats$deaths[[as.character(year)]][[as.character(week)]][[age_group]])) {
        deaths_vaccinated <- population_stats$deaths[[as.character(year)]][[as.character(week)]][[age_group]][["1"]]
      }
      if ("0" %in% names(population_stats$deaths[[as.character(year)]][[as.character(week)]][[age_group]])) {
        deaths_unvaccinated <- population_stats$deaths[[as.character(year)]][[as.character(week)]][[age_group]][["0"]]
      }
    }
    
    population_baseline[[age_group]]$deaths$unvaccinated <- 
      population_baseline[[age_group]]$deaths$unvaccinated + deaths_unvaccinated
    population_baseline[[age_group]]$deaths$vaccinated <- 
      population_baseline[[age_group]]$deaths$vaccinated + deaths_vaccinated
    
    # ---- Age-specific rates (unvax/vax) — keep as in your original logic ----
    rate_per_100000_unvaccinated <- 0
    rate_per_100000_vaccinated   <- 0
    esp_rate_unvaccinated <- 0
    esp_rate_vaccinated   <- 0
    
    if (total_unvaccinated > 0) {
      rate_per_100000_unvaccinated <- round(deaths_unvaccinated * 100000 * 52.18 / total_unvaccinated, 7)
      esp_rate_unvaccinated <- round(rate_per_100000_unvaccinated * esp_pop / 100000, 7)
    }
    if (total_vaccinated > 0) {
      rate_per_100000_vaccinated <- round(deaths_vaccinated * 100000 * 52.18 / total_vaccinated, 7)
      esp_rate_vaccinated <- round(rate_per_100000_vaccinated * esp_pop / 100000, 7)
    }
    
    # ---- Combined rate (Eurostat-comparable) ----
    # Use Eurostat person-weeks denominator for this year & age_group
    rate_per_100000_total <- 0
    esp_rate_total <- 0
    deaths_total <- deaths_unvaccinated + deaths_vaccinated
    
    pw <- NA_real_
    if (!is.null(person_weeks_lookup[[as.character(year)]])) {
      pw <- person_weeks_lookup[[as.character(year)]][[age_group]]
      if (is.null(pw)) pw <- NA_real_
    }
    
    if (!is.na(pw) && pw > 0) {
      # Note: D / (pop/52.1775) * 1e5  ==  D * 1e5 / person_weeks
      rate_per_100000_total <- round(deaths_total * 100000 / pw, 7)
      esp_rate_total <- round(rate_per_100000_total * esp_pop / 100000, 7)
    }
    
    weekly_data[[age_group]] <- list(
      esp_rate_unvaccinated = esp_rate_unvaccinated,
      esp_rate_vaccinated   = esp_rate_vaccinated,
      esp_rate_total        = esp_rate_total,   # <-- now Eurostat-denominator based
      esp_pop               = esp_pop
    )
    
    # Update cohort (for vax/unvax series only)
    total_vaccinated   <- total_vaccinated - deaths_vaccinated
    total_unvaccinated <- total_unvaccinated - deaths_unvaccinated
    population_baseline[[age_group]]$total_unvaccinated <- total_unvaccinated
    population_baseline[[age_group]]$total_vaccinated   <- total_vaccinated
    
    writeLines(paste(year, week, age_group, esp_pop, total_unvaccinated, total_vaccinated, 
                     vaccinated_this_week, deaths_vaccinated, deaths_unvaccinated,
                     rate_per_100000_unvaccinated, rate_per_100000_vaccinated, 
                     esp_rate_unvaccinated, esp_rate_vaccinated, sep = ","), 
               out_file)
  }
  
  # Weekly ASMR (ESP2013, 90+ open) — total now uses Eurostat denominators
  ags <- names(weekly_data)
  
  esp_num_unvax <- sum(sapply(ags, function(ag) weekly_data[[ag]]$esp_rate_unvaccinated))
  esp_num_vax   <- sum(sapply(ags, function(ag) weekly_data[[ag]]$esp_rate_vaccinated))
  esp_num_total <- sum(sapply(ags, function(ag) weekly_data[[ag]]$esp_rate_total))   # <-- Eurostat denom
  esp_den_scaled <- sum(sapply(ags, function(ag) weekly_data[[ag]]$esp_pop)) / 100000
  
  asmr_unvaccinated <- 0
  asmr_vaccinated   <- 0
  asmr_total        <- 0
  
  if (esp_den_scaled > 0) {
    asmr_unvaccinated <- round(esp_num_unvax / esp_den_scaled, 7)
    asmr_vaccinated   <- round(esp_num_vax   / esp_den_scaled, 7)
    asmr_total        <- round(esp_num_total / esp_den_scaled, 7)   # <-- Eurostat-comparable
  }
  
  writeLines(paste(year, week, asmr_unvaccinated, asmr_vaccinated, asmr_total, sep = ","), 
             out_asmr_file)
  
  next_vals <- next_week(year, week)
  year <- next_vals[1]
  week <- next_vals[2]
}

close(out_file)
close(out_asmr_file)

# Print final population baseline
print(population_baseline)

cmp %>%
  mutate(ratio = asmr_15plus_eurostat_esp90p / asmr_15plus_first) %>%
  summarise(median_ratio = median(ratio, na.rm = TRUE),
            iqr = IQR(ratio, na.rm = TRUE))

den <- readr::read_csv("outputs/weekly_death_rates_deaths_with_imputation_eurostat_pop.csv", show_col_types = FALSE) %>%
  dplyr::mutate(
    age_start = as.integer(sub("-.*$", "", age_group)),
    age_end   = ifelse(grepl("999$", age_group), 999L, as.integer(sub("^.*-", "", age_group))),
    N         = total_unvaccinated + total_vaccinated
  ) %>%
  dplyr::filter(age_start >= 15)

# Collapse Eurostat population to 90+ open as in your compare script -> pop_bands_90p (year, age_start, age_end, pop)

coverage <- den %>%
  dplyr::group_by(year, week, age_start, age_end) %>%
  dplyr::summarise(N = dplyr::first(N), .groups = "drop") %>%   # N used in that week
  dplyr::left_join(pop_bands_90p, by = c("year", "age_start", "age_end")) %>%
  dplyr::mutate(coverage = N / pop)

coverage %>%
  dplyr::group_by(age_start, age_end) %>%
  dplyr::summarise(med_cov = median(coverage, na.rm = TRUE))

