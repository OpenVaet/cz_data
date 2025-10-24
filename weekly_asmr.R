#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

# ------------------------------------------------------------------
# INPUTS
# ------------------------------------------------------------------
source_file <- Sys.getenv(
  "MZCR_DL_FILE",
  unset = "data/mzcr_no_or_first_infection.csv" # or ..._normalized_doses.csv
)

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# In-memory stores
population_stats <- list(
  population = list(),  # age_group -> list(total_population, total_deaths)
  vaccinated = list(),  # year -> week -> age_group -> count
  deaths = list()       # year -> week -> age_group -> list("0"=unvax,"1"=vax)
)

# =============================================================================
# ESP 2013
# =============================================================================
esp_age_start <- c(0, 1, 5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85, 90)
esp_age_end   <- c(0, 4, 9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89, 999)
esp_pop <- c(1000,4000,5500,5500,5500,6000,6000,6500,7000,7000,7000,7000,6500,6000,5500,5000,4000,2500,1500,1000)
esp_total <- sum(esp_pop)
cat("ESP 2013 total population:", esp_total, "\n")

esp <- tibble(
  idx = seq_along(esp_pop),
  age_start = esp_age_start,
  age_end   = esp_age_end,
  esp_pop   = esp_pop
) %>%
  mutate(age_group = if_else(age_end==999, paste0(age_start,"-999"), paste0(age_start,"-",age_end)),
         age_mid   = if_else(age_end==999, age_start+2.5, (age_start+age_end)/2.0))

# ------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------
iso_weeks_from_year <- function(year) {
  dec28 <- as.Date(sprintf("%04d-12-28", as.integer(year)))
  as.integer(format(dec28, "%V"))
}

ymd_to_iso_year_week <- function(ymd) {
  stopifnot(grepl("^\\d{4}-\\d{2}-\\d{2}$", ymd))
  date_obj <- as.Date(ymd)
  thu <- date_obj + (4 - as.integer(format(date_obj, "%u")))
  c(as.integer(format(thu, "%Y")), as.integer(format(date_obj, "%V")))
}

iso_week_monday_date <- function(iso_week_str) {
  # Accept "YYYY-WW" or "YYYY-WW-<d>" or "YYYYWW"
  m <- regexec("^\\s*(\\d{4})[- ]?W?(\\d{1,2})(?:[- ]?[1-7])?\\s*$", iso_week_str)
  parts <- regmatches(iso_week_str, m)[[1]]
  if (length(parts) < 3) stop("Expect ISO 'YYYY-WW' like 2015-25 or 2015-W25")
  year <- as.integer(parts[2]); week <- as.integer(parts[3])
  wiy <- iso_weeks_from_year(year)
  if (week < 1 || week > wiy) stop(sprintf("Year %d has only %d ISO weeks", year, wiy))
  jan4 <- as.Date(sprintf("%04d-01-04", year))
  week1_monday <- jan4 - (as.integer(format(jan4, "%u")) - 1L)
  format(week1_monday + (week - 1L) * 7L, "%Y-%m-%d")
}

next_week <- function(y, w) {
  wiy <- iso_weeks_from_year(y)
  if (w < wiy) c(y, w + 1L) else c(y + 1L, 1L)
}

# Age -> ESP age_group label
make_age_group <- function(age) {
  if (is.na(age)) return(NA_character_)
  for (i in seq_along(esp_age_start)) {
    if (age >= esp_age_start[i] && age <= esp_age_end[i]) {
      return(if (esp_age_end[i]==999) sprintf("%d-999", esp_age_start[i])
             else sprintf("%d-%d", esp_age_start[i], esp_age_end[i]))
    }
  }
  NA_character_
}

# ------------------------------------------------------------------
# Load dataset
# Differences from imputed file:
#  - Must exclude unknown year_of_birth_end
#  - Date_First_Dose often in ISO-week scalar "YYYY-WW(-d)" form
# ------------------------------------------------------------------
first_line <- readLines(source_file, n = 1, encoding = "UTF-8")
first_line <- gsub("^\ufeff", "", first_line)  # drop BOM if present
sep_char <- if (grepl(";", first_line) && !grepl(",", first_line)) ";" else
            if (grepl("\t", first_line) && !grepl(",|;", first_line)) "\t" else ","

dat <- read.csv(
  source_file,
  sep = sep_char,
  stringsAsFactors = FALSE,
  encoding = "UTF-8",
  check.names = TRUE   # ensures unique names (adds .1, .2 if needed)
)

# Helper: find a column by regex (case-insensitive), return its *current* name
find_col <- function(df, pattern) {
  hits <- grep(pattern, names(df), ignore.case = TRUE, value = TRUE)
  if (length(hits) == 0) NA_character_ else hits[1]
}

# --- Canonical columns we need (without creating name collisions) ---
# year_of_birth_end: use existing if present; else derive from YearOfBirth band
col_yob_end <- find_col(dat, "^year[_ ]?of[_ ]?birth[_ ]?end$")
if (is.na(col_yob_end)) {
  col_yob <- find_col(dat, "^year[_ ]?of[_ ]?birth$")
  if (is.na(col_yob)) stop("No year_of_birth_end or YearOfBirth column found.")
  dat$year_of_birth_end <- suppressWarnings(as.integer(sub(".*-(\\d{4})$", "\\1", dat[[col_yob]])))
} else {
  dat$year_of_birth_end <- suppressWarnings(as.integer(dat[[col_yob_end]]))
}

# week_date_of_death: copy into a canonical column
col_wdod <- find_col(dat, "^week[_ ]?date[_ ]?of[_ ]?death$")
if (is.na(col_wdod)) stop("week_date_of_death column missing.")
dat$week_date_of_death <- as.character(dat[[col_wdod]])

# Date_First_Dose (ISO week scalar like YYYY-WW[-d])
col_d1 <- find_col(dat, "^date[_ ]?[_-]?first[_ ]?dose$")
if (is.na(col_d1)) stop("Date_First_Dose column missing.")
dat$date_first_dose <- as.character(dat[[col_d1]])

# Optional sanity check
if (any(duplicated(names(dat)))) {
  message("Note: input still has duplicated columns (harmless), but canonical columns are unique.")
}

# --- Strictly drop unknown year_of_birth_end (your requirement) -----
dat <- dat %>% filter(!is.na(year_of_birth_end))

# ---- Ensure age_group exists (15+ ESP bin at 2024-01-01) ----
if (!"age_group" %in% names(dat)) {
  if (!"year_of_birth_end" %in% names(dat)) stop("year_of_birth_end missing.")
  make_age_group <- function(age) {
    esp_age_start <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90)
    esp_age_end   <- c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,999)
    for (i in seq_along(esp_age_start)) {
      if (!is.na(age) && age >= esp_age_start[i] && age <= esp_age_end[i]) {
        return(if (esp_age_end[i]==999) sprintf("%d-999", esp_age_start[i])
               else sprintf("%d-%d", esp_age_start[i], esp_age_end[i]))
      }
    }
    return(NA_character_)
  }
  dat <- dat %>%
    mutate(age_baseline_2024 = 2024L - as.integer(year_of_birth_end),
           age_group = vapply(age_baseline_2024, make_age_group, character(1), USE.NAMES = FALSE))
}

# Drop rows with missing/blank age_group (and keep your yob_end filter)
dat <- dat %>%
  filter(!is.na(year_of_birth_end)) %>%
  mutate(age_group = trimws(as.character(age_group))) %>%
  filter(!is.na(age_group), nzchar(age_group))

# ------------------------------------------------------------------
# Build population/vaccination/death tallies (row-wise, like your script)
# ------------------------------------------------------------------
total_rows <- nrow(dat)
cpt <- 0; cur <- 0

for (i in seq_len(total_rows)) {
  row <- dat[i, ]

  # Always coerce to a single scalar string
  age_group <- as.character(row[["age_group"]])
  if (length(age_group) == 0L || is.na(age_group) || !nzchar(age_group)) next
  age_group <- age_group[1]

  week_date_of_death <- as.character(row[["week_date_of_death"]])
  date_first_dose    <- as.character(row[["date_first_dose"]])


  # Baseline alive on 2024-01-01?
  alive_on_jan_1st_2024 <- 1L
  comp_death <- NULL
  if (!is.na(week_date_of_death) && nzchar(week_date_of_death)) {
    # Expect "YYYY-MM-DD" (your pipeline writes Dates as ISO)
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", week_date_of_death)) {
      stop(paste("Bad week_date_of_death format:", week_date_of_death))
    }
    comp_death <- as.integer(gsub("\\D", "", week_date_of_death))
    if (comp_death <= 20240101L) {
      alive_on_jan_1st_2024 <- 0L
    } else {
      comp_death <- NULL  # only keep comp_death if <= baseline for later checking
    }
  }

  # Track vaccinations (use first dose ISO week before 2024)
  vaccinated_on_death <- 0L
  if (!is.na(date_first_dose) && nzchar(date_first_dose)) {
    # Accept "YYYY-WW", "YYYY-WW-d", "YYYYWW"
    m <- regexec("^\\s*(\\d{4})[- ]?W?(\\d{1,2}).*$", date_first_dose)
    parts <- regmatches(date_first_dose, m)[[1]]
    if (length(parts) >= 3) {
      vax_year <- as.integer(parts[2]); vax_week <- as.integer(parts[3])
      if (vax_year < 2024L) {
        comp_dose <- as.integer(gsub("\\D", "", iso_week_monday_date(sprintf("%04d-%02d", vax_year, vax_week))))
        if (!is.null(comp_death)) {
          if (comp_death < comp_dose) stop("Death before vaccination")
          vaccinated_on_death <- 1L
        }
        # Vaccinated per year/week/age_group
        yk <- as.character(vax_year); wk <- as.character(vax_week)
        if (is.null(population_stats$vaccinated[[yk]])) population_stats$vaccinated[[yk]] <- list()
        if (is.null(population_stats$vaccinated[[yk]][[wk]])) population_stats$vaccinated[[yk]][[wk]] <- list()
        if (is.null(population_stats$vaccinated[[yk]][[wk]][[age_group]])) population_stats$vaccinated[[yk]][[wk]][[age_group]] <- 0L
        population_stats$vaccinated[[yk]][[wk]][[age_group]] <- population_stats$vaccinated[[yk]][[wk]][[age_group]] + 1L
      }
    }
  }

  # Population (alive at baseline) and deaths (before baseline) by age_group
  if (alive_on_jan_1st_2024 == 1L) {
    if (!age_group %in% names(population_stats$population)) population_stats$population[[age_group]] <- list(total_population = 0L, total_deaths = 0L)
    population_stats$population[[age_group]]$total_population <- population_stats$population[[age_group]]$total_population + 1L
  } else {
    # Assign death week/year for any death (<= 2024-01-01 decision already made)
    ywk <- ymd_to_iso_year_week(week_date_of_death)
    death_year <- as.character(ywk[1]); death_week <- as.character(ywk[2])
    if (!death_year %in% names(population_stats$deaths)) population_stats$deaths[[death_year]] <- list()
    if (!death_week %in% names(population_stats$deaths[[death_year]])) population_stats$deaths[[death_year]][[death_week]] <- list()
    if (!age_group %in% names(population_stats$deaths[[death_year]][[death_week]])) population_stats$deaths[[death_year]][[death_week]][[age_group]] <- list()
    key <- as.character(vaccinated_on_death)
    if (!key %in% names(population_stats$deaths[[death_year]][[death_week]][[age_group]]))
      population_stats$deaths[[death_year]][[death_week]][[age_group]][[key]] <- 0L
    population_stats$deaths[[death_year]][[death_week]][[age_group]][[key]] <-
      population_stats$deaths[[death_year]][[death_week]][[age_group]][[key]] + 1L

    if (!age_group %in% names(population_stats$population)) population_stats$population[[age_group]] <- list(total_population = 0L, total_deaths = 0L)
    population_stats$population[[age_group]]$total_deaths <- population_stats$population[[age_group]]$total_deaths + 1L
  }

  cur <- cur + 1; cpt <- cpt + 1
  if (cpt == 1000) { cpt <- 0; cat("\rParsing MZCR (download) - [", cur, "/", total_rows, "]", sep = "") }
}
cat("\rParsing MZCR (download) - [", cur, "/", total_rows, "]\n", sep = "")

# ------------------------------------------------------------------
# Baseline population
# ------------------------------------------------------------------
population_at_baseline <- function() {
  population_baseline <- list()
  for (age_group in sort(names(population_stats$population))) {
    total_deaths <- population_stats$population[[age_group]]$total_deaths
    total_population <- population_stats$population[[age_group]]$total_population
    population_baseline[[age_group]] <- list(
      total_unvaccinated = total_deaths + total_population,
      total_vaccinated = 0L,
      deaths = list(unvaccinated = 0L, vaccinated = 0L)
    )
  }
  population_baseline
}
population_baseline <- population_at_baseline()

# ------------------------------------------------------------------
# Determine min/max week across vax + deaths
# ------------------------------------------------------------------
min_year <- NULL; min_week <- NULL; max_year <- NULL; max_week <- NULL

for (y in names(population_stats$vaccinated)) {
  for (w in names(population_stats$vaccinated[[y]])) {
    y_num <- as.integer(y); w_num <- as.integer(w)
    if (is.null(min_year) || y_num < min_year || (y_num == min_year && w_num < min_week)) { min_year <- y_num; min_week <- w_num }
    if (is.null(max_year) || y_num > max_year || (y_num == max_year && w_num > max_week)) { max_year <- y_num; max_week <- w_num }
  }
}
for (y in names(population_stats$deaths)) {
  for (w in names(population_stats$deaths[[y]])) {
    y_num <- as.integer(y); w_num <- as.integer(w)
    if (is.null(min_year) || y_num < min_year || (y_num == min_year && w_num < min_week)) { min_year <- y_num; min_week <- w_num }
    if (is.null(max_year) || y_num > max_year || (y_num == max_year && w_num > max_week)) { max_year <- y_num; max_week <- w_num }
  }
}

# ------------------------------------------------------------------
# Weekly processing + ASMR (15+ only)
# ------------------------------------------------------------------
out_file <- file("outputs/weekly_death_rates_from_download.csv", "w")
out_asmr_file <- file("outputs/weekly_asmr_from_download.csv", "w")
writeLines("year,week,age_group,esp_pop,total_unvaccinated,total_vaccinated,vaccinated_this_week,deaths_vaccinated,deaths_unvaccinated,rate_per_100000_unvaccinated,rate_per_100000_vaccinated,esp_rate_unvaccinated,esp_rate_vaccinated", out_file)
writeLines("year,week,asmr_unvaccinated,asmr_vaccinated,asmr_total", out_asmr_file)

year <- min_year; week <- min_week
if (is.null(year)) stop("No vaccination or death weeks found to iterate.")

while (!is.null(year) && (year < max_year || (year == max_year && week <= max_week))) {
  weekly_data <- list()

  for (i in seq_len(nrow(esp))) {
    age_group_data <- esp[i, ]
    age_group <- age_group_data$age_group
    from_age <- as.integer(strsplit(age_group, "-")[[1]][1])
    if (from_age < 15) next

    esp_pop <- age_group_data$esp_pop
    if (!age_group %in% names(population_baseline)) next  # skip groups absent in baseline

    total_unvaccinated <- population_baseline[[age_group]]$total_unvaccinated
    total_vaccinated   <- population_baseline[[age_group]]$total_vaccinated

    # Vaccinations this ISO week
    vaccinated_this_week <- 0L
    yk <- as.character(year); wk <- as.character(week)
    if (yk %in% names(population_stats$vaccinated) &&
        wk %in% names(population_stats$vaccinated[[yk]]) &&
        age_group %in% names(population_stats$vaccinated[[yk]][[wk]])) {
      vaccinated_this_week <- population_stats$vaccinated[[yk]][[wk]][[age_group]]
    }

    # Move people between cohorts
    total_vaccinated   <- total_vaccinated + vaccinated_this_week
    total_unvaccinated <- total_unvaccinated - vaccinated_this_week

    # Deaths this week by status
    deaths_vaccinated <- 0L; deaths_unvaccinated <- 0L
    if (yk %in% names(population_stats$deaths) &&
        wk %in% names(population_stats$deaths[[yk]]) &&
        age_group %in% names(population_stats$deaths[[yk]][[wk]])) {
      if ("1" %in% names(population_stats$deaths[[yk]][[wk]][[age_group]])) {
        deaths_vaccinated <- population_stats$deaths[[yk]][[wk]][[age_group]][["1"]]
      }
      if ("0" %in% names(population_stats$deaths[[yk]][[wk]][[age_group]])) {
        deaths_unvaccinated <- population_stats$deaths[[yk]][[wk]][[age_group]][["0"]]
      }
    }

    population_baseline[[age_group]]$deaths$unvaccinated <- population_baseline[[age_group]]$deaths$unvaccinated + deaths_unvaccinated
    population_baseline[[age_group]]$deaths$vaccinated   <- population_baseline[[age_group]]$deaths$vaccinated + deaths_vaccinated

    # Rates per 100k (annualized with 52.18)
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

    # Combined (optional)
    den_total <- total_unvaccinated + total_vaccinated
    esp_rate_total <- 0
    if (den_total > 0) {
      deaths_total <- deaths_unvaccinated + deaths_vaccinated
      rate_per_100000_total <- round(deaths_total * 100000 * 52.18 / den_total, 7)
      esp_rate_total <- round(rate_per_100000_total * esp_pop / 100000, 7)
    }

    weekly_data[[age_group]] <- list(
      esp_rate_unvaccinated = esp_rate_unvaccinated,
      esp_rate_vaccinated   = esp_rate_vaccinated,
      esp_rate_total        = esp_rate_total,
      esp_pop               = esp_pop
    )

    # Update stocks after deaths
    total_vaccinated   <- total_vaccinated - deaths_vaccinated
    total_unvaccinated <- total_unvaccinated - deaths_unvaccinated
    population_baseline[[age_group]]$total_unvaccinated <- total_unvaccinated
    population_baseline[[age_group]]$total_vaccinated   <- total_vaccinated

    writeLines(paste(year, week, age_group, esp_pop, total_unvaccinated, total_vaccinated,
                     vaccinated_this_week, deaths_vaccinated, deaths_unvaccinated,
                     rate_per_100000_unvaccinated, rate_per_100000_vaccinated,
                     weekly_data[[age_group]]$esp_rate_unvaccinated,
                     weekly_data[[age_group]]$esp_rate_vaccinated, sep = ","),
               out_file)
  }

  # Weekly ASMR (ESP-weighted), 15+
  ags <- names(weekly_data)
  esp_num_unvax <- sum(vapply(ags, function(ag) weekly_data[[ag]]$esp_rate_unvaccinated, numeric(1)))
  esp_num_vax   <- sum(vapply(ags, function(ag) weekly_data[[ag]]$esp_rate_vaccinated,   numeric(1)))
  esp_num_total <- sum(vapply(ags, function(ag) weekly_data[[ag]]$esp_rate_total,        numeric(1)))
  esp_den_scaled <- sum(vapply(ags, function(ag) weekly_data[[ag]]$esp_pop, numeric(1))) / 100000

  asmr_unvaccinated <- if (esp_den_scaled > 0) round(esp_num_unvax / esp_den_scaled, 7) else 0
  asmr_vaccinated   <- if (esp_den_scaled > 0) round(esp_num_vax / esp_den_scaled, 7) else 0
  asmr_total        <- if (esp_den_scaled > 0) round(esp_num_total / esp_den_scaled, 7) else 0

  writeLines(paste(year, week, asmr_unvaccinated, asmr_vaccinated, asmr_total, sep=","), out_asmr_file)

  # Advance ISO week
  nxt <- next_week(year, week); year <- nxt[1]; week <- nxt[2]
}

close(out_file); close(out_asmr_file)

cat("✅ Done. Wrote:\n",
    " - outputs/weekly_death_rates_from_download.csv\n",
    " - outputs/weekly_asmr_from_download.csv\n", sep = "")
