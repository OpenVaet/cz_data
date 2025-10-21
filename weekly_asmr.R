# --------------------------------------------------
# R rewrite of the provided Perl script
# - Computes weekly death rates by vaccination status and ESP 2013 weights
# - Expects input CSV: data/mzcr_no_or_first_infection_with_imputation.csv
# - Writes: outputs/weekly_death_rates.csv
# --------------------------------------------------

# You may want to install data.table for performance on large files
# install.packages("data.table")

suppressWarnings({
  have_dt <- requireNamespace("data.table", quietly = TRUE)
})

# -------------------------------
# Helpers
# -------------------------------
round_nearest <- function(x, step = 1) {
  # Equivalent to Math::Round::nearest(step, x)
  round(x / step) * step
}

iso_weeks_in_year <- function(year) {
  # Number of ISO weeks: week number of Dec 28 of the year
  dec28 <- as.Date(sprintf("%04d-12-28", year))
  as.integer(strftime(dec28, "%V"))
}

iso_week_monday_date <- function(iso_week) {
  # iso_week: string like "YYYY-WW" or "YYYYWW" or "YYYY-WWW" (1-53)
  m <- regexec("^\\s*(\\d{4})[- ]?W?(\\d{1,2})\\s*$", iso_week)
  res <- regmatches(iso_week, m)
  if (length(res) == 0 || length(res[[1]]) < 3) stop("Expect ISO 'YYYY-WW', got: ", iso_week)
  year <- as.integer(res[[1]][2])
  week <- as.integer(res[[1]][3])
  wmax <- iso_weeks_in_year(year)
  if (week < 1 || week > wmax) stop(sprintf("Year %d has only %d ISO weeks (got %d)", year, wmax, week))
  jan4 <- as.Date(sprintf("%04d-01-04", year))
  iso_dow <- as.integer(strftime(jan4, "%u")) # 1..7, Mon=1
  week1_monday <- jan4 - (iso_dow - 1)
  monday <- week1_monday + (week - 1) * 7
  as.Date(monday)
}

ymd_to_iso_year_week <- function(ymd) {
  # ymd: character vector "YYYY-MM-DD"; returns data.frame(iso_year, iso_week)
  if (length(ymd) == 0) return(data.frame(iso_year = integer(), iso_week = integer()))
  d <- as.Date(ymd)
  if (any(is.na(d))) stop("Bad date in ymd_to_iso_year_week: ", paste(ymd[is.na(d)], collapse = ", "))
  iso_week <- as.integer(strftime(d, "%V"))
  iso_wday <- as.integer(strftime(d, "%u"))
  thu <- d + (4L - iso_wday)
  iso_year <- as.integer(format(thu, "%Y"))
  data.frame(iso_year = iso_year, iso_week = iso_week)
}

# -------------------------------
# ESP 2013 reference population
# -------------------------------
esp_age_start <- c(0, 1, 5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85, 90)
esp_age_end   <- c(0, 4, 9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89, 999)
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
cat(sprintf("ESP 2013 total population: %d\n", esp_total))

esp <- data.frame(
  idx = seq_along(esp_pop),
  age_start = esp_age_start,
  age_end = esp_age_end,
  esp_pop = esp_pop,
  stringsAsFactors = FALSE
)
esp$age_group <- ifelse(esp$age_end == 999, sprintf("%d-999", esp$age_start), sprintf("%d-%d", esp$age_start, esp$age_end))
esp$age_mid <- ifelse(esp$age_end == 999, esp$age_start + 2.5, (esp$age_start + esp$age_end) / 2)

age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-999")

# -------------------------------
# Inputs
# -------------------------------
mzcr_origin_file <- "data/mzcr_no_or_first_infection_with_imputation.csv"
if (!file.exists(mzcr_origin_file)) stop("Input file not found: ", mzcr_origin_file)

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# -------------------------------
# Read CSV (fast path via data.table::fread, fallback to base)
# -------------------------------
read_mzcr <- function(path) {
  if (have_dt) {
    dt <- data.table::fread(
      path,
      encoding = "UTF-8",
      showProgress = TRUE,
      na.strings = c("", "NA", "N/A")
    )
    # Trim header whitespace
    new_names <- trimws(names(dt))
  data.table::setnames(dt, new_names)
    return(dt)
  } else {
    # Try to sniff separator from header
    con <- file(path, open = "rb")
    on.exit(close(con), add = TRUE)
    header_line <- readLines(con, n = 1L, warn = FALSE, encoding = "UTF-8")
    header_line <- gsub("^\\ufeff|^\\xEF\\xBB\\xBF", "", header_line)
    sep <- ","
    if (grepl(";", header_line) && !grepl(",", header_line)) sep <- ";"
    else if (grepl("\\t", header_line) && !grepl(",|;", header_line)) sep <- "\t"
    df <- utils::read.csv(path, sep = sep, quote = '"', stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
    names(df) <- trimws(names(df))
    df
  }
}

cat("Reading input...\n")
M <- read_mzcr(mzcr_origin_file)

req_cols <- c("id", "week_date_of_death", "age_group", "year_of_birth_end", "sex", "age_at_death", "Date_First_Dose")
missing_cols <- setdiff(req_cols, names(M))
if (length(missing_cols)) stop("Missing required columns in input: ", paste(missing_cols, collapse = ", "))

# Normalize types
if (have_dt && data.table::is.data.table(M)) {
  # make a copy to avoid by-reference surprises
  M <- data.table::copy(M)
} else {
  M <- as.data.frame(M, stringsAsFactors = FALSE)
}

# Ensure age_group is character
M$age_group <- as.character(M$age_group)

# -------------------------------
# Compute alive_on_jan_1st_2024 and vaccination features
# -------------------------------
cat("Preprocessing rows...\n")

# comp_death: numeric yyyymmdd if week_date_of_death exists and <= 2023-12-31, else NA
strip_digits <- function(x) gsub("\\D", "", x)

# Coerce empty strings to NA
null_if_empty <- function(x) { x[x == ""] <- NA_character_; x }
M$week_date_of_death <- null_if_empty(as.character(M$week_date_of_death))
M$Date_First_Dose    <- null_if_empty(as.character(M$Date_First_Dose))

comp_death_str <- ifelse(is.na(M$week_date_of_death), NA_character_, strip_digits(M$week_date_of_death))
comp_death_num <- suppressWarnings(as.numeric(comp_death_str))
M$alive_on_2024 <- is.na(comp_death_num) | comp_death_num > 20231231

# Parse first dose iso year-week if present and < 2024
parse_iso_year_week <- function(x) {
  m <- regexec("^\\s*(\\d{4})[- ]?W?(\\d{1,2})\\s*$", x)
  regmatches(x, m)
}

first_dose_matches <- parse_iso_year_week(M$Date_First_Dose)
get_or_na <- function(lst, idx) {
  sapply(lst, function(v) if (length(v) >= idx) v[idx] else NA_character_)
}

vax_year <- suppressWarnings(as.integer(get_or_na(first_dose_matches, 2)))
vax_week <- suppressWarnings(as.integer(get_or_na(first_dose_matches, 3)))
M$vax_year <- ifelse(!is.na(vax_year), vax_year, NA_integer_)
M$vax_week <- ifelse(!is.na(vax_week), vax_week, NA_integer_)

# vaccinated_on_death flag, following Perl logic
M$vaccinated_on_death <- 0L

# Only consider doses before 2024
has_pre2024_dose <- !is.na(M$vax_year) & M$vax_year < 2024

# For rows with death <= 2023-12-31, ensure dose Monday date <= death date, and set vaccinated_on_death=1
strict_check <- TRUE
if (any(has_pre2024_dose & !is.na(comp_death_num))) {
  idx <- which(has_pre2024_dose & !is.na(comp_death_num) & comp_death_num <= 20231231)
  if (length(idx)) {
    # Compute Monday date of the ISO week for these rows
    dose_monday <- as.Date(mapply(function(y, w) iso_week_monday_date(sprintf("%04d-%02d", y, w)), M$vax_year[idx], M$vax_week[idx]))
    death_date  <- as.Date(M$week_date_of_death[idx])
    comp_dose_num <- as.integer(format(dose_monday, "%Y%m%d"))
    comp_death_subset <- as.integer(format(death_date, "%Y%m%d"))
    if (strict_check && any(comp_death_subset < comp_dose_num, na.rm = TRUE)) {
      bad <- which(comp_death_subset < comp_dose_num)
      stop(sprintf("Found %d records with death before first dose (e.g., row %d)", length(bad), idx[bad[1]]))
    }
    # Set vaccinated_on_death=1 when death by 2023 and pre-2024 dose exists
    M$vaccinated_on_death[idx] <- 1L
  }
}

# -------------------------------
# Aggregate population baseline and weekly events
# -------------------------------
cat("Aggregating...\n")

# Baseline components at Jan 1, 2024
if (have_dt && data.table::is.data.table(M)) {
  DT <- M
  pop_alive <- DT[alive_on_2024 == TRUE, .N, by = .(age_group)]
  data.table::setnames(pop_alive, "N", "total_population")
  pop_dead  <- DT[alive_on_2024 == FALSE, .N, by = .(age_group)]
  data.table::setnames(pop_dead, "N", "total_deaths")
  baseline <- merge(pop_alive, pop_dead, by = "age_group", all = TRUE)
  baseline[is.na(total_population), total_population := 0L]
  baseline[is.na(total_deaths), total_deaths := 0L]

  # Vaccinations per (year, week, age_group) for years < 2024
  vax_counts <- DT[!is.na(vax_year) & vax_year < 2024, .N, by = .(year = vax_year, week = vax_week, age_group)]
  data.table::setnames(vax_counts, "N", "vaccinated_this_week")

  # Weekly deaths by vaccination flag, for those who died by 2023-12-31
  dead_rows <- DT[alive_on_2024 == FALSE & !is.na(week_date_of_death)]
  iso_yw <- ymd_to_iso_year_week(dead_rows$week_date_of_death)
  dead_rows$year <- iso_yw$iso_year
  dead_rows$week <- iso_yw$iso_week
  deaths_counts <- dead_rows[, .N, by = .(year, week, age_group, vaccinated_on_death)]
  deaths_wide <- data.table::dcast(
    deaths_counts,
    year + week + age_group ~ vaccinated_on_death,
    value.var = "N",
    fill = 0L
  )
  # Columns "0" and "1" for unvaccinated / vaccinated deaths
  if (!"0" %in% names(deaths_wide)) deaths_wide[["0"]] <- 0L
  if (!"1" %in% names(deaths_wide)) deaths_wide[["1"]] <- 0L

} else {
  # data.frame fallback (slower)
  pop_alive <- aggregate(list(total_population = as.integer(M$alive_on_2024)), list(age_group = M$age_group), function(x) sum(x == 1L, na.rm = TRUE))
  pop_dead  <- aggregate(list(total_deaths = as.integer(!M$alive_on_2024)), list(age_group = M$age_group), function(x) sum(x == 1L, na.rm = TRUE))
  baseline  <- merge(pop_alive, pop_dead, by = "age_group", all = TRUE)
  baseline$total_population[is.na(baseline$total_population)] <- 0L
  baseline$total_deaths[is.na(baseline$total_deaths)] <- 0L

  pre_vax <- which(!is.na(M$vax_year) & M$vax_year < 2024)
  if (length(pre_vax)) {
    vax_counts <- aggregate(list(vaccinated_this_week = rep(1L, length(pre_vax))),
                            list(year = M$vax_year[pre_vax], week = M$vax_week[pre_vax], age_group = M$age_group[pre_vax]), sum)
  } else {
    vax_counts <- data.frame(year = integer(), week = integer(), age_group = character(), vaccinated_this_week = integer(), stringsAsFactors = FALSE)
  }

  dead_idx <- which(M$alive_on_2024 == FALSE & !is.na(M$week_date_of_death))
  if (length(dead_idx)) {
    yw <- ymd_to_iso_year_week(M$week_date_of_death[dead_idx])
    deaths_df <- data.frame(year = yw$iso_year, week = yw$iso_week, age_group = M$age_group[dead_idx], vaccinated_on_death = M$vaccinated_on_death[dead_idx])
    deaths_counts <- aggregate(list(N = rep(1L, nrow(deaths_df))), deaths_df, sum)
    # wide
    key <- paste(deaths_counts$year, deaths_counts$week, deaths_counts$age_group, sep = "\t")
    deaths_wide <- unique(data.frame(year = deaths_counts$year, week = deaths_counts$week, age_group = deaths_counts$age_group, stringsAsFactors = FALSE))
    deaths_wide$`0` <- 0L; deaths_wide$`1` <- 0L
    for (i in seq_len(nrow(deaths_counts))) {
      k <- paste(deaths_counts$year[i], deaths_counts$week[i], deaths_counts$age_group[i], sep = "\t")
      idx <- which(paste(deaths_wide$year, deaths_wide$week, deaths_wide$age_group, sep = "\t") == k)
      col <- as.character(deaths_counts$vaccinated_on_death[i])
      deaths_wide[[col]][idx] <- deaths_wide[[col]][idx] + deaths_counts$N[i]
    }
  } else {
    deaths_wide <- data.frame(year = integer(), week = integer(), age_group = character(), `0` = integer(), `1` = integer(), stringsAsFactors = FALSE)
  }
}

# -------------------------------
# Initialize baseline populations (total_unvaccinated, total_vaccinated)
# -------------------------------
baseline$total_unvaccinated <- baseline$total_deaths + baseline$total_population
baseline$total_vaccinated   <- 0L
row.names(baseline) <- NULL

# Utility lookup tables
esp_lookup <- setNames(esp$esp_pop, esp$age_group)
age_from <- setNames(esp$age_start, esp$age_group)

# Only consider age groups with from_age >= 15
eligible_age_groups <- esp$age_group[esp$age_start >= 15]

# Collect all (year, week) pairs present in deaths
if (have_dt && exists("deaths_wide") && data.table::is.data.table(deaths_wide)) {
  yw_pairs <- unique(deaths_wide[, .(year, week)])
  data.table::setorder(yw_pairs, year, week)
} else {
  if (nrow(deaths_wide)) {
    yw_pairs <- unique(deaths_wide[, c("year", "week")])
    yw_pairs <- yw_pairs[order(yw_pairs$year, yw_pairs$week), , drop = FALSE]
  } else {
    yw_pairs <- data.frame(year = integer(), week = integer())
  }
}

# Build fast indexers
key_ywa <- function(y, w, a) paste(y, w, a, sep = "\t")

if (have_dt && data.table::is.data.table(vax_counts)) {
  vax_key <- key_ywa(vax_counts$year, vax_counts$week, vax_counts$age_group)
  vax_map <- setNames(vax_counts$vaccinated_this_week, vax_key)  # named numeric vector
} else {
  if (exists("vax_counts") && nrow(vax_counts)) {
    vax_map <- setNames(vax_counts$vaccinated_this_week, key_ywa(vax_counts$year, vax_counts$week, vax_counts$age_group))  # named numeric vector
  } else {
    vax_map <- setNames(numeric(0), character(0))
  }
}

if (have_dt && data.table::is.data.table(deaths_wide)) {
  deaths_key <- key_ywa(deaths_wide$year, deaths_wide$week, deaths_wide$age_group)
  deaths_v_map <- setNames(deaths_wide$`1`, deaths_key)  # named numeric vector # vaccinated deaths
  deaths_u_map <- setNames(deaths_wide$`0`, deaths_key)  # named numeric vector # unvaccinated deaths
} else {
  if (nrow(deaths_wide)) {
    dk <- key_ywa(deaths_wide$year, deaths_wide$week, deaths_wide$age_group)
    deaths_v_map <- setNames(deaths_wide$`1`, dk)
    deaths_u_map <- setNames(deaths_wide$`0`, dk)
  } else {
    deaths_v_map <- setNames(numeric(0), character(0)); deaths_u_map <- setNames(numeric(0), character(0))
  }
}

# Baseline state per age_group
baseline_state <- baseline
row.names(baseline_state) <- baseline_state$age_group

# -------------------------------
# Iterate weeks and compute rates
# -------------------------------
cat("Computing weekly death rates...\n")

results <- vector("list", max(1L, nrow(yw_pairs) * length(eligible_age_groups)))
rr <- 0L

for (i in seq_len(nrow(yw_pairs))) {
  y <- as.integer(yw_pairs$year[i])
  w <- as.integer(yw_pairs$week[i])
  for (age_group in eligible_age_groups) {
    esp_pop_i <- esp_lookup[[age_group]]
    bs_row <- baseline_state[age_group, , drop = FALSE]
    if (nrow(bs_row) == 0) {
      total_unvaccinated <- 0L; total_vaccinated <- 0L
    } else {
      total_unvaccinated <- as.numeric(bs_row$total_unvaccinated)
      total_vaccinated   <- as.numeric(bs_row$total_vaccinated)
    }

    vk <- key_ywa(y, w, age_group)
    vaccinated_this_week <- as.numeric(vax_map[vk])
    if (length(vaccinated_this_week) == 0L || is.na(vaccinated_this_week)) vaccinated_this_week <- 0

    # Adjust baseline for vaccinations
    total_vaccinated   <- total_vaccinated + vaccinated_this_week
    total_unvaccinated <- total_unvaccinated - vaccinated_this_week
    if (total_unvaccinated < 0) total_unvaccinated <- 0

    # Deaths this week by status
    dv <- as.numeric(deaths_v_map[vk])
    if (length(dv) == 0L || is.na(dv)) dv <- 0
    du <- as.numeric(deaths_u_map[vk])
    if (length(du) == 0L || is.na(du)) du <- 0

    # Remove deaths from baseline
    total_vaccinated   <- total_vaccinated - dv
    total_unvaccinated <- total_unvaccinated - du
    if (total_vaccinated < 0) total_vaccinated <- 0
    if (total_unvaccinated < 0) total_unvaccinated <- 0

    # Save back to baseline state
    baseline_state[age_group, "total_unvaccinated"] <- total_unvaccinated
    baseline_state[age_group, "total_vaccinated"]   <- total_vaccinated

    # Rates per 100k, and ESP-weighted
    rate_unvax <- 0; rate_vax <- 0; esp_rate_unvax <- 0; esp_rate_vax <- 0
    if (total_unvaccinated > 0) {
      rate_unvax <- round_nearest((du * 1e5) / total_unvaccinated, 1e-5)
      esp_rate_unvax <- round_nearest(rate_unvax * esp_pop_i / 1e5, 1e-5)
    }
    if (total_vaccinated > 0) {
      rate_vax <- round_nearest((dv * 1e5) / total_vaccinated, 1e-5)
      esp_rate_vax <- round_nearest(rate_vax * esp_pop_i / 1e5, 1e-5)
    }

    rr <- rr + 1L
    results[[rr]] <- data.frame(
      year = y,
      week = w,
      age_group = age_group,
      esp_pop = esp_pop_i,
      total_unvaccinated = total_unvaccinated,
      total_vaccinated   = total_vaccinated,
      vaccinated_this_week = vaccinated_this_week,
      deaths_vaccinated = dv,
      deaths_unvaccinated = du,
      rate_per_100k_unvaccinated = rate_unvax,
      rate_per_100k_vaccinated   = rate_vax,
      esp_rate_unvaccinated = esp_rate_unvax,
      esp_rate_vaccinated   = esp_rate_vax,
      stringsAsFactors = FALSE
    )
  }
}

if (rr == 0L) {
  OUT <- data.frame(
    year = integer(), week = integer(), age_group = character(), esp_pop = integer(),
    total_unvaccinated = integer(), total_vaccinated = integer(), vaccinated_this_week = integer(),
    deaths_vaccinated = integer(), deaths_unvaccinated = integer(),
    rate_per_100k_unvaccinated = numeric(), rate_per_100k_vaccinated = numeric(),
    esp_rate_unvaccinated = numeric(), esp_rate_vaccinated = numeric(),
    stringsAsFactors = FALSE
  )
} else {
  OUT <- do.call(rbind, results[seq_len(rr)])
}

# -------------------------------
# Write output CSV
# -------------------------------
cat("Writing outputs/weekly_death_rates.csv ...\n")
utils::write.table(
  OUT,
  file = "outputs/weekly_death_rates.csv",
  sep = ",",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE,
  fileEncoding = "UTF-8"
)

# -------------------------------
# Weekly ESP-standardized totals & visualization
# -------------------------------
cat("Building weekly ESP-standardized summary...
")
weekly_esp <- if (nrow(OUT)) {
  stats::aggregate(cbind(esp_rate_unvaccinated, esp_rate_vaccinated) ~ year + week, data = OUT, sum)
} else {
  data.frame(year = integer(), week = integer(), esp_rate_unvaccinated = numeric(), esp_rate_vaccinated = numeric())
}

if (nrow(weekly_esp)) {
  weekly_esp$date <- as.Date(mapply(function(y, w) iso_week_monday_date(sprintf("%04d-%02d", y, w)), weekly_esp$year, weekly_esp$week))
  weekly_esp <- weekly_esp[order(weekly_esp$date), ]

  # Write summary CSV
  utils::write.table(
    weekly_esp[, c("year","week","date","esp_rate_unvaccinated","esp_rate_vaccinated")],
    file = "outputs/weekly_esp_rates_summary.csv",
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    fileEncoding = "UTF-8"
  )

  # Make line plot and save PNG
  png_path <- "outputs/esp_rates_vax_vs_unvax.png"
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    plot_df <- data.frame(
      date = rep(weekly_esp$date, 2),
      rate = c(weekly_esp$esp_rate_unvaccinated, weekly_esp$esp_rate_vaccinated),
      group = rep(c("Unvaccinated", "Vaccinated"), each = nrow(weekly_esp))
    )
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = date, y = rate, color = group)) +
      ggplot2::geom_line() +
      ggplot2::labs(
        title = "ESP-standardized mortality rates by vaccination status",
        subtitle = "Weekly (ISO) — all eligible age groups combined",
        x = "Week (ISO, Monday)",
        y = "Deaths per 100k (ESP-standardized)",
        color = "Status"
      ) +
      ggplot2::theme_minimal()
    p
    ggplot2::ggsave(filename = png_path, plot = p, width = 9, height = 5, dpi = 120)
  } else {
    grDevices::png(filename = png_path, width = 1200, height = 700, res = 120)
    plot(weekly_esp$date, weekly_esp$esp_rate_unvaccinated, type = "l",
         xlab = "Week (ISO, Monday)", ylab = "Deaths per 100k (ESP-standardized)",
         main = "ESP-standardized mortality rates by vaccination status")
    lines(weekly_esp$date, weekly_esp$esp_rate_vaccinated, lty = 2)
    legend("topright", legend = c("Unvaccinated", "Vaccinated"), lty = c(1, 2), bty = "n")
    grDevices::dev.off()
  }

  cat(sprintf("Wrote summary CSV: %s
", "outputs/weekly_esp_rates_summary.csv"))
  cat(sprintf("Saved plot: %s
", png_path))
} else {
  cat("No weekly data to summarize — skipping plot.
")
}

cat("Done.
")

# Optionally inspect final baseline
# print(baseline_state)
