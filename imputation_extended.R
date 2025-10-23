# imputation_extended.R
# R port of the Perl main script, using OptimalSliding.R
# ------------------------------------------------------------------
# Usage:
#   Rscript imputation_extended.R <eurostat_csv> <mzcr_csv>
# or edit the defaults below.
# Output files are written under ./outputs/ (auto-created).
# ------------------------------------------------------------------
# We cast age-group "sliding" as a minimum-cost flow on a graph where each node is an age bin with supply (surplus vs. Eurostat) or demand (deficit).
# Directed edges connect every pair of distinct bins with capacity = min(surplusᵢ, deficitⱼ) and cost = 1 + distance_penalty·|i−j|; moves from older → younger incur an extra factor (1 + reverse_flow_penalty) to discourage implausible back-shifts.
# A successive shortest-path routine finds flows: a modified Dijkstra initializes all surplus nodes at distance 0 and only relaxes edges into bins that still have deficit. This enforces single-hop moves (one age group to another) per unit of flow.
# Each iteration augments by the path’s bottleneck (min of source surplus and sink deficit), updates residuals, and repeats until reaching min(total surplus, total deficit) or a fixed iteration cap.
# After sliding, any unknown records are imputed proportionally to remaining deficits across age bins; ages are drawn uniformly within the target bin (capping 90+), and unknown sex is assigned with a fixed split.
# We report before/after absolute error per age group and in total, flagging bins whose error increases >10%, and verify conservation of totals between original and adjusted counts.
# Intuitively, the cost design favors the smallest necessary age displacement while penalizing reverse (older→younger) transfers, yielding minimal, biologically plausible corrections.
# ------------------------------------------------------------------

suppressWarnings({
  options(stringsAsFactors = FALSE)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---------------- Configuration ----------------
eurostat_deaths_file <- "data/demo_r_mwk_05_linear_2_0.csv"
mzcr_file            <- "data/mzcr_no_or_first_infection.csv"

sliding_config <- list(
  max_iterations = 50,
  convergence_threshold = 0.01,
  distance_penalty = 0.15,
  reverse_flow_penalty = 0.3,
  verbose = 1
)

# Age groups (same order as the Perl script: older -> younger)
age_groups <- c("90-999","85-89","80-84","75-79","70-74","65-69",
                "60-64","55-59","50-54","45-49","40-44","35-39",
                "30-34","25-29","20-24","15-19")

# ---------------- Helpers ----------------

guess_sep_and_read <- function(path) {
  con <- file(path, open = "r", encoding = "UTF-8")
  on.exit(close(con))
  header_line <- readLines(con, n = 1, warn = FALSE)
  if (length(header_line) == 0) stop(sprintf("Empty CSV: %s", path))
  # strip BOM
  header_line <- sub("^\uFEFF", "", header_line, perl = TRUE)
  header_line <- sub("^\xEF\xBB\xBF", "", header_line, perl = TRUE)
  sep <- ","
  if (grepl(";", header_line) && !grepl(",", header_line)) {
    sep <- ";"
  } else if (grepl("\t", header_line) && !grepl(",|;", header_line)) {
    sep <- "\t"
  }
  # re-open fully with chosen sep
  df <- read.csv(path, sep = sep, quote = '"', check.names = FALSE, header = TRUE, encoding = "UTF-8")
  df
}

ymd_to_iso_year_week <- function(ymd) {
  if (is.na(ymd) || !grepl("^\\d{4}-\\d{2}-\\d{2}$", ymd)) stop(sprintf("Bad date: %s", ymd))
  d <- as.Date(ymd)
  iso_week <- as.integer(strftime(d, "%V"))
  iso_year <- as.integer(strftime(d, "%G"))  # ISO week-based year
  c(iso_year, iso_week)
}

from_year_to_year_from_age <- function(age) {
  a <- as.integer(age)
  if (is.na(a)) stop("age is NA")
  if (a <= 4) return(c(0,4))
  if (a <= 9) return(c(5,9))
  if (a <= 14) return(c(10,14))
  if (a <= 19) return(c(15,19))
  if (a <= 24) return(c(20,24))
  if (a <= 29) return(c(25,29))
  if (a <= 34) return(c(30,34))
  if (a <= 39) return(c(35,39))
  if (a <= 44) return(c(40,44))
  if (a <= 49) return(c(45,49))
  if (a <= 54) return(c(50,54))
  if (a <= 59) return(c(55,59))
  if (a <= 64) return(c(60,64))
  if (a <= 69) return(c(65,69))
  if (a <= 74) return(c(70,74))
  if (a <= 79) return(c(75,79))
  if (a <= 84) return(c(80,84))
  if (a <= 89) return(c(85,89))
  if (a >= 90) return(c(90,999))
  stop("age < 0?")
}

ensure_outputs_dir <- function() {
  if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE, showWarnings = FALSE)
}

# ---------------- Load module ----------------
# Expect OptimalSliding.R to be in the same folder or provide absolute path.
if (!exists("OptimalSliding")) {
  # Try to source from working directory
  if (file.exists("OptimalSliding.R")) {
    source("OptimalSliding.R", encoding = "UTF-8")
  } else {
    stop("OptimalSliding.R not found. Place it next to this script or source it before running.")
  }
}

# ---------------- Main ----------------

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 2) {
  eurostat_deaths_file <- args[1]
  mzcr_file <- args[2]
}

cat("Loading Eurostat deaths data...\n")
eu <- guess_sep_and_read(eurostat_deaths_file)

# Column names exactly as in the Perl script
col_sex  <- "sex: Sex"
col_geo  <- "geo: Geopolitical entity (reporting)"
col_val  <- "OBS_VALUE: Observation value"
col_time <- "TIME_PERIOD: Time"
col_age  <- "age: Age class"

# eurostat_deaths[year][week][age_group] = count
eurostat_deaths <- new.env(parent = emptyenv())

for (i in seq_len(nrow(eu))) {
  sex  <- eu[[col_sex]][i]
  geo  <- eu[[col_geo]][i]
  value <- suppressWarnings(as.numeric(eu[[col_val]][i]))
  week <- eu[[col_time]][i]
  age  <- eu[[col_age]][i]

  if (is.na(value)) next
  if (!identical(geo, "CZ: Czechia")) next
  if (identical(sex, "T: Total")) next
  if (identical(eu[[col_age]][i], "TOTAL: Total")) next

  # Parse TIME_PERIOD like "2020-W23"
  parts <- strsplit(as.character(week), "-", fixed = TRUE)[[1]]
  if (length(parts) != 2) next
  year <- suppressWarnings(as.integer(parts[1])); week_number <- suppressWarnings(as.integer(sub("^W", "", parts[2])))
  if (is.na(year) || is.na(week_number)) next

  # Map age label to bins (skip UNK and 0-14)
  if (identical(age, "UNK: Unknown")) next
  from_year <- to_year <- NA_integer_
  if (grepl("Y_LT5|Y5-9|Y10-14", age, perl = TRUE)) {
    next
  } else if (identical(age, "Y15-19: From 15 to 19 years")) {
    from_year <- 15; to_year <- 19
  } else if (identical(age, "Y20-24: From 20 to 24 years")) {
    from_year <- 20; to_year <- 24
  } else if (identical(age, "Y25-29: From 25 to 29 years")) {
    from_year <- 25; to_year <- 29
  } else if (identical(age, "Y30-34: From 30 to 34 years")) {
    from_year <- 30; to_year <- 34
  } else if (identical(age, "Y35-39: From 35 to 39 years")) {
    from_year <- 35; to_year <- 39
  } else if (identical(age, "Y40-44: From 40 to 44 years")) {
    from_year <- 40; to_year <- 44
  } else if (identical(age, "Y45-49: From 45 to 49 years")) {
    from_year <- 45; to_year <- 49
  } else if (identical(age, "Y50-54: From 50 to 54 years")) {
    from_year <- 50; to_year <- 54
  } else if (identical(age, "Y55-59: From 55 to 59 years")) {
    from_year <- 55; to_year <- 59
  } else if (identical(age, "Y60-64: From 60 to 64 years")) {
    from_year <- 60; to_year <- 64
  } else if (identical(age, "Y65-69: From 65 to 69 years")) {
    from_year <- 65; to_year <- 69
  } else if (identical(age, "Y70-74: From 70 to 74 years")) {
    from_year <- 70; to_year <- 74
  } else if (identical(age, "Y75-79: From 75 to 79 years")) {
    from_year <- 75; to_year <- 79
  } else if (identical(age, "Y80-84: From 80 to 84 years")) {
    from_year <- 80; to_year <- 84
  } else if (identical(age, "Y85-89: From 85 to 89 years")) {
    from_year <- 85; to_year <- 89
  } else if (identical(age, "Y_GE90: 90 years or over")) {
    from_year <- 90; to_year <- 999
  } else {
    next
  }
  age_group <- sprintf("%d-%d", from_year, to_year)

  # Increment
  if (is.null(eurostat_deaths[[as.character(year)]])) eurostat_deaths[[as.character(year)]] <- new.env(parent = emptyenv())
  year_env <- eurostat_deaths[[as.character(year)]]
  if (is.null(year_env[[as.character(week_number)]])) year_env[[as.character(week_number)]] <- new.env(parent = emptyenv())
  week_env <- year_env[[as.character(week_number)]]
  week_env[[age_group]] <- (week_env[[age_group]] %||% 0) + value
}
cat("Eurostat data loaded.\n\n")

# ---------------- Load MZCR Data ----------------
cat("Loading MZCR deaths data...\n")
mz <- guess_sep_and_read(mzcr_file)
mzcr_total_rows <- 12125969L  # as in Perl (informative)
mzcr_current_rows <- 0L
cpt <- 0L

# mzcr_deaths[year][week] has sub envs 'age_groups' (env) and 'unknown' (scalar)
mzcr_deaths <- new.env(parent = emptyenv())
unknown_records <- new.env(parent = emptyenv())
known_records <- new.env(parent = emptyenv())

col_wdod <- "week_date_of_death"
col_yob  <- "year_of_birth_end"
col_age  <- "age_at_death"
col_gnd  <- "gender"
col_id   <- "ID"

for (i in seq_len(nrow(mz))) {
  mzcr_current_rows <- mzcr_current_rows + 1L
  cpt <- cpt + 1L
  if (cpt == 1000L) {
    cpt <- 0L
    cat(sprintf("\rParsing MZCR - [%d / %d]", mzcr_current_rows, mzcr_total_rows))
    flush.console()
  }

  week_date_of_death <- as.character(mz[[col_wdod]][i])
  year_of_birth_end  <- as.character(mz[[col_yob]][i])
  age_at_death       <- mz[[col_age]][i]
  gender             <- as.character(mz[[col_gnd]][i])
  id                 <- as.character(mz[[col_id]][i])

  if (is.na(week_date_of_death) || nchar(week_date_of_death) == 0) next

  yw <- tryCatch(ymd_to_iso_year_week(week_date_of_death), error = function(e) NULL)
  if (is.null(yw)) next
  death_year <- yw[1]; death_week <- yw[2]

  sex <- "U"
  if (identical(gender, "1")) sex <- "M" else if (identical(gender, "2")) sex <- "F"

  cond_known <- (!is.na(year_of_birth_end) && nchar(year_of_birth_end) > 0 &&
                  !is.na(age_at_death) && age_at_death != "" && as.numeric(age_at_death) >= 15)

  if (isTRUE(cond_known)) {
    a <- suppressWarnings(as.numeric(age_at_death))
    if (is.na(a)) next
    br <- from_year_to_year_from_age(a)
    age_group <- sprintf("%d-%d", br[1], br[2])

    # increment age_groups count
    if (is.null(mzcr_deaths[[as.character(death_year)]])) mzcr_deaths[[as.character(death_year)]] <- new.env(parent = emptyenv())
    year_env <- mzcr_deaths[[as.character(death_year)]]
    if (is.null(year_env[[as.character(death_week)]])) year_env[[as.character(death_week)]] <- new.env(parent = emptyenv())
    week_env <- year_env[[as.character(death_week)]]
    if (is.null(week_env[["age_groups"]])) week_env[["age_groups"]] <- new.env(parent = emptyenv())
    ag_env <- week_env[["age_groups"]]
    ag_env[[age_group]] <- (ag_env[[age_group]] %||% 0) + 1

    # record known record
    known_records[[id]] <- list(
      id = id, sex = sex, year_of_birth = year_of_birth_end, week_date_of_death = week_date_of_death,
      age_at_death = a, age_group = age_group, death_year = death_year, death_week = death_week
    )
  } else if (is.na(year_of_birth_end) || identical(sex, "U")) {
    # unknown bucket
    if (is.null(mzcr_deaths[[as.character(death_year)]])) mzcr_deaths[[as.character(death_year)]] <- new.env(parent = emptyenv())
    year_env <- mzcr_deaths[[as.character(death_year)]]
    if (is.null(year_env[[as.character(death_week)]])) year_env[[as.character(death_week)]] <- new.env(parent = emptyenv())
    week_env <- year_env[[as.character(death_week)]]
    week_env[["unknown"]] <- (week_env[["unknown"]] %||% 0) + 1

    if (is.null(unknown_records[[as.character(death_year)]])) unknown_records[[as.character(death_year)]] <- new.env(parent = emptyenv())
    ur_year <- unknown_records[[as.character(death_year)]]
    if (is.null(ur_year[[as.character(death_week)]])) ur_year[[as.character(death_week)]] <- new.env(parent = emptyenv())
    ur_week <- ur_year[[as.character(death_week)]]
    ur_week[[id]] <- list(
      id = id, sex = sex, week_date_of_death = week_date_of_death, death_year = death_year, death_week = death_week
    )
  }
}
cat("\nMZCR data loaded.\n\n")

# ---------------- Perform Optimal Imputation ----------------
cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("Starting conflict resolution with optimal flow-based sliding...\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")

ensure_outputs_dir()
out_summary <- file("outputs/optimal_imputation_summary.csv", open = "w", encoding = "UTF-8")
writeLines("year,week,age_group,eurostat,mzcr_orig,initial_diff,after_slide,imputed,final_mzcr,final_diff,improvement", con = out_summary)
out_slides  <- file("outputs/optimal_slides.csv", open = "w", encoding = "UTF-8")
writeLines("year,week,from_group,to_group,amount", con = out_slides)
out_final   <- file("outputs/optimal_imputed_final.csv", open = "w", encoding = "UTF-8")
writeLines("id,orig_sex,orig_yob,imputed_sex,imputed_yob,imputed_age_group,death_year,death_week,method", con = out_final)
out_example <- file("outputs/example_weeks.txt", open = "w", encoding = "UTF-8")

global_stats <- list(
  total_slid = 0,
  total_imputed = 0,
  weeks_processed = 0,
  total_improvement = 0
)

OS <- do.call(OptimalSliding, sliding_config)

# iterate weeks present in mzcr_deaths
years <- sort(as.integer(ls(mzcr_deaths)))
for (year in years) {
  weeks_env <- mzcr_deaths[[as.character(year)]]
  weeks <- sort(as.integer(ls(weeks_env)))
  for (week in weeks) {
    if ((year == 2020 && week < 10) || year >= 2024) next

    global_stats$weeks_processed <- global_stats$weeks_processed + 1L

    weekly_env <- weeks_env[[as.character(week)]]
    weekly_unknown <- weekly_env[["unknown"]] %||% 0

    # Build weekly_data
    weekly_data <- list()
    for (ag in age_groups) {
      euro <- ((eurostat_deaths[[as.character(year)]] %||% new.env())[[as.character(week)]] %||% new.env())[[ag]] %||% 0
      mzcr <- ((weekly_env[["age_groups"]] %||% new.env())[[ag]] %||% 0)
      weekly_data[[ag]] <- list(
        eurostat = as.numeric(euro),
        mzcr_original = as.numeric(mzcr),
        mzcr_adjusted = as.numeric(mzcr),
        initial_diff = as.numeric(euro) - as.numeric(mzcr),
        imputed = 0
      )
    }

    # Initial stats
    initial_deficit <- sum(vapply(weekly_data, function(d) max(0, d$initial_diff), numeric(1)))
    initial_surplus <- sum(vapply(weekly_data, function(d) max(0, -d$initial_diff), numeric(1)))
    initial_error   <- sum(vapply(weekly_data, function(d) abs(d$initial_diff), numeric(1)))

    cat("\n", paste(rep("-", 60), collapse = ""), "\n", sep = "")
    cat(sprintf("Year %d, Week %d\n", year, week))
    cat(sprintf("Initial: Deficit=%d, Surplus=%d, Unknown=%d, Error=%d\n",
                initial_deficit, initial_surplus, weekly_unknown, initial_error))

    is_example <- (year == 2021 && week == 12) || (year == 2022 && week == 1)
    if (is_example) {
      writeLines(c("", paste(rep("=",70), collapse = ""), sprintf("EXAMPLE: Year %d, Week %d", year, week),
                   paste(rep("=",70), collapse = ""), "", "Initial State:"), con = out_example)
      for (ag in age_groups) {
        d <- weekly_data[[ag]]
        if (abs(d$initial_diff) > 0) {
          writeLines(sprintf("  %-10s: Euro=%4d, MZCR=%4d, Diff=%+5d", ag, d$eurostat, d$mzcr_original, d$initial_diff), con = out_example)
        }
      }
      writeLines(sprintf("\nTotal deficit: %d\nTotal surplus: %d\nUnknown deaths: %d", initial_deficit, initial_surplus, weekly_unknown), con = out_example)
    }

    # Sliding for 2021w11+
    slides_performed <- list()
    if (year > 2021 || (year == 2021 && week >= 11)) {
      cat("Applying optimal flow-based sliding...\n")
      res <- OS$optimize_sliding(weekly_data, age_groups)
      slides_performed <- res$slides
      weekly_data <- res$weekly_data

      # Output slides
      week_slid <- 0
      if (length(slides_performed)) {
        for (from in names(slides_performed)) {
          for (sl in slides_performed[[from]]) {
            writeLines(sprintf("%d,%d,%s,%s,%d", year, week, from, sl$to, as.integer(sl$amount)), con = out_slides)
            week_slid <- week_slid + as.numeric(sl$amount)
            global_stats$total_slid <- global_stats$total_slid + as.numeric(sl$amount)
            if (is_example) writeLines(sprintf("\nSlide: %s \u2192 %s: %d deaths", from, sl$to, as.integer(sl$amount)), con = out_example)
          }
        }
      }
      cat(sprintf("  Total slid this week: %d deaths\n", as.integer(week_slid)))

      # Validate
      validation <- OS$validate_sliding(weekly_data, age_groups, slides_performed)
      if (!is.null(validation$warnings) && length(validation$warnings)) {
        cat("  Warnings:\n")
        for (w in validation$warnings) cat("    ", w, "\n", sep = "")
      }
      global_stats$total_improvement <- global_stats$total_improvement + (validation$total_improvement %||% 0)
    } else {
      cat("Using simple imputation (pre-2021w11)\n")
    }

    # Post-sliding stats
    post_slide_deficit <- sum(vapply(age_groups, function(ag) {
      d <- weekly_data[[ag]]; max(0, d$eurostat - d$mzcr_adjusted)
    }, numeric(1)))
    post_slide_error <- sum(vapply(age_groups, function(ag) {
      d <- weekly_data[[ag]]; abs(d$eurostat - d$mzcr_adjusted)
    }, numeric(1)))

    if (is_example) {
      writeLines("\nAfter Sliding:", con = out_example)
      for (ag in age_groups) {
        d <- weekly_data[[ag]]
        diff <- d$eurostat - d$mzcr_adjusted
        if (abs(diff) > 0 || abs(d$initial_diff) > 0) {
          writeLines(sprintf("  %-10s: Euro=%4d, MZCR=%4d, Diff=%+5d (was %+5d)",
                             ag, d$eurostat, d$mzcr_adjusted, diff, d$initial_diff), con = out_example)
        }
      }
      writeLines(sprintf("\nRemaining deficit: %d\nTotal error: %d (was %d)", post_slide_deficit, post_slide_error, initial_error), con = out_example)
    }

    # Phase 2: Impute unknowns
    if (weekly_unknown > 0 && post_slide_deficit > 0) {
      cat(sprintf("\nImputing %d unknown deaths...\n", weekly_unknown))
      imputation_plan <- setNames(rep(0L, length(age_groups)), age_groups)
      assigned <- 0L

      # Distribute proportionally to deficits
      for (ag in age_groups) {
        d <- weekly_data[[ag]]
        deficit <- d$eurostat - d$mzcr_adjusted
        if (deficit > 0) {
          proportion <- deficit / post_slide_deficit
          to_impute <- as.integer(round(weekly_unknown * proportion))
          to_impute <- min(to_impute, deficit, weekly_unknown - assigned)
          if (to_impute > 0) {
            weekly_data[[ag]]$mzcr_adjusted <- weekly_data[[ag]]$mzcr_adjusted + to_impute
            weekly_data[[ag]]$imputed <- to_impute
            imputation_plan[[ag]] <- to_impute
            assigned <- assigned + to_impute
            global_stats$total_imputed <- global_stats$total_imputed + to_impute
            cat(sprintf("  Imputed %d to %s\n", to_impute, ag))
            if (is_example) writeLines(sprintf("Impute: %d unknowns \u2192 %s", to_impute, ag), con = out_example)
          }
        }
      }

      # Assign specific records
      ur_year <- unknown_records[[as.character(year)]] %||% new.env()
      ur_week <- ur_year[[as.character(week)]] %||% new.env()
      unknown_ids <- ls(ur_week)
      idx <- 1L

      for (ag in age_groups) {
        count <- imputation_plan[[ag]] %||% 0
        if (count <= 0) next
        for (k in seq_len(count)) {
          if (idx > length(unknown_ids)) break
          id <- unknown_ids[[idx]]; idx <- idx + 1L
          rec <- ur_week[[id]]
          # Random age within group
          parts <- strsplit(ag, "-", fixed = TRUE)[[1]]
          min_age <- as.integer(parts[1]); max_age <- as.integer(parts[2]); if (max_age == 999) max_age <- 105
          imputed_age <- min_age + as.integer(floor(runif(1) * (max_age - min_age + 1)))
          imputed_yob <- year - imputed_age
          imputed_sex <- if (identical(rec$sex, "U")) if (runif(1) < 0.52) "M" else "F" else rec$sex

          writeLines(sprintf("%s,%s,,%s,%d,%s,%d,%d,%s",
                             rec$id, rec$sex, imputed_sex, imputed_yob, ag, year, week,
                             if (year > 2021 || (year == 2021 && week >= 11)) "optimal_imputation" else "simple_imputation"),
                     con = out_final)
        }
      }
    }

    # Final stats
    final_error <- sum(vapply(age_groups, function(ag) {
      d <- weekly_data[[ag]]; abs(d$eurostat - d$mzcr_adjusted)
    }, numeric(1)))
    improvement <- initial_error - final_error
    cat(sprintf("Final: Error=%d (improvement=%d, %.1f%%)\n",
                as.integer(final_error), as.integer(improvement),
                if (initial_error > 0) (improvement/initial_error*100) else 0))

    if (is_example) {
      writeLines("\nFinal State:", con = out_example)
      for (ag in age_groups) {
        d <- weekly_data[[ag]]
        diff <- d$eurostat - d$mzcr_adjusted
        writeLines(sprintf("  %-10s: Euro=%4d, Final=%4d, Diff=%+5d, Improvement=%d",
                           ag, d$eurostat, d$mzcr_adjusted, diff, abs(d$initial_diff) - abs(diff)), con = out_example)
      }
      writeLines(sprintf("\nTotal improvement: %d deaths", as.integer(improvement)), con = out_example)
      writeLines(sprintf("Improvement: %.1f%%", if (initial_error > 0) (improvement/initial_error*100) else 0), con = out_example)
    }

    # Output summary rows
    for (ag in age_groups) {
      d <- weekly_data[[ag]]
      after_slide <- d$mzcr_adjusted - (d$imputed %||% 0)
      final_diff <- d$eurostat - d$mzcr_adjusted
      improvement_ag <- abs(d$initial_diff) - abs(final_diff)
      writeLines(sprintf("%d,%d,%s,%d,%d,%d,%d,%d,%d,%d,%d",
                         year, week, ag,
                         as.integer(d$eurostat), as.integer(d$mzcr_original), as.integer(d$initial_diff),
                         as.integer(after_slide), as.integer(d$imputed %||% 0),
                         as.integer(d$mzcr_adjusted), as.integer(final_diff), as.integer(improvement_ag)), con = out_summary)
    }
  }
}

# Output known records (originals)
known_ids <- sort(ls(known_records))
for (id in known_ids) {
  rec <- known_records[[id]]
  writeLines(sprintf("%s,%s,%s,%s,%s,%s,%d,%d,original",
                     rec$id, rec$sex, rec$year_of_birth, rec$sex, rec$year_of_birth,
                     rec$age_group, rec$death_year, rec$death_week), con = out_final)
}

close(out_summary)
close(out_slides)
close(out_final)
close(out_example)

# Final report
cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("OPTIMAL IMPUTATION COMPLETE\n")
cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("\nGlobal Statistics:\n")
cat(sprintf("  Weeks processed: %d\n", as.integer(global_stats$weeks_processed)))
cat(sprintf("  Total deaths slid: %d\n", as.integer(global_stats$total_slid)))
cat(sprintf("  Total unknowns imputed: %d\n", as.integer(global_stats$total_imputed)))
cat(sprintf("  Total error improvement: %d\n", as.integer(global_stats$total_improvement)))
cat("\nOutput Files Created:\n")
cat("  - outputs/optimal_imputation_summary.csv: Complete statistics\n")
cat("  - outputs/optimal_slides.csv: All sliding movements\n")
cat("  - outputs/optimal_imputed_final.csv: Final imputed dataset\n")
cat("  - outputs/example_weeks.txt: Detailed examples\n")
cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")
