# Run from the extracted article project with Rscript 18_verify_younger_no_dose_excess.R
# or source("18_verify_younger_no_dose_excess.R", encoding = "UTF-8").
# Set CZ_NOVEMBER_RAW_CSV to the original uncompressed November CSV for a record-level recount.
# The default uses the article snapshot. CZ_STEPnn_DIR can select a specific complete run.
verify_younger_excess <- function(root = Sys.getenv("CZ_PROJECT_ROOT", "."),
                                  raw_csv = Sys.getenv("CZ_NOVEMBER_RAW_CSV")) {
  root <- normalizePath(root, mustWork = TRUE)
  csv <- function(p) read.csv(p, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  sha <- function(p) {
    if (requireNamespace("digest", quietly = TRUE)) return(digest::digest(file = p, algo = "sha256"))
    if ("sha256sum" %in% getNamespaceExports("tools")) return(unname(getExportedValue("tools","sha256sum")(p)))
    if (nzchar(Sys.which("sha256sum"))) return(substr(system2("sha256sum", shQuote(p), stdout = TRUE), 1, 64))
    stop("Install the R package 'digest' for SHA-256 verification.")
  }
  inputs <- data.frame(step = character(), file = character(), sha256 = character())
  read_input <- function(step, name) {
    key <- paste0("CZ_STEP", substr(step,1,2), "_DIR")
    path <- file.path(Sys.getenv(key, file.path(root, "publication_source", "snapshot", step)), name)
    manifest <- csv(file.path(dirname(path), "outputs.csv"))
    hash <- sha(path)
    stopifnot(identical(hash, manifest$sha256[manifest$file == name]))
    inputs <<- rbind(inputs, data.frame(step = step, file = name, sha256 = hash))
    csv(path)
  }
  equal <- function(x, y, tolerance = 1e-6) {
    stopifnot(length(x) == length(y), all(is.finite(x)), all(is.finite(y)),
              all(abs(x-y) < tolerance))
  }
  weeks <- sprintf("2023-W%02d", 1:52)
  five_year_ages <- seq(25, 45, by = 5)
  s05 <- "05_profile_missing_demographics"
  dose <- read_input(s05, "weekly_deaths_by_dose_status.csv")
  dose <- dose[dose$week %in% weeks, ]
  status <- c("No recorded dose", "First dose before death week", "First dose in death week",
              "First dose after death week", "Dose recorded, first date unavailable")
  stopifnot(all(table(factor(dose$week, weeks), factor(dose$status, status)) == 1L),
            nrow(dose) == length(weeks) * length(status))
  dose$missing_with_recorded_dose <- ifelse(dose$status == "No recorded dose", 0, dose$missing_birth_deaths)
  lpz <- aggregate(cbind(deaths, missing_birth_deaths, missing_with_recorded_dose) ~ week,
                   dose, sum, na.action = na.fail)
  totals <- read_input(s05, "release_totals.csv")
  identity <- read_input(s05, "input_files.csv")
  raw_hash <- identity$sha256[identity$id == "november"]
  stopifnot(nrow(totals) == 1L, length(raw_hash) == 1L)
  # Compare published all-age Eurostat counts with November LPZ counts on the same 52 weeks.
  national <- read_input("04_compare_event_counts", "weekly_counts.csv")
  national <- national[national$week %in% weeks & national$series == "Eurostat" &
                       national$event == "All-cause deaths", ]
  stopifnot(setequal(national$week, weeks), nrow(national) == 52L, all(national$available & national$full_week))
  lpz$eurostat_all_ages <- national$count[match(lpz$week, national$week)]
  stopifnot(!anyNA(lpz), all(lpz$missing_birth_deaths <= lpz$deaths))

  # Optional independent recount. Keep blank/first infection rows, as in Step 02.
  # LPZ uses ISO year and week. All seven dose dates determine "any dose recorded".
  if (nzchar(raw_csv)) {
    stopifnot(identical(sha(raw_csv), raw_hash))
    con <- file(raw_csv, open = "rt", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    header <- sub("^\ufeff", "", readLines(con, 1L))
    names_in <- names(read.csv(text = header, check.names = FALSE))
    dose_cols <- paste0("Datum_", c("Prvni","Druha","Treti","Ctvrta","Pata","Sesta","Sedma"), "_davka")
    keep <- c("Infekce", "RokNarozeni", "DatumUmrtiLPZ", "Umrti", dose_cols)
    stopifnot(!anyDuplicated(names_in), all(keep %in% names_in))
    classes <- ifelse(names_in %in% keep, "character", "NULL")
    counted <- matrix(0, 52, 3)
    all_records <- all_missing <- any_with_dose <- any_with_dose_and_death <- 0
    repeat {
      lines <- readLines(con, n = 50000L, warn = FALSE)
      if (!length(lines)) break
      t <- read.csv(text = paste(c(header, lines), collapse = "\n"), colClasses = classes,
                    check.names = FALSE, na.strings = character(), comment.char = "")
      stopifnot(nrow(t) == length(lines), all(t$Infekce == "" | grepl("^[1-9][0-9]*$", t$Infekce)))
      t <- t[t$Infekce %in% c("", "1"), ]
      unknown <- t$RokNarozeni %in% c("", "-")
      has_dose <- Reduce(`|`, lapply(t[dose_cols], nzchar))
      i <- match(t$DatumUmrtiLPZ, sub("-W", "-", weeks))
      stopifnot(all(!startsWith(t$DatumUmrtiLPZ, "2023-") | !is.na(i)))
      dated <- !is.na(i)
      counted <- counted + cbind(tabulate(i[dated], 52), tabulate(i[dated & unknown], 52),
                                  tabulate(i[dated & unknown & has_dose], 52))
      all_records <- all_records + nrow(t)
      all_missing <- all_missing + sum(unknown)
      any_with_dose <- any_with_dose + sum(unknown & has_dose)
      any_with_dose_and_death <- any_with_dose_and_death +
        sum(unknown & has_dose & (nzchar(t$DatumUmrtiLPZ) | nzchar(t$Umrti)))
    }
    equal(as.vector(counted), as.vector(as.matrix(lpz[match(weeks, lpz$week), 2:4])))
    equal(c(all_records, all_missing, any_with_dose, any_with_dose_and_death),
          unlist(totals[c("records", "birth_missing", "birth_missing_with_dose","birth_missing_with_dose_and_death")]))
    message("Raw November recount agrees in all 52 weeks and on the release-wide checks.")
  }

  ages <- read_input("12_compare_asmr_direct_vs_eurostat", "age_week_cells.csv")
  ages <- ages[ages$release == "november" & ages$age %in% five_year_ages & ages$week %in% weeks, ]
  base <- read_input("14_november_mortality_excess_and_trends", "weekly_five_year_components.csv")
  base <- base[base$age %in% five_year_ages & base$week %in% weeks, ]
  for (t in list(ages, base))
    stopifnot(all(table(factor(t$week, weeks), factor(t$age, five_year_ages)) == 1L))
  x <- merge(ages, base, by = c("week", "age", "week_start"), suffixes = c("", "_step14"))
  stopifnot(nrow(x) == 260L, all(is.finite(x$baseline_deaths_per_person_day) & x$baseline_deaths_per_person_day > 0))
  for (field in c("deaths", "days")) {
    value <- function(arm) x[[paste(arm, field, sep = "_")]]
    equal(value("subtraction_no_dose"), value("national") - value("vaccinated"))
    equal(value("release_total"), value("direct_no_dose") + value("vaccinated"))
    equal(value("subtraction_no_dose"), value("no_dose"))
    equal(value("vaccinated"), x[[paste0("vaccinated_", field, "_step14")]])
    equal(value("national"), x[[paste0("national_", field, "_step14")]])
  }
  arm_row <- function(arm) {
    days <- x[[paste0(arm, "_days")]]
    observed <- x[[paste0(arm, "_deaths")]]
    expected <- days * x$baseline_deaths_per_person_day
    data.frame(series = arm, allocated_deaths = sum(observed), person_years = sum(days)/365.25,
               seasonal_expected = sum(expected), excess = sum(observed - expected))
  }
  arms <- c("direct_no_dose", "subtraction_no_dose", "vaccinated", "release_total", "national")
  compare <- do.call(rbind, lapply(arms, arm_row))
  rownames(compare) <- compare$series
  equal(compare["subtraction_no_dose","seasonal_expected"], sum(x$no_dose_expected))
  equal(compare["vaccinated","seasonal_expected"], sum(x$vaccinated_expected))
  # Cross-check direct counts and time against the unrounded T06–T09 inputs.
  t09 <- read_input("09_compare_annual_mortality_2022_2023", "annual_age_contributions.csv")
  t09 <- subset(t09, year == 2023 & min_age == 15 & age %in% five_year_ages)
  check_age <- function(arm, source, specification, status_value) {
    a <- subset(t09, release == source & spec == specification & status == status_value)
    stopifnot(nrow(a) == 5L, setequal(a$age, five_year_ages))
    for (age0 in five_year_ages) {
      idx <- x$age == age0; band <- a[a$age == age0, ]
      equal(sum(x[[paste0(arm,"_deaths")]][idx]), band$deaths)
      equal(sum(x[[paste0(arm,"_days")]][idx]), band$person_days, tolerance = .001)
    }
  }
  check_age("direct_no_dose", "november", "primary", "no_dose")
  check_age("vaccinated", "november", "primary", "vaccinated")
  check_age("release_total", "november", "primary", "total")
  check_age("national", "Eurostat", "national", "total")
  direct <- compare["direct_no_dose", ]; residual <- compare["subtraction_no_dose", ]
  delta <- residual
  delta$series <- "change_from_known_band_to_residual"
  cols <- c("allocated_deaths", "person_years", "seasonal_expected", "excess")
  delta[cols] <- residual[cols] - direct[cols]
  equal(delta$excess, delta$allocated_deaths - delta$seasonal_expected)
  compare <- rbind(compare, delta)
  audit <- data.frame(iso_year = 2023, lpz_all_ages = sum(lpz$deaths),
                      lpz_missing_birth = sum(lpz$missing_birth_deaths),
                      missing_birth_with_recorded_dose_2023 = sum(lpz$missing_with_recorded_dose),
                      eurostat_all_ages = sum(lpz$eurostat_all_ages),
                      missing_birth_with_dose_full_release = totals$birth_missing_with_dose,
                      missing_birth_with_dose_and_death_full_release = totals$birth_missing_with_dose_and_death)
  # Publish CSVs and copy-ready text only after completing all checks.
  out <- file.path(root, "data", "analysis", "18_verify_younger_no_dose_excess")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  temp <- tempfile(".building-", tmpdir = out)
  stopifnot(dir.create(temp))
  on.exit(unlink(temp, recursive = TRUE), add = TRUE)
  write_out <- function(d, name) write.csv(d, file.path(temp, name), row.names = FALSE)
  write_out(compare, "excess_decomposition.csv")
  write_out(audit, "missing_band_check.csv")
  write_out(lpz, "lpz_weekly_2023.csv")
  cell <- data.frame(x[c("week","age","baseline_deaths_per_person_day","direct_no_dose_days","direct_no_dose_deaths",
                         "subtraction_no_dose_days","subtraction_no_dose_deaths")],
                     direct_expected = x$direct_no_dose_days * x$baseline_deaths_per_person_day,
                     residual_expected = x$no_dose_expected)
  write_out(cell, "age_week_check.csv")
  write_out(inputs, "source_checksums.csv")
  fmt <- function(x, digits = 1) formatC(x, digits = digits, format = "f", big.mark = ",")
  pct <- 100 * delta$excess/residual$excess
  vacc <- compare["vaccinated", ]; total <- compare["release_total", ]; nat <- compare["national", ]
  paragraphs <- c(
    paste0("At ages 25–49, Eurostat counted ", fmt(nat$allocated_deaths, 0),
      " resident deaths in the 52 ISO weeks of 2023. Using November records with a known birth band, we assign ",
      fmt(total$allocated_deaths), " deaths to those ages (", fmt(vacc$allocated_deaths),
      " after a first dose and ", fmt(direct$allocated_deaths), " before any recorded dose). ",
      "The fractional counts reflect how we estimate attained age. With the vaccinated count held fixed, ",
      "subtracting it from Eurostat’s total gives ", fmt(residual$allocated_deaths),
      " no-dose deaths, ", fmt(delta$allocated_deaths), " more than the known-band estimate."),
    paste0("The subtraction also adds person-time. With known birth bands alone, the no-dose group contributed ",
      fmt(direct$person_years, 0), " person-years. Applying the 2015–2019 seasonal rates for each week and five-year age group gives ",
      fmt(direct$seasonal_expected), " expected deaths and an excess of ", fmt(direct$excess),
      ". On the resident basis, the corresponding figures are ", fmt(residual$person_years, 0),
      " person-years, ", fmt(residual$seasonal_expected), " expected deaths and an excess of ", fmt(residual$excess),
      ". Moving to the resident basis therefore adds ", fmt(delta$excess), " to the excess (", fmt(pct),
      "% of the resident-based excess)."),
    paste0("Across all ages, November contains ", fmt(audit$lpz_missing_birth, 0),
      " death-certificate records dated in ISO 2023 with no birth band. ",
      if (audit$missing_birth_with_recorded_dose_2023 == 0) "None has a recorded dose. " else
        paste0("The number with a recorded dose is ", fmt(audit$missing_birth_with_recorded_dose_2023, 0), ". "),
      "Their ages are unknown, so we cannot count how many occurred at ages 25–49. ",
      "The ", fmt(delta$allocated_deaths), " difference compares totals from two sources. ",
      "It could reflect missing birth bands, uncertainty about attained age, or differences in who each source covers. ",
      "It cannot tell us how many vaccination histories were missed. A resident who arrived from abroad could have received a dose elsewhere or could have remained unvaccinated."))
  guide <- c("<!doctype html><html lang='en'><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'><title>Section 7 · The no-dose excess at ages 25–49</title>",
    "<style>body{font:18px/1.55 Georgia,serif;max-width:48rem;margin:2rem auto;padding:0 1rem;color:#1d3035}aside,h1,h2{font-family:system-ui,sans-serif}aside{padding:.8rem;background:#eef3f5}h2{font-size:1.05rem}</style></head><body><h1>Section 7 · The no-dose excess at ages 25–49</h1>",
    "<aside>Insert the following three paragraphs immediately after the paragraph ending “Errors in the estimated number of vaccinated residents would also affect the rates” and its existing note 27, before “What the death certificates show”.</aside>",
    paste0("<p>", paragraphs, "</p>"),
    "<h2>Optional addition to existing note 27</h2>",
    paste0("<p>ISO 2023 runs from 2 January through 31 December. November contains ",fmt(audit$lpz_all_ages,0),
       " LPZ deaths at all ages for those weeks, compared with ",fmt(audit$eurostat_all_ages,0),
       " resident deaths in Eurostat. We summed the unrounded estimates behind Tables T06 and T07. The expected counts use the same pre-pandemic resident rates for both estimates, weighted by each group’s person-days in each age band and week.</p></body></html>"))
  writeLines(enc2utf8(guide), file.path(temp, "section7_known_band_insert.html"), useBytes = TRUE)
  files <- sort(list.files(temp))
  manifest <- data.frame(file = files, sha256 = unname(vapply(file.path(temp, files), sha, "")))
  write_out(manifest, "outputs.csv")
  dest <- file.path(out, substr(sha(file.path(temp, "outputs.csv")), 1, 20))
  if (!dir.exists(dest) && !file.rename(temp, dest) && !dir.exists(dest)) stop("Could not commit ", dest)
  equal_hashes <- unname(vapply(file.path(dest, manifest$file), sha, ""))
  stopifnot(identical(equal_hashes, manifest$sha256), identical(csv(file.path(dest, "outputs.csv")), manifest))
  message("Verified ", dest)
  invisible(dest)
}
verify_younger_excess()
