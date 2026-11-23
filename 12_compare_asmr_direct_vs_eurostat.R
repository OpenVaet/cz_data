# Czechia | Weekly all-cause ASMR by recorded vaccination status, ages 15+
# RStudio: open the Czech project and source("12_compare_asmr_direct_vs_eurostat.R")
# Shell:   Rscript 12_compare_asmr_direct_vs_eurostat.R
# Inputs: completed step 07 in data/analysis/ or results/ (also in the supplied ZIP).
# Optional: CZ_PROJECT_ROOT, CZ_STEP07_DIR (a specific run), CZ_MANIFEST.
# Outputs: data/analysis/12_compare_asmr_direct_vs_eurostat/<run>/.
# Uses base R and SHA-256 via digest (already used in step 07), tools or sha256sum.

compare_asmr_direct_vs_eurostat <- function(root = Sys.getenv("CZ_PROJECT_ROOT", ".")) {
  root <- normalizePath(root, mustWork = TRUE)
  csv <- function(path) read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  sha256 <- function(path) {
    if (requireNamespace("digest", quietly = TRUE))
      return(digest::digest(file = path, algo = "sha256", serialize = FALSE))
    if ("sha256sum" %in% getNamespaceExports("tools"))
      return(unname(getExportedValue("tools", "sha256sum")(path)))
    if (nzchar(Sys.which("sha256sum"))) {
      x <- system2("sha256sum", shQuote(path), stdout = TRUE)
      if (length(x) == 1L && grepl("^[0-9a-f]{64} ", x)) return(substr(x, 1L, 64L))
    }
    stop("Install the R package 'digest' for SHA-256 verification.")
  }
  verify <- function(dir, files) {
    if (anyNA(files$sha256) || anyDuplicated(files$file)) stop("Invalid file inventory: ", dir)
    for (i in seq_len(nrow(files))) {
      path <- file.path(dir, files$file[i])
      if (!file.exists(path) || sha256(path) != files$sha256[i]) stop("Missing or changed file: ", path)
    }
  }

  required <- c("input_files.csv", "weekly_age_cells.csv", "weekly_rates.csv", "esp2013_weights.csv")
  step <- "07_build_weekly_mortality_rates"
  pin <- Sys.getenv("CZ_STEP07_DIR")
  paths <- file.path(root, c("data/analysis", "results"), step)
  dirs <- if (nzchar(pin)) pin else unique(c(paths, unlist(lapply(paths, list.dirs, recursive = FALSE))))
  dirs <- dirs[file.exists(file.path(dirs, "outputs.csv"))]
  dirs <- dirs[order(file.info(file.path(dirs, "outputs.csv"))$mtime, decreasing = TRUE)]
  manifest <- Sys.getenv("CZ_MANIFEST")
  ids <- c("march", "november", "eurostat_population", "eurostat_deaths")
  if (nzchar(manifest)) {
    m <- csv(manifest)
    idx <- match(ids, m$id)
    stopifnot(!anyDuplicated(m$id), !anyNA(idx), all(m$status[idx] == "ok"), !anyNA(m$sha256[idx]))
    selected <- m$sha256[idx]
  }
  src <- NULL
  for (d in dirs) {
    inv <- csv(file.path(d, "outputs.csv"))
    if (!all(required %in% inv$file)) next
    used <- inv[match(required, inv$file), ]
    verify(d, used[used$file == "input_files.csv", , drop = FALSE])
    identity <- csv(file.path(d, "input_files.csv"))
    if (nzchar(manifest) && !identical(identity$sha256[match(ids, identity$id)], selected)) next
    verify(d, used[used$file != "input_files.csv", , drop = FALSE])
    src <- d
    break
  }
  if (is.null(src)) stop("Cannot find matching step 07 outputs. Run step 07, extract the supplied ZIP, or set CZ_STEP07_DIR.")

  base <- file.path(root, "data", "analysis", "12_compare_asmr_direct_vs_eurostat")
  dir.create(base, showWarnings = FALSE, recursive = TRUE)
  stage <- tempfile(".building-", tmpdir = base)
  if (!dir.create(stage)) stop("Cannot create ", stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  write_csv <- function(x, name) write.csv(x, file.path(stage, name), row.names = FALSE, na = "")
  key <- file.path(stage, "signature")
  writeLines(c(deparse(body(compare_asmr_direct_vs_eurostat), width.cutoff = 120L), used$sha256), key)
  dest <- file.path(base, substr(sha256(key), 1L, 20L))
  unlink(key)
  if (file.exists(file.path(dest, "outputs.csv"))) {
    verify(dest, csv(file.path(dest, "outputs.csv")))
    message("Verified ", dest)
    return(invisible(dest))
  }
  write_csv(used, "step07_inputs.csv")
  weight <- subset(csv(file.path(src, "esp2013_weights.csv")), age >= 15)
  ages <- seq(15L, 90L, 5L) # 90 is open-ended
  stopifnot(identical(as.integer(weight$age), ages), all(weight$standard_n > 0),
            sum(weight$standard_n) == 84000)
  w <- weight$standard_n / sum(weight$standard_n)

  raw <- csv(file.path(src, "weekly_age_cells.csv"))
  raw <- raw[raw$age >= 15 & raw$spec %in% c("national", "primary"), ]
  keys <- c("week_start", "week", "age")
  group_cells <- function(source, spec, status, label) {
    keep <- raw$release == source & raw$spec == spec & raw$status == status
    z <- raw[keep, c(keys, "standard_n", "deaths", "person_days")]
    stopifnot(nrow(z) > 0, !anyNA(z), !anyDuplicated(z[keys]),
              all(z$deaths >= 0), all(z$person_days >= 0),
              all(z$standard_n == weight$standard_n[match(z$age, ages)]))
    z$standard_n <- NULL
    names(z)[match(c("deaths", "person_days"), names(z))] <- paste0(label, c("_deaths", "_days"))
    z
  }
  national <- group_cells("Eurostat", "national", "total", "national")
  releases <- c("march", "november")
  release_end <- c(march = "2022-12-19", november = "2023-12-25")
  measures <- c("vaccinated", "direct_no_dose", "subtraction_no_dose", "release_total", "national")
  count_cols <- as.vector(outer(measures, c("_deaths", "_days"), paste0))

  # Join full age-by-week grids before calculating complements. Counts and
  # person-time add within age/week. Separately standardized rates do not.
  make_cells <- function(rel) {
    dates <- seq(as.Date("2020-03-02"), as.Date(release_end[[rel]]), by = "week")
    grid <- expand.grid(week_start = as.character(dates), age = ages, stringsAsFactors = FALSE)
    nat <- national[national$week_start %in% grid$week_start, ]
    x <- merge(grid, nat, by = c("week_start", "age"), all.x = TRUE)
    for (status in c("vaccinated", "no_dose", "total")) {
      label <- switch(status, vaccinated = "vaccinated", no_dose = "direct_no_dose", total = "release_total")
      s <- group_cells(rel, "primary", status, label)
      if (any(!s$week_start %in% grid$week_start)) stop("Unexpected release coverage: ", rel)
      x <- merge(x, s, by = keys, all.x = TRUE)
    }
    stopifnot(nrow(x) == nrow(grid), !anyNA(x), !anyDuplicated(x[c("week_start", "age")]),
              length(unique(x$week)) == length(dates))
    x$subtraction_no_dose_deaths <- x$national_deaths - x$vaccinated_deaths
    x$subtraction_no_dose_days <- x$national_days - x$vaccinated_days
    # Signed residual deaths are retained even when a modelled age allocation
    # slightly exceeds the reported national cell. Never clip or reassign them.
    x$negative_subtraction_deaths <- x$subtraction_no_dose_deaths < -1e-9
    x$nonpositive_subtraction_days <- x$subtraction_no_dose_days <= 0
    x$release <- rel
    x$esp_weight_15plus <- w[match(x$age, ages)]
    for (suffix in c("_deaths", "_days")) {
      error <- x[[paste0("release_total", suffix)]] - x[[paste0("direct_no_dose", suffix)]] -
        x[[paste0("vaccinated", suffix)]]
      stopifnot(max(abs(error)) < 1e-6)
    }
    x[order(x$week_start, x$age), c("release", keys, "esp_weight_15plus", count_cols,
                                    "negative_subtraction_deaths", "nonpositive_subtraction_days")]
  }
  cells <- do.call(rbind, lapply(releases, make_cells))
  write_csv(cells, "age_week_cells.csv")

  # Pool deaths and days in each attained-age stratum over the chosen weeks,
  # then standardize. A stratum without person-time makes the ASMR undefined.
  summarize <- function(x) {
    a <- rowsum(as.matrix(x[count_cols]), x$age, reorder = TRUE)
    a <- a[as.character(ages), , drop = FALSE]
    rates <- vapply(measures, function(m) {
      days <- a[, paste0(m, "_days")]
      if (any(days <= 0)) return(NA_real_)
      sum(w * a[, paste0(m, "_deaths")] / days) * 365.25 * 1e5
    }, 0.0)
    ratios <- if (is.finite(rates["vaccinated"]) && rates["vaccinated"] > 0)
      unname(rates[c("direct_no_dose", "subtraction_no_dose")] / rates["vaccinated"]) else rep(NA_real_, 2L)
    # A crude share of time at ages 15+. Use each method's own population,
    # pooling the actual person-days across ages before taking the ratio.
    vaccinated_days <- sum(a[, "vaccinated_days"])
    total_days <- colSums(a[, c("release_total_days", "national_days"), drop = FALSE])
    stopifnot(all(is.finite(total_days)), all(total_days > 0))
    coverage_pct <- 100 * vaccinated_days / total_days
    data.frame(release = x$release[1L], start_date = x$week_start[1L],
               first_week = x$week[1L], last_week = tail(x$week, 1L),
               weeks = length(unique(x$week)),
               as.list(setNames(rates, paste0(measures, "_asmr"))),
               direct_no_dose_to_vaccinated_ratio = ratios[1],
               subtraction_no_dose_to_vaccinated_ratio = ratios[2],
               direct_vaccinated_pct = unname(coverage_pct["release_total_days"]),
               subtraction_vaccinated_pct = unname(coverage_pct["national_days"]),
               negative_death_cells = sum(x$negative_subtraction_deaths),
               nonpositive_subtraction_time_cells = sum(x$nonpositive_subtraction_days))
  }
  week_groups <- split(cells, list(cells$week_start, cells$release), drop = TRUE)
  weekly <- do.call(rbind, lapply(week_groups, summarize))
  weekly <- weekly[order(weekly$release, weekly$start_date), ]
  # Independently compare every non-subtraction weekly series against step 07,
  # including the pre-rollout weeks where vaccinated ASMR must be NA.
  official <- subset(csv(file.path(src, "weekly_rates.csv")),
                     min_age == 15 & spec %in% c("primary", "national"))
  check_same <- function(a, b) {
    stopifnot(identical(is.na(a), is.na(b)), all(abs(a[!is.na(a)] - b[!is.na(b)]) < 1e-7))
  }
  for (rel in releases) {
    x <- weekly[weekly$release == rel, ]
    for (m in c("vaccinated", "direct_no_dose", "release_total", "national")) {
      source <- if (m == "national") "Eurostat" else rel
      status <- switch(m, vaccinated = "vaccinated", direct_no_dose = "no_dose", "total")
      z <- official[official$release == source & official$status == status, ]
      idx <- match(x$first_week, z$week)
      stopifnot(!anyNA(idx), !anyDuplicated(z$week))
      check_same(x[[paste0(m, "_asmr")]], z$asmr[idx])
    }
  }
  write_csv(weekly, "weekly_asmr.csv")

  periods <- data.frame(period = c("Delta", "Summer 2022", "ISO year 2022", "ISO year 2023"),
                        first = c("2021-W47","2022-W23","2022-W01","2023-W01"),
                        last = c("2021-W50","2022-W35","2022-W52","2023-W52"),
                        weeks = c(4L, 13L, 52L, 52L))
  pooled <- list()
  for (i in seq_len(nrow(periods))) {
    p <- periods[i, ]
    for (rel in releases) {
      x <- cells[cells$release == rel & cells$week >= p$first & cells$week <= p$last, ]
      if (length(unique(x$week)) != p$weeks) next # partial March years do not qualify
      pooled[[length(pooled) + 1L]] <- cbind(period = p$period, summarize(x))
    }
  }
  period_rates <- do.call(rbind, pooled)
  write_csv(period_rates, "pooled_period_asmr.csv")
  annual <- subset(period_rates, release == "november" & period %in% c("ISO year 2022", "ISO year 2023"))
  rate_cols <- paste0(measures, "_asmr")
  get_year <- function(period) as.numeric(unlist(annual[annual$period == period, rate_cols], use.names = FALSE))
  annual_change <- data.frame(series = measures, asmr_2022 = get_year("ISO year 2022"),
                              asmr_2023 = get_year("ISO year 2023"))
  annual_change$change_pct <- 100 * (annual_change$asmr_2023 / annual_change$asmr_2022 - 1)
  write_csv(annual_change, "november_annual_change.csv")
  flags <- cells$negative_subtraction_deaths | cells$nonpositive_subtraction_days
  write_csv(cells[flags, ], "flagged_age_week_cells.csv")

  # Three curves per panel, on the same date and rate axes across the figure.
  # The direct total is the eligible release population with known age; the
  # subtraction total is the national rate. Both use the same vaccinated curve.
  draw <- function() {
    png(file.path(stage, "weekly_asmr_direct_vs_eurostat.png"), width = 16, height = 10,
        units = "in", res = 160, bg = "white")
    on.exit(dev.off())
    layout(rbind(c(1,1), c(2,3), c(4,5), c(6,6)), heights = c(1.12, 3.5, 3.5, .92))
    ink <- "#152e3b"; grey <- "#475e6a"; grid <- "#e0e7e9"
    cols <- c("#b34b0d", "#145780", "#237562")
    coverage_fill <- "#e4dcef"; coverage_edge <- "#877099"
    par(cex = .94, mex = .85, las = 1, family = "sans", col.axis = grey,
        fg = ink, xaxs = "i", yaxs = "i")
    blank <- function() { plot.new(); plot.window(c(0,1), c(0,1)) }
    fmt <- function(x) format(round(x), trim = TRUE, big.mark = ",", scientific = FALSE)
    par(mar = c(0, 1.2, 0, 1.2))
    blank()
    text(.02, .78, "Weekly all-cause mortality by recorded vaccination status",
         adj = 0, font = 2, cex = 1.75, col = ink)
    text(.02, .41, "Czechia  |  Ages 15+  |  ESP 2013 age-standardized deaths per 100,000 person-years",
         adj = 0, cex = 1.08, col = grey)
    positions <- c(.02, .22, .47)
    for (k in 1:3) {
      segments(positions[k], .09, positions[k] + .030, .09, col = cols[k], lwd = 2.5,
               lty = c(1,2,3)[k])
      text(positions[k] + .038, .09,
           c("No recorded dose", "At least one recorded dose", "Total population for the method")[k],
           adj = 0, col = ink, cex = 1)
    }
    rect(.74, .035, .77, .145, col = coverage_fill, border = coverage_edge)
    text(.778, .09, "Vaccinated share (right axis)", adj = 0, col = ink, cex = 1)
    xr <- as.numeric(range(as.Date(weekly$start_date))) + c(-8, 8)
    ticks <- seq(as.Date("2020-07-01"), max(as.Date(weekly$start_date)), by = "6 months")
    dates_label <- paste(ifelse(format(ticks, "%m") == "01", "Jan", "Jul"), format(ticks, "%y"))
    ymax <- max(pretty(c(0, max(as.matrix(weekly[rate_cols]), na.rm = TRUE)), n = 6))
    ytick <- pretty(c(0, ymax), n = 6)
    # Common percentage scale for all four panels. Preserve any out-of-range
    # resident coverage should a future input exceed its benchmark.
    pct_top <- 25 * ceiling(max(100, weekly$direct_vaccinated_pct,
                                weekly$subtraction_vaccinated_pct) / 25)
    pct_ticks <- seq(0, pct_top, by = 25)
    for (method in c("direct", "subtraction")) {
      for (rel in releases) {
        x <- weekly[weekly$release == rel, ]
        dates <- as.numeric(as.Date(x$start_date))
        series <- if (method == "direct") c("direct_no_dose_asmr", "vaccinated_asmr", "release_total_asmr") else
          c("subtraction_no_dose_asmr", "vaccinated_asmr", "national_asmr")
        par(mar = c(2.0, 4.5, 3.5, 3.8))
        plot.new(); plot.window(xr, c(0, ymax))
        if (rel == "march") rect(max(dates) + 3.5, 0, xr[2], ymax, col = "#f2f4f5", border = NA)
        coverage <- x[[paste0(method, "_vaccinated_pct")]] * ymax / pct_top
        polygon(c(dates[1], dates, tail(dates, 1L)), c(0, coverage, 0),
                col = coverage_fill, border = NA)
        abline(h = ytick, v = as.numeric(ticks), col = grid, lwd = .7)
        lines(dates, coverage, col = coverage_edge, lwd = 1)
        # Draw the no-dose line over the total. The curves coincide before rollout.
        for (j in c(3, 1, 2))
          lines(dates, x[[series[j]]], col = cols[j], lty = c(1,2,3)[j], lwd = c(1.8,1.7,2)[j])
        axis(2, at = ytick, labels = fmt(ytick), tick = FALSE, line = -.35, cex.axis = .84)
        axis(4, at = pct_ticks * ymax / pct_top, labels = paste0(pct_ticks, "%"),
             tick = FALSE, line = -.4, cex.axis = .80, col.axis = coverage_edge)
        axis(1, at = as.numeric(ticks), labels = dates_label, tick = FALSE, line = -.48, cex.axis = .82)
        title <- paste(if (rel == "march") "March FOI" else "November NZIP",
                       if (method == "direct") "Direct release rates" else "Eurostat subtraction", sep = "   |   ")
        mtext(title, side = 3, line = 1.85, adj = 0, font = 2, cex = 1.05, col = ink)
        peaks <- vapply(series[1:2], function(s) which.max(x[[s]]), 1L)
        peak_values <- c(x[[series[1]]][peaks[1]], x[[series[2]]][peaks[2]])
        points(dates[peaks], peak_values, pch = 16, cex = .65, col = cols[1:2])
        mtext(sprintf("No-dose peak %s (%s)     Vaccinated peak %s (%s)",
                      fmt(peak_values[1]), x$first_week[peaks[1]], fmt(peak_values[2]), x$first_week[peaks[2]]),
              side = 3, line = .42, adj = 0, cex = .76, col = grey)
        if (rel == "march") text(mean(c(max(dates), xr[2])), ymax * .51, "After March\ncoverage",
                                  col = grey, cex = .76)
      }
    }
    par(mar = c(0, 1.2, 0, 1.2))
    blank()
    text(.02, .80, "Shading shows the weekly share of person-days at ages 15+ after a recorded first dose. The upper panels use eligible release person-days and the lower panels use estimated resident person-days.",
         adj = 0, cex = .85, col = ink)
    text(.02, .49, "Direct rates use records with known birth information. Subtraction estimates no-dose deaths and person-time as national totals less the vaccinated component in each age group.",
         adj = 0, cex = .85, col = ink)
    text(.02, .18, "Dots mark weekly peaks. Gaps indicate insufficient person-time for a standardized rate. Early vaccinated rates can fluctuate when few people have received their first dose.",
         adj = 0, cex = .85, col = grey)
  }
  draw()
  files <- sort(list.files(stage))
  write_csv(data.frame(file = files, sha256 = unname(vapply(file.path(stage, files), sha256, ""))), "outputs.csv")
  if (!file.rename(stage, dest)) {
    if (!file.exists(file.path(dest, "outputs.csv"))) stop("Cannot commit output directory: ", dest)
    verify(dest, csv(file.path(dest, "outputs.csv")))
  }
  message("Wrote ", dest)
  invisible(dest)
}
compare_asmr_direct_vs_eurostat()
