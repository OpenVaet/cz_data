# Weekly Czech all-cause mortality by attained age and recorded dose status
# RStudio: source("13_eurostat_mortality_by_age_group.R")
# Shell:   Rscript 13_eurostat_mortality_by_age_group.R
# Uses step 12 in data/analysis/ or results/. Set CZ_PROJECT_ROOT if needed or
# pin CZ_STEP12_DIR to the directory containing age_week_cells.csv.
# Reads Steps 03, 04 and 07 to verify the age prior and independent dose totals.
# For doses use the offline cohort counts in the ZIP, or the retained Step 02
# records referenced by Step 04. CZ_PREPARED_DIR can pin their directory.
# Output: data/analysis/13_eurostat_mortality_by_age_group/<run>/

eurostat_mortality_by_age_group <- function(root = Sys.getenv("CZ_PROJECT_ROOT", ".")) {
  root <- normalizePath(root, mustWork = TRUE)
  csv <- function(path) read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  hash <- function(path) {
    if (requireNamespace("digest", quietly = TRUE))
      return(digest::digest(file = path, algo = "sha256", serialize = FALSE))
    if ("sha256sum" %in% getNamespaceExports("tools"))
      return(unname(getExportedValue("tools", "sha256sum")(path)))
    if (nzchar(Sys.which("sha256sum"))) {
      ans <- system2("sha256sum", shQuote(path), stdout = TRUE)
      if (length(ans) == 1L && grepl("^[0-9a-f]{64} ", ans)) return(substr(ans, 1L, 64L))
    }
    stop("Install the R package 'digest' to verify the analysis files.")
  }
  verify <- function(dir, files) {
    stopifnot(nrow(files) > 0L, !anyNA(files), !anyDuplicated(files$file))
    for (j in seq_len(nrow(files))) {
      path <- file.path(dir, files$file[j])
      if (!file.exists(path) || hash(path) != files$sha256[j]) stop("Missing or changed file: ", path)
    }
  }
  # Follow Step 12's source fingerprints through Step 07. This keeps the
  # population prior and the independent Step 04 dose totals on the same releases.
  parent <- function(step, files, pin_name, source_hashes = character(), output_hashes = character()) {
    bases <- file.path(root, c("data/analysis", "results"), step)
    pin <- Sys.getenv(pin_name)
    dirs <- if (nzchar(pin)) pin else unique(c(bases, unlist(lapply(bases, list.dirs, recursive = FALSE))))
    dirs <- dirs[file.exists(file.path(dirs, "outputs.csv"))]
    dirs <- dirs[order(file.info(file.path(dirs, "outputs.csv"))$mtime, decreasing = TRUE)]
    for (d in dirs) {
      inv <- csv(file.path(d, "outputs.csv"))
      if (!all(files %in% inv$file)) next
      files_used <- inv[match(files, inv$file), ]
      if (length(output_hashes) && !all(names(output_hashes) %in% inv$file)) next
      if (length(output_hashes) && !all(output_hashes == inv$sha256[match(names(output_hashes), inv$file)])) next
      if (length(source_hashes)) {
        ids <- csv(file.path(d, "input_files.csv"))
        idx <- match(names(source_hashes), ids$id)
        if (anyNA(idx) || !all(source_hashes == ids$sha256[idx])) next
      }
      verify(d, files_used)
      return(list(dir = d, files = files_used))
    }
    stop("Missing matching ", step, " outputs. Set ", pin_name, " or extract the updated companion ZIP.")
  }
  s12 <- parent("12_compare_asmr_direct_vs_eurostat",
                c("age_week_cells.csv", "weekly_asmr.csv", "step07_inputs.csv"), "CZ_STEP12_DIR")
  src <- s12$dir
  used <- s12$files
  fp <- csv(file.path(src, "step07_inputs.csv"))
  s7 <- parent("07_build_weekly_mortality_rates", "input_files.csv", "CZ_STEP07_DIR",
               output_hashes = setNames(fp$sha256[match("input_files.csv", fp$file)], "input_files.csv"))
  ids <- csv(file.path(s7$dir, "input_files.csv"))
  ids <- setNames(ids$sha256, ids$id)
  s3 <- parent("03_census_ageing_migration", c("input_files.csv", "january_population_by_age.csv"),
               "CZ_STEP03_DIR", ids["eurostat_population"])
  s4 <- parent("04_compare_event_counts", c("input_files.csv", "weekly_counts.csv"),
               "CZ_STEP04_DIR", ids[c("march", "november")])
  ids04 <- csv(file.path(s4$dir, "input_files.csv"))
  # The offline bundle includes sufficient statistics by dose week, published
  # birth cohort and dose order. Otherwise read the prepared Step 02 records.
  compact <- file.path(root, "results", "13_dose_cohorts")
  have_cube <- file.exists(file.path(compact, "outputs.csv"))
  cube_hashes <- character()
  if (have_cube) {
    inv <- csv(file.path(compact, "outputs.csv"))
    idx <- match(c("dose_cohorts.csv", "sources.csv"), inv$file)
    stopifnot(!anyNA(idx))
    verify(compact, inv[idx, ])
    origin <- csv(file.path(compact, "sources.csv"))
    stopifnot(setequal(origin$id, c("march","november")), !anyDuplicated(origin$id))
    have_cube <- all(origin$sha256 == ids[origin$id])
    if (have_cube) cube_hashes <- inv$sha256[idx]
  }
  base <- file.path(root, "data", "analysis", "13_eurostat_mortality_by_age_group")
  dir.create(base, showWarnings = FALSE, recursive = TRUE)
  stage <- tempfile(".building-", tmpdir = base)
  if (!dir.create(stage)) stop("Cannot create ", stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  sig <- file.path(stage, "signature")
  writeLines(c(deparse(body(eurostat_mortality_by_age_group), width.cutoff = 120L), used$sha256,
               s7$files$sha256, s3$files$sha256, s4$files$sha256, cube_hashes), sig)
  dest <- file.path(base, substr(hash(sig), 1L, 20L))
  unlink(sig)
  if (file.exists(file.path(dest, "outputs.csv"))) {
    verify(dest, csv(file.path(dest, "outputs.csv")))
    message("Verified ", dest)
    return(invisible(dest))
  }
  save_csv <- function(x, f) write.csv(x, file.path(stage, f), row.names = FALSE, na = "")
  save_csv(used, "step12_inputs.csv")
  save_csv(rbind(data.frame(step="03", s3$files), data.frame(step="04", s4$files),
                 data.frame(step="07", s7$files)), "dose_source_inputs.csv")

  z <- csv(file.path(src, "age_week_cells.csv"))
  ref <- csv(file.path(src, "weekly_asmr.csv"))
  names(ref)[match("start_date", names(ref))] <- "week_start"
  ages <- seq(15L, 90L, 5L)
  groups <- c("15-24", "25-49", "50-59", "60-69", "70-79", "80+")
  releases <- c(march = "March FOI", november = "November NZIP")
  keys <- c("release", "week_start", "week")
  counts <- c("national_deaths", "vaccinated_deaths", "national_days", "vaccinated_days")
  stopifnot(!anyNA(z[c(keys, "age", "esp_weight_15plus", counts)]),
            !anyDuplicated(z[c("release", "week_start", "age")]),
            setequal(z$release, names(releases)), setequal(z$age, ages),
            all(z[counts] >= 0), all(z$national_days > 0))
  per_year <- 365.25 * 100000
  # Keep raw signed differences for the algebra and diagnostics. A negative
  # count cannot produce an interpretable group mortality rate. Leave that
  # line value undefined and mark the affected week. Zero deaths with positive
  # exposure give a genuine rate of zero.
  rates <- function(x) {
    x$no_dose_deaths <- x$national_deaths - x$vaccinated_deaths
    x$no_dose_days <- x$national_days - x$vaccinated_days
    x$vaccinated_pct <- 100 * x$vaccinated_days / x$national_days
    x$negative_no_dose_deaths <- x$no_dose_deaths < -1e-9
    x$nonpositive_no_dose_days <- x$no_dose_days <= 0
    x$vaccinated_rate <- ifelse(x$vaccinated_days > 0,
                                per_year * x$vaccinated_deaths / x$vaccinated_days, NA_real_)
    x$no_dose_rate <- ifelse(!x$negative_no_dose_deaths & !x$nonpositive_no_dose_days,
                             per_year * pmax(0, x$no_dose_deaths) / x$no_dose_days, NA_real_)
    x
  }
  band <- cut(z$age, breaks = c(15, 25, 50, 60, 70, 80, Inf), labels = groups, right = FALSE)
  # Sum the four counts within the reporting band BEFORE taking either rate
  # or coverage. There is no ESP reweighting within these broad age groups.
  wide <- aggregate(z[counts], z[c(keys)], FUN = sum) # all ages 15+, for reconciliation
  by_band <- aggregate(z[counts], c(z[keys], list(age_group = band)), FUN = sum)
  by_band <- rates(by_band)
  by_band <- by_band[order(by_band$release, by_band$week_start, by_band$age_group), ]
  fine <- z[c(keys, "age", counts)]
  fine$age_group <- ifelse(fine$age == 90L, "90+", paste0(fine$age, "-", fine$age + 4L))
  fine <- rates(fine)
  fine <- fine[order(fine$release, fine$week_start, fine$age), ]
  # Independently reconstruct step 12's ASMR from its component age-specific
  # rates, using signed residual deaths for this audit only.
  assert_close <- function(x, y, tol = 1e-6) {
    if (!identical(is.na(x), is.na(y)) || any(abs(x[!is.na(x)] - y[!is.na(y)]) > tol))
      stop("Reconciliation to step 12 failed.")
  }
  assert_close(z$national_deaths - z$vaccinated_deaths, z$subtraction_no_dose_deaths)
  assert_close(z$national_days - z$vaccinated_days, z$subtraction_no_dose_days)
  wk_key <- function(x) paste(x$release, x$week_start)
  stopifnot(!anyDuplicated(wk_key(ref)), setequal(wk_key(ref), wk_key(wide)))
  splits <- split(seq_len(nrow(z)), wk_key(z))
  check <- lapply(splits, function(i) {
    a <- z[i, ]
    stopifnot(nrow(a) == length(ages), setequal(a$age, ages), abs(sum(a$esp_weight_15plus) - 1) < 1e-9)
    standardize <- function(d, p) {
      if (any(p <= 0)) return(NA_real_)
      per_year * sum(a$esp_weight_15plus * d / p)
    }
    c(standardize(a$vaccinated_deaths, a$vaccinated_days),
      standardize(a$national_deaths - a$vaccinated_deaths, a$national_days - a$vaccinated_days))
  })
  checked <- do.call(rbind, check)[wk_key(ref), , drop = FALSE]
  assert_close(unname(checked[, 1]), ref$vaccinated_asmr)
  assert_close(unname(checked[, 2]), ref$subtraction_no_dose_asmr)
  back <- aggregate(by_band[counts], by_band[keys], FUN = sum)
  back <- back[match(wk_key(wide), wk_key(back)), ]
  for (col in counts) assert_close(back[[col]], wide[[col]])
  if ("subtraction_vaccinated_pct" %in% names(ref)) {
    idx <- match(wk_key(ref), wk_key(wide))
    assert_close(100 * wide$vaccinated_days[idx] / wide$national_days[idx], ref$subtraction_vaccinated_pct)
  }
  # Count administrations in the published order. Each of the seven date
  # columns contributes one event; a later dose need not have a first-dose date.
  dose_cohorts <- function(rel, weeks) {
    id <- paste0(rel, "_records.csv")
    entry <- ids04[ids04$id == id, ]
    stopifnot(nrow(entry) == 1L)
    pin <- Sys.getenv("CZ_PREPARED_DIR")
    path <- if (nzchar(pin)) file.path(pin, id) else file.path(root, entry$path)
    if (!file.exists(path) || hash(path) != entry$sha256)
      stop("Missing or changed Step 02 file: ", path, ". Use the corresponding prepared run or the offline ZIP.")
    message("Counting dated administrations in ", id)
    weeks <- as.Date(weeks)
    year <- as.integer(format(weeks + 3L, "%Y"))
    jan4 <- as.Date(paste0(year, "-01-04"))
    w1 <- jan4 - (as.integer(jan4) + 3L) %% 7L
    tags <- sprintf("%04d-%02d", year, as.integer(weeks - w1) %/% 7L + 1L)
    march <- rel == "march"
    dose <- if (march) paste0("Datum_", 1:7) else
      paste0("Datum_", c("Prvni","Druha","Treti","Ctvrta","Pata","Sesta","Sedma"), "_davka")
    birth <- if (march) "Rok_narozeni" else "RokNarozeni"
    # Prepared release tables have already excluded November repeat episodes.
    # The check also protects this scan against an incorrectly prepared table.
    needed <- c(birth, dose, if (!march) "Infekce")
    con <- file(path, open = "rt", encoding = "UTF-8")
    on.exit(close(con))
    fields <- names(read.csv(text = readLines(con, n = 1L), check.names = FALSE))
    if (!all(needed %in% fields)) stop("Missing dose columns in ", id)
    classes <- ifelse(fields %in% needed, "character", "NULL")
    all_days <- as.character(seq(min(weeks), max(weeks) + 6L, by = "day"))
    nw <- length(weeks)
    chunks <- list()
    repeat {
      # Read complete CSV records from the connection, including quoted fields.
      line <- readLines(con, n = 1L, warn = FALSE)
      if (!length(line)) break
      pushBack(line, con)
      x <- read.csv(con, header = FALSE, col.names = fields, colClasses = classes,
                    nrows = 150000L, check.names = FALSE, comment.char = "")
      if (!march && any(!is.na(x$Infekce) & !x$Infekce %in% c("", "1"))) stop("Repeat episode in ", id)
      b <- x[[birth]]
      unknown <- is.na(b) | b %in% c("", "-")
      fmt <- if (march) "^[0-9]{4}$" else "^[0-9]{4}-[0-9]{4}$"
      if (any(!grepl(fmt, b[!unknown]))) stop("Invalid birth field in ", id)
      if (!march && any(as.integer(substr(b[!unknown],6,9)) - as.integer(substr(b[!unknown],1,4)) != 4L))
        stop("Invalid five-year birth band.")
      low <- suppressWarnings(as.integer(substr(b, 1, 4)))
      low[unknown] <- -1L
      cohorts <- sort(unique(low))
      ix <- match(low, cohorts) - 1L
      as_grid <- function(j) {
        v <- x[[dose[j]]]
        which_week <- if (march) (match(v, all_days) - 1L) %/% 7L + 1L else match(v, tags)
        tabulate(which_week + nw * ix, nbins = nw * length(cohorts))
      }
      first <- as_grid(1L)
      later <- Reduce(`+`, lapply(2:7, as_grid))
      keep <- which(first + later > 0)
      if (length(keep)) chunks[[length(chunks) + 1L]] <- data.frame(
          week_start = as.character(weeks[(keep - 1L) %% nw + 1L]),
          birth = cohorts[(keep - 1L) %/% nw + 1L], first = first[keep], later = later[keep])
      if (nrow(x) < 150000L) break
    }
    x <- do.call(rbind, chunks)
    a <- aggregate(x[c("first", "later")], x[c("week_start", "birth")], sum)
    a$birth[a$birth == -1L] <- NA_integer_
    data.frame(release = rel, a)
  }

  cube <- if (have_cube) csv(file.path(compact, "dose_cohorts.csv")) else
    do.call(rbind, lapply(names(releases), function(rel)
      dose_cohorts(rel, sort(unique(z$week_start[z$release == rel])))))
  stopifnot(!anyDuplicated(cube[c("release", "week_start", "birth")]),
            all(cube$first >= 0 & cube$later >= 0), all(cube$first == floor(cube$first)),
            all(cube$later == floor(cube$later)), setequal(unique(cube$release), names(releases)))
  pop <- csv(file.path(s3$dir, "january_population_by_age.csv"))
  pop$attained_age[pop$age_code == "Y_OPEN"] <- 100L
  jan <- subset(pop, year == 2021 & !is.na(attained_age))
  babies <- subset(pop, attained_age == 0)
  stopifnot(setequal(jan$attained_age, 0:100), !anyDuplicated(jan$attained_age), !anyDuplicated(babies$year))
  prior <- function(yob) {
    p <- jan$residents[match(2020L - yob, jan$attained_age)]
    future <- yob > 2020L
    p[future] <- babies$residents[match(pmin(yob[future] + 1L, max(babies$year)), babies$year)]
    p[is.na(p) | p <= 0] <- 1
    p
  }
  control <- csv(file.path(s4$dir, "weekly_counts.csv"))
  allocate_doses <- function(rel) {
    g <- cube[cube$release == rel, ]
    cal <- unique(z[z$release == rel, c("week_start","week")])
    cal <- cal[order(cal$week_start), ]
    nw <- nrow(cal)
    iw <- match(g$week_start, cal$week_start)
    stopifnot(!anyNA(iw))
    known <- which(!is.na(g$birth))
    width <- if (rel == "march") 1L else 5L
    i <- rep(known, each = width)
    yob <- g$birth[i] + rep(0:(width - 1L), length(known))
    mid <- as.Date(cal$week_start) + 3L
    yr <- as.integer(format(mid, "%Y"))
    jan1 <- as.Date(paste0(yr, "-01-01"))
    f <- as.numeric(mid - jan1) / as.numeric(as.Date(paste0(yr + 1L, "-01-01")) - jan1)
    a <- yr[iw[i]] - yob
    attained <- c(a - 1L, a)
    mass <- c(prior(yob) * (1 - f[iw[i]]), prior(yob) * f[iw[i]])
    i <- rep(i, 2L)
    valid <- attained >= 0 & mass > 0
    i <- i[valid]; attained <- attained[valid]; mass <- mass[valid]
    totals <- rowsum(mass, i, reorder = FALSE)
    share <- mass / totals[match(i, as.integer(rownames(totals))), 1L]
    boundary <- setdiff(known, unique(i))
    if (length(boundary)) {
      stopifnot(all(g$birth[boundary] <= as.integer(format(mid[iw[boundary]] + 3L, "%Y"))))
      i <- c(i, boundary); attained <- c(attained, rep(0, length(boundary)))
      share <- c(share, rep(1, length(boundary)))
    }
    stopifnot(all(abs(rowsum(share, i) - 1) < 1e-10))
    band5 <- pmin(90L, 5L * (attained %/% 5L))
    # Use a separate slot for entirely missing birth information.
    unknown <- which(is.na(g$birth))
    all_i <- c(i, unknown)
    bands <- c(seq(0L,90L,5L), -1L)
    k <- iw[all_i] + nw * (match(c(band5, rep(-1L, length(unknown))), bands) - 1L)
    share <- c(share, rep(1, length(unknown)))
    count <- function(v) {
      s <- rowsum(v[all_i] * share, k, reorder = FALSE)
      ans <- numeric(nw * length(bands)); ans[as.integer(rownames(s))] <- s[, 1L]
      ans
    }
    doses <- data.frame(release = rel, cal[rep(seq_len(nw), times = length(bands)), ],
                        age = rep(bands, each = nw), first_doses = count(g$first), later_doses = count(g$later))
    doses$all_doses <- doses$first_doses + doses$later_doses
    total <- aggregate(doses[c("first_doses", "later_doses", "all_doses")], doses[keys], sum)
    for (event in c("First doses","All doses")) {
      official <- control[control$series == releases[[rel]] & control$event == event, ]
      j <- match(total$week_start, official$week_start)
      stopifnot(!anyNA(j), !anyDuplicated(official$week_start))
      n <- official$count[j]
      # Pre-rollout weeks precede the first observed dose; all other missing
      # reference values need investigation before drawing the chart.
      pre <- total$week_start < min(official$week_start[official$available])
      n[pre & is.na(n)] <- 0
      col <- if (event == "First doses") "first_doses" else "all_doses"
      stopifnot(!anyNA(n), all(abs(total[[col]] - n) < 1e-6))
      total[[paste0("step04_", col)]] <- n
    }
    list(counts = doses, reconciliation = total)
  }
  step_doses <- lapply(names(releases), allocate_doses)
  dc <- do.call(rbind, lapply(step_doses, `[[`, "counts"))
  dose_cols <- c("first_doses", "later_doses", "all_doses")
  save_csv(do.call(rbind, lapply(step_doses, `[[`, "reconciliation")), "weekly_all_age_dose_reconciliation.csv")
  dc$age_group <- as.character(cut(dc$age, c(-Inf,-.5,15,25,50,60,70,80,Inf), right=FALSE,
                              labels = c("Unknown","0-14",groups)))
  broad <- aggregate(dc[dose_cols], dc[c(keys, "age_group")], sum)
  save_csv(broad, "weekly_doses_all_age_groups.csv")
  by_band <- merge(by_band, broad, by = c(keys, "age_group"), all.x = TRUE, sort = FALSE)
  fine <- merge(fine, dc[c(keys, "age", dose_cols)], by = c(keys, "age"), all.x = TRUE, sort = FALSE)
  by_band <- by_band[order(by_band$release, by_band$week_start, match(by_band$age_group, groups)), ]
  fine <- fine[order(fine$release, fine$week_start, fine$age), ]
  stopifnot(!anyNA(by_band[dose_cols]), !anyNA(fine[dose_cols]))

  save_csv(by_band, "weekly_rates_by_age_group.csv")
  save_csv(fine, "weekly_rates_by_five_year_age.csv")

  draw <- function(rel) {
    png(file.path(stage, paste0(rel, "_weekly_mortality_by_age.png")), width = 15.2, height = 13.3,
        units = "in", res = 155, bg = "white")
    on.exit(dev.off())
    layout(rbind(c(1,1), c(2,4), c(3,5), c(6,8), c(7,9), c(10,12), c(11,13), c(14,14)),
           heights = c(1.35, rep(c(2.45, 1.17), 3), .88))
    ink <- "#152e3b"; grey <- "#465c68"; grid <- "#e0e7e9"
    orange <- "#b34b0d"; blue <- "#145780"; fill <- "#e4dcef"; edge <- "#877099"
    first_col <- "#237468"; later_col <- "#9ecac0"
    par(cex = .94, mex = .85, las = 1, family = "sans", fg = ink, col.axis = grey, xaxs = "i", yaxs = "i")
    blank <- function() { plot.new(); plot.window(c(0,1), c(0,1)) }
    par(mar = c(0, 1.4, 0, 1.4))
    blank()
    x <- by_band[by_band$release == rel, ]
    text(.02, .85, paste("Weekly all-cause mortality by age group  | ", releases[[rel]]),
         adj = 0, font = 2, cex = 1.62, col = ink)
    text(.02, .59, paste0("Czechia  |  ", x$week[1], " through ", tail(x$week, 1),
         "  |  Deaths per 100,000 person-years within each age and dose group"), adj = 0, cex = 1.04, col = grey)
    segments(.02, .35, .05, .35, col = orange, lwd = 2.4)
    text(.058, .35, "No recorded dose (Eurostat subtraction)", adj = 0, col = ink, cex = .94)
    segments(.37, .35, .40, .35, col = blue, lwd = 2.2, lty = 2)
    text(.408, .35, "At least one recorded dose", adj = 0, col = ink, cex = .94)
    rect(.67, .30, .70, .40, col = fill, border = edge)
    text(.708, .35, "Vaccinated share of age group (right axis)", adj = 0, col = ink, cex = .94)
    rect(.02, .06, .05, .16, col = first_col, border = NA)
    text(.058, .11, "First doses", adj = 0, col = ink, cex = .94)
    rect(.20, .06, .23, .16, col = later_col, border = NA)
    text(.238, .11, "Doses 2-7", adj = 0, col = ink, cex = .94)
    text(.37, .11, "Column height gives all doses administered that week.", adj = 0, col = grey, cex = .92)
    dates_all <- as.Date(x$week_start)
    ticks <- seq(as.Date("2020-07-01"), max(dates_all), by = "6 months")
    tick_labels <- paste(ifelse(format(ticks, "%m") == "01", "Jan", "Jul"), format(ticks, "%y"))
    max_pct <- ceiling(max(100, by_band$vaccinated_pct) / 25) * 25
    pct_ticks <- seq(0, max_pct, by = 25)
    for (group in groups) {
      a <- x[x$age_group == group, ]
      dates <- as.numeric(as.Date(a$week_start))
      # Both releases use the same rate-axis scale for a given age group.
      both <- by_band[by_band$age_group == group, ]
      upper <- max(1, both$no_dose_rate, both$vaccinated_rate, na.rm = TRUE)
      marks <- pretty(c(0, upper), n = 4)
      top <- max(marks)
      par(mar = c(.12, 4.9, 1.65, 3.35))
      plot.new(); plot.window(range(dates) + c(-4, 4), c(0, top))
      cov <- a$vaccinated_pct * top / max_pct
      polygon(c(dates[1], dates, tail(dates, 1)), c(0, cov, 0), col = fill, border = NA)
      abline(h = marks, v = as.numeric(ticks), col = grid, lwd = .65)
      lines(dates, cov, col = edge, lwd = .9)
      lines(dates, a$no_dose_rate, col = orange, lwd = 1.8)
      lines(dates, a$vaccinated_rate, col = blue, lwd = 1.7, lty = 2)
      bad <- a$negative_no_dose_deaths | a$nonpositive_no_dose_days
      if (any(bad)) points(dates[bad], rep(.018 * top, sum(bad)), pch = 4, col = orange, lwd = 1.5, cex = .71)
      axis(2, at = marks, labels = format(marks, big.mark = ",", scientific = FALSE, trim = TRUE),
           tick = FALSE, line = -.34, cex.axis = .82)
      axis(4, at = top * pct_ticks / max_pct, labels = paste0(pct_ticks, "%"), tick = FALSE,
           line = -.38, col.axis = edge, cex.axis = .76)
      mtext(paste("Ages", group), side = 3, line = .47, adj = 0, font = 2, cex = 1.04, col = ink)
      # A dedicated count axis keeps doses distinct from rates and percentages.
      par(mar = c(1.55, 4.9, .57, 3.35))
      bar_marks <- pretty(c(0, max(1, both$all_doses)), n = 3)
      plot.new(); plot.window(range(dates) + c(-4,4), c(0, max(bar_marks)))
      abline(h = bar_marks, v = as.numeric(ticks), col = grid, lwd = .65)
      rect(dates - 3, 0, dates + 3, a$first_doses, col = first_col, border = NA)
      rect(dates - 3, a$first_doses, dates + 3, a$all_doses, col = later_col, border = NA)
      labs <- ifelse(bar_marks >= 1000, paste0(format(bar_marks / 1000, trim=TRUE), "k"), bar_marks)
      axis(2, at = bar_marks, labels = labs, tick = FALSE, line = -.34, cex.axis = .72)
      mtext("Doses", side = 2, line = 3.25, las = 0, cex = .72, col = grey)
      axis(1, at = as.numeric(ticks), labels = tick_labels, tick = FALSE, line = -.50, cex.axis = .78)
    }
    par(mar = c(0, 1.4, 0, 1.4)); blank()
    text(.02, .73, "Shading shows the vaccinated share of resident person-time. Columns count administrations by estimated attained age in the dose week. Each later dose counts separately.",
         adj = 0, cex = .87, col = ink)
    note <- "A rate starts when its group has person-time. A few deaths can produce a large weekly rate early in rollout."
    if (any(x$negative_no_dose_deaths | x$nonpositive_no_dose_days))
      note <- paste(note, "An orange cross flags an undefined no-dose estimate.")
    text(.02, .27, note, adj = 0, cex = .84, col = grey)
  }
  for (rel in names(releases)) draw(rel)
  files <- sort(list.files(stage))
  save_csv(data.frame(file = files, sha256 = unname(vapply(file.path(stage, files), hash, ""))), "outputs.csv")
  if (!file.rename(stage, dest)) {
    if (!file.exists(file.path(dest, "outputs.csv"))) stop("Cannot commit output directory: ", dest)
    verify(dest, csv(file.path(dest, "outputs.csv")))
  }
  message("Wrote ", dest)
  invisible(dest)
}
eurostat_mortality_by_age_group()
