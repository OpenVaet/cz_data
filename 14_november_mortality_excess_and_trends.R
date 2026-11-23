# November NZIP: weekly all-cause mortality, doses, quarterly group size and
# excess deaths, plus a January 2021 to July 2023 linear mortality trend.
# RStudio: set the working directory to the project or extracted ZIP, then
# source("14_november_mortality_excess_and_trends.R"). Also runs with Rscript.
# Prerequisites in a full project: Step 13 with dose counts and its Step 03
# population input. The ZIP includes those inputs and the 2015-2019 Eurostat TSV.
november_excess_and_trends <- function(root = Sys.getenv("CZ_PROJECT_ROOT", ".")) {
  root <- normalizePath(root, mustWork = TRUE)
  csv <- function(f) read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  hash <- function(f) {
    if (requireNamespace("digest", quietly = TRUE))
      return(digest::digest(file = f, algo = "sha256", serialize = FALSE))
    if ("sha256sum" %in% getNamespaceExports("tools"))
      return(unname(getExportedValue("tools", "sha256sum")(f)))
    if (nzchar(Sys.which("sha256sum"))) {
      s <- system2("sha256sum", shQuote(f), stdout = TRUE)
      if (length(s) == 1L && grepl("^[[:xdigit:]]{64} ", s)) return(substr(s, 1, 64))
    }
    stop("Install the R package 'digest' for SHA-256 verification.")
  }
  source_dir <- function(step, files, pin, match_hash = NULL) {
    dirs <- file.path(root, c("data/analysis","results"), step)
    dirs <- unique(c(dirs, unlist(lapply(dirs, list.dirs, recursive = FALSE))))
    if (nzchar(Sys.getenv(pin))) dirs <- Sys.getenv(pin)
    dirs <- dirs[file.exists(file.path(dirs, "outputs.csv"))]
    dirs <- dirs[order(file.mtime(file.path(dirs, "outputs.csv")), decreasing = TRUE)]
    for (d in dirs) {
      inv <- csv(file.path(d, "outputs.csv"))
      if (anyDuplicated(inv$file) || !all(files %in% inv$file)) next
      h <- setNames(inv$sha256, inv$file)[files]
      if (length(match_hash) && !identical(unname(h[names(match_hash)]), unname(match_hash))) next
      paths <- file.path(d, files)
      if (!all(file.exists(paths)) || !all(vapply(paths, hash, "") == h))
        stop("Missing or changed Step input in ", d)
      return(setNames(paths, files))
    }
    stop("Could not locate ", step, ". Set ", pin, " or use the offline ZIP.")
  }
  s13 <- source_dir("13_eurostat_mortality_by_age_group",
    c("weekly_rates_by_five_year_age.csv","weekly_rates_by_age_group.csv","dose_source_inputs.csv"),
    "CZ_STEP13_DIR")
  pop_file <- "january_population_by_age.csv"
  link <- csv(s13[["dose_source_inputs.csv"]])
  expected_hash <- link$sha256[link$step == 3 & link$file == pop_file]
  stopifnot(length(expected_hash) == 1L)
  pop_path <- source_dir("03_census_ageing_migration", pop_file, "CZ_STEP03_DIR",
                         setNames(expected_hash, pop_file))

  url <- paste0("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/",
    "demo_r_mwk_05/W..T.NR.CZ?format=TSV&compressed=false&startPeriod=2015-W01&endPeriod=2019-W52")
  hist_path <- file.path(root, "data","sources","step14","demo_r_mwk_05_CZ_2015_2019.tsv")
  if (!file.exists(hist_path)) {
    dir.create(dirname(hist_path), recursive = TRUE, showWarnings = FALSE)
    tmp <- tempfile(tmpdir = dirname(hist_path)); on.exit(unlink(tmp), add = TRUE)
    download.file(url, tmp, mode = "wb", quiet = TRUE)
    if (!startsWith(readLines(tmp, n = 1L), "freq,age,sex,unit,geo") ||
        !file.rename(tmp, hist_path)) stop("Could not cache the historical Eurostat TSV.")
  }
  files <- c(s13, pop_path, historical_deaths_tsv = hist_path)
  provenance <- data.frame(file = names(files), sha256 = unname(vapply(files, hash, "")))
  dir_base <- file.path(root,"data","analysis","14_november_mortality_excess_and_trends")
  dir.create(dir_base, recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile(".stage-", tmpdir = dir_base)
  if (!dir.create(stage)) stop("Cannot create the output directory.")
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  sig <- file.path(stage, "signature")
  writeLines(c(deparse(body(november_excess_and_trends), width.cutoff = 120L), provenance$sha256), sig)
  dest <- file.path(dir_base, substr(hash(sig), 1, 20)); unlink(sig)
  verify_outputs <- function(dir) {
    v <- csv(file.path(dir,"outputs.csv"))
    p <- file.path(dir, v$file)
    stopifnot(nrow(v) > 0, all(file.exists(p)), all(vapply(p, hash, "") == v$sha256))
  }
  if (dir.exists(dest)) { verify_outputs(dest); message("Verified ", dest); return(invisible(dest)) }
  save_csv <- function(x, f) write.csv(x, file.path(stage, f), row.names = FALSE, na = "")
  save_csv(provenance, "input_sha256.csv")
  per100k <- 365.25 * 1e5
  fine_ages <- seq(15L, 90L, by = 5L)
  groups <- c("15-24","25-49","50-59","60-69","70-79","80+")
  arm_labels <- c(no_dose = "No recorded dose", vaccinated = "At least one dose")
  arm_ids <- names(arm_labels)
  fit_start <- as.Date("2021-01-01"); fit_end <- as.Date("2023-07-31")
  group_for <- function(age) as.character(cut(age, c(15,25,50,60,70,80,Inf), right = FALSE, labels = groups))
  same <- function(x,y) isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = 1e-9))
  weeknum <- function(x) as.integer(sub("^.*-W","",x))
  # Construct the complete historical age-by-week grid, refusing missing counts.
  h <- read.delim(hist_path, check.names = FALSE, colClasses = "character", quote = "", comment.char = "")
  age_code <- c(paste0("Y",fine_ages[-16],"-",fine_ages[-16] + 4L), "Y_GE90")
  keys <- paste("W",age_code,"T","NR","CZ",sep = ",")
  stopifnot(!anyDuplicated(h[[1]]), all(keys %in% h[[1]]))
  counts <- as.matrix(h[match(keys, h[[1]]), -1L, drop = FALSE])
  iso <- trimws(colnames(counts))
  grid <- seq(as.Date("2015-01-01"), as.Date("2019-12-26"), by = "7 days")
  # ISO Thursday belongs to the ISO year. Week 1 contains January 4.
  iso_label <- function(th) {
    yr <- format(th, "%Y")
    jan4 <- as.Date(paste0(yr, "-01-04"))
    start <- jan4 - (as.integer(jan4) + 3L) %% 7L
    sprintf("%s-W%02d", yr, as.integer(th - start) %/% 7L + 1L)
  }
  stopifnot(!anyDuplicated(iso), setequal(iso, iso_label(grid)))
  hs <- expand.grid(age = fine_ages, week = iso, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  raw <- trimws(as.vector(counts))
  if (!all(grepl("^[0-9]+([[:space:]]+[a-zA-Z ]+)?$",raw)))
    stop("A historical weekly death count is missing or nonnumeric.")
  hs$deaths <- as.numeric(sub("[[:space:]].*$","",raw))
  hs$week_mid <- grid[match(hs$week, iso_label(grid))]
  hs$iso_week <- weeknum(hs$week)
  pop <- csv(pop_path)
  pop <- pop[pop$year %in% 2015:2020 & (!is.na(pop$attained_age) | pop$age_code == "Y_OPEN"), ]
  pop$attained_age[pop$age_code == "Y_OPEN"] <- 100L
  stopifnot(!anyNA(pop$residents), !anyDuplicated(pop[c("year","attained_age")]),
            identical(sort(unique(pop$year)), 2015:2020),
            all(vapply(split(pop$attained_age, pop$year), function(a) setequal(a,0:100), TRUE)))
  pop$age <- pmin(pop$attained_age %/% 5L * 5L,90L)
  jan <- aggregate(residents ~ year + age, pop, sum)
  jan$date <- as.Date(paste0(jan$year,"-01-01"))
  hs$resident_days <- NA_real_
  for (a in fine_ages) {
    i <- hs$age == a; p <- jan[jan$age == a, ]
    hs$resident_days[i] <- 7 * approx(as.numeric(p$date), p$residents, xout = as.numeric(hs$week_mid[i]))$y
  }
  stopifnot(all(is.finite(hs$resident_days) & hs$resident_days > 0))
  hs$deaths_per_person_day <- hs$deaths / hs$resident_days
  base <- aggregate(deaths_per_person_day ~ age + iso_week, hs, mean)
  n_years <- aggregate(week ~ age + iso_week, hs, length)
  base$historical_years <- n_years$week[match(paste(base$age,base$iso_week),paste(n_years$age,n_years$iso_week))]
  stopifnot(all(base$historical_years == ifelse(base$iso_week == 53,1L,5L)))
  base$deaths_per_100k_py <- base$deaths_per_person_day * per100k
  save_csv(hs,"historical_weekly_deaths_and_exposure.csv")
  save_csv(base,"seasonal_rates_2015_2019.csv")

  # Keep observed vaccination histories and the signed no-dose residual from
  # Step 13. Estimate expected deaths separately for each status and fine age.
  fine <- subset(csv(s13[["weekly_rates_by_five_year_age.csv"]]), release == "november")
  wide <- subset(csv(s13[["weekly_rates_by_age_group.csv"]]), release == "november")
  required <- c("no_dose_days","vaccinated_days","no_dose_deaths","vaccinated_deaths","national_days","national_deaths")
  stopifnot(nrow(wide)>0L, setequal(fine$age,fine_ages), setequal(wide$age_group,groups),
            !anyDuplicated(fine[c("week","age")]), !anyDuplicated(wide[c("week","age_group")]),
            !anyNA(fine[required]), all(fine$no_dose_days >= 0 & fine$vaccinated_days >= 0),
            all(c("first_doses","later_doses","all_doses") %in% names(wide)))
  fine$age_group <- group_for(fine$age)
  fine$iso_week <- weeknum(fine$week)
  i <- match(paste(fine$age,fine$iso_week), paste(base$age,base$iso_week))
  stopifnot(!anyNA(i))
  fine$baseline_deaths_per_person_day <- base$deaths_per_person_day[i]
  for (arm in arm_ids)
    fine[[paste0(arm,"_expected")]] <- fine[[paste0(arm,"_days")]] * fine$baseline_deaths_per_person_day
  fine$resident_expected <- fine$national_days * fine$baseline_deaths_per_person_day
  stopifnot(same(fine$no_dose_expected + fine$vaccinated_expected, fine$resident_expected))
  save_csv(fine[c("week_start","week","age","age_group",required,"baseline_deaths_per_person_day",
                  "no_dose_expected","vaccinated_expected","resident_expected")], "weekly_five_year_components.csv")
  cols <- c(required, "no_dose_expected","vaccinated_expected","resident_expected")
  sums <- aggregate(fine[cols],fine[c("week","age_group")],sum)
  wi <- match(paste(wide$week,wide$age_group),paste(sums$week,sums$age_group))
  stopifnot(!anyNA(wi), nrow(wide) == nrow(sums), all(vapply(required,function(k) same(wide[[k]],sums[[k]][wi]),TRUE)))
  for (k in setdiff(cols,required)) wide[[k]] <- sums[[k]][wi]
  # Convert the expected death counts used in the quarterly table back to
  # rates. Each status supplies its own weights across the five-year ages.
  for (arm in arm_ids) {
    days <- wide[[paste0(arm,"_days")]]
    expected <- wide[[paste0(arm,"_expected")]]
    wide[[paste0(arm,"_seasonal_rate")]] <- ifelse(days > 0, per100k * expected / days, NA_real_)
  }
  wide$week_mid <- as.Date(wide$week_start) + 3L
  calendar <- sort(unique(wide$week_mid))
  stopifnot(all(diff(as.integer(calendar)) == 7L), all(iso_label(wide$week_mid) == wide$week),
            nrow(wide) == length(calendar) * length(groups), nrow(fine) == length(calendar)*length(fine_ages))
  quarter <- function(x) paste0(format(x,"%Y"),"-Q", (as.integer(format(x,"%m"))-1L) %/% 3L+1L)
  wide$quarter <- quarter(wide$week_mid)
  long <- do.call(rbind,lapply(arm_ids,function(arm) {
    days <- wide[[paste0(arm,"_days")]]; deaths <- wide[[paste0(arm,"_deaths")]]
    exp <- wide[[paste0(arm,"_expected")]]
    data.frame(wide[c("week_start","week_mid","week","age_group","quarter")], status = arm,
      person_days = days, mean_people = days/7, observed_deaths = deaths,
      expected_deaths = exp, excess_deaths = deaths - exp, rate_per_100k_py = wide[[paste0(arm,"_rate")]],
      seasonal_rate_per_100k_py = wide[[paste0(arm,"_seasonal_rate")]],
      signed_negative_week = deaths < -1e-9)
  }))
  long <- long[order(match(long$age_group,groups), long$week_mid, match(long$status,arm_ids)), ]
  save_csv(long,"weekly_status.csv")
  idx <- split(seq_len(nrow(long)), list(long$age_group,long$quarter,long$status), drop = TRUE)
  qr <- do.call(rbind,lapply(idx,function(i) {
    q <- long[i, ]; n <- nrow(q); days <- sum(q$person_days)
    data.frame(q[1,c("age_group","quarter","status")], first_week = q$week[1], last_week = tail(q$week,1),
       included_weeks = n, person_days = days, mean_people = days/(7*n),
       observed_deaths = sum(q$observed_deaths), expected_deaths = sum(q$expected_deaths),
       excess_deaths = sum(q$excess_deaths), negative_residual_weeks = sum(q$signed_negative_week),
       has_exposure = days > 0)
  }))
  qr <- qr[order(match(qr$age_group,groups),qr$quarter,match(qr$status,arm_ids)), ]
  # Reconcile each quarter's two statuses to the independent Eurostat totals.
  check <- aggregate(cbind(observed_deaths, expected_deaths, person_days) ~ age_group + quarter, qr, sum)
  nat <- aggregate(cbind(national_deaths,resident_expected,national_days) ~ age_group + quarter,wide,sum)
  stopifnot(identical(check[c("age_group","quarter")],nat[c("age_group","quarter")]),
            same(check$observed_deaths,nat$national_deaths),
            same(check$expected_deaths,nat$resident_expected),same(check$person_days,nat$national_days))
  save_csv(qr,"quarterly_group_size_and_excess.csv")

  # The regression follows the weekly broad-age rate, weighted by the number
  # of person-days contributing to it. Evaluate only on valid weeks in range.
  idx <- split(seq_len(nrow(long)), list(long$age_group,long$status), drop = TRUE)
  tr <- do.call(rbind,lapply(idx,function(i) {
    q <- long[i, ]; in_window <- q$week_mid >= fit_start & q$week_mid <= fit_end
    q <- q[in_window & q$person_days > 0 & is.finite(q$rate_per_100k_py), ]
    if (nrow(q) < 2L) stop("Too few weeks for a linear trend.")
    q$t_years <- as.numeric(q$week_mid - fit_start)/365.25
    fit <- lm(rate_per_100k_py ~ t_years, data = q, weights = person_days)
    b <- unname(coef(fit))
    data.frame(q[1,c("age_group","status")], requested_from = fit_start, requested_through = fit_end,
      first_fitted_midweek = min(q$week_mid), last_fitted_midweek = max(q$week_mid),
      weeks_used = nrow(q), weeks_excluded = sum(in_window)-nrow(q),
      person_years_used = sum(q$person_days)/365.25, intercept_at_2021_01_01 = b[1],
      annual_change_in_rate_per_100k_py = b[2])
  }))
  tr <- tr[order(match(tr$age_group,groups),match(tr$status,arm_ids)), ]
  save_csv(tr,"linear_trends.csv")
  save_csv(wide[c("week_start","week_mid","week","age_group","vaccinated_pct","first_doses","later_doses","all_doses")],
           "weekly_coverage_and_doses.csv")

  draw <- function(g) {
    w <- wide[wide$age_group == g, ]; w <- w[order(w$week_mid), ]
    q <- qr[qr$age_group == g, ]; fit <- tr[tr$age_group == g, ]
    ink <- "#182B37"; muted <- "#485C66"; grid <- "#DBE3E7"; white <- "#FFFFFF"
    colors <- c(no_dose = "#A64713", vaccinated = "#096C91")
    shade <- "#E8E0F1"; shade_edge <- "#78618F"
    first <- "#177469"; later <- "#8EC7B9"
    par(mar = rep(0,4), xaxs = "i", yaxs = "i", family = "sans", lend = "round")
    plot.new(); plot.window(xlim = c(0,1), ylim = c(0,1))
    label <- function(x, y, s, size=.94, col=ink, adj=0, font=1)
      text(x,y,s,cex=size,col=col,adj=c(adj,.5),font=font)
    xleft <- .128; xright <- .943
    yrs <- range(as.integer(format(w$week_mid,"%Y")))
    jan <- as.Date(sprintf("%d-01-01", yrs + c(0,1)))
    starts <- seq(jan[1], jan[2], by = "3 months")
    X <- function(d) xleft + (xright-xleft)*as.numeric(as.Date(d)-jan[1])/as.numeric(diff(jan))
    mid <- (X(head(starts,-1))+X(tail(starts,-1)))/2
    periods <- quarter(head(starts,-1))
    term <- function(row, dates) {
      row$intercept_at_2021_01_01 + row$annual_change_in_rate_per_100k_py * as.numeric(dates-fit_start)/365.25
    }
    ends <- Map(function(i) c(fit$first_fitted_midweek[i],fit$last_fitted_midweek[i]),seq_len(nrow(fit)))
    pred <- unlist(lapply(seq_len(nrow(fit)),function(i) term(fit[i, ],ends[[i]])))
    rticks <- pretty(range(c(0,w$no_dose_rate,w$vaccinated_rate,
                             w$no_dose_seasonal_rate,w$vaccinated_seasonal_rate,pred),na.rm = TRUE), n=5)
    rlo <- min(rticks); rhi <- max(rticks)
    rb <- .465; rt <- .796
    Y <- function(v) rb + (rt-rb)*(v-rlo)/(rhi-rlo)
    # Titles, rate legend and slope readouts.
    label(.026,.961,paste0("Weekly all-cause mortality  |  Ages ",g),2.20,font=2)
    label(.027,.918,"Czechia   /   November NZIP with Eurostat resident estimates   /   March 2020 to December 2023",1.14,col=muted)
    points(.032,.862,pch=16,cex=1.15,col=colors[1])
    label(.044,.862,arm_labels[1],1.02,col=colors[1],font=2)
    points(.182,.862,pch=16,cex=1.15,col=colors[2])
    label(.194,.862,arm_labels[2],1.02,col=colors[2],font=2)
    segments(.362,.862,.395,.862,col=ink,lwd=1.35)
    label(.403,.862,"Observed",.98)
    segments(.489,.862,.524,.862,lty="13",lwd=2.2,col=ink)
    label(.532,.862,"2015-2019 seasonal baseline",.98)
    segments(.737,.862,.775,.862,lty="64",lwd=2.1,col=ink)
    label(.783,.862,"Trend, Jan 2021 to Jul 2023",.98)
    label(.027,.815,"Mortality rate",.98,font=2)
    label(xleft,.815,"Deaths per 100,000 person-years",.88,col=muted)
    label(xright,.815,"Each baseline uses the group's age mix that week",.86,col=muted,adj=1)
    dates <- X(w$week_mid)
    quarter_lines <- X(starts)
    year_lines <- X(seq(jan[1],jan[2],by="year"))
    segments(quarter_lines,rb,quarter_lines,rt,col="#EDF1F2",lwd=.65)
    segments(year_lines,rb,year_lines,rt,col=grid,lwd=.9)
    segments(xleft,Y(rticks),xright,Y(rticks),col=adjustcolor(grid,alpha.f=.83),lwd=.7)
    label(xleft-.009,Y(rticks),format(rticks,big.mark=",",trim=TRUE),.88,col=muted,adj=1)
    for (arm in arm_ids)
      lines(dates,Y(w[[paste0(arm,"_seasonal_rate")]]),col=colors[arm],lwd=2.2,lty="13")
    for (arm in arm_ids) {
      lines(dates,Y(w[[paste0(arm,"_rate")]]),col=colors[arm],lwd=1.35)
    }
    for (arm in arm_ids) {
      row <- fit[fit$status == arm, ]; xs <- c(row$first_fitted_midweek,row$last_fitted_midweek)
      ys <- Y(term(row,xs))
      # White underlay keeps the straight fitted lines visible during waves.
      lines(X(xs),ys,col=adjustcolor(white,alpha.f=.75),lwd=3.0)
      lines(X(xs),ys,col=colors[arm],lwd=2.1,lty="64")
    }
    if (any(w$negative_no_dose_deaths))
      points(dates[w$negative_no_dose_deaths],rep(Y(0),sum(w$negative_no_dose_deaths)),
             pch=4,col=colors[1],lwd=1.3,cex=.66)
    signed <- function(v, digits=0) {
      n <- abs(round(v,digits)); s <- formatC(n,format="f",digits=digits,big.mark=",")
      ifelse(n == 0,s,ifelse(v < 0,paste0("-",s),paste0("+",s)))
    }
    label(.027,.437,"Fitted change per year",1.0,font=2)
    label(.214,.437,paste0(arm_labels[1],"   ", signed(fit$annual_change_in_rate_per_100k_py[fit$status=="no_dose"],1)),
          1.02,col=colors[1],font=2)
    label(.475,.437,paste0(arm_labels[2],"   ", signed(fit$annual_change_in_rate_per_100k_py[fit$status=="vaccinated"],1)),
          1.02,col=colors[2],font=2)
    label(.735,.437,"Deaths per 100,000 person-years per calendar year",.86,col=muted)

    # Count and share use separate scales in the lower panel. Draw the
    # coverage fill behind the columns and its boundary above the columns.
    db <- .279; dt <- .374
    ticks <- pretty(c(0,max(w$all_doses)),n=3); dmax <- max(ticks,1)
    D <- function(v) db+(dt-db)*v/dmax
    C <- function(v) db+(dt-db)*v/100
    stopifnot(all(is.finite(w$vaccinated_pct) & w$vaccinated_pct >= 0 & w$vaccinated_pct <= 100))
    label(.027,.396,"Doses given",.98,font=2)
    rect(xleft,.388,xleft+.012,.404,col=first,border=NA); label(xleft+.018,.396,"First doses",.90)
    rect(xleft+.118,.388,xleft+.130,.404,col=later,border=NA); label(xleft+.136,.396,"Doses 2-7",.90)
    rect(xleft+.238,.388,xleft+.250,.404,col=shade,border=shade_edge)
    label(xleft+.256,.396,"Share with at least one dose (right axis)",.90,col=shade_edge)
    polygon(c(dates,rev(dates)),c(C(w$vaccinated_pct),rep(db,nrow(w))),col=shade,border=NA)
    segments(quarter_lines,db,quarter_lines,dt,col="#EDF1F2",lwd=.65)
    segments(xleft,D(ticks),xright,D(ticks),col=grid,lwd=.7)
    label(xleft-.009,D(ticks),ifelse(ticks>=1000,paste0(ticks/1000,"k"),as.character(ticks)),.82,col=muted,adj=1)
    xl <- X(w$week_mid-3L); xr <- X(w$week_mid+3L)
    rect(xl,db,xr,D(w$first_doses),col=first,border=NA)
    rect(xl,D(w$first_doses),xr,D(w$all_doses),col=later,border=NA)
    lines(dates,C(w$vaccinated_pct),col=shade_edge,lwd=1.35)
    label(xright+.007,C(c(0,50,100)),c("0%","50%","100%"),.82,col=shade_edge)

    # Columns below use exactly the same calendar scale as the two panels.
    present <- periods %in% q$quarter
    y <- c(.181,.152,.111,.082)
    label(.027,.246,"Quarterly estimates",1.05,font=2)
    for (yr in seq(yrs[1],yrs[2])) {
      sel <- startsWith(periods,as.character(yr))
      label(mean(mid[sel]),.249,as.character(yr),1.06,adj=.5,font=2)
      if ((yr-yrs[1]) %% 2L == 0L)
        rect(min(quarter_lines[which(sel)]),.067,max(quarter_lines[which(sel)+1L]),.231,col="#F1F5F5",border=NA)
    }
    segments(year_lines,.067,year_lines,.233,col=grid,lwd=.9)
    qlab <- paste0("Q",substr(periods,7,7))
    qlab[periods == "2020-Q1"] <- "Q1*"
    label(mid,.215,qlab,.86,col=muted,adj=.5,font=2)
    label(.027,y[1],"Mean size, no dose",.92,col=colors[1])
    label(.027,y[2],"Excess deaths",.92,col=colors[1])
    label(.027,y[3],"Mean size, 1+ dose",.92,col=colors[2])
    label(.027,y[4],"Excess deaths",.92,col=colors[2])
    segments(xleft,.132,xright,.132,col=grid,lwd=.8)
    size_text <- function(v) {
      ifelse(v>=1e6,paste0(formatC(v/1e6,digits=2,format="f"),"m"),
        ifelse(v>=1000,paste0(formatC(v/1000,digits=ifelse(v>=1e5,0,1),format="f"),"k"),
          ifelse(v>0 & v<1,"<1",formatC(round(v),format="f",digits=0))))
    }
    for (j in seq_along(arm_ids)) {
      s <- q[q$status==arm_ids[j], ]; ix <- match(periods[present],s$quarter)
      yy <- y[(2*j-1):(2*j)]
      label(mid[present],yy[1],vapply(s$mean_people[ix],size_text,""),.88,adj=.5)
      label(mid[present],yy[2],ifelse(s$has_exposure[ix],signed(s$excess_deaths[ix]),"--"),.89,
            col=colors[j],adj=.5,font=2)
    }
    foot <- "* 2020 Q1 covers four March weeks. Mean size = person-days / days covered. Excess = observed minus expected at 2015-2019 national rates for the same ISO week and five-year age group."
    label(.027,.041,foot,.81,col=muted)
    note <- "Weeks follow their Thursday. Dashed fits are weighted by group person-time. No-dose deaths and person-time are estimated by subtraction. -- indicates no exposure."
    if (any(w$negative_no_dose_deaths))
      note <- paste0(note,"  x = negative weekly residual.")
    label(.027,.018,note,.80,col=muted)
  }

  files_png <- paste0("november_age_",c("15_24","25_49","50_59","60_69","70_79","80_plus"),".png")
  for (i in seq_along(groups)) {
    png(file.path(stage,files_png[i]),width=16,height=8.8,units="in",res=155,pointsize=12,bg="white")
    tryCatch(draw(groups[i]), finally=dev.off())
  }
  pdf(file.path(stage,"November_mortality_by_age.pdf"),width=16,height=8.8,pointsize=12,
      useDingbats=FALSE, title="Czechia | November weekly mortality by age")
  tryCatch(for (g in groups) draw(g), finally=dev.off())
  names_out <- sort(list.files(stage))
  save_csv(data.frame(file=names_out,sha256=unname(vapply(file.path(stage,names_out),hash,""))),"outputs.csv")
  if (!file.rename(stage,dest)) {
    if (!dir.exists(dest)) stop("Could not publish the completed output directory.")
    verify_outputs(dest)
  }
  message("Wrote ",dest)
  invisible(dest)
}
november_excess_and_trends()
