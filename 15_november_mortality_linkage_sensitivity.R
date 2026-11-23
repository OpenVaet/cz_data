# A numerator-only sensitivity analysis of the November NZIP release.
# Run from the project root or extracted ZIP:
#   source("15_november_mortality_linkage_sensitivity.R")
# CZ_STEP14_DIR can point to a specific completed Step 14 run. Optional:
#   Sys.setenv(CZ_MISSED_DEATH_FRACTION = "0.15")  # fraction of TRUE vaccinated deaths
#   Sys.setenv(CZ_LINKAGE_CALIBRATION = "iso_year")  # optional annual calibration
# Step 15 writes a separate directory for each specification and input version.
november_linkage_sensitivity <- function(root = Sys.getenv("CZ_PROJECT_ROOT", "."),
    missed_fraction = as.numeric(Sys.getenv("CZ_MISSED_DEATH_FRACTION", "0.15")),
    calibration_period = Sys.getenv("CZ_LINKAGE_CALIBRATION", "study")) {
  if (length(calibration_period)!=1L || !calibration_period %in% c("study","iso_year"))
    stop("Set CZ_LINKAGE_CALIBRATION to study or iso_year.")
  annual_target <- calibration_period=="iso_year"
  scope <- if (annual_target) "each ISO year" else "the full study"
  if (length(missed_fraction) != 1L || !is.finite(missed_fraction) ||
      missed_fraction < 0 || missed_fraction >= 1)
    stop("Set CZ_MISSED_DEATH_FRACTION to a number in [0, 1), for example 0.15.")
  sensitivity <- 1 - missed_fraction
  pct <- paste0(format(100 * missed_fraction, digits = 6, trim = TRUE), "%")
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
  source_dir <- function(step, files, pin) {
    dirs <- file.path(root, c("data/analysis","results"), step)
    dirs <- unique(c(dirs, unlist(lapply(dirs, list.dirs, recursive = FALSE))))
    if (nzchar(Sys.getenv(pin))) dirs <- Sys.getenv(pin)
    dirs <- dirs[file.exists(file.path(dirs, "outputs.csv"))]
    dirs <- dirs[order(file.mtime(file.path(dirs, "outputs.csv")), decreasing = TRUE)]
    for (d in dirs) {
      inv <- csv(file.path(d, "outputs.csv"))
      if (anyDuplicated(inv$file) || !all(files %in% inv$file)) next
      h <- setNames(inv$sha256, inv$file)[files]
      paths <- file.path(d, files)
      if (!all(file.exists(paths)) || !all(vapply(paths, hash, "") == h))
        stop("Missing or changed Step input in ", d)
      return(setNames(paths, files))
    }
    stop("Could not locate ", step, ". Set ", pin, " or use the offline ZIP.")
  }
  files <- source_dir("14_november_mortality_excess_and_trends",
    c("weekly_status.csv","weekly_five_year_components.csv","weekly_coverage_and_doses.csv"),
    "CZ_STEP14_DIR")
  provenance <- data.frame(file = names(files), sha256 = unname(vapply(files, hash, "")))
  dir_base <- file.path(root,"data","analysis","15_november_mortality_linkage_sensitivity")
  dir.create(dir_base, recursive = TRUE, showWarnings = FALSE)
  stage <- tempfile(".stage-", tmpdir = dir_base)
  if (!dir.create(stage)) stop("Cannot create the output directory.")
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  sig <- file.path(stage, "signature")
  writeLines(c(deparse(body(november_linkage_sensitivity), width.cutoff = 120L),
               sprintf("%.17g", missed_fraction), calibration_period, provenance$sha256), sig)
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
  arm_labels <- c(no_dose = "No dose (modelled)", vaccinated = "At least one dose")
  arm_ids <- names(arm_labels)
  fit_start <- as.Date("2021-01-01"); fit_end <- as.Date("2023-07-31")

  same <- function(x,y) isTRUE(all.equal(as.numeric(x),as.numeric(y),tolerance=1e-9))
  save_csv(data.frame(missed_share_of_true_vaccinated_deaths=missed_fraction,
    capture_probability=sensitivity, multiplier=1/sensitivity,
    geography="CZ", release="November NZIP", denominator="Step 14 person-days held fixed",
    calibration=paste("Death-weighted missed share per five-year age over",scope),
    calibration_period=calibration_period,
    weekly_rule="Common log-odds shift after within-week baseline age reconciliation"),
    "scenario.csv")
  fine <- csv(files[["weekly_five_year_components.csv"]])
  status <- csv(files[["weekly_status.csv"]])
  wide <- csv(files[["weekly_coverage_and_doses.csv"]])
  key <- function(x) paste(x$age_group,x$week)
  wide$week_mid <- as.Date(wide$week_mid)
  wide <- wide[order(match(wide$age_group,groups),wide$week_mid), ]
  days <- c("national_days","no_dose_days","vaccinated_days")
  counts <- c("national_deaths","no_dose_deaths","vaccinated_deaths")
  expected <- c("no_dose_expected","vaccinated_expected","resident_expected")
  calendar <- sort(unique(wide$week_mid))
  stopifnot(length(calendar)>0, all(diff(as.integer(calendar))==7L),
    !anyDuplicated(key(wide)), !anyDuplicated(fine[c("age","week")]),
    !anyDuplicated(status[c("age_group","week","status")]), setequal(status$status,arm_ids),
    setequal(wide$age_group,groups), setequal(fine$age,fine_ages),
    nrow(fine)==length(calendar)*length(fine_ages),
    nrow(wide)==length(calendar)*length(groups), nrow(status)==2*nrow(wide),
    !anyNA(fine[c(counts,days,expected)]), all(fine$national_deaths>=0 & fine$vaccinated_deaths>=0),
    all(fine$national_days>0 & fine$vaccinated_days>=0 & fine$no_dose_days>=0),
    same(fine$no_dose_deaths + fine$vaccinated_deaths,fine$national_deaths),
    same(fine$no_dose_days + fine$vaccinated_days,fine$national_days),
    same(fine$no_dose_expected + fine$vaccinated_expected, fine$resident_expected))

  # Keep the original death week. If fractional age attribution put more
  # identified vaccinated deaths in a fine age cell than resident deaths,
  # retain the overflow in the same week and use the nearest eligible age.
  # Prefer the original displayed age group; split tied distances by headroom.
  infeasible <- function(n) n < -1e-9
  fine$iso_year <- substr(fine$week,1,4)
  fine$infeasible_before <- infeasible(fine$no_dose_deaths)
  fine$reconciled_vaccinated_deaths <- pmin(fine$vaccinated_deaths,fine$national_deaths)
  fine$age_out <- fine$vaccinated_deaths - fine$reconciled_vaccinated_deaths
  fine$age_in <- 0
  moves <- list()
  donors <- which(fine$age_out > 0)
  for (i in donors[order(fine$week[donors],fine$age[donors])]) {
    remaining <- fine$age_out[i]
    capacity <- fine$national_deaths - fine$reconciled_vaccinated_deaths
    candidates <- which(fine$week == fine$week[i] & capacity > 0 &
                        fine$vaccinated_days > 0 & fine$vaccinated_deaths > 0)
    # Process candidates in priority order. The distance is between the
    # age-band lower bounds (so 90 denotes the open-ended 90+ band).
    priority <- data.frame(outside=fine$age_group[candidates]!=fine$age_group[i],
                           distance=abs(fine$age[candidates]-fine$age[i]))
    levels <- unique(priority[order(priority$outside,priority$distance),,drop=FALSE])
    for (j in seq_len(nrow(levels))) {
      ids <- candidates[priority$outside==levels$outside[j] & priority$distance==levels$distance[j]]
      space <- sum(capacity[ids])
      amount <- min(remaining,space)
      portion <- amount * (capacity[ids]/space)
      fine$reconciled_vaccinated_deaths[ids] <- pmin(fine$national_deaths[ids],
        fine$reconciled_vaccinated_deaths[ids]+portion)
      fine$age_in[ids] <- fine$age_in[ids]+portion
      moves[[length(moves)+1L]] <- data.frame(week=fine$week[i],from_age=fine$age[i],
        to_age=fine$age[ids],from_group=fine$age_group[i],to_group=fine$age_group[ids],amount=portion)
      remaining <- remaining-amount
      if (remaining<=1e-12) break
    }
    if (remaining>1e-9) stop("Cannot reconcile the identified vaccinated total in ",fine$week[i],
      " without changing the death week or adding to cells without a recorded vaccinated death.")
  }
  move_log <- data.frame(week=character(),from_age=integer(),to_age=integer(),
    from_group=character(),to_group=character(),amount=numeric())
  if (length(moves)) move_log <- do.call(rbind,moves)
  save_csv(move_log,"within_week_age_reconciliation.csv")
  fine$age_adjustment <- fine$age_in-fine$age_out
  fine$reconciled_no_dose_deaths <- fine$national_deaths-fine$reconciled_vaccinated_deaths
  by_week <- aggregate(fine[c("vaccinated_deaths","reconciled_vaccinated_deaths")],fine["week"],sum)
  stopifnot(all(abs(by_week$vaccinated_deaths-by_week$reconciled_vaccinated_deaths)<1e-7),
            same(fine$vaccinated_deaths+fine$age_adjustment,fine$reconciled_vaccinated_deaths),
            all(fine$reconciled_no_dose_deaths>=0), all(fine$reconciled_vaccinated_deaths>=0))

  # Within each fine age and calibration period, shift the log odds of the vaccinated
  # share of deaths by one common amount across weeks. A finite shift keeps
  # mixed weeks inside (0,D); cells already at 0 or D remain on that boundary.
  # Choose the shift to achieve sum(missed) / sum(modelled vaccinated) = f.
  fine$scenario_vaccinated_deaths <- fine$reconciled_vaccinated_deaths
  period_id <- if (annual_target) fine$iso_year else rep("full study",nrow(fine))
  blocks <- split(seq_len(nrow(fine)),list(fine$age,period_id),drop=TRUE)
  calibration <- lapply(blocks,function(idx) {
    b <- fine$reconciled_vaccinated_deaths[idx]; d <- fine$national_deaths[idx]
    n <- sum(b); eligible <- b>0 & b<d
    room <- sum(d[eligible]-b[eligible])
    target <- n*missed_fraction/sensitivity
    max_fraction <- if (n>0) room/(n+room) else NA_real_
    id <- paste0(fine$age[idx[1]]," in ",period_id[idx[1]])
    tolerance <- 1e-10*max(1,n)
    if (target>room+tolerance)
      stop(sprintf("Unattainable %.4f%% target for age band starting at %s. Maximum %.4f%% with these weekly bounds.",
                   100*missed_fraction,id,100*max_fraction))
    corrected <- b; delta <- 0
    if (target>0 && target>=room) {
      corrected[eligible] <- d[eligible]; delta <- Inf
    } else if (target>0) {
      log_odds <- log(b[eligible])-log(d[eligible]-b[eligible])
      shifted <- function(t) d[eligible]*plogis(log_odds+t)
      objective <- function(t) sum(shifted(t)-b[eligible])-target
      # As t grows, all eligible shares approach one.
      upper <- 1
      while (objective(upper)<0 && upper<1024) upper <- 2*upper
      if (objective(upper)<0) stop("Unable to bracket calibration for ",id)
      delta <- if (objective(0)>=0) 0 else uniroot(objective,c(0,upper),tol=1e-13)$root
      corrected[eligible] <- pmin(d[eligible],pmax(b[eligible],shifted(delta)))
    }
    added <- sum(corrected-b)
    if (abs(added-target)>tolerance) stop("Calibration check failed for ",id)
    list(idx=idx,corrected=corrected,audit=data.frame(period=period_id[idx[1]],age=fine$age[idx[1]],
      age_group=fine$age_group[idx[1]],recorded_vaccinated=sum(fine$vaccinated_deaths[idx]),
      reconciled_vaccinated=n,modelled_vaccinated=sum(corrected),target_added=target,added=added,
      target_missed_fraction=missed_fraction,achieved_missed_fraction=if(n>0) added/sum(corrected) else NA_real_,
      maximum_missed_fraction=max_fraction,log_odds_shift=delta,
      total_weeks=length(idx),weeks_with_room=sum(eligible)))
  })
  for (block in calibration) fine$scenario_vaccinated_deaths[block$idx] <- block$corrected
  save_csv(do.call(rbind,lapply(calibration,function(z) z$audit)),"calibration_by_five_year_age.csv")
  fine$missed_deaths <- fine$scenario_vaccinated_deaths-fine$reconciled_vaccinated_deaths
  # Keep transferred_deaths as the net change from Step 14 for existing callers.
  fine$transferred_deaths <- fine$scenario_vaccinated_deaths-fine$vaccinated_deaths
  fine$scenario_no_dose_deaths <- fine$national_deaths-fine$scenario_vaccinated_deaths
  fine$infeasible_after <- infeasible(fine$scenario_no_dose_deaths)
  fraction <- function(n,d) ifelse(d>0,n/d,NA_real_)
  fine$weekly_missed_fraction <- fraction(fine$missed_deaths,fine$scenario_vaccinated_deaths)
  stopifnot(all(fine$scenario_no_dose_deaths>=0), all(fine$missed_deaths>=0),
            same(fine$age_adjustment+fine$missed_deaths,fine$transferred_deaths),
            same(fine$no_dose_deaths-fine$transferred_deaths,fine$scenario_no_dose_deaths))
  annual_age <- aggregate(fine[c("vaccinated_deaths","reconciled_vaccinated_deaths",
    "scenario_vaccinated_deaths","scenario_no_dose_deaths","missed_deaths","national_deaths")],
    fine[c("age","age_group","iso_year")],sum)
  annual_age$achieved_missed_fraction <- fraction(annual_age$missed_deaths,annual_age$scenario_vaccinated_deaths)
  save_csv(annual_age,"annual_missed_shares_by_five_year_age.csv")
  save_csv(fine,"weekly_five_year_components.csv")
  cols <- c(counts,days,expected,"scenario_vaccinated_deaths","scenario_no_dose_deaths",
            "transferred_deaths","age_in","age_out","age_adjustment","reconciled_vaccinated_deaths",
            "reconciled_no_dose_deaths","missed_deaths","infeasible_before","infeasible_after")
  pooled <- aggregate(fine[cols],fine[c("age_group","week")],sum)
  i <- match(key(wide),key(pooled))
  stopifnot(!anyNA(i), nrow(pooled)==nrow(wide))
  wide[cols] <- pooled[i,cols]
  names(wide)[match(c("infeasible_before","infeasible_after"),names(wide))] <-
    c("n_infeasible_fine_before","n_infeasible_fine_after")
  wide$infeasible_before <- infeasible(wide$no_dose_deaths)
  wide$infeasible_week <- infeasible(wide$scenario_no_dose_deaths)
  wide$weekly_missed_fraction <- fraction(wide$missed_deaths,wide$scenario_vaccinated_deaths)
  stopifnot(!any(wide$infeasible_week),
            same(wide$scenario_vaccinated_deaths+wide$scenario_no_dose_deaths,wide$national_deaths),
            same(100*wide$vaccinated_days/wide$national_days, wide$vaccinated_pct))
  quarter <- function(d) paste0(format(d,"%Y"),"-Q",(as.integer(format(d,"%m"))-1L)%/%3L+1L)
  wide$quarter <- quarter(wide$week_mid)
  safe_rate <- function(deaths,days) ifelse(days>0,per100k*deaths/days,NA_real_)
  parts <- list()
  for (arm in arm_ids) {
    s <- status[status$status==arm, ]
    i <- match(key(wide),key(s))
    stopifnot(!anyNA(i)); s <- s[i, ]
    d <- wide[[paste0(arm,"_days")]]
    original <- wide[[paste0(arm,"_deaths")]]
    model <- wide[[paste0("scenario_",arm,"_deaths")]]
    e <- wide[[paste0(arm,"_expected")]]
    stopifnot(same(d,s$person_days), same(original,s$observed_deaths), same(e,s$expected_deaths),
              all(wide$week_mid==as.Date(s$week_mid)), all(wide$quarter==s$quarter))
    model_rate <- safe_rate(model,d)
    reconciled <- wide[[paste0("reconciled_",arm,"_deaths")]]
    wide[[paste0(arm,"_rate")]] <- model_rate
    wide[[paste0(arm,"_seasonal_rate")]] <- safe_rate(e,d)
    parts[[arm]] <- data.frame(wide[c("age_group","week_start","week_mid","week","quarter","infeasible_week")],
      status=arm,person_days=d,mean_people=d/7,original_deaths=original,scenario_deaths=model,
      reconciled_deaths=reconciled,age_adjustment=reconciled-original,linkage_adjustment=model-reconciled,
      change_in_deaths=model-original,expected_deaths=e,original_excess_deaths=original-e,
      scenario_excess_deaths=model-e,original_rate_per_100k_py=s$rate_per_100k_py,
      scenario_rate_per_100k_py=model_rate,seasonal_rate_per_100k_py=safe_rate(e,d))
  }
  long <- do.call(rbind,parts)
  long <- long[order(match(long$age_group,groups),long$week_mid,match(long$status,arm_ids)), ]
  save_csv(long,"weekly_status.csv")
  save_csv(wide,"weekly_age_groups.csv")

  # Pool all weeks before testing that a period-level count split is feasible.
  period_summary <- function(period) {
    keys <- data.frame(age_group=long$age_group,period=period,status=long$status)
    cols <- c("original_deaths","scenario_deaths","reconciled_deaths","age_adjustment","linkage_adjustment",
              "change_in_deaths","expected_deaths","person_days")
    a <- aggregate(long[cols],keys,sum)
    n <- aggregate(data.frame(included_weeks=rep(1L,nrow(long)),
                              infeasible_weeks=as.integer(long$infeasible_week)),keys,sum)
    a <- merge(a,n,by=names(keys),sort=FALSE)
    id <- paste(a$age_group,a$period)
    a$infeasible_period <- id %in% id[infeasible(a$scenario_deaths)]
    a$missed_fraction_among_vaccinated <- ifelse(a$status=="vaccinated",fraction(a$linkage_adjustment,a$scenario_deaths),NA_real_)
    a$has_exposure <- a$person_days>0
    a$mean_people <- a$person_days/(7*a$included_weeks)
    a$original_excess_deaths <- a$original_deaths-a$expected_deaths
    a$scenario_excess_deaths <- a$scenario_deaths-a$expected_deaths
    a$original_rate_per_100k_py <- safe_rate(a$original_deaths,a$person_days)
    a$scenario_rate_per_100k_py <- ifelse(a$infeasible_period,NA_real_,safe_rate(pmax(a$scenario_deaths,0),a$person_days))
    a$seasonal_rate_per_100k_py <- safe_rate(a$expected_deaths,a$person_days)
    chk <- aggregate(a[c("original_deaths","scenario_deaths","change_in_deaths")],a[c("age_group","period")],sum)
    stopifnot(same(chk$original_deaths,chk$scenario_deaths),all(abs(chk$change_in_deaths)<1e-7))
    a[order(match(a$age_group,groups),a$period,match(a$status,arm_ids)), ]
  }
  qr <- period_summary(long$quarter); names(qr)[names(qr)=="period"] <- "quarter"
  annual <- period_summary(substr(long$week,1,4)); names(annual)[names(annual)=="period"] <- "iso_year"
  save_csv(qr,"quarterly_group_size_and_excess.csv")
  save_csv(annual,"annual_original_vs_scenario.csv")
  # Fit the scenario and the original on both original and common support.
  # This makes the effects of a numerator change and of omitted weeks separable.
  trend_row <- function(q,col,keep,kind) {
    in_window <- q$week_mid>=fit_start & q$week_mid<=fit_end
    ok <- in_window & keep & q$person_days>0 & is.finite(q[[col]])
    x <- q[ok, ]
    first <- last <- as.Date(NA_character_)
    intercept <- slope <- NA_real_
    if (nrow(x)>=2L) {
      t_years <- as.numeric(x$week_mid-fit_start)/365.25
      model <- lm(x[[col]] ~ t_years, weights=x$person_days)
      b <- unname(coef(model)); intercept <- b[1]; slope <- b[2]
      first <- min(x$week_mid); last <- max(x$week_mid)
    }
    data.frame(q[1,c("age_group","status")],series=kind,requested_from=fit_start,requested_through=fit_end,
      first_fitted_midweek=first,last_fitted_midweek=last,weeks_used=nrow(x),weeks_excluded=sum(in_window)-nrow(x),
      person_years_used=sum(x$person_days)/365.25,intercept_at_2021_01_01=intercept,annual_change_in_rate_per_100k_py=slope)
  }
  fits <- lapply(split(long,list(long$age_group,long$status),drop=TRUE),function(q) rbind(
    trend_row(q,"original_rate_per_100k_py",TRUE,"step14_original"),
    trend_row(q,"original_rate_per_100k_py",!q$infeasible_week,"original_on_scenario_weeks"),
    trend_row(q,"scenario_rate_per_100k_py",TRUE,"scenario")))
  trends <- do.call(rbind,fits)
  save_csv(trends,"linear_trends.csv")
  tr <- trends[trends$series=="scenario", ]
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
    ends <- lapply(seq_len(nrow(fit)),function(i) c(fit$first_fitted_midweek[i],fit$last_fitted_midweek[i]))
    pred <- unlist(lapply(seq_len(nrow(fit)),function(i) term(fit[i, ],ends[[i]])))
    rticks <- pretty(range(c(0,w$no_dose_rate,w$vaccinated_rate,
                             w$no_dose_seasonal_rate,w$vaccinated_seasonal_rate,pmax(0,pred)),na.rm = TRUE), n=5)
    rlo <- min(rticks); rhi <- max(rticks)
    rb <- .465; rt <- .796
    Y <- function(v) rb + (rt-rb)*(v-rlo)/(rhi-rlo)
    # Titles, rate legend and slope readouts.
    label(.026,.961,paste0("Modelled all-cause mortality  |  Ages ",g),2.20,font=2)
    label(.027,.918,paste0(pct," missed in ",scope," per five-year age band   /   November NZIP and Eurostat   /   March 2020 to December 2023"),1.14,col=muted)
    points(.032,.862,pch=16,cex=1.15,col=colors[1])
    label(.044,.862,arm_labels[1],1.02,col=colors[1],font=2)
    points(.182,.862,pch=16,cex=1.15,col=colors[2])
    label(.194,.862,arm_labels[2],1.02,col=colors[2],font=2)
    segments(.362,.862,.395,.862,col=ink,lwd=1.35)
    label(.403,.862,"Modelled",.98)
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
      row <- fit[fit$status == arm, ]; if (row$weeks_used < 2L) next
      xs <- c(row$first_fitted_midweek,row$last_fitted_midweek)
      ys <- Y(term(row,xs))
      # Draw the linear fit only in the nonnegative plotted range.
      clip(xleft,xright,rb,rt)
      lines(X(xs),ys,col=adjustcolor(white,alpha.f=.75),lwd=3.0)
      lines(X(xs),ys,col=colors[arm],lwd=2.1,lty="64")
      clip(0,1,0,1)
    }
    signed <- function(v, digits=0) {
      n <- abs(round(v,digits)); s <- formatC(n,format="f",digits=digits,big.mark=",")
      ifelse(!is.finite(v),"--",ifelse(n == 0,s,ifelse(v < 0,paste0("-",s),paste0("+",s))))
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
    label(.027,y[2],"Modelled excess",.92,col=colors[1])
    label(.027,y[3],"Mean size, 1+ dose",.92,col=colors[2])
    label(.027,y[4],"Modelled excess",.92,col=colors[2])
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
      label(mid[present],yy[2],ifelse(s$has_exposure[ix] & !s$infeasible_period[ix],signed(s$scenario_excess_deaths[ix]),"--"),.89,
            col=colors[j],adj=.5,font=2)
    }
    foot <- "* 2020 Q1 covers four March weeks. Mean size = person-days / days covered. Excess = modelled minus expected deaths at 2015-2019 seasonal rates."
    label(.027,.041,foot,.81,col=muted)
    note <- "Weekly transfers vary with available deaths. Dashed fits use weekly rates weighted by person-time. Group person-time is held fixed. -- indicates no exposure."
    label(.027,.018,note,.80,col=muted)
  }

  files_png <- paste0("scenario_november_age_",c("15_24","25_49","50_59","60_69","70_79","80_plus"),".png")
  for (i in seq_along(groups)) {
    png(file.path(stage,files_png[i]),width=16,height=8.8,units="in",res=155,pointsize=12,bg="white")
    tryCatch(draw(groups[i]), finally=dev.off())
  }
  pdf(file.path(stage,"November_mortality_linkage_scenario.pdf"),width=16,height=8.8,pointsize=12,
      useDingbats=FALSE, title="Czechia | Vaccinated death linkage sensitivity analysis")
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
november_linkage_sensitivity()
