# 07_build_weekly_mortality_rates
# Run after 06. Packages: data.table, digest, jsonlite. Week starts are ISO Mondays.
run_analysis <- function() {
  step <- "07_build_weekly_mortality_rates"
  needed <- c("march","november","eurostat_population","eurostat_deaths")
  suppressPackageStartupMessages(library(data.table))
  root <- normalizePath(Sys.getenv("CZ_PROJECT_ROOT", "."), mustWork=TRUE)
  manifest_dir <- file.path(root, "data", "sources", "manifests")
  explicit <- Sys.getenv("CZ_MANIFEST")
  candidates <- if (nzchar(explicit)) explicit else rev(sort(list.files(manifest_dir, "^manifest-.*\\.csv$", full.names=TRUE)))
  manifest <- NULL
  for (p in candidates) {
    x <- fread(p, colClasses="character", na.strings=NULL)
    if (all(needed %in% x$id)) { manifest <- p; break }
  }
  if (is.null(manifest)) stop("No manifest covers the required sources. Run 01_acquire_sources.R with CZ_SOURCE_SET=all.")
  inputs <- x[match(needed, id), .(id,path,sha256,url,status)]
  if (anyDuplicated(x$id) || any(inputs$status!="ok")) stop("Incomplete or ambiguous source manifest: ", manifest)
  hash <- function(f) digest::digest(file=f, algo="sha256", serialize=FALSE)
  src <- function(id) file.path(root, inputs$path[match(id,inputs$id)])
  # Select a committed upstream run with the same source identities. Override the
  # default with the step-specific directory if reproducing an earlier analysis.
  find_parent <- function(step_name,files,env) {
    pin <- Sys.getenv(env)
    dirs <- if (nzchar(pin)) pin else list.dirs(file.path(root,"data","analysis",step_name),recursive=FALSE,full.names=TRUE)
    dirs <- dirs[file.exists(file.path(dirs,"outputs.csv"))]
    dirs <- dirs[order(file.info(file.path(dirs,"outputs.csv"))$mtime,decreasing=TRUE)]
    for(d in dirs) {
      ids <- fread(file.path(d,"input_files.csv"),colClasses="character")
      # Match source hashes even when one machine uses an archive and another an extracted CSV.
      common <- intersect(inputs$id,ids$id)
      if(!all(c("march","november") %in% common) || !all(ids$sha256[match(common,ids$id)]==inputs$sha256[match(common,inputs$id)])) next
      inv <- fread(file.path(d,"outputs.csv"),colClasses="character")
      if(!all(files %in% inv$file)) next
      a <- inv[match(files,file),.(id=file,path=file.path("data","analysis",step_name,basename(d),file),sha256)]
      for(i in seq_len(nrow(a))) if(hash(file.path(d,a$id[i]))!=a$sha256[i]) stop("Changed upstream file: ",a$id[i])
      return(a)
    }
    stop("Run ",step_name," for the selected sources, or set ",env,".")
  }
  parents <- find_parent("06_compare_resident_population",c("march_event_patterns.csv","november_event_patterns.csv"),"CZ_STEP06_DIR")
  # The release objects for steps 04/05 were verified in step 02. Check the
  # actual files read here against their manifest or prepared-file inventory.
  for (id in setdiff(needed,c("march","november")))
    if (hash(src(id))!=inputs$sha256[match(id,inputs$id)]) stop("Source checksum mismatch: ",id)
  provenance <- rbindlist(list(inputs[,.(id,path,sha256)],parents),fill=TRUE)
  code <- paste(deparse(body(run_analysis), control=c("keepInteger","keepNA")),collapse="\n")
  key <- digest::digest(paste(c(code,provenance$sha256),collapse="|"),algo="sha256",serialize=FALSE)
  base <- file.path(root,"data","analysis",step)
  dir.create(base,recursive=TRUE,showWarnings=FALSE)
  dest <- file.path(base,substr(key,1L,20L))
  if (file.exists(file.path(dest,"outputs.csv"))) {
    inv <- fread(file.path(dest,"outputs.csv"))
    for (i in seq_len(nrow(inv))) if (hash(file.path(dest,inv$file[i]))!=inv$sha256[i]) stop("Changed output: ",inv$file[i])
    message("Verified ",dest)
    return(invisible(dest))
  }
  stage <- tempfile(".building-",tmpdir=base)
  if (!dir.create(stage)) stop("Cannot create output directory.")
  on.exit(unlink(stage,recursive=TRUE),add=TRUE)
  save_table <- function(x,name) fwrite(x,file.path(stage,paste0(name,".csv")),na="")
  save_table(provenance,"input_files")
  figure <- function(name,width,height,draw) {
    png(file.path(stage,paste0(name,".png")),width=width,height=height,units="in",res=160,bg="white")
    on.exit(dev.off())
    par(family="sans",las=1,fg="#243843",col.axis="#344956",col.lab="#344956",col.main="#192f3a")
    draw()
  }
  monday <- function(d) { d <- as.Date(d); d-(as.integer(d)+3L)%%7L }
  iso_week <- function(d) {
    m <- monday(d); y <- as.integer(format(m+3L,"%Y"))
    sprintf("%04d-W%02d",y,as.integer(m-monday(sprintf("%04d-01-04",y)))%/%7L+1L)
  }
  week_date <- function(x) {
    y <- as.integer(substr(x,1L,4L)); w <- as.integer(sub("^[0-9]{4}-W?","",x))
    m <- monday(sprintf("%04d-01-04",y))+7L*(w-1L)
    if (anyNA(m) || any(iso_week(m)!=sprintf("%04d-W%02d",y,w))) stop("Invalid ISO week.")
    m
  }
  # JSON-stat uses zero-based cell indices, with the last dimension varying fastest.
  estat <- function(path) {
    j <- jsonlite::fromJSON(path,simplifyVector=FALSE)
    dims <- unlist(j$id,use.names=FALSE); sizes <- unlist(j$size,use.names=FALSE)
    axes <- lapply(dims,function(k) {
      idx <- unlist(j$dimension[[k]]$category$index)
      if (is.null(names(idx))) return(as.character(idx))
      names(sort(idx))
    })
    grid <- as.data.table(do.call(expand.grid,c(rev(setNames(axes,dims)),stringsAsFactors=FALSE)))
    stopifnot(nrow(grid)==prod(sizes))
    unpack <- function(values,missing_value) {
      a <- rep(missing_value,nrow(grid))
      if (length(values)) {
        ix <- if (is.null(names(values))) seq_along(values) else as.integer(names(values))+1L
        good <- lengths(values)>0L
        a[ix[good]] <- unlist(values[good],use.names=FALSE)
      }
      a
    }
    grid[, value := unpack(j$value,NA_real_)]
    grid[, flag := unpack(j$status,NA_character_)]
    grid
  }
  esp <- data.table(age=seq(0,90,5),standard_n=c(5000,5500,5500,5500,6000,6000,6500,7000,7000,7000,7000,6500,6000,5500,5000,4000,2500,1500,1000))
  stopifnot(sum(esp$standard_n)==100000)
  per_year <- 365.25*100000
  save_table(esp,"esp2013_weights")
  origin <- as.Date("2019-12-30"); last <- as.Date("2023-12-25")
  calendar <- seq(origin,last,by="7 days"); ages <- esp$age
  p <- estat(src("eurostat_population"))[geo=="CZ" & sex=="T" & unit=="NR" & freq=="A"]
  p[, single := fcase(age=="Y_LT1",0L,age=="Y_OPEN",100L,default=suppressWarnings(as.integer(sub("^Y","",age))))]
  p <- p[!is.na(single)]
  p21 <- p[time=="2021"]; stopifnot(!anyDuplicated(p21$single),all(0:100 %in% p21$single))
  # Exact birth years within a five-year band receive January 2021 resident weights
  # (age at 31 December 2020). For later births use the age-zero count from
  # the following January, carrying the last available newborn count forward.
  mix <- function(cohorts,width,distribution) {
    ans <- CJ(birth=cohorts,offset=0:(width-1L))
    ans[, yob := birth+offset]
    ans[, prior := p21$value[match(2020L-yob,p21$single)]]
    newborn <- p[single==0,.(y=as.integer(time)-1L,n=value)][order(y)]
    ans[yob>2020L,prior:=newborn$n[match(pmin(yob,max(newborn$y)),newborn$y)]]
    ans[is.na(prior) | prior<=0 | distribution=="uniform",prior:=1]
    ans[, fraction := prior/sum(prior),by=birth]
    ans
  }
  # Aggregate first into cohort/week event counts. The cumulative state treats
  # entry, first dose and exit as transitions halfway through their respective weeks.
  models <- CJ(release=c("march","november"),spec=c("primary","uniform_birth_year","death_first_if_tied","first_contact","ignore_postdeath_dose","birthday_early","birthday_late","full_week_tied_dose"))
  models <- rbind(models,data.table(release="march",spec=c("coarsened","coarsened_uniform","coarsened_death_first")))
  models[, width := ifelse(release=="november" | startsWith(spec,"coarsened"),5L,1L)]
  models[, `:=`(entry=ifelse(spec=="first_contact","first observed event","start of study"),
               tie=ifelse(grepl("death_first",spec),"death first","dose first"),
               birthdays=fcase(spec=="birthday_early","first quarter",spec=="birthday_late","last quarter",default="uniform within year"),
               prior=ifelse(grepl("uniform",spec),"uniform","population"),
               conflicts=ifelse(spec=="ignore_postdeath_dose","retain record, ignore impossible dose","exclude record"))]
  save_table(models,"specifications")
  result <- list(); audits <- list(); unknown <- list()
  for(rel in unique(models$release)) {
    pat <- fread(file.path(root,parents$path[parents$id==paste0(rel,"_event_patterns.csv")]))
    for(k in which(models$release==rel)) {
      model <- models[k]; specname <- model$spec
      message(rel," / ",specname)
      a <- copy(pat); n_input <- sum(a$n)
      incomplete_history <- sum(a$n[a$later_without_first])
      a <- a[later_without_first==FALSE]
      conflict <- !is.na(a$death) & !is.na(a$dose) & a$dose>a$death
      conflict_rows <- sum(a$n[conflict])
      if(specname=="ignore_postdeath_dose") a[conflict,dose:=NA_integer_] else a <- a[!conflict]
      if(model$tie=="death first") a[!is.na(death) & dose==death,dose:=NA_integer_]
      if(startsWith(specname,"coarsened")) a[,birth:=5L*(birth%/%5L)]
      start <- 1L
      contact <- specname=="first_contact"
      a[,enter := if(contact) first_event else start]
      end <- if(rel=="march") match(as.Date("2022-12-19"),calendar) else length(calendar)
      a <- a[!is.na(enter) & enter<=end]
      stopifnot(all(is.na(a$death) | a$enter<=a$death),all(is.na(a$dose) | a$enter<=a$dose))
      # Blank ages contribute to the exposure ledger. No age is assigned here.
      a[, unknown_age := is.na(birth)]; a[unknown_age==TRUE,birth:=-1L]
      a[, died_v := !is.na(dose) & !is.na(death) & dose<=death]
      cohorts <- sort(unique(a$birth)); nb <- length(cohorts)
      tab <- function(t,field) {
        q <- t[!is.na(get(field)) & get(field)<=end,.(n=sum(as.numeric(n))),by=.(w=get(field),birth)]
        m <- matrix(0,end,nb); m[cbind(q$w,match(q$birth,cohorts))] <- q$n; m
      }
      entries <- tab(a,"enter"); doses <- tab(a,"dose"); dv <- tab(a[died_v==TRUE],"death"); du <- tab(a[died_v==FALSE],"death")
      change_v <- doses-dv; change_u <- entries-doses-du
      # Baseline records enter at the study's opening, including in week one.
      midpoint <- function(ch) apply(ch,2,cumsum)-.5*ch
      pv <- midpoint(change_v)*7; pu <- midpoint(change_u)*7
      if(!contact) pu[1,] <- pu[1,]+3.5*entries[1,]
      if(specname=="full_week_tied_dose") {
        # Upper within-week exposure for a tied record already included at the
        # start: first dose at Monday opening, death at Sunday close. The primary
        # rule gives it 3.5 undosed days and zero dosed days.
        tied <- tab(a[!is.na(dose) & dose==death],"death")
        pv <- pv+7*tied; pu <- pu-3.5*tied
      } else tied <- 0
      stopifnot(min(pv,pu)>=0,max(abs((pv+pu)/7-.5*tied-midpoint(entries-dv-du)-
          (if(contact) 0 else rbind(entries[1,,drop=FALSE]*.5,matrix(0,end-1,nb)))))<1e-6)
      unk <- match(-1L,cohorts)
      if(!is.na(unk) && specname %in% c("primary","first_contact")) {
        unknown[[length(unknown)+1L]] <- data.table(release=rel,spec=specname,week_start=calendar[1:end],
           no_dose_deaths=du[,unk],no_dose_days=pu[,unk],vaccinated_deaths=dv[,unk],vaccinated_days=pv[,unk],
           undated_no_dose_days=if(contact) 0 else 7*sum(a$n[a$unknown_age & is.na(a$first_event)]))
      }
      audits[[k]] <- data.table(release=rel,spec=specname,raw_records=n_input,no_first_but_later_dose=incomplete_history,postdeath_dose_records=conflict_rows,
           eligible_before_age=sum(a$n),no_dated_contact=sum(pat$n[is.na(pat$first_event)]),
           first_event_after_end=sum(pat$n[!is.na(pat$first_event) & pat$first_event>end]),
           known_birth_records=sum(a$n[!a$unknown_age]),unknown_birth_records=sum(a$n[a$unknown_age]),
           death_week_is_entry=sum(a$n[!is.na(a$death) & a$death==a$enter & a$death<=end]),
           deaths_before_age=sum(dv+du),days_before_age=sum(pv+pu))
      known <- which(cohorts>=0); yrs <- cohorts[known]
      fractions <- mix(yrs,model$width,model$prior)
      # Age advances at every week's midpoint. Uniform birthdays within exact
      # years integrate both possible attained ages without random birthday draws.
      model_rows <- vector("list",end)
      boundary_newborn_deaths <- 0; impossible_birth_deaths <- 0
      for(w in seq_len(end)) {
        mid <- calendar[w]+3L; yr <- as.integer(format(mid,"%Y"))
        jan <- as.Date(sprintf("%d-01-01",yr))
        f <- as.numeric(mid-jan)/as.numeric(as.Date(sprintf("%d-01-01",yr+1L))-jan)
        if(specname=="birthday_early") f <- min(1,4*f)
        if(specname=="birthday_late") f <- max(0,4*f-3)
        yr_age <- yr-fractions$yob
        B <- matrix(0,length(yrs),length(ages))
        for(birthday in 0:1) {
          age <- yr_age-1L+birthday; index <- age>=0
          z <- data.table(i=match(fractions$birth[index],yrs),j=pmin(age[index]%/%5L,18L)+1L,
                          mass=fractions$fraction[index]*(if(birthday==1L) f else 1-f))
          z <- z[,.(mass=sum(mass)),by=.(i,j)]
          ix <- cbind(z$i,z$j); B[ix] <- B[ix]+z$mass
        }
        # Cohorts partly unborn can supply only their born fraction of exposure.
        # A dated death establishes birth; condition the death allocation on birth.
        mass <- rowSums(B); D <- B/pmax(mass,1e-15)
        # A January newborn may die in an ISO week whose midpoint is still in
        # December. A birth possible by Sunday locates this death in ages 0-4.
        newborn <- mass==0 & yrs<=as.integer(format(calendar[w]+6,"%Y"))
        D[newborn,1] <- 1
        boundary_newborn_deaths <- boundary_newborn_deaths+sum((du[w,known]+dv[w,known])[newborn])
        u <- as.numeric(pu[w,known] %*% B); v <- as.numeric(pv[w,known] %*% B)
        ud <- as.numeric(du[w,known] %*% D); vd <- as.numeric(dv[w,known] %*% D)
        unplaced <- sum((du[w,known]+dv[w,known])[mass==0 & !newborn])
        impossible_birth_deaths <- impossible_birth_deaths+unplaced
        stopifnot(abs(sum(ud+vd)+unplaced-sum(du[w,known]+dv[w,known]))<1e-7)
        model_rows[[w]] <- data.table(week_start=calendar[w],age=ages,no_dose_days=u,vaccinated_days=v,
                                        no_dose_deaths=ud,vaccinated_deaths=vd)
      }
      audits[[k]][, `:=`(newborn_week_boundary_deaths=boundary_newborn_deaths,death_before_possible_birth=impossible_birth_deaths)]
      z <- rbindlist(model_rows); z[, `:=`(release=rel,spec=specname)]
      result[[k]] <- z
    }
  }
  # National reference on the same ISO weeks, five-year ages and ESP weights.
  # Interpolate resident counts linearly between adjacent January snapshots.
  nat <- estat(src("eurostat_deaths"))[geo=="CZ" & sex=="T" & unit=="NR" & freq=="W" &
                                      age!="TOTAL" & age!="UNK"]
  nat[, `:=`(week_start=week_date(time),age=as.character(age))]
  nat[, band := fcase(age=="Y_LT5",0L,age=="Y_GE90",90L,
                         default=suppressWarnings(as.integer(sub("^Y([0-9]+)-.*$","\\1",age))))]
  stopifnot(!anyNA(nat$band))
  pop <- p[,.(resident=sum(value)),by=.(year=as.integer(time),band=pmin(single%/%5L*5L,90L))]
  pop[, day := as.Date(sprintf("%d-01-01",year))]
  nat <- nat[week_start>=as.Date("2020-03-02") & week_start<=last]
  nat[, persons := {this_band <- .BY$band; v <- pop[band==this_band]; approx(as.numeric(v$day),v$resident,xout=as.numeric(week_start+3L))$y},by=band]
  national <- nat[,.(release="Eurostat",spec="national",week_start,age=band,status="total",deaths=value,person_days=7*persons)]
  stopifnot(!anyNA(national),!anyDuplicated(national[,.(week_start,age)]))
  cells <- rbindlist(result)
  long <- rbindlist(lapply(c("no_dose","vaccinated","total"),function(s) {
    z <- cells[,.(release,spec,week_start,age)]
    z[, `:=`(status=s,deaths=if(s=="total") cells$no_dose_deaths+cells$vaccinated_deaths else cells[[paste0(s,"_deaths")]],
             person_days=if(s=="total") cells$no_dose_days+cells$vaccinated_days else cells[[paste0(s,"_days")]])]
    z
  }))
  long <- rbindlist(list(long,national),use.names=TRUE)
  # Both sources publish full death weeks from W10 in 2020. Carry earlier
  # transitions in the state calculation, then limit the reported comparison.
  long <- long[week_start>=as.Date("2020-03-02")]
  long[, week:=iso_week(week_start)]
  long <- merge(long,esp,by="age",sort=FALSE)
  save_table(long,"weekly_age_cells")
  summarize <- function(x,floor_age) {
    x[age>=floor_age,.(asmr=if(.N==sum(ages>=floor_age) && all(person_days>0))
                        sum(standard_n*deaths/person_days)/sum(standard_n)*per_year else NA_real_,
                        deaths=sum(deaths),person_days=sum(person_days),positive_exposure_strata=sum(person_days>0)),
      by=.(release,spec,week_start,week,status)][,min_age:=floor_age]
  }
  rates <- rbind(summarize(long,15),summarize(long,0))
  save_table(rates,"weekly_rates")
  save_table(rbindlist(unknown),"unknown_age_weeks"); save_table(rbindlist(audits),"eligibility")
  example <- data.table(example="Born in 1940; first dose 2021-W09; death 2021-W11",
      week=c("2021-W08","2021-W09","2021-W10","2021-W11","2021-W12"),
      no_dose_person_days=c(7,3.5,0,0,0),vaccinated_person_days=c(0,3.5,7,3.5,0),vaccinated_deaths=c(0,0,0,1,0))
  save_table(example,"worked_person_time_example")
  files <- sort(list.files(stage))
  save_table(data.table(file=files,sha256=vapply(file.path(stage,files),hash,character(1))),"outputs")
  if (!file.rename(stage,dest)) stop("Could not commit output directory: ",dest)
  message("Wrote ",dest)
  invisible(dest)
}
run_analysis()
