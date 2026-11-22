# 05_profile_missing_demographics
# From the project root: source("05_profile_missing_demographics.R")
# Requires steps 01 and 02 and packages data.table and digest. Reads the Ministry
# daily case source directly; step 04's output files are optional.
run_analysis <- function() {
  step <- "05_profile_missing_demographics"
  needed <- c("march","november","moh_cases")
  prepared_files <- c("november_records.csv","reconciliation.csv")
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
  parent <- file.path(root,"data","prepared")
  pinned <- Sys.getenv("CZ_PREPARED_DIR")
  dirs <- if (nzchar(pinned)) pinned else list.dirs(parent,recursive=FALSE,full.names=TRUE)
  committed <- dirs[file.exists(file.path(dirs,"outputs.csv"))]
  committed <- committed[order(file.info(file.path(committed,"outputs.csv"))$mtime,decreasing=TRUE)]
  prep <- NULL
  for (d in committed) {
    identity <- fread(file.path(d,"input_identity.csv"))
    if (all(c("march","november") %in% identity$release) &&
        all(identity$source_sha256[match(c("march","november"),identity$release)] ==
            inputs$sha256[match(c("march","november"),inputs$id)])) { prep <- d; break }
  }
  if (is.null(prep)) stop("No completed step 02 tables match both release hashes in the manifest.")
  inventory <- fread(file.path(prep,"outputs.csv"),select=c("file","sha256"))
  if (!all(prepared_files %in% inventory$file)) stop("Incomplete step 02 inventory.")
  prepared_path <- function(name) file.path(prep,name)
  parents <- inventory[match(prepared_files,file),.(id=file,path=file.path("data","prepared",basename(prep),file),sha256)]
  for (i in seq_len(nrow(parents))) {
    if (hash(prepared_path(parents$id[i]))!=parents$sha256[i]) stop("Prepared checksum mismatch: ",parents$id[i])
  }
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
  d <- fread(prepared_path("november_records.csv"),
       select=c("birth_missing","sex_missing","any_dose_recorded","DatumPozitivity","Umrti","DatumUmrtiLPZ",
                "DCCI","death_week_start","first_dose_week_start"),
       colClasses=list(character=c("DatumPozitivity","Umrti","DatumUmrtiLPZ","DCCI")),
       na.strings=NULL,showProgress=FALSE)
  qc <- fread(prepared_path("reconciliation.csv"))[release=="november"]
  stopifnot(nrow(d)==qc[metric=="retained_records",observed])
  has <- function(x) !is.na(x) & nzchar(x)
  d[, `:=`(positive_recorded=has(DatumPozitivity),surveillance_recorded=has(Umrti),
            lpz_recorded=has(DatumUmrtiLPZ),dose_recorded=as.logical(any_dose_recorded),
            missing_birth=as.logical(birth_missing),missing_sex=as.logical(sex_missing))]
  stopifnot(!anyNA(d[,.(dose_recorded,missing_birth,missing_sex)]))
  # DCCI is a published score. Zero supplies no claims-coverage indicator.
  cells <- d[,.(records=.N),by=.(positive_recorded,dose_recorded,lpz_recorded,surveillance_recorded,
                                    DCCI,missing_birth,missing_sex)]
  setorderv(cells,setdiff(names(cells),"records"))
  save_table(cells,"observed_field_combinations")
  profile <- cells[,.(records=sum(records),missing_birth_records=sum(records[missing_birth]),
                      missing_sex_records=sum(records[missing_sex]),
                      missing_both_records=sum(records[missing_birth & missing_sex])),
                   by=.(positive_recorded,dose_recorded)]
  profile[, missing_birth_pct := 100*missing_birth_records/records]
  save_table(profile[order(positive_recorded,dose_recorded)],"positivity_and_dose_groups")
  score <- cells[,.(records=sum(records)),by=.(DCCI,missing_birth,missing_sex)]
  save_table(score[order(DCCI,missing_birth,missing_sex)],"dcci_and_demographics")
  counts <- cells[,.(records=sum(records),birth_missing=sum(records[missing_birth]),
                     sex_missing=sum(records[missing_sex]),both_missing=sum(records[missing_birth & missing_sex]),
                     birth_missing_with_dose=sum(records[missing_birth & dose_recorded]),
                     birth_missing_with_dose_and_death=sum(records[missing_birth & dose_recorded & (lpz_recorded | surveillance_recorded)]),
                     birth_missing_with_surveillance_no_dose=sum(records[missing_birth & surveillance_recorded & !dose_recorded]))]
  stopifnot(counts$birth_missing==qc[metric=="birth_missing",observed],
            counts$both_missing==qc[metric=="both_demographics_missing",observed],
            counts$records==sum(profile$records))
  save_table(counts,"release_totals")

  # Only a dated LPZ death is anchored to a week. A surveillance date on the
  # same row is a stratifier, even when the two death fields give different weeks.
  deaths <- d[lpz_recorded==TRUE,.(week_start=as.Date(death_week_start),
          first=as.Date(first_dose_week_start),
          dose_recorded,missing_birth,missing_sex,surveillance_recorded)]
  rm(d); invisible(gc())
  check <- data.table(lpz_records=nrow(deaths),undatable_lpz=sum(is.na(deaths$week_start)))
  save_table(check,"death_date_accounting")
  deaths <- deaths[!is.na(week_start)]
  statuses <- c("No recorded dose","First dose before death week","First dose in death week",
                "First dose after death week","Dose recorded, first date unavailable")
  deaths[, status := fcase(!dose_recorded,statuses[1],first<week_start,statuses[2],
                           first==week_start,statuses[3],first>week_start,statuses[4],default=statuses[5])]
  weeks <- seq(min(deaths$week_start),max(deaths$week_start),by="7 days")
  obs <- deaths[,.(deaths=.N,missing_birth_deaths=sum(missing_birth),missing_sex_deaths=sum(missing_sex)),
                  by=.(week_start,status,surveillance_recorded)]
  weekly <- merge(CJ(week_start=weeks,status=statuses,surveillance_recorded=c(FALSE,TRUE)),
                  obs,by=c("week_start","status","surveillance_recorded"),all.x=TRUE)
  fields <- c("deaths","missing_birth_deaths","missing_sex_deaths")
  for (nm in fields) set(weekly,which(is.na(weekly[[nm]])),nm,0L)
  pct <- function(x,n) fifelse(n>0,100*x/n,NA_real_)
  weekly[, `:=`(week=iso_week(week_start),missing_birth_pct=pct(missing_birth_deaths,deaths),
                missing_sex_pct=pct(missing_sex_deaths,deaths))]
  stopifnot(sum(weekly$deaths)+check$undatable_lpz==qc[metric=="all_cause_death_recorded",observed],
            sum(weekly$missing_birth_deaths)==sum(deaths$missing_birth))
  save_table(weekly,"weekly_deaths_by_dose_and_surveillance")
  total <- weekly[,lapply(.SD,sum),by=.(week_start,week),.SDcols=fields]
  total[, missing_birth_pct := pct(missing_birth_deaths,deaths)]
  save_table(total,"weekly_deaths_all")
  split_surv <- weekly[,lapply(.SD,sum),by=.(week_start,week,surveillance_recorded),.SDcols=fields]
  split_surv[, missing_birth_pct := pct(missing_birth_deaths,deaths)]
  save_table(split_surv,"weekly_deaths_by_surveillance")
  split_dose <- weekly[,lapply(.SD,sum),by=.(week_start,week,status),.SDcols=fields]
  split_dose[, missing_birth_pct := pct(missing_birth_deaths,deaths)]
  save_table(split_dose,"weekly_deaths_by_dose_status")
  intervals <- data.table(period=c("Spring 2021","Delta window","Summer 2022"),
                          start=c("2021-W09","2021-W47","2022-W23"),end=c("2021-W20","2021-W50","2022-W35"))
  periods <- rbindlist(lapply(seq_len(nrow(intervals)),function(i) {
    t <- weekly[week>=intervals$start[i] & week<=intervals$end[i]]
    stopifnot(uniqueN(t$week)==1L+as.integer(week_date(intervals$end[i])-week_date(intervals$start[i]))%/%7L)
    collapse <- function(by) t[,lapply(.SD,sum),by=by,.SDcols=fields]
    strata <- rbindlist(list(collapse(c("status","surveillance_recorded"))[, surveillance := fifelse(surveillance_recorded,"present","absent")][,surveillance_recorded:=NULL],
        collapse("status")[,surveillance:="all"],collapse("surveillance_recorded")[, `:=`(status="All statuses",surveillance=fifelse(surveillance_recorded,"present","absent"))][,surveillance_recorded:=NULL],
        t[,lapply(.SD,sum),.SDcols=fields][,`:=`(status="All statuses",surveillance="all")]),use.names=TRUE)
    strata[, `:=`(period=intervals$period[i],first_week=intervals$start[i],last_week=intervals$end[i],
                 missing_birth_pct=pct(missing_birth_deaths,deaths))]
    strata
  }))
  save_table(periods,"pooled_periods")
  day <- fread(src("moh_cases"),select=c("datum","prirustkovy_pocet_nakazenych"))
  stopifnot(!anyDuplicated(day$datum))
  cases <- day[,.(reported_cases=sum(prirustkovy_pocet_nakazenych),observed_days=.N),by=.(week_start=monday(datum))]
  cases[observed_days!=7L,reported_cases:=NA_real_]
  spring <- merge(total[week>="2021-W09" & week<="2021-W20"],cases,by="week_start",all.x=TRUE)
  b <- dcast(split_surv[week>="2021-W09" & week<="2021-W20"],week_start~surveillance_recorded,
             value.var=c("deaths","missing_birth_deaths","missing_birth_pct"))
  setnames(b,names(b),sub("_TRUE$","_with_surveillance",sub("_FALSE$","_without_surveillance",names(b))))
  spring <- merge(spring,b,by="week_start",all.x=TRUE)
  stopifnot(nrow(spring)==12L)
  save_table(spring,"spring_2021_weeks_09_20")
  end <- week_date("2023-W52")
  figure("V07_missing_birth_band_among_deaths",13.3,8.1,function() {
    par(mfrow=c(2,2),mar=c(2.5,5.1,2.1,.8),oma=c(2.5,.5,3.1,.55))
    t <- total[week_start<=end]
    s <- split_surv[week_start<=end]
    u <- split_dose[week_start<=end]
    groups <- list(
      list("All LPZ deaths"=t,"Surveillance field absent"=s[surveillance_recorded==FALSE],
           "Surveillance field present"=s[surveillance_recorded==TRUE]),
      list("All LPZ deaths"=t,"No dose recorded"=u[status==statuses[1]],
           "First dose before death week"=u[status==statuses[2]]))
    colors <- c("#25394b","#ae633c","#157875")
    for (field in c("missing_birth_deaths","missing_birth_pct")) for (col in 1:2) {
      g <- groups[[col]]
      vals <- unlist(lapply(g,function(z) z[[field]]))
      plot(range(t$week_start),range(c(0,vals),na.rm=TRUE),type="n",xaxt="n",xlab="",
           ylab=if(field=="missing_birth_deaths") "Deaths missing birth band" else "% of LPZ deaths in group")
      axis.Date(1,at=as.Date(paste0(2020:2024,"-01-01")),format="%Y"); abline(h=0,col="grey80")
      for (k in seq_along(g)) lines(g[[k]]$week_start,g[[k]][[field]],col=colors[k],lty=k,lwd=if(k==1) 2 else 1.5)
      if(field=="missing_birth_deaths") {
        title(if(col==1) "By COVID surveillance death field" else "By recorded first-dose timing",cex.main=.94)
        legend("topright",names(g),col=colors,lty=1:3,lwd=2,bty="n",cex=.77)
      }
    }
    mtext("When is the birth band missing from a death record?",3,outer=TRUE,line=1.4,adj=0,font=2,cex=1.24)
    mtext("Each percentage uses deaths in its own group that week. Totals include same-week and conflicting dose dates, tabulated separately in the CSV.",1,outer=TRUE,line=.65,adj=0,cex=.78)
  })
  figure("V08_spring_2021_cases_and_missing_age",11.4,7.6,function() {
    par(mfrow=c(2,1),mar=c(2.7,5,1.3,1.1),oma=c(2,.5,3,.5))
    x <- 9:20
    plot(x,spring$reported_cases/1000,type="o",pch=16,col="#216c84",lwd=2,xlab="",ylab="Reported cases (thousands)",xaxt="n")
    axis(1,at=x); z <- list(All=spring$missing_birth_pct,Absent=spring$missing_birth_pct_without_surveillance,
                            Present=spring$missing_birth_pct_with_surveillance)
    cols <- c("#25394b","#ae633c","#157875")
    plot(range(x),range(c(0,unlist(z)),na.rm=TRUE),type="n",xaxt="n",xlab="",ylab="% of LPZ deaths missing birth band")
    axis(1,at=x); for(k in seq_along(z)) lines(x,z[[k]],type="o",pch=15+k,col=cols[k],lwd=1.6,lty=k)
    legend("topright",c("All deaths","Surveillance absent","Surveillance present"),col=cols,lty=1:3,lwd=2,bty="n",cex=.78)
    mtext("ISO week of 2021",1,line=1.8)
    mtext("Case counts and missing birth information in spring 2021",3,outer=TRUE,line=1.3,adj=0,font=2,cex=1.2)
    mtext("The panels describe the same calendar weeks. Their timing alone cannot distinguish changes in epidemic composition from changes in recording.",1,outer=TRUE,line=.35,adj=0,cex=.76)
  })
  files <- sort(list.files(stage))
  save_table(data.table(file=files,sha256=vapply(file.path(stage,files),hash,character(1))),"outputs")
  if (!file.rename(stage,dest)) stop("Could not commit output directory: ",dest)
  message("Wrote ",dest)
  invisible(dest)
}
run_analysis()
