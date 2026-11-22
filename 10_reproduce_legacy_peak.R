# 10_reproduce_legacy_peak
# Standalone R replay of the v0.1 weekly_asmr.R / imputation_2.pl /
# imputation_4.pl age and allocation rules. Requires 01-02. Uses original row order.
# Historical date reassignment, including its edge cases, is confined to this appendix.
run_analysis <- function() {
  step <- "10_reproduce_legacy_peak"
  needed <- c("march","november","eurostat_deaths")
  prepared_files <- c("november_records.csv")
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
  origin <- as.Date("2019-12-30"); calendar <- seq(origin,as.Date("2025-12-29"),by="7 days")
  iso <- iso_week(calendar); yr <- as.integer(substr(iso,1,4)); calyear <- as.integer(format(calendar,"%Y"))
  weeks <- which(iso>="2020-W10" & iso<="2023-W52")
  peak <- match("2023-W01",iso); ages <- esp[age>=15,age]
  weights <- esp[age>=15,standard_n/sum(standard_n)]; bin <- function(x) pmin(90L,pmax(0L,as.integer(x)%/%5L*5L))
  path <- prepared_path("november_records.csv")
  dose_fields <- paste0("Datum_",c("Prvni","Druha","Treti","Ctvrta","Pata","Sesta","Sedma"),"_davka")
  base <- fread(path,select=c("source_row","birth_year_high","death_week_start","surveillance_week_start","first_dose_week_start"),
               colClasses=list(character=c("death_week_start","surveillance_week_start","first_dose_week_start")))
  dates <- as.character(calendar)
  index <- function(x,lookup) {
    i <- match(x,lookup); if(any(!is.na(x) & x!="" & is.na(i))) stop("Date outside the replay calendar.")
    i
  }
  stopifnot(!is.unsorted(base$source_row,strictly=TRUE))
  base[, `:=`(lpz=index(death_week_start,dates),surv=index(surveillance_week_start,dates),v=index(first_dose_week_start,dates))]
  base[, `:=`(death_week_start=NULL,surveillance_week_start=NULL,first_dose_week_start=NULL)]
  base[, dead:=fcase(is.na(lpz),surv,is.na(surv),lpz,abs(lpz-surv)<=1L,pmin(lpz,surv),default=pmax(lpz,surv))]
  latest <- base$v; raw_week <- sub("-W","-",iso)
  for(chunk in split(dose_fields[-1L],rep(1:2,each=3))) {
    t <- fread(path,select=chunk,colClasses="character"); stopifnot(nrow(t)==nrow(base))
    for(nm in chunk) latest <- pmax(latest,index(t[[nm]],raw_week),na.rm=TRUE)
    rm(t); invisible(gc())
  }
  conflict <- which(latest>base$dead); changed_date <- length(conflict)
  # The original rule used the Gregorian year of the last dose's ISO Monday.
  base[conflict,dead:=match(sprintf("%d-W01",calyear[latest[conflict]]+1L),iso)]
  stopifnot(!anyNA(base$dead[conflict])); rm(latest)
  # Preserve the single missing-band vaccinated record as an explicit diagnostic.
  tracer <- base[is.na(birth_year_high) & !is.na(v),.(records=.N,with_lpz=sum(!is.na(lpz)),with_surveillance=sum(!is.na(surv)),
                                                 recorded_vaccine_weeks=paste(sort(unique(iso[v])),collapse="|"))]
  save_table(tracer,"missing_band_vaccinated_records")
  base[, `:=`(lpz=NULL,surv=NULL)]
  d <- base[!is.na(dead),.(source_row,dead,v,old=birth_year_high)]
  d[, `:=`(new=old,source_age=bin(yr[dead]-old),margin_age=bin(calyear[dead]-old))]
  eu <- estat(src("eurostat_deaths"))[age %in% c(paste0("Y",ages[-16],"-",ages[-16]+4),"Y_GE90") &
                                      geo=="CZ" & sex=="T" & unit=="NR" & freq=="W"]
  eu[, `:=`(w=match(time,iso),a=ifelse(age=="Y_GE90",90L,as.integer(sub("^Y([0-9]+).*","\\1",sub("Y_GE90","Y90",age)))))]
  # Derive quotas from weekly Eurostat margins exactly as in the archived Perl.
  # Its across-age loop records one outgoing slide at the target too, unless 90+.
  quotas <- function(observed,target,blank,across) {
    moved <- matrix(0L,16,16); current <- observed
    for(i in 1:16) {
      for(j in c(i+1L,i-1L)) if(j>=1 && j<=16) {
        n <- min(max(0,current[i]-target[i]),max(0,target[j]-current[j]))
        if(n>0) {moved[i,j] <- moved[i,j]+n; current[i] <- current[i]-n; current[j] <- current[j]+n}
      }
    }
    if(across) for(i in 1:15) {
      left <- current[i]-target[i]; if(left<=0) next
      plan <- integer(16)
      for(j in 16:(i+1L)) {
        n <- min(left,max(0,target[j]-current[j])); plan[j]<-n; left<-left-n
        if(left==0) break
      }
      for(j in which(plan>0)) {
        n <- plan[j]
        if(j>i+1L && any(current[(i+1L):(j-1L)]<n)) stop("Unsupported legacy cascade.")
        for(g in i:min(j,15L)) moved[g,g+1L] <- moved[g,g+1L]+n
        current[i] <- current[i]-n; current[j] <- current[j]+n
      }
    }
    deficit <- pmax(0,target-current); use <- min(blank,sum(deficit)); fill <- integer(16)
    if(use>0) {
      pct <- use*100/sum(deficit)
      fill <- floor(deficit*pct/100+.5)
      if(sum(fill)>use) fill[16] <- fill[16]-(sum(fill)-use)
      if(sum(fill)!=use) stop("The historical deficit-rounding rule does not reconcile.")
    }
    extra <- blank-use
    if(extra>0 && extra<16) fill[seq(16,length.out=extra,by=-1)] <- fill[seq(16,length.out=extra,by=-1)]+1L
    if(extra>=16) {
      add <- floor(extra*(target*100/sum(target))/100+.5); add[16] <- add[16]+extra-sum(add)
      fill <- fill+add
    }
    stopifnot(sum(fill)==blank,all(fill>=0)); list(slide=moved,fill=fill)
  }
  by_week <- split(seq_len(nrow(d)),d$dead)
  quota_rows <- list()
  for(w in weeks) {
    rows <- by_week[[as.character(w)]]; if(!length(rows)) next
    week_id <- w; target <- eu[w==week_id][order(a),value]
    stopifnot(length(target)==16,!anyNA(target))
    counts <- tabulate(match(d$margin_age[rows],ages),nbins=16)
    unknown <- rows[is.na(d$old[rows])]
    q <- quotas(counts,target,length(unknown),iso[w]>="2021-W11")
    if(length(unknown)) {
      to <- rep(ages,q$fill); endpoint <- ifelse(to==90L,999L,to+4L)
      set(d,unknown,"new",as.integer(yr[w]-endpoint))
    }
    used <- matrix(0L,16,16)
    for(i in which(rowSums(q$slide)>0)) {
      from <- rows[which(d$source_age[rows]==ages[i])]
      to <- rep(seq_along(ages),q$slide[i,])
      take <- min(length(from),length(to)); if(!take) next
      from <- head(from,take); to <- head(to,take)
      set(d,from,"new",as.integer(d$old[from]+ifelse(to>i,-5L,5L)))
      used[i,] <- tabulate(to,nbins=16)
    }
    ix <- which(q$slide>0,arr.ind=TRUE)
    quota_rows[[as.character(w)]] <- rbindlist(list(
      data.table(week=rep(iso[w],nrow(ix)),operation=rep("move known band",nrow(ix)),from=ages[ix[,1]],to=ages[ix[,2]],requested=q$slide[ix],applied=used[ix]),
      data.table(week=iso[w],operation="assign missing band",from=NA_integer_,to=ages,requested=q$fill,applied=q$fill)))
  }
  save_table(rbindlist(quota_rows),"weekly_reallocation_quotas")
  affected <- !is.na(d$old) & d$new!=d$old
  audit <- data.table(death_dates_moved_after_last_dose=changed_date,known_bands_changed=sum(affected),
        changed_with_dose_by_death=sum(affected & !is.na(d$v) & d$v<=d$dead),missing_bands_allocated=sum(is.na(d$old)&!is.na(d$new)),
        synthetic_age_999=sum(yr[d$dead]-d$new==999,na.rm=TRUE))
  save_table(audit,"historical_changes")
  # People without a death keep the survivor reference age throughout. Deaths
  # after the plot still affect the outcome-dependent age rule in earlier weeks.
  live <- base[is.na(dead) & !is.na(birth_year_high),.(n=.N),by=.(v,end=birth_year_high)]
  take <- function(stage_no) {
    endpoint <- if(stage_no==3L) d$new else d$old
    g <- if(stage_no==1L) bin(2024L-endpoint) else bin(yr[d$dead]-endpoint)
    deceased <- data.table(age=g,v=d$v,dead=d$dead)[!is.na(age),.(n=.N),by=.(age,v,dead)]
    surv <- live[,.(age=bin((if(stage_no==1L) 2024L else 2023L)-end),v,dead=NA_integer_,n)]
    a <- rbind(deceased,surv)[age>=15]; a[v>=match("2024-W01",iso),v:=NA_integer_]
    a <- a[,.(n=sum(as.numeric(n))),by=.(age,v,dead)]
    ans <- lapply(ages,function(age0) {
      z <- a[age==age0]; nv <- numeric(length(calendar)); nd <- nv
      v <- z[!is.na(v),.(n=sum(n)),by=v]; nv[v$v] <- v$n
      dd <- z[!is.na(v) & !is.na(dead) & v<=dead,.(n=sum(n)),by=dead]; nd[dd$dead] <- dd$n
      den <- cumsum(nv)-shift(cumsum(nd),fill=0)
      data.table(stage=stage_no,week=iso[weeks],age=age0,vaccinated_deaths=nd[weeks],
                 vaccinated_at_risk=den[weeks],weight=weights[match(age0,ages)])
    })
    rbindlist(ans)
  }
  cells <- rbindlist(lapply(1:3,take))
  cells[, contribution := fifelse(vaccinated_at_risk>0,52.18e5*weight*vaccinated_deaths/vaccinated_at_risk,0)]
  weekly <- cells[,.(vaccinated_asmr=sum(contribution)),by=.(stage,week)]
  save_table(weekly,"weekly_legacy_vaccinated_rates"); save_table(cells,"legacy_age_cells")
  peak_cells <- cells[week==iso[peak]]; save_table(peak_cells,"peak_age_contributions")
  out <- weekly[week==iso[peak]][order(stage)]
  out[, `:=`(rule=c("Fixed age from 2024","Outcome-dependent fixed age","Outcome-dependent age and death reallocation"),
            change=vaccinated_asmr-shift(vaccinated_asmr))]
  save_table(out,"peak_reconstruction")
  maxima <- weekly[, .SD[vaccinated_asmr==max(vaccinated_asmr)],by=stage]
  save_table(maxima,"maximum_weeks_by_stage")
  checks <- peak_cells[stage==3,.(age,original_contribution=contribution,
    contribution_with_one_extra_record=52.18e5*weight*vaccinated_deaths/(vaccinated_at_risk+1))]
  checks[,vaccinated_asmr_with_survivor:=out$vaccinated_asmr[3]-original_contribution+contribution_with_one_extra_record]
  save_table(checks,"single_survivor_sensitivity")
  figure("V12_legacy_peak",10.2,5.8,function(){
    par(mar=c(4.5,11,3.7,1.4))
    b <- barplot(out$vaccinated_asmr,horiz=TRUE,names.arg=c("Original rule","Change age rule","Also reallocate deaths"),
        col=c("#486779","#678d83","#986244"),border=NA,xlim=c(0,max(out$vaccinated_asmr)*1.18),xlab="Annualized ASMR per 100,000 at ages 15+")
    text(out$vaccinated_asmr,b,sprintf("%.1f",out$vaccinated_asmr),pos=4)
    title("Reconstructing the vaccinated peak in 2023-W01",adj=0)
    mtext("Archived procedure, using 52.18 weeks per year.",3,.35,adj=0,cex=.81)
  })
  files <- sort(list.files(stage))
  save_table(data.table(file=files,sha256=vapply(file.path(stage,files),hash,character(1))),"outputs")
  if (!file.rename(stage,dest)) stop("Could not commit output directory: ",dest)
  message("Wrote ",dest)
  invisible(dest)
}
run_analysis()
