# 06_compare_resident_population
# Run after 01 and 02. Packages: data.table, digest, jsonlite.
# source("06_compare_resident_population.R") from the project root.
run_analysis <- function() {
  step <- "06_compare_resident_population"
  needed <- c("march","november","eurostat_population","csu_population_balance")
  prepared_files <- c("march_records.csv","november_records.csv","reconciliation.csv")
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
  ref <- as.Date("2024-01-01"); base_monday <- as.Date("2019-12-30")
  week_index <- function(v,kind) {
    u <- unique(v); valid <- !is.na(u) & nzchar(u); idx <- rep(NA_integer_,length(u))
    if(any(valid)) {
      dates <- if(kind=="day") monday(u[valid]) else week_date(u[valid])
      idx[valid] <- as.integer(dates-base_monday)%/%7L+1L
    }
    idx[match(v,u)]
  }
  pop <- estat(src("eurostat_population"))[geo=="CZ" & sex=="T" & unit=="NR" & freq=="A"]
  resident <- pop[time=="2024" & age=="TOTAL",value]
  balance <- fread(src("csu_population_balance"),colClasses="character")
  official <- balance[CasR2A=="2023" & IndicatorType=="2406K2",as.numeric(Hodnota)]
  stopifnot(length(resident)==1,length(official)==1,is.finite(official))
  at <- as.integer(ref-base_monday)%/%7L+1L
  qc <- fread(prepared_path("reconciliation.csv"))
  for(id in c("march","november")) {
    message("Summarizing ",id," records and their first dated events")
    path <- prepared_path(paste0(id,"_records.csv")); head <- names(fread(path,nrows=0))
    march <- id=="march"
    ds <- if(march) grep("^Datum_[0-9]+$",head,value=TRUE) else grep("^Datum_.*_davka$",head,value=TRUE)
    fields <- if(march) c("DatumUmrti",ds) else c("DatumUmrtiLPZ","Umrti","DatumPozitivity","DatumVysledku","Vylecen",
                                      grep("^(min|max)_",head,value=TRUE),ds)
    stopifnot(length(ds)==7)
    d <- fread(path,select=c("birth_year_low","birth_year_high","first_dose_week_start","death_week_start","any_dose_recorded"),
               colClasses=list(character=c("first_dose_week_start","death_week_start")))
    n <- nrow(d); first <- rep(NA_integer_,n)
    # Read a few event fields at a time, preserving CSV row order. This keeps
    # peak memory bounded without discarding later doses or hospital dates.
    for(chunk in split(fields,ceiling(seq_along(fields)/6))) {
      t <- fread(path,select=chunk,colClasses="character",na.strings=NULL,showProgress=FALSE)
      stopifnot(nrow(t)==n)
      for(k in chunk) first <- pmin(first,week_index(t[[k]],if(march) "day" else "week"),na.rm=TRUE)
      rm(t); invisible(gc())
    }
    d[, `:=`(first_event=first,dose=week_index(first_dose_week_start,"day"),death=week_index(death_week_start,"day"))]
    patterns <- d[,.(n=.N),by=.(birth=birth_year_low,end_birth=birth_year_high,first_event,dose,death,later_without_first=any_dose_recorded==1 & is.na(dose))]
    stopifnot(sum(patterns$n)==qc[release==id & metric=="retained_records",observed])
    # Index 1 is Monday 2019-12-30. Missing event dates are left blank.
    save_table(patterns[order(birth,first_event,dose,death,later_without_first)],paste0(id,"_event_patterns"))
    if(march) {
      march_counts <- patterns[,.(records=sum(n),no_dose_or_death=sum(n[is.na(dose)&!later_without_first&is.na(death)]),
                                earliest_birth=min(birth),latest_birth=max(end_birth))]
      march_counts[, resident_baseline_1_jan_2020 := pop[time=="2020" & age=="TOTAL",value]]
      births <- balance[CasR2A %in% as.character(2020:2022) & IndicatorType=="4355ZN",.(year=as.integer(CasR2A),live_births=as.numeric(Hodnota))]
      stopifnot(nrow(births)==3)
      save_table(births[order(year)],"march_construction_births")
      save_table(march_counts,"march_construction_context")
    } else {
      remaining <- patterns[is.na(death) | death>=at]
      table <- rbindlist(lapply(list(all_retained=patterns,lpz_death_before_reference=patterns[!is.na(death)&death<at],
                         no_earlier_lpz_death=remaining),function(t)
                         t[,.(known_band=sum(n[!is.na(birth)]),missing_band=sum(n[is.na(birth)]),records=sum(n))]),idcol="step")
      stopifnot(table$records[1]-table$records[2]==table$records[3])
      save_table(table,"record_reconciliation")
      groups <- remaining[,.(records=sum(n)),by=.(birth_evidence=fcase(is.na(birth),"Unknown birth band",end_birth<2024,"Born before 2024",
          birth>=2024,"Born in/after 2024",default="Birth band straddles 1 Jan 2024"),
          contact_evidence=fcase(is.na(first_event),"No dated event",first_event<at,"Dated before reference",
                                 first_event==at,"First event in reference week",default="First event after reference week"))]
      stopifnot(sum(groups$records)==sum(remaining$n))
      save_table(groups[order(birth_evidence,contact_evidence)],"evidence_at_reference_date")
      # Use attained age at year-end as a birth-band proxy for the resident
      # estimates. A January 1 birthday can cross the boundary. Pool the open tail.
      residents <- copy(pop[time=="2024" & age!="TOTAL"])
      residents[, attained := fcase(age=="Y_LT1",0L,age=="Y_OPEN",100L,
                                  default=suppressWarnings(as.integer(sub("^Y","",age))))]
      residents[, band_low := pmax(1920L,5L*((2023L-attained)%/%5L))]
      aged <- merge(remaining[,.(records=sum(n)),by=.(band_low=pmax(1920L,birth))],
                    residents[,.(residents=sum(value)),by=band_low],by="band_low",all=TRUE)
      aged[is.na(records),records:=0]; aged[is.na(residents),residents:=0]
      aged[, band_label := fcase(is.na(band_low),"Unknown age / birth band",band_low==1920L,"Born through 1924",
                               default=sprintf("%d-%d",band_low,band_low+4L))]
      aged[, record_minus_resident_proxy:=records-residents]
      stopifnot(sum(aged$records)==sum(remaining$n),sum(aged$residents)==resident)
      save_table(aged,"birth_band_resident_comparison")
      v <- tail(table,1)
      bench <- data.table(series=c("Eurostat residents","CSU residents","Known birth band","All remaining records"),
                          count=c(resident,official,v$known_band,v$records))
      bench[, difference_from_eurostat := count-resident]
      save_table(bench,"resident_benchmark")
      figure("V09_resident_population_comparison",10.8,5.4,function(){
        par(mar=c(4,12,3.8,2)); bars <- barplot(bench$count/1e6,names.arg=bench$series,horiz=TRUE,
              xlab="Millions",xlim=c(0,max(bench$count)/1e6*1.17),col=c("#376785","#376785","#67988a","#985e43"),border=NA)
        text(bench$count/1e6,bars,format(bench$count,big.mark=",",scientific=FALSE),pos=4,cex=.83)
        title("Residents and November records at 1 January 2024",adj=0)
        mtext("Records remain after removing earlier LPZ deaths. Residence and migration dates are unavailable.",3,.35,adj=0,cex=.83)
      })
    }
    rm(d,first,patterns); invisible(gc())
  }
  files <- sort(list.files(stage))
  save_table(data.table(file=files,sha256=vapply(file.path(stage,files),hash,character(1))),"outputs")
  if (!file.rename(stage,dest)) stop("Could not commit output directory: ",dest)
  message("Wrote ",dest)
  invisible(dest)
}
run_analysis()
