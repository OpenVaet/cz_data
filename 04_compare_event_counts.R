# 04_compare_event_counts
# From the project root: source("04_compare_event_counts.R")
# Requires steps 01 and 02 and packages data.table, digest and jsonlite.
# Set CZ_MANIFEST or CZ_PREPARED_DIR to select specific input versions.
run_analysis <- function() {
  step <- "04_compare_event_counts"
  needed <- c("march","november","moh_doses","moh_cases","moh_cases_week","ecdc_doses","eurostat_deaths")
  prepared_files <- c("march_records.csv","november_records.csv","november_infection_episodes.csv","reconciliation.csv")
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
  weeks <- seq(week_date("2020-W01"),week_date("2023-W52"),by="7 days")
  output <- list()
  # Fill zeros only for an enumerated release's event tally inside its observed
  # coverage. An absent source-week in a published aggregate remains missing.
  add <- function(tab,series,event,first,last,enumerated=FALSE,first_full=first,last_full=last) {
    stopifnot(!anyDuplicated(tab$week_start))
    a <- merge(data.table(week_start=weeks),tab,by="week_start",all.x=TRUE)
    a[, available := week_start>=first & week_start<=last]
    a[, observation := fifelse(!available,"outside coverage",
                               fifelse(is.na(count),"unreported","reported"))]
    if (enumerated) a[available & is.na(count), `:=`(count=0,observation="no events in covered extract")]
    a[, full_week := available & week_start>=first_full & week_start<=last_full]
    if ("days_in_source" %in% names(a) && series=="Ministry daily cases") a[is.na(days_in_source) | days_in_source!=7L,full_week:=FALSE]
    if ("published_complete" %in% names(a)) a[is.na(published_complete) | !published_complete,full_week:=FALSE]
    a[, `:=`(series=series,event=event,week=iso_week(week_start))]
    output[[length(output)+1L]] <<- a
  }
  # Group the raw strings first; converting millions of repeated dates is unnecessary.
  tally <- function(x,precision) {
    t <- data.table(v=x)[!is.na(v) & v!="",.(count=.N),by=v]
    t[, week_start := if (precision=="week") week_date(v) else monday(v)]
    t[,.(count=sum(count)),by=week_start]
  }
  frozen <- fread(prepared_path("reconciliation.csv"))
  for (release in c("march","november")) {
    message("Counting events in ",release)
    release_id <- release
    march <- release=="march"
    label <- if (march) "March FOI" else "November NZIP"
    doses <- if (march) paste0("Datum_",1:7) else paste0("Datum_",c("Prvni","Druha","Treti","Ctvrta","Pata","Sesta","Sedma"),"_davka")
    d <- fread(prepared_path(paste0(release,"_records.csv")),select=c(doses,"death_week_start"),colClasses="character",showProgress=FALSE)
    stopifnot(nrow(d)==frozen[release==release_id & metric=="retained_records",observed])
    deaths <- tally(d$death_week_start,"day")
    stopifnot(sum(deaths$count)==frozen[release==release_id & metric=="all_cause_death_recorded",observed])
    # March starts on Wed 1 Jan 2020 and ends Sat 31 Dec 2022. November's
    # LPZ field first has a death in 2020-W10. The article ends at 2023-W52.
    add(deaths,label,"All-cause deaths",min(deaths$week_start),max(deaths$week_start),TRUE,
        first_full=if (march) week_date("2020-W02") else min(deaths$week_start),
        last_full=if (march) week_date("2022-W51") else max(deaths$week_start)-7)
    parts <- lapply(doses,function(k) tally(d[[k]],if(march) "day" else "week"))
    all <- rbindlist(parts)[,.(count=sum(count)),by=week_start]
    for (event in c("First doses","All doses")) {
      t <- if (event=="First doses") parts[[1]] else all
      add(t,label,event,min(all$week_start),max(all$week_start),TRUE,
          first_full=week_date("2020-W53"),last_full=max(all$week_start)-7)
    }
    rm(d); invisible(gc())
  }
  e <- fread(prepared_path("november_infection_episodes.csv"),select=c("positive_week_start","episode_class"),colClasses="character")
  ep <- e[positive_week_start!="" & !is.na(positive_week_start),.(count=.N),by=.(week_start=as.Date(positive_week_start),episode_class)]
  stopifnot(sum(ep$count)+frozen[release=="november" & metric=="undated_episodes",observed]==nrow(e))
  for (event in c("Primary infections","Repeat infections","All infection episodes")) {
    t <- switch(event,"Primary infections"=ep[episode_class=="primary"],"Repeat infections"=ep[episode_class=="repeat"],
                ep[,.(count=sum(count)),by=week_start])
    add(t[,.(week_start,count)],"November NZIP",event,min(ep$week_start),max(ep$week_start),TRUE,
        first_full=week_date("2020-W10"),last_full=max(ep$week_start)-7)
  }
  rm(e); invisible(gc())
  eu <- estat(src("eurostat_deaths"))[age=="TOTAL" & geo=="CZ" & sex=="T" & unit=="NR" & freq=="W"]
  eu[, week_start := week_date(time)]
  add(eu[,.(week_start,count=value,eurostat_flag=flag)],"Eurostat","All-cause deaths",min(eu$week_start),max(eu$week_start))

  moh <- fread(src("moh_doses"),select=c("datum","prvnich_davek","celkem_davek"))
  moh[, week_start := monday(datum)]
  for (event in c("First doses","All doses")) {
    field <- if (event=="First doses") "prvnich_davek" else "celkem_davek"
    t <- moh[,.(count=sum(get(field)),days_in_source=uniqueN(datum)),by=week_start]
    add(t,"Ministry doses",event,min(t$week_start),max(t$week_start),
        first_full=week_date("2020-W53"),last_full=monday(max(moh$datum)-6))
  }
  # This file lists administration events. A date with no row is not an explicit daily zero.
  ecdc <- fread(src("ecdc_doses"))[ReportingCountry=="CZ" & Region=="CZ" & TargetGroup %in% c("ALL","Age<18")]
  fields <- c("FirstDose","SecondDose",paste0("DoseAdditional",1:5),"UnknownDose")
  stopifnot(all(fields %in% names(ecdc)),!anyDuplicated(ecdc[,.(YearWeekISO,TargetGroup,Vaccine)]))
  ecdc[, week_start := week_date(YearWeekISO)]
  ecdc[, administered := rowSums(.SD),.SDcols=fields]
  for (event in c("First doses","All doses")) {
    field <- if (event=="First doses") "FirstDose" else "administered"
    t <- ecdc[,.(count=sum(get(field))),by=.(week_start,TargetGroup)]
    for (g in c("ALL","Age<18")) {
      name <- if (g=="ALL") "ECDC adults" else "ECDC under 18"
      t1 <- t[TargetGroup==g,.(week_start,count)]
      add(t1,name,event,min(t1$week_start),max(t1$week_start),first_full=week_date("2020-W53"))
    }
    # A blank under-18 return stays missing. Overlapping age subgroups are excluded.
    wide <- dcast(t,week_start~TargetGroup,value.var="count")
    add(wide[,.(week_start,count=ALL+`Age<18`)],"ECDC combined",event,min(wide$week_start),max(wide$week_start),
        first_full=week_date("2020-W53"))
  }
  daily_names <- c("Primary infections"="prirustkovy_pocet_nove_nakazenych_primoinfekce",
                   "Repeat infections"="prirustkovy_pocet_nove_nakazenych_reinfekce",
                   "All infection episodes"="prirustkovy_pocet_nakazenych")
  daily <- fread(src("moh_cases"),select=c("datum",unname(daily_names)))
  stopifnot(!anyDuplicated(daily$datum))
  daily[, week_start := monday(datum)]
  published <- fread(src("moh_cases_week"))
  published[, week_start := week_date(tyden)]
  stopifnot(!anyDuplicated(published$week_start),all(monday(published$tyden_od)==published$week_start))
  published[, published_complete := as.Date(tyden_od)==week_start & as.Date(tyden_do)==week_start+6]
  published[, repeats := prvni_reinfekce+druhe_reinfekce+treti_a_dalsi_reinfekce]
  weekly_names <- c("Primary infections"="primoinfekce","Repeat infections"="repeats","All infection episodes"="nove_pripady_celkem")
  for (event in names(daily_names)) {
    t <- daily[,.(count=sum(get(daily_names[[event]])),days_in_source=uniqueN(datum)),by=week_start]
    add(t,"Ministry daily cases",event,min(t$week_start),max(t$week_start))
    t <- published[,.(week_start,count=get(weekly_names[[event]]),published_complete)]
    add(t,"Ministry weekly cases",event,min(t$week_start),max(t$week_start))
  }
  counts <- rbindlist(output,fill=TRUE)
  setorder(counts,event,series,week_start)
  save_table(counts,"weekly_counts")
  refs <- c("First doses"="Ministry doses","All doses"="Ministry doses","All-cause deaths"="Eurostat",
            setNames(rep("Ministry daily cases",3L),names(daily_names)))
  counts[, reference := unname(refs[event])]
  ref <- counts[series==reference,.(event,week_start,reference_count=count,reference_full=full_week)]
  compare <- merge(counts[series!=reference],ref,by=c("event","week_start"),all.x=TRUE)
  compare[, signed_difference := count-reference_count]
  compare[, absolute_difference := abs(signed_difference)]
  compare[, relative_difference_pct := fifelse(reference_count>0,100*signed_difference/reference_count,NA_real_)]
  compare[, ratio_to_reference := fifelse(reference_count>0,count/reference_count,NA_real_)]
  compare[, common_complete := full_week & reference_full & !is.na(count) & !is.na(reference_count)]
  compare[, coverage_note := fifelse(series %in% c("ECDC adults","ECDC under 18"),
       "Age subset divided by all-age Ministry total","Same nominal age coverage")]
  setorder(compare,event,series,week_start); save_table(compare,"weekly_comparisons")
  summarize <- function(t) t[,.(weeks=.N,first_week=min(week),last_week=max(week),
          source_sum=sum(count),reference_sum=sum(reference_count),net_difference=sum(signed_difference),
          sum_absolute_weekly_differences=sum(absolute_difference),largest_absolute_weekly_difference=max(absolute_difference),
          relative_net_pct=if(sum(reference_count)>0) 100*sum(signed_difference)/sum(reference_count) else NA_real_),
          by=.(event,series,reference,coverage_note)]
  summary <- summarize(compare[common_complete==TRUE])
  save_table(summary,"common_complete_summary")
  windows <- list(shared_deaths=compare[event=="All-cause deaths" & week>="2020-W10" & week<="2022-W51" & common_complete],
                  deaths_to_2023=compare[event=="All-cause deaths" & series=="November NZIP" & week>="2020-W10" & common_complete],
                  doses_from_rollout=compare[event %in% c("First doses","All doses") & series=="November NZIP" &
                                              week>="2020-W52" & !is.na(signed_difference)])
  audit <- rbindlist(lapply(windows,summarize),idcol="window")
  save_table(audit,"reference_windows")
  # Paired counts and differences share a calendar axis. Blank intervals stay visible.
  colors <- c("March FOI"="#ac623a","November NZIP"="#17786d","Eurostat"="#26384b","Ministry doses"="#26384b",
              "Ministry daily cases"="#26384b","Ministry weekly cases"="#9861a3","ECDC combined"="#4978b1")
  draw_counts <- function(file,events,sources,heading,caption) figure(file,12.8,if(length(events)==1) 6.8 else 8.2,function() {
    par(mfrow=c(2,length(events)),mar=c(2.3,4.5,2.6,1),oma=c(2.4,.5,3.5,.6))
    for (difference in c(FALSE,TRUE)) for (ev in events) {
      t <- if (difference) compare[event==ev & series %in% sources & common_complete] else counts[event==ev & series %in% sources]
      # Keep the complete week grid when drawing residuals so gaps break the line.
      vals <- if (difference) t$signed_difference else t$count
      yl <- range(c(0,vals),na.rm=TRUE); if(diff(yl)==0) yl <- c(-1,1)
      plot(range(weeks),yl,type="n",xlab="",ylab=if(difference) "Difference (events)" else "Weekly events",xaxt="n",yaxt="n")
      yt <- axTicks(2); axis(2,at=yt,labels=format(yt,big.mark=",",scientific=FALSE,trim=TRUE),cex.axis=.84)
      axis.Date(1,at=as.Date(paste0(2020:2024,"-01-01")),format="%Y"); abline(h=0,col="grey75")
      for (s in sources) {
        rows <- t[series==s]; if (!nrow(rows)) next
        y <- if(difference) rows$signed_difference else rows$count
        lines(weeks,y[match(weeks,rows$week_start)],col=colors[s],lwd=if(s %in% unname(refs)) 1.2 else 1.6,
              lty=if(s %in% unname(refs)) 3 else if(s=="March FOI") 2 else 1)
      }
      title(if (difference) paste("Difference from",refs[[ev]]) else ev,cex.main=.94)
    }
    mtext(heading,3,outer=TRUE,line=2,adj=0,font=2,cex=1.22)
    # Figure-wide key and caption, independent of the final panel's y scale.
    for(k in seq_along(sources)) mtext(sources[k],3,outer=TRUE,line=.45,
         adj=(k-1)/max(1,length(sources)),col=colors[sources[k]],font=2,cex=.82)
    mtext(caption,1,outer=TRUE,line=.65,adj=0,cex=.77)
  })
  draw_counts("V03_weekly_doses",c("First doses","All doses"),c("March FOI","November NZIP","ECDC combined","Ministry doses"),
       "Vaccination records and administered doses by ISO week","ECDC combined requires both national adult and under-18 returns. The difference panels use common complete weeks.")
  figure("V04_weekly_deaths",12.6,8.8,function() {
    calendar <- weeks[weeks>=week_date("2020-W10")]
    par(mfrow=c(3,1),mar=c(2.1,5.4,1.65,1.3),oma=c(2.6,.5,3.4,.5))
    for(s in c("All","November NZIP","March FOI")) {
      if(s=="All") {
        t <- counts[event=="All-cause deaths" & week_start %in% calendar & full_week]
        plot(range(calendar),range(t$count,na.rm=TRUE),type="n",xlab="",ylab="Weekly deaths",xaxt="n")
        for (label in c("March FOI","November NZIP","Eurostat")) {
          a <- t[series==label]
          lines(calendar,a$count[match(calendar,a$week_start)],col=colors[label],lwd=1.7,
                lty=if(label=="Eurostat") 3L else if(label=="March FOI") 2L else 1L)
        }
        legend("topright",c("March FOI","November NZIP","Eurostat"),col=colors[c("March FOI","November NZIP","Eurostat")],
               lty=c(2,1,3),lwd=2,bty="n",cex=.82,horiz=TRUE)
      } else {
        t <- compare[event=="All-cause deaths" & series==s & week_start %in% calendar & common_complete]
        lo <- min(c(-1,t$signed_difference)); hi <- max(c(1,t$signed_difference))
        plot(range(calendar),c(lo,hi)*1.25,type="n",xlab="",ylab="Difference (deaths)",xaxt="n",yaxt="n")
        ticks <- if(hi-lo<=4) seq(lo,hi) else axTicks(2)
        axis(2,at=ticks); abline(h=0,col="grey75")
        lines(calendar,t$signed_difference[match(calendar,t$week_start)],col=colors[s],lwd=1.55)
        if(s=="March FOI") {
          discrepant <- t[signed_difference!=0]
          points(discrepant$week_start,discrepant$signed_difference,col=colors[s],pch=16)
        }
        title(paste(s,"minus Eurostat"),adj=0,cex.main=.9)
      }
      axis.Date(1,at=as.Date(paste0(2020:2024,"-01-01")),format="%Y")
    }
    mtext("Weekly all-cause deaths in the two Czech releases and Eurostat",3,outer=TRUE,line=1.7,adj=0,font=2,cex=1.23)
    mtext("The difference panels use separate scales. Markers on the March panel identify each nonzero weekly difference.",1,outer=TRUE,line=1.05,adj=0,cex=.82)
    mtext("November uses all dated LPZ deaths, including records with missing age. March's last complete ISO week is 2022-W51.",1,outer=TRUE,line=.05,adj=0,cex=.76)
  })
  draw_counts("V05_weekly_infections",c("Primary infections","Repeat infections"),c("November NZIP","Ministry weekly cases","Ministry daily cases"),
       "Primary infections and reinfections by ISO week","November includes repeat episode rows. Weekly Ministry totals and sums of the daily series are tabulated separately.")
  figure("V06_ecdc_age_coverage",11.8,5.8,function() {
    t <- compare[event=="All doses" & week>="2021-W01" & week<="2021-W52"]
    cols <- c("ECDC adults"="#b56d45","ECDC under 18"="#8e78a7","ECDC combined"="#18766f")
    par(mar=c(4.3,5,4,1))
    plot(range(t$week_start),range(c(0,1.05,t$ratio_to_reference),na.rm=TRUE),type="n",
         xlab="",ylab="Share of Ministry all-age doses",xaxt="n")
    axis.Date(1,at=seq(as.Date("2021-01-01"),as.Date("2022-01-01"),by="2 months"),format="%b")
    abline(h=1,col="grey68",lty=2)
    for (s in names(cols)) { z <- t[series==s]; lines(z$week_start,z$ratio_to_reference,col=cols[s],lwd=2) }
    title("Why ECDC's adult series diverges from the Czech all-age total in 2021",adj=0)
    legend("left",names(cols),col=cols,lty=1,lwd=2,bty="n",cex=.88)
    mtext("An absent age-group return leaves the combined ratio missing. Sources: historical ECDC vaccine tracker and Czech Ministry daily vaccinations.",1,2.6,adj=0,cex=.76)
  })
  files <- sort(list.files(stage))
  save_table(data.table(file=files,sha256=vapply(file.path(stage,files),hash,character(1))),"outputs")
  if (!file.rename(stage,dest)) stop("Could not commit output directory: ",dest)
  message("Wrote ",dest)
  invisible(dest)
}
run_analysis()
