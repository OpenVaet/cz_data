# 09_compare_annual_mortality_2022_2023
# Run after 07. Packages: data.table, digest, jsonlite.
run_analysis <- function() {
  step <- "09_compare_annual_mortality_2022_2023"
  needed <- c("march","november","eurostat_asmr")
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
  parents <- find_parent("07_build_weekly_mortality_rates",c("weekly_age_cells.csv","unknown_age_weeks.csv"),"CZ_STEP07_DIR")
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
  load_parent <- function(n) fread(file.path(root,parents$path[parents$id==n]))
  a <- load_parent("weekly_age_cells.csv")[substr(week,1,4) %in% c("2022","2023") & release %in% c("november","Eurostat")]
  a[,year:=as.integer(substr(week,1,4))]
  stopifnot(all(a[,uniqueN(week),by=.(release,spec,year)]$V1==52))
  aggregate_cells <- function(x) x[,.(deaths=sum(deaths),person_days=sum(person_days),standard_n=first(standard_n)),
                                     by=.(year,release,spec,status,age)]
  annual <- aggregate_cells(a)
  u <- load_parent("unknown_age_weeks.csv")[release=="november" & spec=="primary"]
  u <- merge(u,unique(a[,.(week_start,year)]),by="week_start")
  # Unknown-band scenarios preserve recorded vaccination states. Within each
  # week and status allocate unknown deaths and days using the corresponding
  # known-age distribution. The older-age variant tilts both distributions.
  grid <- data.table(spec=c("unknown_like_known","unknown_older","half_undated_exposure"),
                     older_multiplier=c(1,2,1),undated_fraction=c(1,1,.5))
  save_table(grid,"unknown_age_assumptions")
  checks <- list()
  for(i in seq_len(nrow(grid))) {
    pieces <- list()
    for(s in c("no_dose","vaccinated")) {
      z <- copy(a[release=="november" & spec=="primary" & status==s])
      ix <- match(z$week_start,u$week_start)
      stopifnot(!anyNA(ix))
      ud <- u[[paste0(s,"_deaths")]][ix]; up <- u[[paste0(s,"_days")]][ix]
      excluded <- if(s=="no_dose") (1-grid$undated_fraction[i])*u$undated_no_dose_days[ix] else rep(0,nrow(z))
      z[, tilt := fifelse(age>=65,grid$older_multiplier[i],1)]
      z[, wd := if(sum(deaths*tilt)>0) deaths*tilt/sum(deaths*tilt) else person_days*tilt/sum(person_days*tilt),by=week_start]
      z[, wp := person_days*tilt/sum(person_days*tilt),by=week_start]
      z[, `:=`(extra_deaths=ud*wd,extra_days=(up-excluded)*wp)]
      chk <- z[,.(extra_deaths=sum(extra_deaths),extra_days=sum(extra_days)),by=week_start]
      w <- match(chk$week_start,u$week_start)
      expected_p <- u[[paste0(s,"_days")]][w]; omitted <- if(s=="no_dose") (1-grid$undated_fraction[i])*u$undated_no_dose_days[w] else 0
      stopifnot(max(abs(chk$extra_deaths-u[[paste0(s,"_deaths")]][w]))<1e-7,
                max(abs(chk$extra_days+omitted-expected_p))<1e-5)
      chk[, `:=`(omitted_undated_person_days=omitted,spec=grid$spec[i],status=s)]
      checks[[length(checks)+1L]] <- chk
      z[, `:=`(deaths=deaths+extra_deaths,person_days=person_days+extra_days,spec=grid$spec[i])]
      pieces[[s]] <- aggregate_cells(z)
    }
    part <- rbindlist(pieces); tot <- part[,.(deaths=sum(deaths),person_days=sum(person_days),standard_n=first(standard_n)),
                           by=.(year,release,spec,age)][,status:="total"]
    annual <- rbindlist(list(annual,part,tot),use.names=TRUE)
  }
  save_table(rbindlist(checks),"unknown_age_allocation_check")
  cells <- rbindlist(lapply(c(0L,15L),function(minimum) {
    z <- copy(annual[age>=minimum]); z[,weight:=standard_n/sum(standard_n),by=.(year,release,spec,status)]
    z[, `:=`(min_age=minimum,contribution=fifelse(person_days>0,365.25e5*weight*deaths/person_days,NA_real_))]
    z
  }))
  save_table(cells,"annual_age_contributions")
  rates <- cells[,.(deaths=sum(deaths),person_days=sum(person_days),asmr=sum(contribution)),
                  by=.(year,release,spec,status,min_age)]
  save_table(rates,"annual_pooled_rates")
  change <- dcast(rates,release+spec+status+min_age~year,value.var="asmr")
  change[, `:=`(absolute_change=`2023`-`2022`,change_pct=100*(`2023`/`2022`-1))]
  save_table(change,"year_over_year_change")
  age_change <- dcast(cells,release+spec+status+min_age+age~year,value.var="contribution")
  age_change[,change:=`2023`-`2022`]; save_table(age_change,"age_contributions_to_change")
  office <- estat(src("eurostat_asmr"))[geo=="CZ" & age=="TOTAL" & sex=="T" & icd10=="TOTAL" & unit=="RT" & time %in% c("2022","2023")]
  stopifnot(nrow(office)==2)
  save_table(office[,.(calendar_year=time,official_all_age_rate=value,unit="Rate per 100,000; published national series")],"official_calendar_year_rates")
  figure("V11_annual_asmr_and_assumptions",12,6.5,function(){
    par(mfrow=c(1,2),mar=c(4.3,5,2.4,1.2),oma=c(2,0,3,.4))
    specs <- c("primary","unknown_like_known","unknown_older","half_undated_exposure","first_contact")
    cols <- c("#274958","#187c68","#ad7349","#795ca1","#7b858b")
    for(s in c("no_dose","vaccinated")) {
      z <- rates[release=="november" & min_age==15 & status==s & spec %in% specs]
      plot(2022:2023,range(c(0,z$asmr),na.rm=TRUE),type="n",xaxt="n",xlab="",ylab="Annualized deaths per 100,000")
      axis(1,2022:2023,2022:2023)
      for(j in seq_along(specs)) {g <- z[spec==specs[j]][order(year)];lines(g$year,g$asmr,type="o",pch=15+j,col=cols[j],lty=j,lwd=1.7)}
      title(if(s=="no_dose") "No recorded dose yet" else "After first recorded dose",cex.main=1)
    }
    mtext("Annual standardized mortality under alternative denominator assumptions",3,outer=TRUE,line=1.6,adj=0,font=2,cex=1.13)
    mtext("Known birth bands",1,outer=TRUE,adj=0,col=cols[1],cex=.76);mtext("Allocate unknown",1,outer=TRUE,adj=.25,col=cols[2],cex=.76)
    mtext("Older allocation",1,outer=TRUE,adj=.5,col=cols[3],cex=.76);mtext("Half undated exposure",1,outer=TRUE,adj=.75,col=cols[4],cex=.76)
    mtext("Entry at first contact",1,outer=TRUE,adj=1,col=cols[5],cex=.76)
  })
  files <- sort(list.files(stage))
  save_table(data.table(file=files,sha256=vapply(file.path(stage,files),hash,character(1))),"outputs")
  if (!file.rename(stage,dest)) stop("Could not commit output directory: ",dest)
  message("Wrote ",dest)
  invisible(dest)
}
run_analysis()
