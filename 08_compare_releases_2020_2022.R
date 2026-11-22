# 08_compare_releases_2020_2022
# Run after 07. Packages: data.table and digest. The reported windows use unsmoothed weekly ASMR.
run_analysis <- function() {
  step <- "08_compare_releases_2020_2022"
  needed <- c("march","november")
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
  parents <- find_parent("07_build_weekly_mortality_rates",c("weekly_rates.csv","weekly_age_cells.csv","specifications.csv"),"CZ_STEP07_DIR")
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
  load_parent <- function(n) fread(file.path(root,parents$path[parents$id==n]))
  rates <- load_parent("weekly_rates.csv")[week>="2020-W10" & week<="2022-W51"]
  cells <- load_parent("weekly_age_cells.csv")[week>="2020-W10" & week<="2022-W51"]
  rates[,week_start:=as.Date(week_start)]
  # Each pair uses identical event-week precision, age eligibility and standard.
  pairs <- data.table(comparison=c("primary","coarsen March","uniform years","uniform years; coarsen March",
          "death first when tied","death first; coarsen March","entry at first event","retain post-death dose conflicts","birthdays in first quarter","birthdays in last quarter","full vaccinated week for tied events"),
          march=c("primary","coarsened","uniform_birth_year","coarsened_uniform","death_first_if_tied","coarsened_death_first","first_contact","ignore_postdeath_dose","birthday_early","birthday_late","full_week_tied_dose"),
          november=c("primary","primary","uniform_birth_year","uniform_birth_year","death_first_if_tied","death_first_if_tied","first_contact","ignore_postdeath_dose","birthday_early","birthday_late","full_week_tied_dose"))
  paired <- rbindlist(lapply(seq_len(nrow(pairs)),function(i) {
    a <- rates[release=="march" & spec==pairs$march[i],.(week,week_start,status,min_age,march_rate=asmr)]
    b <- rates[release=="november" & spec==pairs$november[i],.(week,status,min_age,november_rate=asmr)]
    z <- merge(a,b,by=c("week","status","min_age"),all=TRUE)
    z[, `:=`(comparison=pairs$comparison[i],difference=november_rate-march_rate,
              pct=ifelse(march_rate>0,100*(november_rate/march_rate-1),NA_real_))]
    z
  }))
  save_table(paired,"weekly_matched_comparisons")
  total_series <- rates[status=="total" & (release=="Eurostat" | spec=="primary"),
                        .(week,week_start,min_age,release,asmr)]
  resident_check <- dcast(total_series,week+week_start+min_age~release,value.var="asmr")
  resident_check[, `:=`(march_minus_national=march-Eurostat,november_minus_national=november-Eurostat)]
  save_table(resident_check,"weekly_total_against_residents")
  win <- data.table(period=c("Delta","Summer 2022"),first=c("2021-W47","2022-W23"),last=c("2021-W50","2022-W35"),expected=c(4L,13L))
  windows <- rbindlist(lapply(seq_len(nrow(win)),function(i) {
    z <- paired[week>=win$first[i] & week<=win$last[i]]
    q <- z[,{
      ok <- is.finite(march_rate) & is.finite(november_rate); full <- sum(ok)==win$expected[i]
      list(comparable_weeks=sum(ok),expected_weeks=win$expected[i],
           mean_march=if(full) mean(march_rate) else NA_real_,mean_november=if(full) mean(november_rate) else NA_real_)
    },by=.(comparison,min_age,status)]
    q[, `:=`(period=win$period[i],first_week=win$first[i],last_week=win$last[i])]
    q[, difference_pct := fifelse(mean_march>0,100*(mean_november/mean_march-1),NA_real_)]
    q
  }))
  save_table(windows,"window_mean_weekly_rates")
  # Also provide the distinct pooled-period statistic, with the component cells.
  pooled <- rbindlist(lapply(seq_len(nrow(win)),function(i) {
    z <- cells[week>=win$first[i] & week<=win$last[i],
          .(deaths=sum(deaths),person_days=sum(person_days),standard_n=first(standard_n)),by=.(release,spec,status,age)]
    z[,period:=win$period[i]]; z
  }))
  save_table(pooled,"window_age_cells")
  pr <- rbindlist(lapply(c(0L,15L),function(floor_age) pooled[age>=floor_age,
       .(pooled_asmr=if(all(person_days>0)) 365.25e5*weighted.mean(deaths/person_days,standard_n) else NA_real_),
       by=.(period,release,spec,status)][,min_age:=floor_age]))
  save_table(pr,"pooled_period_rates")
  specmap <- c("March FOI"="march/primary","March in five-year bands"="march/coarsened","November NZIP"="november/primary","Eurostat"="Eurostat/national")
  colors <- c("#8f5236","#806bb0","#147968","#243746")
  figure("V10_mortality_in_the_two_releases",12.8,8.6,function(){
    par(mfrow=c(2,3),mar=c(3.2,4.4,2.5,.8),oma=c(2.2,.4,3.8,.5))
    weeks <- sort(unique(rates$week_start))
    for(delta in c(FALSE,TRUE)) for(s in c("total","no_dose","vaccinated")) {
      ids <- if(s=="total") 1:4 else 1:3
      if(delta) ids <- setdiff(ids,1L)
      series <- lapply(specmap,function(tag) {
        z <- rates[status==s & min_age==15 & paste(release,spec,sep="/")==tag]; z$asmr[match(weeks,z$week_start)]
      })
      draw <- if(delta) lapply(series,function(x) x-series[[1]]) else series
      vals <- unlist(draw[ids]); lim <- range(c(0,vals[is.finite(vals)]))
      plot(range(weeks),lim,type="n",xaxt="n",xlab="",ylab=if(delta) "Difference from March" else "Annualized rate per 100,000")
      axis.Date(1,at=as.Date(c("2020-07-01","2021-01-01","2021-07-01","2022-01-01","2022-07-01")),format="%b %y",cex.axis=.76)
      abline(h=0,col="grey80"); for(j in ids) lines(weeks,draw[[j]],col=colors[j],lty=c(2,3,1,4)[j],lwd=1.5)
      title(switch(s,total="All included records",no_dose="No recorded dose yet",vaccinated="After first recorded dose"),cex.main=.92)
    }
    mtext("Weekly age-standardized all-cause mortality at ages 15 and over",3,outer=TRUE,line=2.3,adj=0,font=2,cex=1.2)
    for(j in seq_along(specmap)) mtext(names(specmap)[j],3,outer=TRUE,line=.65,adj=(j-1)/3,col=colors[j],font=2,cex=.8)
    mtext("ESP 2013. A gap indicates that one or more age strata have no person-time. Eurostat supplies a resident total.",1,outer=TRUE,line=.55,adj=0,cex=.81)
  })
  files <- sort(list.files(stage))
  save_table(data.table(file=files,sha256=vapply(file.path(stage,files),hash,character(1))),"outputs")
  if (!file.rename(stage,dest)) stop("Could not commit output directory: ",dest)
  message("Wrote ",dest)
  invisible(dest)
}
run_analysis()
