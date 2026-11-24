# Rscript 17_november_linkage_adjusted_asmr.R
# RStudio: source("17_november_linkage_adjusted_asmr.R", encoding="UTF-8")
# Produces Figure V19 (ages 15+) and Supplements S05–S10 (the six article age groups).
# The inputs are the November/Eurostat cells used for S01 and the Step 15
# full-study sensitivity run. Set CZ_STEP12_DIR and CZ_STEP15_DIR to pin runs.
build_linkage_asmr <- function(root=Sys.getenv("CZ_PROJECT_ROOT","."),
                               step12_dir=NULL, step15_dir=NULL) {
  root <- normalizePath(root,mustWork=TRUE)
  csv <- function(path) read.csv(path,stringsAsFactors=FALSE,check.names=FALSE)
  sha <- function(path) {
    if (requireNamespace("digest",quietly=TRUE)) return(digest::digest(file=path,algo="sha256",serialize=FALSE))
    if ("sha256sum" %in% getNamespaceExports("tools")) return(unname(tools::sha256sum(path)))
    if (nzchar(Sys.which("sha256sum"))) return(substr(system2("sha256sum",shQuote(path),stdout=TRUE),1,64))
    stop("Install the R package 'digest' to verify the input and output files.")
  }
  equal <- function(a,b,tol=1e-7) {
    length(a)==length(b) && identical(is.na(a),is.na(b)) && all(abs(a[!is.na(a)]-b[!is.na(b)])<=tol)
  }
  step12 <- "12_compare_asmr_direct_vs_eurostat"
  step15 <- "15_november_mortality_linkage_sensitivity"
  project <- file.path(root,c("data/analysis","results"))
  mode <- Sys.getenv("CZ_PUBLICATION_INPUTS",
                     if(any(dir.exists(file.path(project,step12))) || any(dir.exists(file.path(project,step15))))
                       "project" else "snapshot")
  if (!mode %in% c("project","snapshot")) stop("CZ_PUBLICATION_INPUTS must be project or snapshot.")
  resolve <- function(step,pin,files,scenario=FALSE) {
    if (is.null(pin)) pin <- Sys.getenv(paste0("CZ_STEP",substr(step,1,2),"_DIR"))
    bases <- file.path(project,step)
    dirs <- if (nzchar(pin)) pin else if(mode=="snapshot") file.path(root,"publication_source/snapshot",step) else
      unique(c(bases,unlist(lapply(bases,list.dirs,recursive=FALSE))))
    dirs <- dirs[file.exists(file.path(dirs,"outputs.csv"))]
    dirs <- dirs[order(file.mtime(file.path(dirs,"outputs.csv")),decreasing=TRUE)]
    for (d in dirs) {
      inv <- csv(file.path(d,"outputs.csv")); ix <- match(files,inv$file)
      if (anyNA(ix) || anyDuplicated(inv$file)) next
      paths <- file.path(d,files)
      if (!all(file.exists(paths)) || !all(vapply(paths,sha,"")==inv$sha256[ix]))
        stop("A listed input is missing or its checksum differs in ",d)
      if (scenario) {
        s <- csv(file.path(d,"scenario.csv"))
        if (nrow(s)!=1 || !identical(s$calibration_period,"study") ||
            !equal(s$missed_share_of_true_vaccinated_deaths,0.15,1e-12)) next
      }
      return(data.frame(step=substr(step,1,2),file=files,path=paths,sha256=inv$sha256[ix]))
    }
    stop("No compatible ",step," outputs. Run the source step or select its directory with CZ_STEP",
         substr(step,1,2),"_DIR. The full package also supports CZ_PUBLICATION_INPUTS=snapshot.")
  }
  a <- resolve(step12,step12_dir,c("age_week_cells.csv","weekly_asmr.csv"))
  b <- resolve(step15,step15_dir,c("scenario.csv","weekly_five_year_components.csv"),TRUE)
  inputs <- rbind(a,b)
  base <- file.path(root,"data","analysis","17_november_linkage_adjusted_asmr")
  dir.create(base,showWarnings=FALSE,recursive=TRUE)
  stage <- tempfile(".build-",tmpdir=base); stopifnot(dir.create(stage))
  on.exit(unlink(stage,recursive=TRUE),add=TRUE)
  signature <- file.path(stage,"signature")
  writeLines(c(deparse(body(build_linkage_asmr),width.cutoff=120L),inputs$sha256),signature,useBytes=TRUE)
  dest <- file.path(base,substr(sha(signature),1,20)); unlink(signature)
  groups <- c("15+","15-24","25-49","50-59","60-69","70-79","80+")
  figure_ids <- c("V19",sprintf("S%02d",5:10))
  names_png <- sprintf("%s_weekly_asmr_15pct_ages_%s.png",figure_ids,
                       gsub("+","_plus",gsub("-","_",groups,fixed=TRUE),fixed=TRUE))
  names_csv <- c("weekly_asmr.csv","annual_asmr.csv","weekly_age_cells.csv","calibration_check.csv","input_sha256.csv")
  verify <- function(d) {
    inv <- csv(file.path(d,"outputs.csv")); p <- file.path(d,inv$file)
    stopifnot(!anyDuplicated(inv$file),setequal(inv$file,c(names_png,names_csv)),
              all(file.exists(p)),all(vapply(p,sha,"")==inv$sha256))
    message("Verified Step 17: ",d)
    invisible(d)
  }
  if (dir.exists(dest)) return(verify(dest))
  f <- csv(b$path[b$file=="weekly_five_year_components.csv"])
  x <- subset(csv(a$path[a$file=="age_week_cells.csv"]),release=="november")
  s01 <- subset(csv(a$path[a$file=="weekly_asmr.csv"]),release=="november")
  dates <- seq(as.Date("2020-03-02"),as.Date("2023-12-25"),by="week")
  ages <- seq.int(15L,90L,5L)
  f <- f[order(f$week_start,f$age),]; x <- x[order(x$week_start,x$age),]
  f$week_start <- as.Date(f$week_start)
  stopifnot(identical(f$age,rep(ages,length(dates))),identical(f$week_start,rep(dates,each=length(ages))),
            identical(f$age,x$age),identical(f$week,x$week),identical(as.character(f$week_start),x$week_start),
            identical(f$week,format(f$week_start,"%G-W%V")))
  # Reproduce the three original November resident-based series in S01.
  for (suffix in c("deaths","days")) for (s in c("national","no_dose","vaccinated")) {
    s12 <- if(s=="no_dose") "subtraction_no_dose" else s
    if (!equal(f[[paste(s,suffix,sep="_")]],x[[paste(s12,suffix,sep="_")]],1e-6))
      stop("Step 12 and 15 disagree on ",s," ",suffix,". Pin runs from the same analysis.")
  }
  weights <- unique(x[c("age","esp_weight_15plus")])
  stopifnot(identical(weights$age,ages),all(weights$esp_weight_15plus>0),
            equal(sum(weights$esp_weight_15plus),1))
  expected_groups <- as.character(cut(f$age,breaks=c(15,25,50,60,70,80,Inf),labels=groups[-1],right=FALSE))
  stopifnot(identical(f$age_group,expected_groups))
  f$esp_weight_15plus <- x$esp_weight_15plus
  age_to_group <- setNames(expected_groups[match(ages,f$age)],ages)
  group_weight <- tapply(weights$esp_weight_15plus,age_to_group[as.character(weights$age)],sum)
  f$esp_weight_within_group <- f$esp_weight_15plus / unname(group_weight[f$age_group])
  stopifnot(all(f$no_dose_days>=0 & f$vaccinated_days>=0 & f$national_days>0),
            equal(f$no_dose_days+f$vaccinated_days,f$national_days,1e-6),
            equal(f$no_dose_deaths+f$vaccinated_deaths,f$national_deaths),
            equal(f$scenario_no_dose_deaths+f$scenario_vaccinated_deaths,f$national_deaths),
            all(f$scenario_no_dose_deaths>=0 & f$scenario_vaccinated_deaths>=0),
            equal(f$scenario_vaccinated_deaths-f$reconciled_vaccinated_deaths,f$missed_deaths))
  # The linkage transfer follows Step 15's feasibility reconciliation.
  # The target is a ratio of summed deaths, not a mean of weekly percentages.
  cal <- aggregate(f[c("vaccinated_deaths","reconciled_vaccinated_deaths","missed_deaths","scenario_vaccinated_deaths")],
                   f[c("age","age_group")],sum)
  cal <- cal[order(cal$age),]; cal$achieved_missed_share <- cal$missed_deaths/cal$scenario_vaccinated_deaths
  stopifnot(equal(cal$achieved_missed_share,rep(.15,length(ages))))
  counts <- c("no_dose_deaths","vaccinated_deaths","scenario_no_dose_deaths","scenario_vaccinated_deaths","national_deaths")
  days <- c("no_dose_days","vaccinated_days","national_days")
  rates <- c("original_no_dose_asmr","original_vaccinated_asmr","adjusted_no_dose_asmr","adjusted_vaccinated_asmr","resident_asmr")
  summarize_period <- function(z,period,group) {
    # Pool events and person-time by attained age before annual standardization.
    cells <- rowsum(as.matrix(z[c(counts,days)]),z$age,reorder=TRUE)
    w <- weights$esp_weight_15plus[match(rownames(cells),weights$age)]
    w <- w/sum(w)
    py <- cells[,c(days[1:2],days[1:2],days[3]),drop=FALSE]/365.25
    r <- cells[,counts,drop=FALSE]/py
    r[py<=0] <- NA_real_
    standard <- colSums(r*w)*1e5
    d <- colSums(cells)
    data.frame(period=period,age_group=group,weeks=length(unique(z$week)),
      as.list(setNames(standard,rates)),as.list(d[counts]),as.list(setNames(d[days]/365.25,sub("_days$","_person_years",days))),
      vaccinated_share_pct=100*d[["vaccinated_days"]]/d[["national_days"]],
      vaccinated_strata_with_exposure=sum(cells[,"vaccinated_days"]>0),required_strata=nrow(cells),
      negative_original_age_weeks=sum(z$no_dose_deaths < -1e-9),row.names=NULL,check.names=FALSE)
  }
  summarize <- function(period) {
    result <- lapply(groups,function(g) {
      ix <- if(g=="15+") seq_len(nrow(f)) else which(f$age_group==g)
      by_period <- split(ix,period[ix])
      do.call(rbind,lapply(names(by_period),function(k) summarize_period(f[by_period[[k]],],k,g)))
    })
    do.call(rbind,result)
  }
  weekly <- summarize(f$week); names(weekly)[1] <- "week"
  weekly$week_start <- f$week_start[match(weekly$week,f$week)]
  annual <- summarize(substr(f$week,1,4)); names(annual)[1] <- "iso_year"
  # March 2020 is the start of this series; keep the partial ISO year explicit.
  annual$first_week <- unname(tapply(f$week,f$iso_year,min)[annual$iso_year])
  annual$last_week <- unname(tapply(f$week,f$iso_year,max)[annual$iso_year])
  all_ages <- weekly[weekly$age_group=="15+",]
  idx <- match(all_ages$week,s01$first_week)
  stopifnot(!anyNA(idx),!anyDuplicated(s01$first_week),nrow(all_ages)==nrow(s01),
            equal(all_ages$original_no_dose_asmr,s01$subtraction_no_dose_asmr[idx]),
            equal(all_ages$original_vaccinated_asmr,s01$vaccinated_asmr[idx]),
            equal(all_ages$resident_asmr,s01$national_asmr[idx]),
            equal(all_ages$vaccinated_share_pct,s01$subtraction_vaccinated_pct[idx]))
  save_csv <- function(z,name) write.csv(z,file.path(stage,name),row.names=FALSE,na="")
  save_csv(weekly,names_csv[1]); save_csv(annual,names_csv[2])
  save_csv(f[c("week","week_start","age","age_group","esp_weight_15plus","esp_weight_within_group",counts,days,
               "reconciled_vaccinated_deaths","missed_deaths","transferred_deaths")],names_csv[3])
  save_csv(cal,names_csv[4]); save_csv(inputs[c("step","file","sha256")],names_csv[5])

  draw <- function(k) {
    g <- groups[k]; z <- weekly[weekly$age_group==g,]
    d <- z$week_start+3; lim <- range(d)
    ticks <- pretty(range(c(0,unlist(z[rates])),finite=TRUE),n=4)
    xt <- seq(as.Date("2020-07-01"),as.Date("2023-07-01"),by="6 months")
    year <- as.Date(paste0(2021:2023,"-01-01"))
    ink <- "#163744"; muted <- "#465D66"; guide <- "#DCE5E5"
    cols <- c("#A44919","#086B91","#475953")
    png(file.path(stage,names_png[k]),width=11.9,height=9.6,units="in",res=170,bg="white",pointsize=11)
    on.exit(dev.off())
    layout(matrix(1:5,ncol=1),heights=c(1.05,2.15,2.15,.80,.60))
    par(family="sans",xaxs="i",yaxs="i",fg=ink,col.axis=muted,lend="round")
    blank <- function() {par(mar=rep(0,4)); plot.new(); plot.window(c(0,1),c(0,1))}
    blank()
    kind <- if (k==1L) "FIGURE" else "SUPPLEMENT"
    text(.025,.86,paste0(kind," ",figure_ids[k],"     /     CZECHIA"),adj=0,col=muted,font=2,cex=.95)
    text(.025,.57,paste0("Weekly age-standardized mortality at ages ",g),adj=0,col=ink,font=2,cex=1.90)
    text(.025,.33,"November NZIP and Eurostat residents   ·   Deaths per 100,000 person-years   ·   ESP 2013",adj=0,col=muted,cex=1.05)
    legend(.02,.22,c("No recorded dose","At least one dose","All residents"),col=cols,lty=c(1,1,2),
           lwd=c(2,2,1.6),horiz=TRUE,bty="n",x.intersp=.6,y.intersp=.75,cex=1.02,text.col=ink)
    for (j in 1:2) {
      par(mar=c(.65,5.3,2.05,1.25))
      plot.new();plot.window(as.numeric(lim),range(ticks))
      abline(v=as.numeric(year),col=guide,lty=3);abline(h=ticks,col=guide)
      if (min(ticks)<0) abline(h=0,col="#859792")
      axis(2,at=ticks,labels=format(ticks,big.mark=",",scientific=FALSE,trim=TRUE),las=1,lwd=0,line=-.4)
      lines(d,z$resident_asmr,col=cols[3],lty=2,lwd=1.65)
      pair <- if(j==1) rates[1:2] else rates[3:4]
      for (i in 1:2) lines(d,z[[pair[i]]],col=cols[i],lwd=1.7)
      title(main=c("A    Original allocation","B    15% missed-linkage scenario")[j],adj=0,col.main=ink,cex.main=1.12,line=.55)
    }
    par(mar=c(1.9,5.3,1.2,1.25))
    plot.new();plot.window(as.numeric(lim),c(0,100))
    polygon(c(d,rev(d)),c(z$vaccinated_share_pct,rep(0,nrow(z))),border=NA,col="#DDD7E9")
    abline(v=as.numeric(year),col=guide,lty=3)
    lines(d,z$vaccinated_share_pct,col="#755E88",lwd=1.4)
    axis(2,at=c(0,100),labels=c("0%","100%"),las=1,lwd=0,line=-.4,cex.axis=.85)
    axis(1,at=as.numeric(xt),labels=paste(rep(c("Jul","Jan"),length.out=length(xt)),format(xt,"%Y")),lwd=0,line=-.35,cex.axis=.85)
    title(main="Share of resident person-time after a first dose",adj=0,col.main=ink,cex.main=.95,line=.1)
    blank()
    text(.025,.70,"15% of modelled vaccinated deaths over the full study within each five-year age band (90+ pooled).",adj=0,cex=.91,col=muted)
    text(.025,.42,"Each transfer preserves resident deaths and person-time. A gap denotes an age stratum with no exposure.",adj=0,cex=.91,col=muted)
    if(any(z$original_no_dose_asmr<0,na.rm=TRUE))
      text(.025,.14,"Negative original estimates arise when recorded vaccinated deaths exceed the resident count used in subtraction.",adj=0,cex=.85,col=muted)
  }
  for(k in seq_along(groups)) draw(k)
  files <- c(names_png,names_csv)
  write.csv(data.frame(file=files,sha256=unname(vapply(file.path(stage,files),sha,""))),
            file.path(stage,"outputs.csv"),row.names=FALSE)
  if (!file.rename(stage,dest) && !dir.exists(dest)) stop("Could not commit Step 17 to ",dest)
  verify(dest)
}
# Source into a private environment to load the function without running it.
if (identical(environment(),globalenv())) build_linkage_asmr()
