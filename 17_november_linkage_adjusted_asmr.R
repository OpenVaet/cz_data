# Rscript 17_november_linkage_adjusted_asmr.R
# RStudio: source("17_november_linkage_adjusted_asmr.R", encoding="UTF-8")
# Produces Figure V19 (ages 15+) and Supplements S05–S10 (the six article age groups).
# The inputs are the November/Eurostat cells used for S01 and the Step 15
# full-study sensitivity run. Set CZ_STEP12_DIR and CZ_STEP15_DIR to pin runs.
# Used by both the publication exporter and the standalone V19 figure.
draw_publication_header <- function(id, title, width_px) {
  dpi <- width_px / grDevices::dev.size("in")[1]
  bar_px <- round(width_px * .041)
  bar <- grid::unit(bar_px / dpi, "inches")
  top <- grid::unit(1, "npc")
  label <- paste(if (startsWith(id, "S")) "Supplement" else "Figure", id)
  grid::grid.rect(x=0, y=top, width=1, height=bar, just=c("left", "top"),
                  gp=grid::gpar(fill="#183B49", col=NA))
  grid::grid.text(paste(label, title, sep="    |    "), x=.025, y=top-bar/2, just="left",
                  gp=grid::gpar(col="white", fontfamily="sans", fontface="bold",
                                fontsize=bar_px * .44 * 72 / dpi))
}

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
  b <- resolve(step15,step15_dir,c("scenario.csv","weekly_five_year_components.csv","weekly_age_groups.csv"),TRUE)
  variants_file <- "ecdc_cz_gisaid_variants.csv"
  variants_path <- file.path(root,"publication_source",variants_file)
  inputs <- rbind(a,b,data.frame(step="ECDC",file=variants_file,path=variants_path,sha256=sha(variants_path)))
  base <- file.path(root,"data","analysis","17_november_linkage_adjusted_asmr")
  dir.create(base,showWarnings=FALSE,recursive=TRUE)
  stage <- tempfile(".build-",tmpdir=base); stopifnot(dir.create(stage))
  on.exit(unlink(stage,recursive=TRUE),add=TRUE)
  signature <- file.path(stage,"signature")
  writeLines(c(deparse(body(build_linkage_asmr),width.cutoff=120L),
               deparse(body(draw_publication_header),width.cutoff=120L),inputs$sha256),signature,useBytes=TRUE)
  dest <- file.path(base,substr(sha(signature),1,20)); unlink(signature)
  groups <- c("15+","15-24","25-49","50-59","60-69","70-79","80+")
  figure_ids <- c("V19",sprintf("S%02d",5:10))
  names_png <- sprintf("%s_weekly_asmr_15pct_ages_%s.png",figure_ids,
                       gsub("+","_plus",gsub("-","_",groups,fixed=TRUE),fixed=TRUE))
  names_csv <- c("weekly_asmr.csv","annual_asmr.csv","weekly_age_cells.csv","calibration_check.csv","input_sha256.csv",
                 "V19_weekly_doses.csv","V19_variant_markers.csv")
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
  # November dose events at attained ages 15+, using the same age allocation
  # as Step 14. Keep fractional counts until display.
  dose_groups <- csv(b$path[b$file=="weekly_age_groups.csv"])
  dose_fields <- c("first_doses","later_doses","all_doses")
  stopifnot(!anyDuplicated(dose_groups[c("week","age_group")]),
            setequal(dose_groups$age_group,groups[-1]),nrow(dose_groups)==length(dates)*(length(groups)-1L),
            all(is.finite(as.matrix(dose_groups[dose_fields])) & as.matrix(dose_groups[dose_fields])>=0),
            equal(dose_groups$first_doses+dose_groups$later_doses,dose_groups$all_doses))
  doses <- aggregate(dose_groups[dose_fields],dose_groups[c("week","week_start")],sum)
  doses <- doses[order(doses$week_start),]
  stopifnot(identical(as.Date(doses$week_start),dates),identical(doses$week,format(dates,"%G-W%V")),
            identical(dose_groups$week,format(as.Date(dose_groups$week_start),"%G-W%V")))

  # Use a single reporting stream. A marker requires a strict majority in
  # consecutive calendar weeks, even when some sampling weeks are absent.
  variants <- csv(variants_path)
  variants$week_start <- dates[match(variants$year_week,format(dates,"%G-%V"))]
  stopifnot(!anyNA(variants),!anyDuplicated(variants[c("year_week","variant")]),
            all(variants$number_sequenced_known_variant>0),all(variants$number_detections_variant>=0),
            all(variants$number_detections_variant<=variants$number_sequenced_known_variant))
  winners <- subset(variants,!variant %in% c("Other","UNK","SGTF") &
                      2*number_detections_variant>number_sequenced_known_variant)
  winners <- winners[order(winners$week_start),]
  following <- match(paste(winners$variant,winners$week_start+7),paste(winners$variant,winners$week_start))
  winners$next_week_detections <- winners$number_detections_variant[following]
  winners$next_week_sequences <- winners$number_sequenced_known_variant[following]
  markers <- winners[!is.na(following),]
  markers <- markers[!duplicated(markers$variant),]
  labels <- c("B.1.1.7"="Alpha","B.1.617.2"="Delta","BA.1"="Omicron BA.1")
  markers$label <- ifelse(markers$variant %in% names(labels),labels[markers$variant],markers$variant)
  markers$plot_date <- markers$week_start+3
  stopifnot(nrow(markers)>0,!anyDuplicated(markers$week_start))
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
  save_csv(doses,"V19_weekly_doses.csv"); save_csv(markers,"V19_variant_markers.csv")

  draw <- function(k) {
    g <- groups[k]; z <- weekly[weekly$age_group==g,]
    d <- z$week_start+3; lim <- range(d)
    ticks <- pretty(range(c(0,unlist(z[rates])),finite=TRUE),n=4)
    xt <- seq(as.Date("2020-07-01"),as.Date("2023-07-01"),by="6 months")
    year <- as.Date(paste0(2021:2023,"-01-01"))
    ink <- "#163744"; muted <- "#465D66"; guide <- "#DCE5E5"
    cols <- c("#A44919","#086B91","#475953")
    main_figure <- k==1L
    marker_colour <- "#685475"
    if (main_figure) lim <- lim+c(-3.5,3.5)
    png(file.path(stage,names_png[k]),width=11.9,height=if(main_figure) 11.4 else 9.6,units="in",res=170,bg="white",pointsize=11)
    on.exit(dev.off())
    if (main_figure) layout(matrix(1:6,ncol=1),heights=c(1.06,.38,2.15,2.15,1.72,.52)) else
      layout(matrix(1:5,ncol=1),heights=c(1.05,2.15,2.15,.80,.60))
    par(family="sans",xaxs="i",yaxs="i",fg=ink,col.axis=muted,lend="round")
    if (main_figure) par(cex=.85)
    blank <- function() {par(mar=rep(0,4)); plot.new(); plot.window(c(0,1),c(0,1))}
    blank()
    plot_title <- paste0("Weekly age-standardized mortality at ages ",g)
    if (k!=1L) {
      text(.025,.86,paste0("SUPPLEMENT ",figure_ids[k],"     /     CZECHIA"),adj=0,col=muted,font=2,cex=.95)
      text(.025,.57,plot_title,adj=0,col=ink,font=2,cex=1.90)
    }
    text(.025,if(k==1L) .51 else .33,"November NZIP and Eurostat residents   ·   Deaths per 100,000 person-years   ·   ESP 2013",adj=0,col=muted,cex=1.05)
    legend(.02,if(k==1L) .33 else .22,c("No recorded dose","At least one dose","All residents"),col=cols,lty=c(1,1,2),
           lwd=c(2,2,1.6),horiz=TRUE,bty="n",x.intersp=.6,y.intersp=.75,cex=1.02,text.col=ink)
    if (main_figure) {
      par(mar=c(0,5.3,0,1.25))
      plot.new(); plot.window(as.numeric(lim),c(0,1))
      text(as.numeric(lim[1]),.48,"Variant in Czech samples",adj=0,cex=.90,col=marker_colour)
      text(markers$plot_date,.54,sub("Omicron ","Omicron\n",markers$label,fixed=TRUE),cex=.91,font=2,col=marker_colour)
      segments(markers$plot_date,0,markers$plot_date,.24,col=marker_colour,lty=3)
    }
    for (j in 1:2) {
      par(mar=c(.65,5.3,2.05,1.25))
      plot.new();plot.window(as.numeric(lim),range(ticks))
      if (!main_figure) abline(v=as.numeric(year),col=guide,lty=3)
      abline(h=ticks,col=guide)
      if (main_figure) abline(v=as.numeric(markers$plot_date),col=marker_colour,lty=3,lwd=1.1)
      if (min(ticks)<0) abline(h=0,col="#859792")
      axis(2,at=ticks,labels=format(ticks,big.mark=",",scientific=FALSE,trim=TRUE),las=1,lwd=0,line=-.4)
      lines(d,z$resident_asmr,col=cols[3],lty=2,lwd=1.65)
      pair <- if(j==1) rates[1:2] else rates[3:4]
      for (i in 1:2) lines(d,z[[pair[i]]],col=cols[i],lwd=1.7)
      title(main=c("A    Original allocation","B    15% missed-linkage scenario")[j],adj=0,col.main=ink,cex.main=1.12,line=.55)
    }
    if (main_figure) {
      par(mar=c(1.9,5.3,2.05,1.25))
      dose_ticks <- pretty(c(0,max(doses$all_doses)),n=4)
      plot.new(); plot.window(as.numeric(lim),range(dose_ticks))
      abline(h=dose_ticks,col=guide)
      fills <- c("#177469","#8EC7B9")
      rect(d-2.9,0,d+2.9,doses$first_doses,col=fills[1],border=NA)
      rect(d-2.9,doses$first_doses,d+2.9,doses$all_doses,col=fills[2],border=NA)
      abline(v=as.numeric(markers$plot_date),col=marker_colour,lty=3,lwd=1.1)
      axis(2,at=dose_ticks,labels=format(dose_ticks/1000,trim=TRUE),las=1,lwd=0,line=-.4)
      title(main="C    Weekly doses administered at ages 15+ (thousands)",adj=0,col.main=ink,cex.main=1.12,line=.55)
      legend("topright",c("First dose","Later doses"),fill=fills,border=NA,bty="n",horiz=TRUE,cex=.94,text.col=ink)
    } else {
      par(mar=c(1.9,5.3,1.2,1.25))
      plot.new();plot.window(as.numeric(lim),c(0,100))
      polygon(c(d,rev(d)),c(z$vaccinated_share_pct,rep(0,nrow(z))),border=NA,col="#DDD7E9")
      abline(v=as.numeric(year),col=guide,lty=3)
      lines(d,z$vaccinated_share_pct,col="#755E88",lwd=1.4)
      axis(2,at=c(0,100),labels=c("0%","100%"),las=1,lwd=0,line=-.4,cex.axis=.85)
      title(main="Share of resident person-time after a first dose",adj=0,col.main=ink,cex.main=.95,line=.1)
    }
    axis(1,at=as.numeric(xt),labels=paste(rep(c("Jul","Jan"),length.out=length(xt)),format(xt,"%Y")),lwd=0,line=-.35,cex.axis=.85)
    blank()
    if (main_figure) {
      text(.025,.74,"Dotted purple lines mark the first of two consecutive weeks above 50% of Czech sequences (ECDC / GISAID).",adj=0,cex=.92,col=muted)
      text(.025,.39,"B applies the 15% assumption within each five-year age band (90+ pooled). Gaps indicate missing exposure in an age stratum.",adj=0,cex=.88,col=muted)
    } else {
      text(.025,.70,"15% of modelled vaccinated deaths over the full study within each five-year age band (90+ pooled).",adj=0,cex=.91,col=muted)
      text(.025,.42,"Each transfer preserves resident deaths and person-time. A gap denotes an age stratum with no exposure.",adj=0,cex=.91,col=muted)
    }
    if(any(z$original_no_dose_asmr<0,na.rm=TRUE))
      text(.025,.14,"Negative original estimates arise when recorded vaccinated deaths exceed the resident count used in subtraction.",adj=0,cex=.85,col=muted)
    if (k==1L) draw_publication_header(figure_ids[k],plot_title,grDevices::dev.size("px")[1])
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
