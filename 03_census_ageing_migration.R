# 03_census_ageing_migration
# From the project root: source("03_census_ageing_migration.R")
# Requires step 01 and packages data.table, digest, jsonlite, openxlsx.
# CZ_PROJECT_ROOT and CZ_MANIFEST select an existing project or source snapshot.
run_analysis <- function() {
  step <- "03_census_ageing_migration"
  needed <- c("eurostat_population","csu_census","csu_population_balance","csu_population_2023","csu_methodology")
  parents <- NULL
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
  pop <- estat(src("eurostat_population"))[geo=="CZ" & sex=="T" & unit=="NR" & freq=="A"]
  pop[, year := as.integer(time)]
  pop <- pop[year %between% c(2010L,2024L)]
  stopifnot(!anyDuplicated(pop[,.(year,age)]),identical(sort(unique(pop$year)),2010:2024))
  pop[, attained_age := suppressWarnings(as.integer(sub("^Y","",age)))]
  pop[age=="Y_LT1", attained_age := 0L]
  single <- pop[!is.na(attained_age),.(year,attained_age,residents=value)]
  # Verify that the open group follows exact ages 0-99 in every snapshot.
  stopifnot(all(single[,identical(sort(attained_age),0:99),by=year]$V1),
            !anyNA(single$residents),all(pop$age %in% c("Y_LT1",paste0("Y",1:99),"TOTAL","Y_OPEN","UNK")))
  by_year <- dcast(pop[age %in% c("TOTAL","UNK","Y_OPEN")],year~age,value.var="value")
  by_year <- merge(by_year,single[,.(single_age_sum=sum(residents)),by=year],by="year")
  stopifnot(!anyNA(by_year),all(by_year$TOTAL==by_year$single_age_sum+by_year$Y_OPEN+by_year$UNK))
  save_table(pop[order(year,attained_age),.(year,age_code=age,attained_age,residents=value,eurostat_flag=flag)],"january_population_by_age")
  previous <- single[,.(year=year+1L,attained_age=attained_age+1L,previous_residents=residents)]
  coh <- merge(single,previous,by=c("year","attained_age"),all.x=TRUE)
  coh[, change := residents-previous_residents]
  # A newborn has no prior January counterpart. For ages 1-4 the comparator is 0-3.
  coh[, band := fifelse(attained_age==0L,"Under 1",fifelse(attained_age<5L,"1-4",
                        sprintf("%d-%d",(attained_age%/%5L)*5L,(attained_age%/%5L)*5L+4L)))]
  grouped <- coh[,.(age_low=min(attained_age),residents=sum(residents),
                    previous_residents=sum(previous_residents),change=sum(change)),by=.(year,band)]
  # The previous 99-year-olds and the previous open group can both enter today's 100+.
  top <- merge(by_year[,.(year,residents=Y_OPEN)],merge(by_year[,.(year=year+1L,prior_open=Y_OPEN)],
               single[attained_age==99,.(year=year+1L,prior_99=residents)],by="year"),by="year",all.x=TRUE)
  grouped <- rbind(grouped,top[,.(year,band="100+",age_low=100L,residents,
                    previous_residents=prior_open+prior_99,change=residents-prior_open-prior_99)])
  # Closed accounting identity across adjacent January population snapshots.
  bridge <- merge(grouped[age_low>0,.(age_one_plus_change=sum(change)),by=year],
                  single[attained_age==0L,.(year,newborn_age_population=residents)],by="year")
  bridge <- merge(bridge,by_year[,.(year,all_ages=TOTAL,unknown_age=UNK)],by="year")
  setorder(bridge,year)
  bridge[, total_change := all_ages-shift(all_ages)]
  bridge[, reconstructed_change := age_one_plus_change+newborn_age_population+unknown_age-shift(unknown_age)]
  bridge[, residual := total_change-reconstructed_change]
  stopifnot(all(bridge[year>2010,residual]==0))
  save_table(bridge,"cohort_population_reconciliation")
  setorder(grouped,age_low,year)
  save_table(grouped,"cohort_changes")
  save_table(coh[attained_age==41L & year==2023L,.(january_year=year,age=attained_age,residents,
             prior_january_year=year-1L,prior_age=attained_age-1L,previous_residents,change)],"cohort_worked_example")

  # Table 5.1 gives total population, ages 0-14, 15-64 and 65+ by census year.
  tab <- as.data.table(openxlsx::read.xlsx(src("csu_census"),sheet="Tab._5",rows=1:10,
                  cols=c(2,3,4,7,10),colNames=FALSE,skipEmptyRows=FALSE))
  stopifnot(any(grepl("Population by age group",tab[[1L]],fixed=TRUE)),
            any(grepl("aged 65",tab[[5L]],fixed=TRUE)))
  setnames(tab,c("year","census_residents","age_0_14","age_15_64","age_65_plus"))
  census <- tab[year %in% c("2011","2021")]
  census[, (names(census)) := lapply(.SD,as.integer)]
  stopifnot(nrow(census)==2L,!anyNA(census))
  census[, age_unknown := census_residents-age_0_14-age_15_64-age_65_plus]
  stopifnot(all(census$age_unknown>=0))
  census[, share_65_plus_pct := 100*age_65_plus/census_residents]
  save_table(census,"census_enumerations")
  codes <- c("2406P"="opening","2406K2"="closing","4355ZN"="live_births","5393"="deaths",
             "5403W"="immigrants","5404W"="emigrants","5402W"="net_migration")
  flows <- fread(src("csu_population_balance"),select=c("CasR2A","IndicatorType","Hodnota"),colClasses="character")
  flows <- flows[as.integer(CasR2A) %between% c(2009L,2024L) & IndicatorType %in% names(codes),
                 .(year=as.integer(CasR2A),measure=unname(codes[IndicatorType]),n=as.numeric(Hodnota))]
  stopifnot(!anyDuplicated(flows[,.(year,measure)]))
  bal <- dcast(flows,year~measure,value.var="n")
  setorder(bal,year)
  bal[, preceding_closing := shift(closing)]
  bal <- bal[year %between% c(2010L,2024L)]
  stopifnot(identical(bal$year,2010:2024),!anyNA(bal))
  bal[, opening_minus_prior_closing := opening-preceding_closing]
  bal[, natural_change := live_births-deaths]
  bal[, within_year_balance_residual := closing-opening-natural_change-net_migration]
  stopifnot(all(bal$immigrants-bal$emigrants==bal$net_migration))
  bal <- merge(bal,by_year[,.(year,eurostat_january=TOTAL)],by="year")
  bal[, eurostat_minus_csu_opening := eurostat_january-opening]
  bal[, following_january_year := year+1L]
  save_table(bal,"annual_population_balance")
  archive <- src("csu_population_2023")
  member <- "OD_DEM02_vse_2024051613085948.CSV"
  temp <- tempfile(tmpdir=stage); dir.create(temp)
  utils::unzip(archive,files=member,exdir=temp)
  d <- fread(file.path(temp,member),select=c("stapro_kod","vuzemi_cis","vuzemi_kod","casref_do","pohlavi_kod","vek_kod","hodnota"),
             colClasses="character",na.strings=NULL)
  v <- d[stapro_kod=="2406" & vuzemi_cis=="97" & vuzemi_kod=="19" & casref_do=="2023-12-31" &
         pohlavi_kod=="" & vek_kod=="",as.numeric(hodnota)]
  stopifnot(length(v)==1L,is.finite(v)); unlink(temp,recursive=TRUE)
  benchmark <- data.table(source=c("CSU May 2024 vintage","CSU annual balance","Eurostat January series"),
                          reference_date=c("2023-12-31","2023-12-31","2024-01-01"),
                          residents=c(v,bal[year==2023,closing],bal[year==2024,eurostat_january]))
  benchmark[, difference_from_2024_vintage := residents-v]
  save_table(benchmark,"year_end_2023_benchmark")

  figure("V01_cohort_heatmap",13.6,9,function() {
    g <- grouped[age_low>0]; bands <- unique(g$band); ys <- 2010:2024
    mat <- matrix(NA_real_,length(ys),length(bands))
    mat[cbind(match(g$year,ys),match(g$band,bands))] <- g$change/1000
    lim <- max(abs(mat),na.rm=TRUE)
    cols <- grDevices::colorRampPalette(c("#903c2a","#e9a18b","#fbfaf5","#94cae1","#175770"))(101)
    par(mar=c(5.4,5.8,4.2,1.4))
    plot.new(); plot.window(c(2009.5,2024.5),c(.5,length(bands)+.5),xaxs="i",yaxs="i")
    for (x in seq_along(ys)) for (y in seq_along(bands)) {
      z <- mat[x,y]; fill <- if (is.na(z)) "#eaedef" else cols[round((z+lim)/(2*lim)*100)+1L]
      rect(ys[x]-.5,y-.5,ys[x]+.5,y+.5,col=fill,border="white")
      if (is.finite(z)) text(ys[x],y,sprintf("%+.1f",z),cex=.66,col=if(abs(z)>.67*lim) "white" else "#1d333d")
    }
    axis(1,at=ys,labels=ys,tick=FALSE,cex.axis=.84)
    axis(2,at=seq_along(bands),labels=bands,tick=FALSE,cex.axis=.8)
    title("How birth cohorts changed from one January to the next",adj=0)
    mtext("Change in thousands of residents, by attained age on 1 January. Blue indicates growth and red indicates a decline.",side=3,line=.3,adj=0,cex=.82)
    ex <- coh[year==2023 & attained_age==41]
    label <- sprintf("Example: %s residents aged 41 in January 2023 minus %s aged 40 in January 2022 gives %+d toward the 40-44 cell.",
                     format(ex$residents,big.mark=","),format(ex$previous_residents,big.mark=","),as.integer(ex$change))
    mtext(label,1,2.6,adj=0,cex=.77)
    mtext("2010 is the baseline. Census years are 2011 and 2021. Migration during 2022 first enters the January 2023 column. Source: Eurostat demo_pjan.",1,3.8,adj=0,cex=.77)
  })
  figure("V02_population_balance",11.5,6.3,function() {
    b <- bal[year<=2023]; yrs <- b$year
    par(mar=c(4.5,5.3,4.4,1.2))
    mat <- rbind(b$natural_change,b$net_migration,b$opening_minus_prior_closing)/1000
    barplot(mat,beside=TRUE,names.arg=yrs,las=1,cex.names=.78,col=c("#8e6040","#28748c","#a78bbb"),border=NA,
            ylab="Thousands of residents",ylim=range(c(0,mat))*1.12)
    abline(h=0,col="#54636c"); title("What changed the Czech resident population?",adj=0)
    legend("topleft",c("Births minus deaths","Net migration","Opening minus previous closing"),
           fill=c("#8e6040","#28748c","#a78bbb"),bty="n",cex=.83)
    mtext("The third series shows the break between successive annual balances, including census rebasing. Source: CSU annual population balance.",1,3,adj=0,cex=.74)
  })
  files <- sort(list.files(stage))
  save_table(data.table(file=files,sha256=vapply(file.path(stage,files),hash,character(1))),"outputs")
  if (!file.rename(stage,dest)) stop("Could not commit output directory: ",dest)
  message("Wrote ",dest)
  invisible(dest)
}
run_analysis()
