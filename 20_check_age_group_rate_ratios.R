# From the extracted project: Rscript 20_check_age_group_rate_ratios.R
# Defaults to the reproducible inputs used for tables T05 and Step 17.
check_rate_ratios <- function(root = Sys.getenv("CZ_PROJECT_ROOT", ".")) {
  root <- normalizePath(root, mustWork = TRUE)
  s15 <- Sys.getenv("CZ_STEP15_DIR",file.path(root,"publication_source","snapshot","15_november_mortality_linkage_sensitivity"))
  s17 <- Sys.getenv("CZ_STEP17_DIR",file.path(root,"step17_results"))
  csv <- function(path) read.csv(path,check.names=FALSE,stringsAsFactors=FALSE)
  sha <- function(path) {
    if (requireNamespace("digest",quietly=TRUE)) return(digest::digest(file=path,algo="sha256"))
    if ("sha256sum" %in% getNamespaceExports("tools")) return(unname(getExportedValue("tools","sha256sum")(path)))
    if (nzchar(Sys.which("sha256sum"))) return(substr(system2("sha256sum",shQuote(path),stdout=TRUE),1,64))
    stop("Install the R package digest for SHA-256 checks.")
  }
  close <- function(x,y) stopifnot(length(x)==length(y),all(is.finite(x)),all(is.finite(y)),
                                    all(abs(x-y)<1e-7 + 1e-10*abs(y)))
  cal <- csv(file.path(s15,"scenario.csv"))
  close(cal$missed_share_of_true_vaccinated_deaths,0.15)
  stopifnot(nrow(cal)==1L,cal$calibration_period=="study")
  provenance <- subset(csv(file.path(s17,"input_sha256.csv")),step==15)
  names15 <- c("scenario.csv","weekly_five_year_components.csv","annual_original_vs_scenario.csv")
  hashes <- vapply(file.path(s15,names15),sha,"",USE.NAMES=FALSE)
  inv <- csv(file.path(s15,"outputs.csv"))
  stopifnot(identical(inv$sha256[match(names15,inv$file)],hashes),
            identical(provenance$sha256[match(names15[1:2],provenance$file)],hashes[1:2]))
  a <- subset(csv(file.path(s15,names15[3])),iso_year==2023)
  standard <- subset(csv(file.path(s17,"annual_asmr.csv")),iso_year==2023 & age_group!="15+")
  all_cells <- csv(file.path(s17,"weekly_age_cells.csv"))
  x <- subset(all_cells,startsWith(week,"2023-W"))
  weeks <- sprintf("2023-W%02d",1:52)
  stopifnot(nrow(x)==52*16,all(table(factor(x$week,weeks),factor(x$age,seq(15,90,5)))==1))
  full <- aggregate(cbind(missed_deaths,scenario_vaccinated_deaths)~age,all_cells,sum,na.action=na.fail)
  close(full$missed_deaths/full$scenario_vaccinated_deaths,rep(0.15,16))
  groups <- c("15-24","25-49","50-59","60-69","70-79","80+")
  rows <- lapply(groups,function(g) {
    u <- subset(a,age_group==g & status=="no_dose")
    v <- subset(a,age_group==g & status=="vaccinated")
    b <- subset(standard,age_group==g)
    stopifnot(nrow(u)==1L,nrow(v)==1L,nrow(b)==1L,u$included_weeks==52L,v$included_weeks==52L)
    cells <- subset(x,age_group==g)
    annual <- aggregate(cbind(no_dose_deaths,vaccinated_deaths,scenario_no_dose_deaths,
      scenario_vaccinated_deaths,no_dose_days,vaccinated_days,missed_deaths) ~ age+esp_weight_within_group,
      cells,sum,na.action=na.fail)
    close(sum(annual$esp_weight_within_group),1)
    close(colSums(annual[c("no_dose_deaths","vaccinated_deaths","scenario_no_dose_deaths",
      "scenario_vaccinated_deaths","no_dose_days","vaccinated_days")]),
      c(u$original_deaths,v$original_deaths,u$scenario_deaths,v$scenario_deaths,u$person_days,v$person_days))
    close(u$original_deaths+v$original_deaths,u$scenario_deaths+v$scenario_deaths)
    rates <- c(u$original_deaths/u$person_days,v$original_deaths/v$person_days,
               u$scenario_deaths/u$person_days,v$scenario_deaths/v$person_days)*365.25e5
    close(rates,c(u$original_rate_per_100k_py,v$original_rate_per_100k_py,
                  u$scenario_rate_per_100k_py,v$scenario_rate_per_100k_py))
    w <- annual$esp_weight_within_group*365.25e5
    asmr <- c(sum(w*annual$no_dose_deaths/annual$no_dose_days),
              sum(w*annual$vaccinated_deaths/annual$vaccinated_days),
              sum(w*annual$scenario_no_dose_deaths/annual$no_dose_days),
              sum(w*annual$scenario_vaccinated_deaths/annual$vaccinated_days))
    close(asmr,unlist(b[c("original_no_dose_asmr","original_vaccinated_asmr","adjusted_no_dose_asmr","adjusted_vaccinated_asmr")]))
    net <- u$original_deaths-u$scenario_deaths
    missed <- sum(annual$missed_deaths)/v$scenario_deaths
    close(missed,v$missed_fraction_among_vaccinated)
    data.frame(iso_year=2023,age_group=g,no_dose_person_years=u$person_days/365.25,
      vaccinated_person_years=v$person_days/365.25,original_no_dose_deaths=u$original_deaths,
      original_vaccinated_deaths=v$original_deaths,scenario_no_dose_deaths=u$scenario_deaths,
      scenario_vaccinated_deaths=v$scenario_deaths,original_no_dose_rate=rates[1],original_vaccinated_rate=rates[2],
      scenario_no_dose_rate=rates[3],scenario_vaccinated_rate=rates[4],original_pooled_ratio=rates[1]/rates[2],
      scenario_pooled_ratio=rates[3]/rates[4],scenario_pct_difference=100*(rates[3]/rates[4]-1),
      original_standardized_ratio=asmr[1]/asmr[2],scenario_standardized_ratio=asmr[3]/asmr[4],
      scenario_missed_pct=100*missed,net_deaths_moved_to_vaccinated=net,
      original_no_dose_deaths_moved_pct=100*net/u$original_deaths)
  })
  out <- do.call(rbind,rows)
  ans <- file.path(root,"data","analysis","20_check_age_group_rate_ratios")
  dir.create(ans,recursive=TRUE,showWarnings=FALSE)
  stage <- tempfile(".building-",tmpdir=ans)
  stopifnot(dir.create(stage))
  on.exit(unlink(stage,recursive=TRUE),add=TRUE)
  write.csv(out,file.path(stage,"age_group_rate_ratios_2023.csv"),row.names=FALSE)
  dest <- file.path(ans,substr(sha(file.path(stage,"age_group_rate_ratios_2023.csv")),1,20))
  if (!dir.exists(dest) && !file.rename(stage,dest) && !dir.exists(dest)) stop("Could not commit ",dest)
  saved <- csv(file.path(dest,"age_group_rate_ratios_2023.csv"))
  stopifnot(identical(out$age_group,saved$age_group),identical(names(out),names(saved)))
  for (nm in setdiff(names(out),"age_group")) close(out[[nm]],saved[[nm]])
  message("Verified ",file.path(dest,"age_group_rate_ratios_2023.csv"))
  invisible(dest)
}
check_rate_ratios()
