# From the project root: Rscript 19_check_migration_and_mortality.R
# Uses the v1.9 article snapshot. CZ_PROJECT_ROOT and CZ_STEP03_DIR, CZ_STEP14_DIR,
# CZ_STEP15_DIR can select another project and corresponding completed steps.
check_migration <- function(root = Sys.getenv("CZ_PROJECT_ROOT", ".")) {
  root <- normalizePath(root, mustWork = TRUE)
  src <- file.path(root, "publication_source", "migration_2023")
  read <- function(p) read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
  sha <- function(p) {
    if (requireNamespace("digest", quietly = TRUE)) return(digest::digest(file = p, algo = "sha256"))
    if ("sha256sum" %in% getNamespaceExports("tools")) return(unname(getExportedValue("tools","sha256sum")(p)))
    if (nzchar(Sys.which("sha256sum"))) return(substr(system2("sha256sum",shQuote(p),stdout=TRUE),1,64))
    stop("Install the R package digest to verify inputs.")
  }
  checked <- function(dir, name) {
    manifest <- read(file.path(dir,"outputs.csv"))
    path <- file.path(dir,name)
    stopifnot(identical(sha(path), manifest$sha256[manifest$file == name]))
    read(path)
  }
  project <- function(step, name) {
    dir <- Sys.getenv(paste0("CZ_STEP", substr(step,1,2), "_DIR"),
                      file.path(root, "publication_source", "snapshot", step))
    checked(dir,name)
  }
  demographic <- checked(src,"csu_2023_age_counts.csv")
  close <- function(x,y) stopifnot(length(x)==length(y), all(is.finite(x)), all(is.finite(y)), all(abs(x-y)<1e-6))
  age_grid <- seq(0,85,5)
  stopifnot(identical(demographic$age_lower, as.integer(age_grid)), all(demographic >= 0),
            sum(demographic$registered_foreign_nationals_dec31)==1065740)
  census <- "03_census_ageing_migration"
  balance <- project(census,"annual_population_balance.csv")
  yr <- subset(balance,year==2023)
  stopifnot(nrow(yr)==1L, sum(demographic$immigrants_2023)==yr$immigrants,
            sum(demographic$emigrants_2023)==yr$emigrants,
            yr$immigrants - yr$emigrants == yr$net_migration)
  population <- project(census,"january_population_by_age.csv")
  for (y in 2023:2024) {
    p <- subset(population,year==y)
    stopifnot(identical(p$residents[p$age_code=="UNK"],0L))
    p$attained_age[p$age_code=="Y_OPEN"] <- 100L
    ages <- p[!is.na(p$attained_age),]
    stopifnot(identical(sort(ages$attained_age),0:100),sum(ages$residents)==p$residents[p$age_code=="TOTAL"])
    by_age <- tapply(ages$residents,pmin(85,5*(ages$attained_age%/%5)),sum)
    demographic[[paste0("residents_jan",y)]] <- as.numeric(by_age[as.character(age_grid)])
  }
  close(colSums(demographic[c("residents_jan2023","residents_jan2024")]), c(yr$opening,yr$closing))
  x <- project("14_november_mortality_excess_and_trends","weekly_five_year_components.csv")
  x <- x[startsWith(x$week,"2023-W"),]
  weeks <- sprintf("2023-W%02d",1:52)
  stopifnot(nrow(x)==52*16,all(table(factor(x$week,weeks),factor(x$age,seq(15,90,5)))==1),
            all(x$vaccinated_days>0),all(x$baseline_deaths_per_person_day>0))
  close(x$vaccinated_expected,x$vaccinated_days*x$baseline_deaths_per_person_day)
  mortality <- aggregate(cbind(vaccinated_deaths,vaccinated_expected,vaccinated_days) ~ age_lower,
                         transform(x,age_lower=pmin(85,age)),sum,na.action=na.fail)
  fixed <- merge(subset(demographic,age_lower>=15),mortality,by="age_lower")
  stopifnot(nrow(fixed)==15L)
  article_breaks <- c(15,25,50,60,70,80,Inf)
  article_labels <- c("15-24","25-49","50-59","60-69","70-79","80+")
  grp <- cut(fixed$age_lower,article_breaks,labels=article_labels,right=FALSE)
  cols <- setdiff(names(fixed),"age_lower")
  broad <- aggregate(fixed[cols],list(age_group=grp),sum)
  add_rates <- function(d) {
    d$foreign_share_pct <- 100*d$registered_foreign_nationals_dec31/d$residents_jan2024
    d$emigrants_per_1000 <- 1000*d$emigrants_2023/((d$residents_jan2023+d$residents_jan2024)/2)
    d$shortfall_deaths <- d$vaccinated_expected-d$vaccinated_deaths
    d$shortfall_pct <- 100*d$shortfall_deaths/d$vaccinated_expected
    d
  }
  broad <- add_rates(broad)
  fixed$age_group <- ifelse(fixed$age_lower==85,"85+",paste0(fixed$age_lower,"-",fixed$age_lower+4))
  fixed <- add_rates(fixed[c("age_group",cols)])
  a <- project("15_november_mortality_linkage_sensitivity","annual_original_vs_scenario.csv")
  a <- subset(a,iso_year==2023 & status=="vaccinated")
  stopifnot(nrow(a)==6L, setequal(a$age_group,article_labels), all(a$included_weeks==52))
  a <- a[match(broad$age_group,a$age_group),]
  close(broad$vaccinated_deaths,a$original_deaths)
  close(broad$vaccinated_expected,a$expected_deaths)
  # Upper envelope for the historical expectation contributed by surplus exposure
  # from registered 2023 exits alone. Allow one record per exit, all vaccinated,
  # all aged 25–49 for every week, choosing the highest rate among those ages.
  z <- subset(x,age>=25 & age<50)
  peaks <- aggregate(baseline_deaths_per_person_day~week,z,max)
  stopifnot(nrow(peaks)==52L)
  bound <- sum(7 * peaks$baseline_deaths_per_person_day) * yr$emigrants
  mid <- broad[broad$age_group=="25-49",]
  bound_table <- data.frame(registered_exits_all_ages=yr$emigrants, days=364L,
      additional_expected_deaths_upper_bound=bound, observed_shortfall=mid$shortfall_deaths,
      upper_bound_pct_of_shortfall=100*bound/mid$shortfall_deaths)

  # Make a fresh output set, and verify a previous identical run before reusing it.
  out <- file.path(root,"data","analysis","19_check_migration_and_mortality")
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  stage <- tempfile(".building-",tmpdir=out)
  stopifnot(dir.create(stage))
  on.exit(unlink(stage,recursive=TRUE),add=TRUE)
  save <- function(obj,name) write.csv(obj,file.path(stage,name),row.names=FALSE)
  save(broad,"article_age_groups_2023.csv")
  save(fixed,"five_year_groups_2023.csv")
  save(bound_table,"registered_exit_exposure_bound.csv")
  num <- function(v,n=1) formatC(v,format="f",digits=n,big.mark=",")
  young <- broad[broad$age_group=="15-24",]
  old <- broad[broad$age_group=="80+",]
  html <- c("<!doctype html><html lang='en'><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'><title>Section 7 · Migration and the mortality gap</title>",
    "<style>body{font:18px/1.55 Georgia,serif;max-width:48rem;margin:2rem auto;padding:0 1rem;color:#18323a}aside,h2,h3{font-family:system-ui,sans-serif}aside{padding:.75rem 1rem;background:#edf4f5}a{color:#126071}</style></head><body>",
    "<aside>Section 7, after Table T04. Insert the heading and three paragraphs after “with no individual cause attached to it” and before “What changes if some vaccinated deaths were missed?” Sources and methods can be appended to the existing denominator note 27.</aside>",
    "<h3>Could migration account for part of the gap?</h3>",
    "<p>The population changes visible in Figure V01 raise a question about vaccination histories. Someone arriving in Czechia might have received a dose abroad. Czech providers could enter some foreign doses in ISIN. If a resident’s dose does not appear in November, our subtraction counts their time and any resident death on the no-recorded-dose side. That is the correct allocation for someone who remained unvaccinated. For someone vaccinated abroad, it places both the time and the death in the wrong group. The effect on the rate depends on the mortality of the people added.</p>",
    "<p>Departures create a different problem. Someone vaccinated in Czechia who emigrates can continue contributing vaccinated person-time in our reconstruction because we have no departure date. With resident time fixed, each such day also removes a day from the estimated no-dose group. This can lower the vaccinated rate and raise the no-dose rate even if the death counts stay the same. Some deaths abroad enter Czech resident statistics, and some lack an LPZ record. Neither release tells us who moved or when.</p>",
    paste0("<p>We compared the six age groups using the original death allocation. At ages 25–49, registered foreign nationals amounted to ",num(mid$foreign_share_pct),
      "% of the resident population at year end, and deaths after a recorded dose were ",num(mid$shortfall_pct),
      "% below their seasonal expectation. At ages 15–24 the corresponding figures were ",num(young$foreign_share_pct),
      "% and ",num(young$shortfall_pct),"%. Registered departures per resident were most frequent at ages ",
      chartr("-","–",as.character(broad$age_group[which.max(broad$emigrants_per_1000)])), ". At ages 80+, registered foreign nationals amounted to only ",
      num(old$foreign_share_pct),"% of residents, yet the vaccinated shortfall was ",num(old$shortfall_pct),
      "%. Migration could contribute to the gap, but the age pattern does not establish how much.</p>"),
    "<h2>Add to existing note 27</h2>",
    "<p>The year-end percentage divides the Foreign Police count of registered foreign nationals on 31 December 2023 by the corresponding resident population estimate on 1 January 2024. It includes people who had lived in Czechia for years. The annual departure rate uses reported departures of all citizenships divided by the mean of the opening and closing resident populations. Deaths after a recorded first dose come from the original allocation in the 52 ISO weeks of 2023. Expected deaths use 2015–2019 resident mortality for the same week and five-year age band, applied to the vaccinated group’s person-time. The published November fields include no citizenship, insurance affiliation, residence status or migration dates.</p>",
    "<p><a href='https://csu.gov.cz/docs/107508/000e5d92-a7c1-8ddb-b3aa-3131124d0513/290027240108.pdf?version=1.0'>ČSÚ foreigners, Table 1-8</a> · <a href='https://csu.gov.cz/docs/107508/31bbd694-c2d3-f318-a6f9-5d99223db4b8/13006724h12.pdf?version=1.0'>International migration, Table H.12</a> · <a href='https://csu.gov.cz/produkty/age-structure-of-the-population'>Resident age structure</a> · <a href='https://www.uzis.cz/res/file/covid/covid-19-ockovani-isin-manual-pl.pdf'>ISIN vaccination manual, pp. 15–16</a> · <a href='https://www.nzip.cz/data/2135-covid-19-prehled-populace'>NZIP field dictionary</a> · <a href='https://csu.gov.cz/docs/107508/ac448014-676e-3761-3f94-a9ffe3198e12/13018423ma.pdf?version=1.0'>Resident death series</a> · <a href='https://csu.gov.cz/docs/107508/6724e4e9-b3e4-9854-408a-26c11bf0086e/13018721.pdf?version=1.0'>ČSÚ on LPZ coverage, methodology</a>.</p>",
    "<h2>Optional sentence for the methods note</h2>",
    paste0("<p>For scale, even assigning all ",num(yr$emigrants,0)," registered departures of 2023 to the vaccinated 25–49 group for all 52 weeks, at the highest seasonal rate of any five-year band in that group each week, would add at most ",
      num(bound)," expected deaths to the denominator-based calculation. The observed shortfall was ",num(mid$shortfall_deaths),
      ". This deliberately generous bound assumes one record per departure and covers only excess person-time from registered exits during 2023. Earlier or unregistered departures and errors in death allocation fall outside it.</p></body></html>"))
  writeLines(enc2utf8(html),file.path(stage,"section7_migration_insert.html"),useBytes=TRUE)
  files <- sort(list.files(stage))
  manifest <- data.frame(file=files,sha256=unname(vapply(file.path(stage,files),sha,"")))
  save(manifest,"outputs.csv")
  dest <- file.path(out,substr(sha(file.path(stage,"outputs.csv")),1,20))
  if (!dir.exists(dest) && !file.rename(stage,dest) && !dir.exists(dest)) stop("Could not commit ",dest)
  stopifnot(identical(manifest,read(file.path(dest,"outputs.csv"))),
    identical(unname(vapply(file.path(dest,files),sha,"")),manifest$sha256))
  message("Verified ",dest)
  invisible(dest)
}
check_migration()
