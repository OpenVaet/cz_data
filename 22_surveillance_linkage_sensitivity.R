# Run from the OpenVaet/cz_data repository root. Uses the article's aggregate snapshot.
# Rscript 22_surveillance_linkage_sensitivity.R
# Optionally recount the supplied aggregate from the original uncompressed November file
# by setting CZ_NOVEMBER_RAW_CSV. On Windows install.packages("digest") for that check.
root <- normalizePath(Sys.getenv("CZ_PROJECT_ROOT", "."), mustWork = TRUE)
args <- commandArgs(FALSE)
script_arg <- grep("^--file=", args, value = TRUE)
script_dir <- if (length(script_arg)) dirname(normalizePath(sub("^--file=", "", script_arg[1]))) else getwd()
out <- Sys.getenv("CZ_SURVEILLANCE_LINKAGE_OUT", file.path(root, "data", "analysis", "22_surveillance_linkage_sensitivity"))
dir.create(out, recursive = TRUE, showWarnings = FALSE)

sha256 <- function(path) {
  if (requireNamespace("digest", quietly = TRUE)) return(digest::digest(file = path, algo = "sha256", serialize = FALSE))
  if ("sha256sum" %in% getNamespaceExports("tools")) return(unname(getExportedValue("tools", "sha256sum")(path)))
  if (!nzchar(Sys.which("sha256sum"))) stop("Install the R package digest.")
  substr(system2("sha256sum", shQuote(path), stdout = TRUE), 1, 64)
}

recount <- function(path) {
  stopifnot(sha256(path) == "fd8cf16b1d09f08ec3d1209135ebd651153ac7b4b9a4392927d3d68decad107b")
  con <- file(path, "rt")
  on.exit(close(con))
  header <- readLines(con, n = 1)
  fields <- names(read.csv(text = header, check.names = FALSE))
  doses <- paste0("Datum_", c("Prvni","Druha","Treti","Ctvrta","Pata","Sesta","Sedma"), "_davka")
  wanted <- c("Infekce","RokNarozeni","Umrti","DatumUmrtiLPZ",doses)
  stopifnot(all(wanted %in% fields))
  classes <- ifelse(fields %in% wanted, "character", "NULL")
  keys <- c("week", "birth", "status", "endpoint")
  acc <- list()
  raw <- kept <- flagged_only <- different_week <- 0
  repeat {
    lines <- readLines(con, n = 100000)
    if (!length(lines)) break
    raw <- raw + length(lines)
    a <- read.csv(text = paste(c(header,lines), collapse="\n"), colClasses = classes,
                  na.strings = "", check.names = FALSE)
    a <- a[is.na(a$Infekce) | a$Infekce == "1", ]
    kept <- kept + nrow(a)
    lpz <- a$DatumUmrtiLPZ
    flag <- !is.na(a$Umrti)
    flagged_only <- flagged_only + sum(flag & is.na(lpz))
    different_week <- different_week + sum(a$Umrti != lpz, na.rm = TRUE)
    # Date and classify the two numerators independently. The ISIN-week numerator
    # can include a death with no LPZ date. It uses the same known-band population.
    for (field in c("DatumUmrtiLPZ","Umrti")) {
      event_week <- a[[field]]
      keep <- !is.na(event_week) & event_week >= "2020-10" & event_week <= "2023-52" &
              !is.na(a$RokNarozeni) & a$RokNarozeni!="-"
      b <- a[keep, ]
      first <- b[[doses[1]]]
      eligible <- (!is.na(first) & first<=b[[field]]) |
                  (is.na(first) & rowSums(!is.na(b[doses[-1]]))==0)
      b <- b[eligible, ]
      if (nrow(b)) {
        endpoint <- if (field=="Umrti") rep("isin",nrow(b)) else
          ifelse(is.na(b$Umrti),"lpz_unflagged","lpz_flagged")
        x <- data.frame(week=b[[field]],birth=as.integer(substr(b$RokNarozeni,1,4)),
                        status=ifelse(is.na(b[[doses[1]]]),"no_dose","vaccinated"),endpoint=endpoint)
        acc[[length(acc)+1L]] <- aggregate(list(n=rep(1L,nrow(x))),x[keys],sum)
      }
    }
  }
  stopifnot(raw == 12597668, kept == 12125969, flagged_only == 827, different_week == 353)
  x <- do.call(rbind, acc)
  x <- aggregate(x["n"], x[keys], sum)
  x[do.call(order,x[keys]), ]
}
raw <- Sys.getenv("CZ_NOVEMBER_RAW_CSV")
counts_file <- file.path(script_dir, "november_study_birth_band_counts.csv")
if (nzchar(raw)) {
  counts <- recount(raw)
  write.csv(counts, file.path(out, basename(counts_file)), row.names = FALSE)
} else {
  stopifnot(sha256(counts_file) == "537483dae653f53d2db07db56d84f04233d04ac0306454a1a8ecd0e4381f8e01")
  counts <- read.csv(counts_file)
}
snap <- file.path(root, "publication_source", "snapshot")
get_input <- function(step, name) {
  directory <- file.path(snap, step)
  inventory <- read.csv(file.path(directory,"outputs.csv"))
  input <- file.path(directory,name)
  stopifnot(identical(sha256(input),inventory$sha256[match(name,inventory$file)]))
  read.csv(input)
}
pop <- get_input("03_census_ageing_migration","january_population_by_age.csv")
national_age <- subset(pop, year == 2021 & !is.na(attained_age))
stopifnot(!anyDuplicated(national_age$attained_age))
all_cells <- get_input("12_compare_asmr_direct_vs_eurostat","age_week_cells.csv")
cells <- subset(all_cells,release=="november")
base <- do.call(rbind,lapply(c("no_dose","vaccinated"),function(status) {
  prefix <- if (status=="no_dose") "direct_no_dose" else "vaccinated"
  data.frame(week=cells$week,age=cells$age,status=status,weight=cells$esp_weight_15plus,
             days=cells[[paste0(prefix,"_days")]],all_deaths=cells[[paste0(prefix,"_deaths")]])
}))
# Step 07 allocates birth years using the January 2021 age distribution,
# then integrates over possible birthdays at each week's Thursday midpoint.
# Only ages 15+ enter this comparison.
weeks <- sort(unique(cells$week))
dates <- as.Date(cells$week_start[match(weeks,cells$week)]) + 3
births <- sort(unique(counts$birth))
mix <- expand.grid(birth=births,offset=0:4)
mix$yob <- mix$birth+mix$offset
mix$prior <- national_age$residents[match(2020L-mix$yob,national_age$attained_age)]
mix$prior[is.na(mix$prior) | mix$prior<=0] <- 1
mix$prob <- mix$prior/ave(mix$prior,mix$birth,FUN=sum)
split_weeks <- lapply(seq_along(weeks), function(i) {
  week <- weeks[i]
  cohort <- counts[counts$week == sub("-W","-",week,fixed=TRUE), ]
  z <- merge(cohort,mix,by="birth")
  year <- as.integer(format(dates[i],"%Y"))
  jan <- as.Date(sprintf("%04d-01-01",year))
  next_jan <- as.Date(sprintf("%04d-01-01",year+1L))
  after_birthday <- as.numeric(dates[i]-jan)/as.numeric(next_jan-jan)
  do.call(rbind,lapply(0:1,function(after) {
    age <- pmin(90, 5*((year-z$yob-1L+after)%/%5L))
    mass <- z$n*z$prob*(if(after==1L) after_birthday else 1-after_birthday)
    data.frame(week=week,age=age,status=z$status,all_check=mass*(z$endpoint!="isin"),
               flagged=mass*(z$endpoint=="lpz_flagged"),isin_week=mass*(z$endpoint=="isin"))
  }))
})
allocated <- do.call(rbind,split_weeks)
allocated <- subset(allocated,age>=15)
allocated_cells <- aggregate(allocated[c("all_check","flagged","isin_week")],allocated[c("week","age","status")],sum)
d <- merge(base,allocated_cells,by=c("week","age","status"),all.x=TRUE)
d$all_check[is.na(d$all_check)] <- 0
d$flagged[is.na(d$flagged)] <- 0
d$isin_week[is.na(d$isin_week)] <- 0
stopifnot(nrow(d)==nrow(base),nrow(d)==200L*16L*2L,max(abs(d$all_check-d$all_deaths))<1e-6)
d$unflagged <- d$all_deaths-d$flagged
stopifnot(min(d$unflagged)> -1e-6)
write.csv(d,file.path(out,"weekly_observed_cells.csv"),row.names=FALSE)


# Calibrate the weekly allocation within each endpoint. The total target for
# the disjoint LPZ endpoints is always defined on the same initial death count.
move_deaths <- function(v, u, target) {
  available <- v>0 & u>0
  capacity <- sum(u[available])
  tol <- 1e-9 * max(1, target)
  stopifnot(all(v>=-1e-7),all(u>=-1e-7),target>=0,target<=capacity+tol)
  move <- numeric(length(v))
  if (target==0) return(move)
  if (target>=capacity) {
    move[available] <- u[available]
  } else {
    log_odds <- log(v[available]/u[available])
    deaths <- v[available]+u[available]
    change <- function(delta) deaths*plogis(log_odds+delta)-v[available]
    objective <- function(delta) sum(change(delta))-target
    upper <- 1
    while (objective(upper)<0 && upper<1024) upper <- upper*2
    delta <- uniroot(objective,c(0,upper),tol=1e-13)$root
    move[available] <- pmin(u[available],pmax(0,change(delta)))
  }
  stopifnot(abs(sum(move)-target)<tol,all(move<=u+tol))
  move
}

endpoints <- c("flagged","unflagged")
u <- subset(d,status=="no_dose"); v <- subset(d,status=="vaccinated")
u <- u[order(u$week,u$age),]; v <- v[order(v$week,v$age),]
stopifnot(identical(as.character(u$week),as.character(v$week)),identical(u$age,v$age),
          identical(u$weight,v$weight),!anyDuplicated(u[c("age","week")]))
scenarios <- data.frame(scenario=c("observed","equal_missed_share","lower_flagged_share","flagged_correct"),
                        all_target=c(0,.15,.15,.15),flagged_target=c(0,.15,.075,0))
windows <- list(weeks_27_to_52=sprintf("2021-W%02d",27:52),
                delta_weeks_47_to_50=sprintf("2021-W%02d",47:50))
period_results <- list(); checks <- list(); weekly_transfers <- list()
for (i in seq_len(nrow(scenarios))) {
  spec <- scenarios[i,]
  moved <- as.data.frame(matrix(0,nrow(v),length(endpoints)))
  names(moved) <- endpoints
  for (age in sort(unique(v$age))) {
    k <- which(v$age==age)
    added_target <- sum(v$all_deaths[k])*spec$all_target/(1-spec$all_target)
    flagged_target <- sum(v$flagged[k])*spec$flagged_target/(1-spec$flagged_target)
    targets <- c(flagged=flagged_target, unflagged=added_target-flagged_target)
    for (e in endpoints) moved[k,e] <- move_deaths(v[[e]][k],u[[e]][k],targets[[e]])
    nmove <- sum(moved$flagged[k]+moved$unflagged[k])
    all_share <- nmove/(nmove+sum(v$all_deaths[k]))
    stopifnot(abs(all_share-spec$all_target)<1e-9)
    checks[[length(checks)+1L]] <- data.frame(scenario=spec$scenario,age=age,
      all_lpz_missed_share=all_share, total_lpz_moved=nmove,
      flagged_missed_share=sum(moved$flagged[k])/sum(moved$flagged[k]+v$flagged[k]),
      unflagged_missed_share=sum(moved$unflagged[k])/sum(moved$unflagged[k]+v$unflagged[k]))
  }
  # Only the flagged and unflagged LPZ endpoints divide the same set of death records.
  moved$all_deaths <- moved$flagged+moved$unflagged
  weekly_transfers[[i]] <- cbind(data.frame(scenario=spec$scenario,week=v$week,age=v$age),moved)
  fields <- names(moved)
  uu <- u; vv <- v
  uu[fields] <- u[fields]-moved; vv[fields] <- v[fields]+moved
  stopifnot(min(as.matrix(uu[fields]))>= -1e-7,
            max(abs(uu$all_deaths-uu$unflagged-uu$flagged))<1e-6,
            max(abs(vv$all_deaths-vv$unflagged-vv$flagged))<1e-6,
            max(abs(as.matrix(uu[fields]+vv[fields]-u[fields]-v[fields])))<1e-6)
  for (period in names(windows)) {
    observed_weeks <- windows[[period]]
    idx <- v$week %in% observed_weeks
    stopifnot(sum(idx)==16L*length(observed_weeks))
    rates <- function(x) {
      p <- aggregate(x[idx,c("days",fields)],x[idx,c("age","weight")],sum)
      stopifnot(all(p$days>0),abs(sum(p$weight)-1)<1e-8)
      vapply(fields,function(e) sum(p$weight*p[[e]]/p$days)*365.25*1e5,0.0)
    }
    ru <- rates(uu); rv <- rates(vv); ratios <- ru/rv
    period_results[[length(period_results)+1L]] <- data.frame(scenario=spec$scenario,period=period,
      endpoint=fields,original_vaccinated_deaths=vapply(v[idx,fields],sum,0.0),
      shifted_deaths=vapply(moved[idx,fields],sum,0.0),
      no_dose_asmr=ru,vaccinated_asmr=rv,no_dose_over_vaccinated=ratios,
      flagged_over_unflagged=unname(ratios["flagged"]/ratios["unflagged"]),row.names=NULL)
  }
}
result <- do.call(rbind,period_results)
write.csv(result,file.path(out,"period_rates.csv"),row.names=FALSE)
write.csv(do.call(rbind,checks),file.path(out,"age_calibration.csv"),row.names=FALSE)
write.csv(do.call(rbind,weekly_transfers),file.path(out,"weekly_transfers.csv"),row.names=FALSE)
print(subset(result,endpoint=="flagged",select=c(scenario,period,no_dose_over_vaccinated,flagged_over_unflagged)),row.names=FALSE)
