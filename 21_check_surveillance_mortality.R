# Run from the OpenVaet/cz_data repository root. Uses the article's aggregate snapshot.
# Rscript 21_check_surveillance_mortality.R
# Optionally recount the supplied aggregate from the original uncompressed November file
# by setting CZ_NOVEMBER_RAW_CSV. On Windows install.packages("digest") for that check.
root <- normalizePath(Sys.getenv("CZ_PROJECT_ROOT", "."), mustWork = TRUE)
args <- commandArgs(FALSE)
script_arg <- grep("^--file=", args, value = TRUE)
script_dir <- if (length(script_arg)) dirname(normalizePath(sub("^--file=", "", script_arg[1]))) else getwd()
out <- Sys.getenv("CZ_SURVEILLANCE_OUT", file.path(root, "data", "analysis", "21_surveillance_check"))
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
      keep <- !is.na(event_week) & substr(event_week,1,4)=="2021" &
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
counts_file <- file.path(script_dir, "november_2021_birth_band_counts.csv")
if (nzchar(raw)) {
  counts <- recount(raw)
  write.csv(counts, file.path(out, basename(counts_file)), row.names = FALSE)
} else {
  stopifnot(sha256(counts_file) == "05c3e86db7eeac900e9e0ce0673805362d2abef0f371a532751c2ab23d8f11be")
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
cells <- subset(all_cells,release=="november" & substr(week,1,4)=="2021")
base <- do.call(rbind,lapply(c("no_dose","vaccinated"),function(status) {
  prefix <- if (status=="no_dose") "direct_no_dose" else "vaccinated"
  data.frame(week=cells$week,age=cells$age,status=status,weight=cells$esp_weight_15plus,
             days=cells[[paste0(prefix,"_days")]],all_deaths=cells[[paste0(prefix,"_deaths")]])
}))
# Step 07 allocates birth years using the January 2021 age distribution,
# then integrates over possible birthdays at each week's Thursday midpoint.
# Only ages 15+ enter this comparison. Cohorts below age 15 in 2021 never contribute.
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
  after_birthday <- as.numeric(dates[i]-as.Date("2021-01-01"))/365
  do.call(rbind,lapply(0:1,function(after) {
    age <- pmin(90, 5*((2021L-z$yob-1L+after)%/%5L))
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
stopifnot(nrow(d)==nrow(base),nrow(d)==52L*16L*2L,max(abs(d$all_check-d$all_deaths))<1e-6)
d$unflagged <- d$all_deaths-d$flagged
stopifnot(min(d$unflagged)> -1e-6)
write.csv(d,file.path(out,"weekly_age_cells_2021.csv"),row.names=FALSE)

summarise <- function(d,period) {
  z <- aggregate(d[c("days","all_deaths","flagged","unflagged","isin_week")],d[c("age","weight","status")],sum)
  stopifnot(all(z$days>0))
  by_status <- lapply(split(z,z$status),function(x) {
    stopifnot(abs(sum(x$weight)-1)<1e-9)
    vapply(c("flagged","unflagged","all_deaths","isin_week"),function(k) sum(x$weight*x[[k]]/x$days)*365.25*1e5,0.0)
  })
  ratio <- by_status$no_dose/by_status$vaccinated
  data.frame(period=period,endpoint=names(ratio),no_dose_asmr=by_status$no_dose,
             vaccinated_asmr=by_status$vaccinated,no_dose_over_vaccinated=ratio,
             ratio_of_ratios=unname(ratio["flagged"]/ratio["unflagged"]),row.names=NULL)
}
windows <- list(weeks_27_to_52=weeks[27:52],delta_weeks_47_to_50=weeks[47:50])
periods <- do.call(rbind,lapply(names(windows),function(k) summarise(subset(d,week %in% windows[[k]]),k)))
write.csv(periods,file.path(out,"period_asmr.csv"),row.names=FALSE)
print(periods,digits=5,row.names=FALSE)
