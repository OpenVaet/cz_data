# Optional rebuild from the project. Pass the Step 05, Step 07 and Step 22
# output directories as three quoted arguments, in that order.
dirs <- commandArgs(TRUE)
if (length(dirs)!=3L) stop("Usage: Rscript prepare_step24_inputs.R STEP05_DIR STEP07_DIR STEP22_DIR")
script <- sub("^--file=", "", grep("^--file=",commandArgs(FALSE),value=TRUE)[1])
out <- file.path(dirname(normalizePath(script)), "step24_inputs")
dir.create(out, showWarnings=FALSE, recursive=TRUE)
load_csv <- function(i, name) read.csv(file.path(dirs[i],name),stringsAsFactors=FALSE)
save_csv <- function(x,name) write.csv(x,file.path(out,name),row.names=FALSE,na="")
calendar <- seq(as.Date("2020-03-02"),as.Date("2023-12-25"),by="week")
weeks <- format(calendar,"%G-W%V")
totals <- load_csv(1,"release_totals.csv")
stopifnot(nrow(totals)==1,totals$birth_missing_with_dose_and_death==0,totals$birth_missing_with_surveillance_no_dose==0)
q <- load_csv(1,"weekly_deaths_by_dose_and_surveillance.csv")
q <- subset(q,week %in% weeks)
stopifnot(all(q$missing_birth_deaths[q$surveillance_recorded | q$status!="No recorded dose"]==0))
missing_count <- aggregate(q["missing_birth_deaths"],q["week_start"],sum)
u <- subset(load_csv(2,"unknown_age_weeks.csv"),release=="november" & spec=="primary" & week_start %in% calendar)
u <- u[match(as.character(calendar),u$week_start),]
stopifnot(nrow(u)==200L,!anyNA(u),
          all(u$no_dose_deaths==missing_count$missing_birth_deaths[match(u$week_start,missing_count$week_start)]),
          all(u$vaccinated_deaths==0))
z <- subset(load_csv(2,"weekly_age_cells.csv"),release=="november" & spec=="primary" & week %in% weeks &
              status %in% c("no_dose","vaccinated"))
ep <- load_csv(3,"weekly_observed_cells.csv")
key <- c("week","age","status")
stopifnot(!anyDuplicated(z[key]),!anyDuplicated(ep[key]),nrow(ep)==200*16*2,nrow(z)==200*19*2)
z <- merge(z,ep[c(key,"flagged","unflagged","all_deaths","days")],by=key,all.x=TRUE)
adult <- z$age>=15
stopifnot(all(abs(z$days[adult]-z$person_days[adult])<1e-5),
          all(abs(z$all_deaths[adult]-z$deaths[adult])<1e-6),
          all(abs(z$flagged[adult]+z$unflagged[adult]-z$deaths[adult])<1e-6))
# Under-15 cells contribute to both allocation denominators. No under-15
# surveillance split is required for the adult analysis.
save_csv(z[c(key,"standard_n","deaths","person_days","flagged")],"known_age.csv")
u$week <- weeks
save_csv(u[c("week","no_dose_deaths","no_dose_days","vaccinated_deaths","vaccinated_days","undated_no_dose_days")],"unknown_age.csv")
