# Run Rscript 23_delta_missing_birth_bands.R from the unpacked bundle.
# --from-project instead reads the original Steps 05, 07 and 21 outputs.
# CZ_PROJECT_ROOT, CZ_STEP05_DIR, CZ_STEP07_DIR and CZ_SURVEILLANCE_OUT
# select those inputs. CZ_DELTA_MISSING_OUT selects the results directory.
args <- commandArgs(FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script <- if (length(file_arg)) sub("^--file=", "", file_arg[1]) else
  unlist(lapply(sys.frames(), function(f) f$ofile), use.names = FALSE)
here <- if (length(script)) dirname(normalizePath(tail(script, 1))) else getwd()
root <- Sys.getenv("CZ_PROJECT_ROOT", getwd())
out <- Sys.getenv("CZ_DELTA_MISSING_OUT", file.path(here, "results"))
dir.create(out, showWarnings = FALSE, recursive = TRUE)
weeks <- sprintf("2021-W%02d", 47:50)
mondays <- as.character(seq(as.Date("2021-11-22"), by = "week", length.out = 4))
arms <- c("no_dose", "vaccinated")
read_table <- function(path) read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
save_table <- function(x, path) write.csv(x, path, row.names = FALSE, na = "")
sha <- function(path) {
  if (requireNamespace("digest", quietly=TRUE)) return(digest::digest(file=path, algo="sha256", serialize=FALSE))
  if ("sha256sum" %in% getNamespaceExports("tools")) return(unname(getExportedValue("tools","sha256sum")(path)))
  if (nzchar(Sys.which("sha256sum"))) return(substr(system2("sha256sum", shQuote(path), stdout=TRUE),1,64))
  stop("Install the R package digest for SHA-256 verification.")
}
step_dir <- function(step, env, required) {
  pin <- Sys.getenv(env)
  if (nzchar(pin)) dirs <- pin else {
    base <- file.path(root, c("data/analysis","results"), step)
    dirs <- unique(c(base, unlist(lapply(base[dir.exists(base)], list.dirs, recursive=FALSE))))
  }
  good <- dirs[vapply(dirs, function(d) all(file.exists(file.path(d,required))), logical(1))]
  if (length(good)!=1L) stop("Select the source run with ", env, ". Found ", length(good), " compatible directories.")
  good
}
verified_table <- function(d, file) {
  inv <- read_table(file.path(d, "outputs.csv"))
  i <- which(inv$file==file)
  stopifnot(length(i)==1L, sha(file.path(d,file))==inv$sha256[i])
  read_table(file.path(d,file))
}
files <- c(counts="death_counts.csv", totals="release_totals.csv", known="known_age.csv",
           unknown="unknown_age.csv", adult="adult_endpoints.csv")
if ("--from-project" %in% commandArgs(TRUE)) {
  s5 <- step_dir("05_profile_missing_demographics","CZ_STEP05_DIR",c("weekly_deaths_by_dose_and_surveillance.csv","release_totals.csv"))
  s7 <- step_dir("07_build_weekly_mortality_rates","CZ_STEP07_DIR",c("weekly_age_cells.csv","unknown_age_weeks.csv"))
  s21 <- step_dir("21_surveillance_check","CZ_SURVEILLANCE_OUT","weekly_age_cells_2021.csv")
  counts <- subset(verified_table(s5,"weekly_deaths_by_dose_and_surveillance.csv"), week %in% weeks)
  totals <- verified_table(s5,"release_totals.csv")
  known <- subset(verified_table(s7,"weekly_age_cells.csv"), release=="november" & spec=="primary" &
                  week %in% weeks & status %in% arms)
  unknown <- subset(verified_table(s7,"unknown_age_weeks.csv"), release=="november" & spec=="primary" &
                    week_start %in% mondays)
  adult <- subset(read_table(file.path(s21,"weekly_age_cells_2021.csv")), week %in% weeks)
  # Save small, reusable extracts with the full precision of the source CSVs.
  d <- file.path(out,"delta_missing_age_inputs")
  dir.create(d, recursive=TRUE, showWarnings=FALSE)
  for (k in names(files)) save_table(get(k), file.path(d, files[k]))
  save_table(data.frame(file=unname(files),sha256=unname(vapply(file.path(d,files),sha,""))),file.path(d,"sha256.csv"))
} else {
  d <- file.path(here,"delta_missing_age_inputs")
  if (!file.exists(file.path(d,"sha256.csv"))) stop("Use the bundled inputs or run with --from-project.")
  inv <- read_table(file.path(d,"sha256.csv"))
  stopifnot(!anyDuplicated(inv$file),setequal(inv$file,files))
  for (k in names(files)) {
    path <- file.path(d,files[k])
    stopifnot(sha(path)==inv$sha256[match(files[k],inv$file)])
    assign(k,read_table(path))
  }
}

# The missingness counts include all ages. For the ASMR, a first dose in
# the LPZ death week counts as vaccinated, as in the original Delta check.
dose_labels <- c("No recorded dose"="no_dose","First dose before death week"="vaccinated",
                 "First dose in death week"="vaccinated")
counts$arm <- unname(dose_labels[counts$status])
stopifnot(all(counts$deaths[is.na(counts$arm)]==0),
          totals$birth_missing_with_dose==1, totals$birth_missing_with_dose_and_death==0,
          totals$birth_missing_with_surveillance_no_dose==0)
counts <- subset(counts,!is.na(arm))
counts$endpoint <- ifelse(counts$surveillance_recorded,"flagged","unflagged")
audit <- aggregate(counts[c("deaths","missing_birth_deaths")],counts[c("arm","endpoint")],sum)
audit$missing_pct <- 100*audit$missing_birth_deaths/audit$deaths
save_table(audit, file.path(out,"period_death_counts_all_ages.csv"))

# All three groups below age 15 have the same allocation multiplier and
# none enters the standard. Combining them before allocation is exact.
known$age <- ifelse(known$age<15L,0L,known$age)
known <- aggregate(known[c("deaths","person_days","standard_n")],known[c("week","age","status")],sum)
keys <- c("week","age","status")
z <- merge(known,adult[c(keys,"flagged","unflagged","days","all_deaths")],by=keys,all.x=TRUE)
stopifnot(nrow(z)==4L*2L*17L,!anyDuplicated(z[keys]),all(z$person_days>0),
          setequal(z$week,weeks),!anyDuplicated(unknown$week_start),setequal(unknown$week_start,mondays))
parts <- split(z, interaction(z$week,z$status,drop=TRUE))
close_enough <- function(a,b,tol=1e-6) isTRUE(all(abs(a-b)<tol))

prepare_cell <- function(g) {
  w <- g$week[1]; s <- g$status[1]
  cts <- subset(counts,week==w & arm==s)
  u <- unknown[match(mondays[match(w,weeks)],unknown$week_start),]
  uf <- sum(cts$missing_birth_deaths[cts$surveillance_recorded])
  ud <- u[[paste0(s,"_deaths")]]
  up <- u[[paste0(s,"_days")]]
  adult_rows <- g$age>=15L
  stopifnot(uf==0, ud==sum(cts$missing_birth_deaths), sum(!adult_rows)==1L,
            close_enough(g$days[adult_rows],g$person_days[adult_rows]),
            close_enough(g$all_deaths[adult_rows],g$deaths[adult_rows]),
            close_enough(g$flagged[adult_rows]+g$unflagged[adult_rows],g$deaths[adult_rows]),
            close_enough(sum(g$deaths)+ud,sum(cts$deaths)))
  g$flagged[!adult_rows] <- sum(cts$deaths[cts$surveillance_recorded])-sum(g$flagged[adult_rows])
  g$unflagged <- g$deaths-g$flagged
  stopifnot(all(g$flagged >= -1e-8),all(g$unflagged >= -1e-8))
  g$missing_deaths <- ud; g$missing_days <- up
  g$undated_days <- if (s=="no_dose") u$undated_no_dose_days else 0
  g
}
parts <- lapply(parts,prepare_cell)
# Exact Section 7 / Step 09 weights use *all* known LPZ deaths for each week
# and status. Separately test known unflagged deaths as the donor population.
specs <- data.frame(
  scenario=c("known_band","deaths_only","like_known","older_65plus","half_undated",
             "unflagged_weights_deaths_only","unflagged_weights_like_known"),
  death_donor=c("all","all","all","all","all","unflagged","unflagged"),
  add_deaths=c(FALSE,rep(TRUE,6)),add_days=c(FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,TRUE),
  older_weight=c(1,1,1,2,1,1,1),undated_fraction=c(1,1,1,1,.5,1,1))
allocate <- function(g,spec) {
  tilt <- ifelse(g$age>=65L,spec$older_weight,1)
  donors <- if (spec$death_donor=="unflagged") g$unflagged else g$deaths
  wd <- donors*tilt; wp <- g$person_days*tilt
  if (sum(wd)<=0) wd <- wp
  added_deaths <- as.numeric(spec$add_deaths)*g$missing_deaths[1]
  added_days <- as.numeric(spec$add_days)*(g$missing_days[1]-(1-spec$undated_fraction)*g$undated_days[1])
  g$extra_unflagged <- added_deaths*wd/sum(wd)
  g$extra_days <- added_days*wp/sum(wp)
  stopifnot(close_enough(sum(g$extra_unflagged),added_deaths),
            close_enough(sum(g$extra_days),added_days),added_days>=0)
  g$unflagged <- g$unflagged+g$extra_unflagged
  g$person_days <- g$person_days+g$extra_days
  g$scenario <- spec$scenario
  g[c("scenario",keys,"standard_n","flagged","unflagged","person_days","extra_unflagged","extra_days")]
}
cells <- do.call(rbind,lapply(seq_len(nrow(specs)), function(i)
  do.call(rbind,lapply(parts,allocate,spec=specs[i,]))))
save_table(cells,file.path(out,"allocated_age_week_cells.csv"))
period <- subset(cells,age>=15L)
# Pool the four weeks first; both endpoints use the same restored denominator.
period <- aggregate(period[c("flagged","unflagged","person_days")],
                    period[c("scenario","status","age","standard_n")],sum)
period_rates <- function(g) {
  stopifnot(setequal(g$age,seq(15L,90L,5L)),nrow(g)==16L,all(g$person_days>0))
  w <- g$standard_n/sum(g$standard_n)
  c(flagged=sum(w*g$flagged/g$person_days),unflagged=sum(w*g$unflagged/g$person_days))*365.25e5
}
rates <- do.call(rbind,lapply(specs$scenario,function(scenario) {
  s <- period[period$scenario==scenario,]
  u <- period_rates(subset(s,status=="no_dose"))
  v <- period_rates(subset(s,status=="vaccinated"))
  rr <- u/v
  data.frame(scenario,no_dose_flagged=u[1],vaccinated_flagged=v[1],flagged_ratio=rr[1],
             no_dose_remaining=u[2],vaccinated_remaining=v[2],remaining_ratio=rr[2],
             ratio_of_ratios=rr[1]/rr[2],row.names=NULL)
}))
stopifnot(close_enough(round(rates$flagged_ratio[1],2),7.47),
          close_enough(round(rates$remaining_ratio[1],2),2.43))
save_table(specs,file.path(out,"assumptions.csv"))
save_table(rates,file.path(out,"period_asmr.csv"))
print(audit,row.names=FALSE)
print(rates[c("scenario","flagged_ratio","remaining_ratio","ratio_of_ratios")],digits=5,row.names=FALSE)
