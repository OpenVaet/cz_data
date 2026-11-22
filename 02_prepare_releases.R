# Run after 01 from the same project root: source("02_prepare_releases.R")
# install.packages(c("data.table", "digest"))
# CZ_PROJECT_ROOT defaults to ".". CZ_MANIFEST optionally selects a specific manifest.
# CZ_CHUNK_ROWS (default 100000) controls memory use. Outputs are uncompressed CSV.
# Allow several GB of free disk for raw data, March archive expansion and prepared tables.
# CSVs retain publisher columns. source_row is the 1-based data-row ordinal in that
# release; never use it or November's ID to join the two releases. All *_week_start
# fields denote ISO Mondays. Within-week order in November is unknown. DCCI and
# all raw dose/death fields are left as published. No ages or dates are imputed.

prepare_releases <- function() {
  for (pkg in c("data.table","digest"))
    if (!requireNamespace(pkg,quietly=TRUE)) stop("Install the R package ",pkg)
  root <- normalizePath(Sys.getenv("CZ_PROJECT_ROOT","."),mustWork=TRUE)
  manifest <- Sys.getenv("CZ_MANIFEST")
  if (!nzchar(manifest)) {
    manifests <- list.files(file.path(root,"data","sources","manifests"),"^manifest-.*\\.csv$",full.names=TRUE)
    if (!length(manifests)) stop("No source manifest. Run 01_acquire_sources.R first.")
    manifest <- tail(sort(manifests),1L)
  }
  meta <- data.table::fread(manifest,colClasses="character",na.strings=NULL)
  releases <- c("march","november")
  meta <- meta[match(releases,meta$id), ]
  if (!identical(meta$id,releases) || !all(meta$status=="ok"))
    stop("The selected manifest must contain both acquired releases: ", manifest)
  sha <- function(path) digest::digest(file=path, algo="sha256", serialize=FALSE)
  paths <- file.path(root,meta$path)
  for (i in seq_along(paths))
    if (!file.exists(paths[i]) || sha(paths[i])!=meta$sha256[i]) stop("Source checksum mismatch: ", paths[i])
  chunk <- suppressWarnings(as.integer(Sys.getenv("CZ_CHUNK_ROWS","100000")))
  if (is.na(chunk) || chunk<1L) stop("CZ_CHUNK_ROWS must be a positive integer")
  prepared <- file.path(root,"data","prepared")
  dir.create(prepared,recursive=TRUE,showWarnings=FALSE)
  # Version the outputs by the preparation code and both source objects.
  code <- paste(deparse(body(prepare_releases), control=c("keepInteger","keepNA")), collapse="\n")
  code_hash <- digest::digest(code,algo="sha256",serialize=FALSE)
  key <- digest::digest(paste(c(code_hash,meta$sha256),collapse="/"),algo="sha256",serialize=FALSE)
  dest <- file.path(prepared,paste0("releases-",substr(key,1,20)))
  if (file.exists(file.path(dest,"outputs.csv"))) {
    inventory <- data.table::fread(file.path(dest,"outputs.csv"),colClasses="character")
    for (i in seq_len(nrow(inventory))) {
      path <- file.path(dest,inventory$file[i])
      if (!file.exists(path) || sha(path)!=inventory$sha256[i]) stop("Prepared file changed: ",path)
    }
    message("Verified existing prepared tables: ",dest)
    return(invisible(dest))
  }
  stage <- tempfile(".prepare-",tmpdir=prepared)
  if (!dir.create(stage)) stop("Cannot create preparation directory")
  on.exit(unlink(stage,recursive=TRUE),add=TRUE)
  work <- file.path(stage,"work")
  dir.create(work)
  source_csv <- function(path,id) {
    if (endsWith(path,".tar.xz")) {
      member <- "Vesely_106_202403141131.csv"
      if (id!="march" || !identical(utils::untar(path,list=TRUE,tar="internal"),member))
        stop("Unexpected March archive")
      utils::untar(path,files=member,exdir=work,tar="internal")
      path <- file.path(work,member)
    } else if (endsWith(path,".gz")) {
      input <- gzfile(path,"rb"); on.exit(close(input),add=TRUE)
      path <- file.path(work,paste0(id,".csv"))
      out <- file(path,"wb"); on.exit(close(out),add=TRUE)
      repeat {
        bytes <- readBin(input,"raw",1024L*1024L)
        if (!length(bytes)) break
        writeBin(bytes,out)
      }
    }
    path
  }
  # Work on distinct date strings, then map back to rows. This also avoids using
  # a platform-specific ISO strptime conversion for a year/week without a weekday.
  monday <- function(day) day - (day+3L) %% 7L
  jan4 <- function(y) as.integer(as.Date(paste0(y,"-01-04"),format="%Y-%m-%d"))
  parse_time <- function(x,weekly) {
    u <- unique(x)
    days <- rep(NA_integer_,length(u))
    if (weekly) {
      ix <- which(grepl("^[0-9]{4}-[0-9]{2}$",u))
      if (length(ix)) {
        year <- as.integer(substr(u[ix],1,4)); week <- as.integer(substr(u[ix],6,7))
        value <- monday(jan4(year))+7L*(week-1L)
        ok <- week>=1L & value<monday(jan4(year+1L))
        days[ix[ok]] <- value[ok]
      }
    } else {
      ix <- which(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",u))
      if (length(ix)) {
        dates <- as.Date(u[ix],format="%Y-%m-%d")
        ok <- !is.na(dates) & format(dates,"%Y-%m-%d")==u[ix]
        days[ix[ok]] <- as.integer(dates[ok])
      }
    }
    days[match(x,u)]
  }
  seen <- function(x) !is.na(x) & nzchar(x)
  date_class <- function(x) structure(as.integer(x),class=c("IDate","Date"))
  compare <- function(a,b,op) !is.na(a) & !is.na(b) & op(a,b)
  # fread parses complete logical CSV records. Close any quoted record crossing
  # a chunk boundary before handing it over, including escaped pairs of quotes.
  read_block <- function(con,n) {
    lines <- readLines(con,n=n,warn=FALSE)
    if (!length(lines)) return(lines)
    odd <- sum(nchar(lines,type="bytes") - nchar(gsub('"','',lines,fixed=TRUE),type="bytes")) %% 2L
    while (odd==1L) {
      more <- readLines(con,n=1L,warn=FALSE)
      if (!length(more)) stop("Unterminated quoted record at end of input")
      lines <- c(lines,more)
      odd <- (odd+nchar(more,type="bytes")-nchar(gsub('"','',more,fixed=TRUE),type="bytes")) %% 2L
    }
    lines
  }
  # For the official CSVs the original ID/ordinal fields and raw date strings
  # are preserved. The derived fields are shared where the source precision permits.
  write_rows <- function(table,name) {
    file <- file.path(stage,paste0(name,".csv"))
    if (nrow(table) || !file.exists(file))
      data.table::fwrite(table,file,append=file.exists(file),na="",logical01=TRUE)
  }
  totals <- ranges <- identities <- list()
  dose_tags <- c("Prvni","Druha","Treti","Ctvrta","Pata","Sesta","Sedma")
  for (i in seq_along(releases)) {
    id <- releases[i]; weekly <- id=="november"
    input_path <- source_csv(paths[i],id)
    csv_hash <- sha(input_path)
    identities[[id]] <- data.frame(release=id,source_sha256=meta$sha256[i],csv_sha256=csv_hash,
                                   manifest=basename(manifest),preparation_code_sha256=code_hash)
    dose_names <- if (weekly) paste0("Datum_",dose_tags,"_davka") else paste0("Datum_",seq_len(7L))
    birth <- if (weekly) "RokNarozeni" else "Rok_narozeni"
    sex <- if (weekly) "Pohlavi" else "Pohlavikod"
    death <- if (weekly) "DatumUmrtiLPZ" else "DatumUmrti"
    related <- if (weekly) c("ID","Infekce","DatumPozitivity","DatumVysledku","TypTestu","Symptom","Umrti","DCCI") else character()
    required <- c(birth,sex,death,dose_names,related)
    metrics <- numeric()
    add <- function(name,n) {
      previous <- unname(metrics[name])
      metrics[name] <<- (if (is.na(previous)) 0 else previous)+as.double(n)
    }
    field_ranges <- list()
    track <- function(field,days,raw) {
      prior <- field_ranges[[field]]
      if (is.null(prior)) prior <- c(first=Inf,last=-Inf,recorded=0,invalid=0)
      valid <- days[!is.na(days)]
      if (length(valid)) {
        prior["first"] <- min(prior["first"],min(valid))
        prior["last"] <- max(prior["last"],max(valid))
      }
      prior["recorded"] <- prior["recorded"]+sum(seen(raw))
      prior["invalid"] <- prior["invalid"]+sum(seen(raw) & is.na(days))
      field_ranges[[field]] <<- prior
    }
    process <- function() {
      con <- file(input_path,"rt",encoding="UTF-8"); on.exit(close(con))
      header <- sub("^\ufeff","",readLines(con,1L,warn=FALSE))
      columns <- names(data.table::fread(text=paste0(header,"\n"),header=TRUE))
      if (anyDuplicated(columns) || !all(required %in% columns))
        stop("Missing or duplicate columns for ",id,": ",paste(setdiff(required,columns),collapse=", "))
      count <- 0
      repeat {
        lines <- read_block(con,chunk)
        if (!length(lines)) break
        d <- withCallingHandlers(data.table::fread(text=paste(c(header,lines),collapse="\n"),
                 header=TRUE,sep=",",colClasses="character",na.strings=NULL,strip.white=FALSE,
                 blank.lines.skip=FALSE,check.names=FALSE,showProgress=FALSE),
                 warning=function(w) stop("CSV parse at source row ",count+1,": ",conditionMessage(w)))
        if (!identical(names(d),columns) || !nrow(d)) stop("Invalid CSV chunk at ",count+1)
        d[, source_row := count + seq_len(.N)]
        before <- count; count <- count+nrow(d); add("raw_rows",nrow(d))
        if (weekly) {
          present <- seen(d$Infekce)
          if (any(present & !grepl("^[1-9][0-9]*$",d$Infekce)))
            stop("Unrecognized infection ordinal near input row ",count)
          ordinal <- as.integer(d$Infekce)
          first <- present & ordinal==1L
          repeat_episode <- present & ordinal>1L
          positive <- seen(d$DatumPozitivity)
          include_episode <- present | positive
          days <- parse_time(d$DatumPozitivity,TRUE)
          track("DatumPozitivity_all_rows",days,d$DatumPozitivity)
          episodes <- d[include_episode,c("source_row","ID","Infekce","Pohlavi","RokNarozeni",
                               "DatumPozitivity","DatumVysledku","TypTestu","Symptom"),with=FALSE]
          episodes[, episode_class := ifelse(!present[include_episode],"unclassified",
                                              ifelse(first[include_episode],"primary","repeat"))]
          episodes[, positive_week_start := date_class(days[include_episode])]
          episodes[, positive_date_invalid := seen(DatumPozitivity) & is.na(positive_week_start)]
          write_rows(episodes,"november_infection_episodes")
          add("episode_rows",nrow(episodes)); add("primary_episodes",sum(first))
          add("repeat_episodes",sum(repeat_episode)); add("undated_episodes",sum(include_episode & !positive))
          add("unclassified_episodes",sum(!present & positive)); add("no_episode",sum(!include_episode))
          add("blank_ordinal",sum(!present))
          add("repeat_rows_excluded_from_records",sum(repeat_episode))
          d <- d[!repeat_episode]
        }
        add("retained_records",nrow(d))
        b <- d[[birth]]; missing_birth <- !seen(b) | b=="-"
        u <- unique(b); pattern <- if (weekly) "^[0-9]{4}-[0-9]{4}$" else "^[0-9]{4}$"
        known <- grepl(pattern,u)
        lo <- hi <- rep(NA_integer_,length(u))
        lo[known] <- as.integer(substr(u[known],1L,4L))
        hi[known] <- if (weekly) as.integer(substr(u[known],6L,9L)) else lo[known]
        k <- match(b,u); d[, birth_year_low := lo[k]]; d[, birth_year_high := hi[k]]
        d[, birth_band_invalid := !missing_birth & (is.na(birth_year_low) | is.na(birth_year_high) |
                                      birth_year_high-birth_year_low != (if (weekly) 4L else 0L))]
        d[, birth_missing := missing_birth]; d[, sex_missing := !seen(get(sex))]
        death_days <- parse_time(d[[death]],weekly); track(death,death_days,d[[death]])
        d[, death_week_start := date_class(monday(death_days))]
        invalid_date <- seen(d[[death]]) & is.na(death_days)
        if (weekly) {
          surv <- parse_time(d$Umrti,TRUE); track("Umrti",surv,d$Umrti)
          invalid_date <- invalid_date | (seen(d$Umrti) & is.na(surv)) |
            (seen(d$DatumPozitivity) & is.na(parse_time(d$DatumPozitivity,TRUE)))
          d[, surveillance_week_start := date_class(surv)]
          d[, death_weeks_disagree := compare(death_days,surv,`!=`)]
          add("death_weeks_disagree",sum(d$death_weeks_disagree))
          add("surveillance_death_only",sum(seen(d$Umrti) & !seen(d[[death]])))
          add("lpz_death_only",sum(!seen(d$Umrti) & seen(d[[death]])))
        }
        last_dose <- rep(NA_integer_,nrow(d)); gap_seen <- dose_any <- gap <- reversed <- after <- rep(FALSE,nrow(d))
        for (j in seq_along(dose_names)) {
          nm <- dose_names[j]; value <- d[[nm]]
          dose <- parse_time(value,weekly); track(nm,dose,value)
          has <- seen(value)
          invalid_date <- invalid_date | (has & is.na(dose))
          after <- after | compare(dose,death_days,`>`)
          reversed <- reversed | compare(last_dose,dose,`>`)
          gap <- gap | (gap_seen & has)
          gap_seen <- gap_seen | !has
          dose_any <- dose_any | has
          set <- !is.na(dose); last_dose[set] <- dose[set]
          if (j==1L) {
            d[, first_dose_week_start := date_class(monday(dose))]
            d[, first_dose_after_death := compare(dose,death_days,`>`)]
            d[, first_dose_same_death_week := compare(monday(dose),monday(death_days),`==`)]
            if (!weekly) d[, first_dose_same_death_day := compare(dose,death_days,`==`)]
          }
        }
        d[, any_dose_recorded := dose_any]; d[, any_dose_after_death := after]
        d[, dose_sequence_gap := gap]; d[, dose_dates_reversed := reversed]
        d[, invalid_event_date := invalid_date]
        for (field in c("birth_missing","sex_missing","birth_band_invalid","any_dose_recorded",
                        "first_dose_after_death","first_dose_same_death_week","any_dose_after_death",
                        "dose_sequence_gap","dose_dates_reversed","invalid_event_date"))
          add(field,sum(d[[field]]))
        add("both_demographics_missing",sum(d$birth_missing & d$sex_missing))
        add("missing_birth_with_any_dose",sum(d$birth_missing & dose_any))
        any_death <- seen(d[[death]])
        if (weekly) any_death <- any_death | seen(d$Umrti)
        add("missing_birth_with_any_dose_and_death",sum(d$birth_missing & dose_any & any_death))
        add("all_cause_death_recorded",sum(seen(d[[death]])))
        if (!weekly) add("first_dose_same_death_day",sum(d$first_dose_same_death_day))
        write_rows(d,paste0(id,"_records"))
        if (before==0 || floor(count/1e6)>floor(before/1e6))
          message("  ",id,": ",format(count,big.mark=",",scientific=FALSE)," source rows")
      }
      if (!count) stop("No data rows in ",id)
    }
    message("Preparing ",id); process()
    if (weekly) stopifnot(metrics["raw_rows"]==metrics["retained_records"]+metrics["repeat_episodes"],
                          metrics["raw_rows"]==metrics["episode_rows"]+metrics["no_episode"],
                          metrics["episode_rows"]==sum(metrics[c("primary_episodes","repeat_episodes","unclassified_episodes")]))
    else stopifnot(metrics["raw_rows"]==metrics["retained_records"])
    totals[[id]] <- data.frame(release=id,metric=names(metrics),observed=unname(metrics))
    ranges[[id]] <- data.table::rbindlist(lapply(names(field_ranges),function(field) {
      r <- field_ranges[[field]]
      fmt <- function(x) if (is.finite(x)) format(as.Date(x,origin="1970-01-01")) else ""
      data.frame(release=id,field,precision=if (weekly) "week starting Monday" else "day",
                 observed_from=fmt(r["first"]),observed_to=fmt(r["last"]),
                 recorded=unname(r["recorded"]),invalid=unname(r["invalid"]),row.names=NULL)
    }))
  }
  qc <- data.table::rbindlist(totals)
  hashes <- c(march="37d1f9db6bc77b045439d0fb41be9eac02f3a0286098badb508f8c6ceb532cdd",
              november="fd8cf16b1d09f08ec3d1209135ebd651153ac7b4b9a4392927d3d68decad107b")
  expected <- list(march=c(raw_rows=11028372,birth_missing=0),
                   november=c(raw_rows=12597668,retained_records=12125969,birth_missing=1630992,
                     both_demographics_missing=1619804,missing_birth_with_any_dose=1,
                     missing_birth_with_any_dose_and_death=0,first_dose_after_death=260,
                     first_dose_same_death_week=319,death_weeks_disagree=353,surveillance_death_only=827))
  checks <- data.table::rbindlist(lapply(releases,function(id) {
    x <- expected[[id]]
    data.frame(release=id,metric=names(x),audit_expected=unname(x),
               same_audit_csv=identical(identities[[id]]$csv_sha256,unname(hashes[id])))
  }))
  qc <- merge(qc,checks,by=c("release","metric"),all.x=TRUE,sort=FALSE)
  qc[, difference_from_audit := observed-audit_expected]
  if (any(qc$same_audit_csv & qc$difference_from_audit!=0,na.rm=TRUE))
    stop("Reference checks failed for the exact audited CSV: ",
         paste(qc$metric[which(qc$same_audit_csv & qc$difference_from_audit!=0)],collapse=", "))
  write_rows(qc,"reconciliation"); write_rows(data.table::rbindlist(ranges),"field_coverage")
  write_rows(data.table::rbindlist(identities),"input_identity")
  unlink(work,recursive=TRUE)
  files <- sort(list.files(stage))
  inventory <- data.frame(file=files,bytes=file.info(file.path(stage,files))$size,
                          sha256=vapply(file.path(stage,files),sha,character(1)),row.names=NULL)
  write_rows(inventory,"outputs")
  if (!file.rename(stage,dest)) stop("Could not publish complete output directory. Check for another run: ",dest)
  message("Prepared records, episodes and verification tables: ",dest)
  invisible(dest)
}
prepare_releases()
