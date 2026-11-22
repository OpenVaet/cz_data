# Run from the project root: source("01_acquire_sources.R") or Rscript 01_acquire_sources.R
# install.packages(c("data.table", "digest", "jsonlite"))
# Default: acquire all sources. Set CZ_SOURCE_SET=releases for only the two extracts.
# CZ_INPUT_DIR can contain publisher-named downloads, including a decompressed March
# CSV or a gzipped November CSV. The project's data/ directory is also searched.
# CZ_REFRESH=1 requests a new snapshot (explicit local inputs still take precedence).
# An older project copy with the translated English header is skipped automatically.
# Existing snapshots are retained. CZ_PROJECT_ROOT defaults to the working directory.

acquire_sources <- function() {
  for (pkg in c("data.table", "digest", "jsonlite"))
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Install the R package ", pkg)
  root <- normalizePath(Sys.getenv("CZ_PROJECT_ROOT", "."), mustWork = TRUE)
  objects <- file.path(root, "data", "sources", "objects")
  manifests <- file.path(root, "data", "sources", "manifests")
  for (dir in c(objects, manifests)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  mode <- match.arg(Sys.getenv("CZ_SOURCE_SET", "all"), c("all", "releases"))
  refresh <- identical(Sys.getenv("CZ_REFRESH"), "1")
  input_dirs <- unique(c(Sys.getenv("CZ_INPUT_DIR"), file.path(root, "data", "raw"), file.path(root, "data")))
  input_dirs <- input_dirs[nzchar(input_dirs) & dir.exists(input_dirs)]
  old_options <- options(timeout = max(1200, getOption("timeout")))
  on.exit(options(old_options), add = TRUE)
  utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  sha <- function(path) digest::digest(file = path, algo = "sha256", serialize = FALSE)
  ministry <- "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/"
  estat <- "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/"
  foi <- "Vesely_106_202403141131"
  nzip <- "Otevrena-data-NR-26-30-COVID-19-prehled-populace-2024-01"
  entry <- function(id, filename, url, edition, coverage, unit, fields = "", time_field = "",
                    audit_hash = "", aliases = "") {
    data.frame(id, filename, url, edition, coverage, unit, fields, time_field, audit_hash, aliases)
  }
  sources <- do.call(rbind, list(
    entry("march", paste0(foi, ".tar.xz"),
          paste0("https://media.githubusercontent.com/media/PalackyUniversity/uzis-data-analysis/main/data/", foi, ".tar.xz"),
          "2024-03", "deaths 2020-01-01 to 2022-12-31; recorded vaccinations to 2024-03-13", "FOI record; reconstructed demographic remainder",
          "Pohlavikod|Rok_narozeni|DatumUmrti|Datum_1", audit_hash="7c99723a376ba115cb4661b5464781c886c176a85056749efba49fbbf31b29e5",
          aliases=paste("foi-actual.tar.xz", paste0(foi, ".csv"), paste0(foi, ".csv.gz"), sep="|")),
    entry("november", paste0(nzip, ".csv"), paste0("https://data.mzcr.cz/data/distribuce/402/", nzip, ".csv"),
          "2024-11 / 2024-01", "2020-2024; field-specific endpoints", "NZIP record, including repeat infection rows",
          "ID|Infekce|Pohlavi|RokNarozeni|DatumPozitivity|DatumUmrtiLPZ|Umrti|DCCI|Datum_Prvni_davka",
          audit_hash="fd8cf16b1d09f08ec3d1209135ebd651153ac7b4b9a4392927d3d68decad107b",
          aliases=paste(paste0(nzip, ".csv.gz"), "nzip.csv", sep="|")),
    entry("march_response", "24-06-inf106-1999-odpoved.pdf",
          "https://www.uzis.cz/res/file/poskytnute-informace/24-06-inf106-1999-odpoved.pdf",
          "March FOI response", "field-specific: deaths to 2022; vaccinations to extraction", "release documentation"),
    entry("november_dictionary", paste0(nzip, ".pdf"),
          paste0("https://www.nzip.cz/data/isin-covid/metodicke-popisy/Otevrena-data-NR-26-30-covid-19-prehled-populace-2024-01.pdf"),
          "NR-26-30 2024-01", "2020-2024", "field dictionary"),
    entry("moh_doses","ockovani.csv",paste0(ministry,"ockovani.csv"),"rolling","observed in datum",
          "daily doses by product, region and age","datum|prvnich_davek|celkem_davek","datum",
          "84c4cbf9b2beed798b04369db8bb2da608f80cba7bebeb34e12986dd093b1348","moh.csv"),
    entry("moh_cases","nakazeni-vyleceni-umrti-testy.csv",paste0(ministry,"nakazeni-vyleceni-umrti-testy.csv"),
          "rolling","observed in datum","national daily counts",
          "datum|prirustkovy_pocet_nove_nakazenych_primoinfekce|prirustkovy_pocet_nove_nakazenych_reinfekce","datum",
          "0c4e6e77f15a52735c9e25ac2eb261e9c06fcd2518a1766695fea84e227d1304","cases.csv"),
    entry("moh_cases_week","nakazeni-reinfekce-tyden.csv",paste0(ministry,"nakazeni-reinfekce-tyden.csv"),
          "rolling","observed week starts","national weekly episodes","tyden_od|primoinfekce|prvni_reinfekce","tyden_od"),
    entry("ecdc_doses","ecdc_vaccine_tracker.csv","https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv",
          "historical tracker","national CZ rows","weekly doses by product, region and target group",
          "YearWeekISO|Region|TargetGroup|FirstDose|UnknownDose","YearWeekISO",
          "290484bdfed16e80dc2bf5da24d0b85a8c1bd841e7b06e5980f394a7da6140f0","ecdc.csv"),
    entry("ecdc_dictionary","Variable_Dictionary_VaccineTracker-5-april-2023.pdf",
          "https://www.ecdc.europa.eu/sites/default/files/documents/Variable_Dictionary_VaccineTracker-5-april-2023.pdf",
          "2023-04-05","historical tracker","variable definitions"),
    entry("eurostat_population","demo_pjan_2010_2024.json",paste0(estat,"demo_pjan?geo=CZ&sex=T&sinceTimePeriod=2010&untilTimePeriod=2024"),
          "rolling","January 2010-2024","resident counts by age on January 1"),
    entry("eurostat_population_audit","demo_pjan_2020_2024.json",paste0(estat,"demo_pjan?geo=CZ&sex=T&sinceTimePeriod=2020&untilTimePeriod=2024"),
          "audit query","January 2020-2024","resident counts by age on January 1",
          audit_hash="b492142d062c5e354a86f75e7d3b5946bec39e5a7ac5d63c6f8b2a1b23ef66e1",aliases="pop.json"),
    entry("eurostat_deaths","demo_r_mwk_05_CZ.json",paste0(estat,"demo_r_mwk_05?geo=CZ&sex=T&unit=NR&sinceTimePeriod=2020-W01&untilTimePeriod=2024-W01"),
          "rolling","2020-W01 to 2024-W01","weekly deaths by age","", "",
          "1e013879ce751a95661bf31b98c15c0d88c6e381b6ea4411d272373963102e47","dead.json"),
    entry("eurostat_asmr","hlth_cd_asdr2_CZ.json",paste0(estat,"hlth_cd_asdr2?geo=CZ&sex=T&age=TOTAL&icd10=TOTAL&unit=RT&sinceTimePeriod=2022&untilTimePeriod=2023"),
          "rolling","2022 and 2023","annual standardized deaths per 100,000","", "",
          "4d6eed8f4ba6e1ac660394d1a53a4b743043a96198c16396b3207ca1bb5d74d9","sd.json"),
    entry("csu_census","sldb2021_obyvatelstvo_en.xlsx",
          "https://scitani.gov.cz/docs/42301/dfe0b6f6-8791-fb2a-1c5a-cb15cda09b1f/sldb2021_obyvatelstvo_en.xlsx",
          "2021 census tables","includes historical census comparisons","census population tables"),
    entry("csu_population_balance","OBY01CR.csv","https://data.csu.gov.cz/opendata/sady/OBY01CR/distribuce/csv",
          "rolling","observed years","Czech national annual population and vital/migration counts","IndicatorType|CasR2A|Hodnota","CasR2A"),
    entry("csu_population_2023","130142-24data051724.zip",
          "https://csu.gov.cz/docs/107508/a53bbc83-5e04-5a74-36f9-549a090a806e/130142-24data051724.zip",
          "2024-05 vintage","includes 31 December 2023","official resident population distribution"),
    entry("csu_methodology","population-methodology.html","https://csu.gov.cz/population-methodology",
          "published methodology","annual population series","definitions and census rebasing")
  ))
  if (mode == "releases") sources <- sources[sources$id %in% c("march","november"), ]
  split_fields <- function(x) if (nzchar(x)) strsplit(x, "|", fixed=TRUE)[[1]] else character()
  check <- function(ok, message) if (!isTRUE(ok)) stop(message, call.=FALSE)
  inspect <- function(path, filename, spec) {
    fields <- first <- last <- vintage <- ""
    if (grepl("\\.csv(\\.gz)?$", filename, ignore.case=TRUE)) {
      con <- if (endsWith(filename,".gz")) gzfile(path,"rt") else file(path,"rt")
      on.exit(close(con))
      header <- sub("^\ufeff","",readLines(con,1L,warn=FALSE))
      fields <- names(data.table::fread(text=paste0(header,"\n"),header=TRUE,check.names=FALSE))
      check(all(split_fields(spec$fields) %in% fields), paste("Unexpected CSV header for", spec$id))
      if (nzchar(spec$time_field)) {
        select <- c(spec$time_field, if (spec$id=="ecdc_doses") "Region")
        vals <- data.table::fread(path, select=select, colClasses="character", showProgress=FALSE)
        if (spec$id=="ecdc_doses") vals <- vals[vals$Region=="CZ", ]
        times <- vals[[spec$time_field]]
        check(any(!is.na(times) & nzchar(times)), "Empty source time range")
        bounds <- range(times[!is.na(times) & nzchar(times)])
        first <- bounds[1]; last <- bounds[2]
      }
    } else if (endsWith(filename,".json")) {
      j <- jsonlite::fromJSON(path, simplifyVector=FALSE)
      check(identical(j$class,"dataset") && length(j$value)>0 && "CZ" %in% names(j$dimension$geo$category$index), "Invalid Eurostat dataset")
      fields <- unlist(j$id)
      times <- names(j$dimension$time$category$index)
      check(length(times)>0, "Eurostat response has no time dimension values")
      first <- min(times); last <- max(times); vintage <- as.character(j$updated)
    } else if (endsWith(filename,".tar.xz")) {
      member <- paste0(foi,".csv")
      check(identical(utils::untar(path, list=TRUE, tar="internal"), member), "Unexpected FOI archive contents")
      unpack <- tempfile("foi-header-"); check(dir.create(unpack), "Cannot inspect FOI CSV")
      on.exit(unlink(unpack,recursive=TRUE), add=TRUE)
      utils::untar(path,files=member,exdir=unpack,tar="internal")
      return(inspect(file.path(unpack,member),member,spec))
    } else if (grepl("\\.(xlsx|zip)$", filename)) {
      fields <- utils::unzip(path,list=TRUE)$Name
      expect <- if (endsWith(filename,".xlsx")) "xl/workbook.xml" else "OD_DEM02_vse_2024051613085948.CSV"
      check(expect %in% fields, "Unexpected ZIP or workbook contents")
      if (endsWith(filename,".zip")) {
        con <- unz(path,expect,open="rb"); on.exit(close(con))
        # Read a short binary prefix; the archive member has a single-line CSV header.
        prefix <- rawToChar(readBin(con,"raw",1024L))
        header <- strsplit(prefix,"\n",fixed=TRUE)[[1]][1]
        fields <- names(data.table::fread(text=paste0(header,"\n"),header=TRUE))
        check("hodnota" %in% fields, "Missing value field in the population CSV")
      }
    } else if (endsWith(filename,".pdf")) {
      check(identical(readBin(path,"raw",5),charToRaw("%PDF-")), "Expected a PDF")
    } else {
      check(any(grepl("population.{1,8}methodology|methodology.{1,8}population",
                      readLines(path,warn=FALSE,encoding="UTF-8"), ignore.case=TRUE)), "Unexpected methodology page")
    }
    list(schema=paste(fields,collapse="|"), observed_from=first, observed_to=last, publisher_updated=vintage)
  }
  history <- sort(list.files(manifests,"^manifest-.*\\.csv$",full.names=TRUE),decreasing=TRUE)
  previous <- data.table::rbindlist(lapply(history, data.table::fread, colClasses="character",na.strings=NULL),fill=TRUE)
  stage <- tempfile(".acquire-",tmpdir=objects)
  check(dir.create(stage), "Cannot create staging directory")
  on.exit(unlink(stage,recursive=TRUE), add=TRUE)
  result <- vector("list",nrow(sources))
  for (i in seq_len(nrow(sources))) {
    s <- sources[i, ]; message("[",i,"/",nrow(sources),"] ",s$id)
    result[[i]] <- tryCatch({
      prior <- if (nrow(previous)) previous[previous$id==s$id & previous$status=="ok" & previous$url==s$url, ] else previous
      if (!refresh && nrow(prior)) {
        row <- as.list(prior[1, ])
        row[c("edition","coverage","unit")] <- as.list(s[c("edition","coverage","unit")])
        check(identical(sha(file.path(root,row$path)),row$sha256),paste("Cached object changed:",row$path))
      } else {
        names_in <- c(s$filename, split_fields(s$aliases))
        candidates <- unlist(lapply(input_dirs, file.path, names_in),use.names=FALSE)
        found <- candidates[file.exists(candidates)]
        path <- file.path(stage, s$filename)
        origin <- "download"; registered <- utc(); retrieved <- registered
        name <- s$filename
        info <- NULL
        for (candidate in found) {
          candidate_name <- basename(candidate)
          trial <- tryCatch(inspect(candidate,candidate_name,s),error=function(e) {
            message("  Skipping ",candidate_name,": ",conditionMessage(e))
            NULL
          })
          if (is.null(trial)) next
          name <- candidate_name; path <- file.path(stage,name)
          check(file.copy(candidate,path), "Cannot copy local source")
          origin <- "local"; retrieved <- ""; info <- trial
          break
        }
        if (is.null(info)) {
          status <- utils::download.file(s$url,path,mode="wb",quiet=TRUE,method="libcurl")
          check(status==0 && file.info(path)$size>0, "Download incomplete")
          info <- inspect(path,name,s)
        }
        hash <- sha(path)
        expected <- s$audit_hash
        if (s$id=="march" && endsWith(name,".csv")) expected <- "37d1f9db6bc77b045439d0fb41be9eac02f3a0286098badb508f8c6ceb532cdd"
        if (endsWith(name,".gz")) expected <- ""
        if (nzchar(expected) && hash!=expected) message("  SHA-256 differs from the audited copy: ",s$id)
        relative <- file.path("data","sources","objects",hash,name)
        dest <- file.path(root,relative)
        dir.create(dirname(dest),showWarnings=FALSE)
        if (file.exists(dest)) check(identical(sha(dest),hash),"Existing object failed checksum")
        else check(file.rename(path,dest),"Cannot commit source object")
        row <- c(as.list(s[c("id","url","edition","coverage","unit")]),
                 list(status="ok",origin=origin,retrieved_utc=retrieved,registered_utc=registered,
                      path=relative,bytes=file.info(dest)$size,sha256=hash,audit_sha256=expected,
                      audit_match=if (nzchar(expected)) as.character(expected==hash) else ""),
                 info, list(error=""))
      }
      row
    }, error=function(e) {
      message("  ",conditionMessage(e))
      c(as.list(s[c("id","url","edition","coverage","unit")]),list(status="failed",error=conditionMessage(e)))
    })
  }
  out <- data.table::rbindlist(result,fill=TRUE)
  name <- paste0("manifest-",format(Sys.time(),"%Y%m%dT%H%M%OS6",tz="UTC"),"-",Sys.getpid(),".csv")
  tmp <- tempfile(".manifest-",tmpdir=manifests)
  data.table::fwrite(out,tmp,na="")
  check(file.rename(tmp,file.path(manifests,name)), "Cannot commit manifest")
  message("Source manifest: ",file.path(manifests,name))
  if (any(out$status!="ok")) stop("Acquisition incomplete: ",paste(out$id[out$status!="ok"],collapse=", "),
                                   ". Successful sources are cached for retry.",call.=FALSE)
  invisible(out)
}
acquire_sources()
