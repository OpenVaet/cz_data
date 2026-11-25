# Run from the extracted project: Rscript 16_substack_assets.R
# In RStudio: source("16_substack_assets.R")
# Dependencies: png; digest on systems without a SHA-256 implementation in R.
# CZ_PROJECT_ROOT selects the project. CZ_PUBLICATION_INPUTS can explicitly
# select "snapshot" (supplied aggregate results) or "project" (your latest runs).
build_substack_assets <- function(root = Sys.getenv("CZ_PROJECT_ROOT", ".")) {
  root <- normalizePath(root, mustWork=TRUE)
  src <- file.path(root, "publication_source")
  csv <- function(p) read.csv(p, stringsAsFactors=FALSE, check.names=FALSE, fileEncoding="UTF-8")
  if (!requireNamespace("png", quietly=TRUE)) stop("Install the R package 'png' and rerun Step 16.")
  hash <- function(p) {
    if (requireNamespace("digest", quietly=TRUE)) return(digest::digest(file=p,algo="sha256",serialize=FALSE))
    if ("sha256sum" %in% getNamespaceExports("tools")) return(unname(getExportedValue("tools","sha256sum")(p)))
    if (nzchar(Sys.which("sha256sum"))) return(substr(system2("sha256sum",shQuote(p),stdout=TRUE),1,64))
    stop("Install the R package 'digest' for SHA-256 verification.")
  }
  placements <- csv(file.path(src,"placements.csv"))
  requirements <- csv(file.path(src,"required_outputs.csv"))
  stopifnot(!anyDuplicated(placements$id), !anyDuplicated(placements$file))
  project_dirs <- file.path(root,c("data/analysis","results"))
  available <- unlist(lapply(project_dirs,list.dirs,recursive=FALSE))
  mode <- Sys.getenv("CZ_PUBLICATION_INPUTS", if (any(basename(available) %in% requirements$directory)) "project" else "snapshot")
  if (!mode %in% c("snapshot","project")) stop("CZ_PUBLICATION_INPUTS must be 'snapshot' or 'project'.")
  message("Step 16 uses ", mode, " inputs.")
  source_dirs <- list()
  provenance <- list()
  for (step in unique(requirements$step)) {
    need <- requirements[requirements$step==step,]
    bases <- file.path(project_dirs, need$directory[1])
    dirs <- if (mode=="snapshot") file.path(src,"snapshot",need$directory[1]) else
      unique(c(bases,unlist(lapply(bases,list.dirs,recursive=FALSE))))
    pin <- Sys.getenv(sprintf("CZ_STEP%02d_DIR",step))
    if (mode=="project" && nzchar(pin)) dirs <- pin
    dirs <- dirs[file.exists(file.path(dirs,"outputs.csv"))]
    dirs <- dirs[order(file.mtime(file.path(dirs,"outputs.csv")),decreasing=TRUE)]
    for (d in dirs) {
      inv <- csv(file.path(d,"outputs.csv"))
      idx <- match(need$file,inv$file)
      if (anyNA(idx) || anyDuplicated(inv$file)) next
      paths <- file.path(d,need$file)
      if (!all(file.exists(paths)) || !all(vapply(paths,hash,"")==inv$sha256[idx])) stop("Missing or changed input in ",d)
      if (step==15) {
        s <- csv(file.path(d,"scenario.csv"))
        if (nrow(s)!=1L || !isTRUE(all.equal(s$missed_share_of_true_vaccinated_deaths,0.15)) ||
            !identical(s$calibration_period,"study")) next
        parent <- csv(file.path(d,"input_sha256.csv"))
        a <- provenance[["14"]]
        if (anyNA(match(parent$file,a$file)) || !all(parent$sha256==a$sha256[match(parent$file,a$file)])) next
      }
      k <- as.character(step)
      source_dirs[[k]] <- d
      provenance[[k]] <- data.frame(step=step,file=need$file,sha256=inv$sha256[idx])
      break
    }
    if (is.null(source_dirs[[as.character(step)]])) stop("No compatible, complete Step ",step," run. Check CZ_STEP",
       sprintf("%02d",step),"_DIR or set CZ_PUBLICATION_INPUTS=snapshot for the supplied run.")
  }
  source_file <- function(step,name) file.path(source_dirs[[as.character(step)]],name)
  read_result <- function(step,name) csv(source_file(step,name))
  inputs <- do.call(rbind,provenance)
  support <- c("placements.csv","required_outputs.csv","csu_g02_u07.csv","csu_g02_sources.csv","age_panel.R","tables.R","guide.R",
               "ecdc_cz_gisaid_variants.csv")
  hashes <- vapply(c(file.path(src,support),file.path(root,c("16_substack_assets.R","17_november_linkage_adjusted_asmr.R"))),hash,"")
  base <- file.path(root,"publication")
  dir.create(base,recursive=TRUE,showWarnings=FALSE)
  stage <- tempfile(".building-",tmpdir=base)
  stopifnot(dir.create(stage)); on.exit(unlink(stage,recursive=TRUE),add=TRUE)
  writeLines(c(inputs$sha256,unname(hashes)),file.path(stage,"signature"))
  dest <- file.path(base,paste0("export-",substr(hash(file.path(stage,"signature")),1,12)))
  unlink(file.path(stage,"signature"))
  verify <- function(d) {
    o <- csv(file.path(d,"outputs.csv"))
    stopifnot(nrow(o)>nrow(placements), !anyDuplicated(o$file), all(file.exists(file.path(d,o$file))),
              all(vapply(file.path(d,o$file),hash,"")==o$sha256))
    message("Verified ",file.path(d,"substack_insertion_guide.html"))
    invisible(d)
  }
  if (dir.exists(dest)) return(verify(dest))
  dir.create(file.path(stage,"png"))
  outfile <- function(id) {
    stopifnot(id %in% placements$id)
    file.path(stage,"png",placements$file[match(id,placements$id)])
  }
  title_for <- function(id) placements$title[match(id,placements$id)]
  write.csv(inputs,file.path(stage,"source_checksums.csv"),row.names=FALSE)
  # The guide and table data use the same values used to typeset the PNGs.
  sys.source(file.path(src,"tables.R"),envir=environment())
  sys.source(file.path(src,"age_panel.R"),envir=environment())
  sys.source(file.path(src,"guide.R"),envir=environment())
  tables <- publication_tables(read_result,src)
  all_limits <- rbind(tables$T03$limits,tables$T05$limits)
  write.csv(all_limits,file.path(stage,"T03_T05_rate_intervals.csv"),row.names=FALSE)
  for (id in names(tables)) draw_table(outfile(id),id,title_for(id),tables[[id]])
  step17_env <- new.env(parent=environment())
  sys.source(file.path(root,"17_november_linkage_adjusted_asmr.R"),envir=step17_env)
  # Add an identifying strip outside the supplied image. Use native raster
  # pixels at their original resolution; the plot area is never cropped.
  frame <- function(id,files) {
    images <- lapply(files,png::readPNG,native=TRUE)
    widths <- vapply(images,ncol,1L); heights <- vapply(images,nrow,1L)
    stopifnot(length(unique(widths))==1)
    w <- widths[1]; bar <- round(w*.041); h <- sum(heights)+bar
    png(outfile(id),width=w,height=h,bg="white")
    on.exit(dev.off())
    step17_env$draw_publication_header(id,title_for(id),w)
    grid::pushViewport(grid::viewport(xscale=c(0,w),yscale=c(0,h)))
    at <- function(x) grid::unit(x,"native")
    top <- h-bar
    for (j in seq_along(images)) {
      grid::grid.raster(images[[j]],x=at(0),y=at(top),width=at(w),height=at(heights[j]),just=c("left","top"),interpolate=FALSE)
      top <- top-heights[j]
    }
    grid::popViewport()
  }
  charts <- requirements[endsWith(requirements$file,".png"),]
  for (j in seq_len(nrow(charts))) {
    name <- charts$file[j]; step <- charts$step[j]
    id <- if(step==10) "V20" else if(step==12) "S01" else if(step==13)
      (if(startsWith(name,"march")) "S02" else "S03") else substr(name,1,3)
    frame(id,source_file(step,name))
  }
  q <- read_result(15,"quarterly_group_size_and_excess.csv")
  tmp <- tempfile(fileext=".png",tmpdir=stage)
  draw_young_quarters(tmp,q)
  frame("V12",tmp); unlink(tmp)
  groups <- c("15-24","25-49","50-59","60-69","70-79","80+")
  w <- read_result(15,"weekly_age_groups.csv"); w$week_mid <- as.Date(w$week_mid)
  s <- read_result(15,"weekly_status.csv"); tr <- read_result(15,"linear_trends.csv")
  for (n in c("first_fitted_midweek","last_fitted_midweek")) tr[[n]] <- as.Date(tr[[n]])
  key <- function(x) paste(x$week,x$age_group,sep=" / ")
  for (arm in c("no_dose","vaccinated")) {
    z <- s[s$status==arm,]
    idx <- match(key(w),key(z))
    stopifnot(!anyDuplicated(key(z)), !anyNA(idx), nrow(w)==nrow(z))
    w[[paste0(arm,"_original")]] <- z$original_rate_per_100k_py[idx]
  }
  stopifnot(all(abs(w$scenario_no_dose_deaths+w$scenario_vaccinated_deaths-w$national_deaths)<1e-7))
  for (j in seq_along(groups)) {
    g <- groups[j]; band <- w[w$age_group==g,]
    fit <- tr[tr$age_group==g & tr$series %in% c("step14_original","scenario"),]
    stopifnot(nrow(fit)==4L, nrow(band)>0)
    pred <- function(d) fit$intercept_at_2021_01_01+fit$annual_change_in_rate_per_100k_py*as.numeric(d-as.Date("2021-01-01"))/365.25
    pred1 <- pred(fit$first_fitted_midweek); pred2 <- pred(fit$last_fitted_midweek)
    scen <- fit$series=="scenario"
    pred1[scen] <- pmax(pred1[scen],0); pred2[scen] <- pmax(pred2[scen],0)
    limits <- range(c(0,band$no_dose_rate,band$vaccinated_rate,band$no_dose_original,band$vaccinated_original,
                      band$no_dose_seasonal_rate,band$vaccinated_seasonal_rate,pred1,pred2),na.rm=TRUE)
    ticks <- pretty(limits,n=5); panels <- file.path(stage,c("original.png","scenario.png"))
    for (k in 1:2) {
      modelled <- k==2; view <- band; qr <- q
      if (!modelled) {
        view$no_dose_rate <- view$no_dose_original
        view$vaccinated_rate <- view$vaccinated_original
      }
      view$negative_no_dose_deaths <- !modelled & view$infeasible_before
      qr$excess_deaths <- if(modelled) q$scenario_excess_deaths else q$original_excess_deaths
      series <- if(modelled) "scenario" else "step14_original"
      png(panels[k],width=16,height=8.8,units="in",res=155,pointsize=12,bg="white")
      tryCatch(draw_age_panel(g,view,qr,fit[fit$series==series,],ticks,modelled),finally=dev.off())
    }
    frame(sprintf("V%02d",j+12L),panels); unlink(panels)
  }
  # Step 17 renders the numbered ASMR figure and its age-group supplements.
  rate_dir <- step17_env$build_linkage_asmr(root,source_dirs[["12"]],source_dirs[["15"]])
  extra <- placements$file[placements$source=="17"]
  stopifnot(length(extra)==7L,all(file.copy(file.path(rate_dir,extra),file.path(stage,"png"))))
  stopifnot(identical(sort(list.files(file.path(stage,"png"))),sort(placements$file)))
  write_guide(stage,placements,tables,rate_dir)
  files <- list.files(stage,recursive=TRUE)
  write.csv(data.frame(file=files,sha256=unname(vapply(file.path(stage,files),hash,""))),file.path(stage,"outputs.csv"),row.names=FALSE)
  if (!file.rename(stage,dest) && !dir.exists(dest)) stop("Cannot commit publication export to ",dest)
  verify(dest)
}
build_substack_assets()
