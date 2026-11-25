# Rscript 24_delta_combined_sensitivity.R   (unpack the bundle first).
# Can also be sourced from RStudio. Only base R is required.
run_combined <- function(input_dir, out_dir) {
  read_input <- function(name) read.csv(file.path(input_dir,name),stringsAsFactors=FALSE)
  known <- read_input("known_age.csv")
  missing <- read_input("unknown_age.csv")
  weeks <- format(seq(as.Date("2020-03-02"),as.Date("2023-12-25"),by="week"),"%G-W%V")
  arms <- c("no_dose","vaccinated")
  ages <- seq(15L,90L,5L)
  adult <- known$age>=15
  stopifnot(nrow(known)==length(weeks)*19L*2L,!anyDuplicated(known[c("week","age","status")]),
            setequal(known$week,weeks),setequal(known$status,arms),setequal(known$age,seq(0,90,5)),
            all(is.finite(known$flagged[adult])),all(known$flagged[adult]>=0),
            all(known$deaths[adult]-known$flagged[adult]>= -1e-6),
            all(is.finite(known$person_days)),all(known$person_days>=0),all(known$deaths>=0),
            setequal(missing$week,weeks),!anyDuplicated(missing$week),all(missing$vaccinated_deaths==0),
            all(missing$undated_no_dose_days<=missing$no_dose_days))
  # Restore both outcomes and exposure across ALL ages before selecting ages 15+.
  # As verified by Step 05, all missing-band deaths were no-dose and unflagged.
  age_rules <- data.frame(age_rule=c("known_only","like_known","older_65plus","half_undated"),
                          restore=c(FALSE,TRUE,TRUE,TRUE),older_weight=c(1,1,2,1),undated_fraction=c(1,1,1,.5))
  link_rules <- data.frame(link_rule=c("original","equal_15","flagged_7_5","flagged_0"),
                           total_share=c(0,.15,.15,.15),flagged_share=c(0,.15,.075,0))
  index <- match(known$week,missing$week)
  undosed <- known$status=="no_dose"
  unknown_deaths <- ifelse(undosed,missing$no_dose_deaths[index],missing$vaccinated_deaths[index])
  unknown_days <- ifelse(undosed,missing$no_dose_days[index],missing$vaccinated_days[index])
  undated_days <- ifelse(undosed,missing$undated_no_dose_days[index],0)
  group <- interaction(known$week,known$status,drop=TRUE)
  group_sum <- function(v) ave(v,group,FUN=sum)
  allocate_ages <- function(rule) {
    z <- known
    tilt <- ifelse(z$age>=65,rule$older_weight,1)
    wp <- z$person_days*tilt
    wd <- z$deaths*tilt
    wd <- ifelse(group_sum(wd)>0,wd,wp)
    add <- as.numeric(rule$restore)*unknown_deaths
    days <- as.numeric(rule$restore)*(unknown_days-(1-rule$undated_fraction)*undated_days)
    distribute <- function(amount,weights) {
      total <- group_sum(weights)
      stopifnot(all(total>0 | amount==0))
      extra <- amount*weights/ifelse(total>0,total,1)
      stopifnot(all(abs(group_sum(extra)-amount)<1e-5))
      extra
    }
    z$unflagged <- pmax(0,z$deaths-z$flagged)+distribute(add,wd)
    z$person_days <- z$person_days+distribute(days,wp)
    z[adult,]
  }

  # Step 22 odds-shift calibration, applied separately to each endpoint.
  # A zero-margin week stays at its original split. One odds multiplier per
  # endpoint and age band meets that band's target across all 200 weeks.
  move_deaths <- function(v,u,target) {
    available <- v>0 & u>0
    capacity <- sum(u[available])
    tol <- 1e-9*max(1,target)
    stopifnot(all(v>=0),all(u>=0),target>=0,target<=capacity+tol)
    move <- numeric(length(v))
    if (target==0) return(move)
    if (target>=capacity) {
      move[available] <- u[available]
    } else {
      log_odds <- log(v[available]/u[available])
      deaths <- v[available]+u[available]
      shift <- function(delta) deaths*plogis(log_odds+delta)-v[available]
      objective <- function(delta) sum(shift(delta))-target
      upper <- 1
      while (objective(upper)<0 && upper<1024) upper <- upper*2
      delta <- uniroot(objective,c(0,upper),tol=1e-13)$root
      move[available] <- pmin(u[available],pmax(0,shift(delta)))
    }
    stopifnot(abs(sum(move)-target)<tol,all(move<=u+tol))
    move
  }
  endpoints <- c("flagged","unflagged")
  delta_weeks <- sprintf("2021-W%02d",47:50)
  summaries <- list(); checks <- list(); audit <- list()
  for (i in seq_len(nrow(age_rules))) {
    ar <- age_rules[i,]
    z <- allocate_ages(ar)
    z <- z[order(z$age,z$week),]
    u <- z[z$status==arms[1],]; v <- z[z$status==arms[2],]
    stopifnot(identical(u$age,v$age),identical(u$week,v$week),identical(u$standard_n,v$standard_n))
    in_delta <- v$week %in% delta_weeks
    stopifnot(sum(in_delta)==4L*16L)
    for (j in seq_len(nrow(link_rules))) {
      lr <- link_rules[j,]
      moved <- v[endpoints]*0
      for (a in ages) {
        ix <- v$age==a
        original <- colSums(v[ix,endpoints])
        required <- sum(original)*lr$total_share/(1-lr$total_share)
        flagged_required <- unname(original["flagged"])*lr$flagged_share/(1-lr$flagged_share)
        targets <- c(flagged=flagged_required,unflagged=required-flagged_required)
        for (ep in endpoints) moved[ix,ep] <- move_deaths(v[[ep]][ix],u[[ep]][ix],targets[ep])
        shifted <- colSums(moved[ix,])
        flagged_total <- unname(original[1]+shifted[1])
        check <- data.frame(age_rule=ar$age_rule,link_rule=lr$link_rule,age=a,
                            flagged_moved=unname(shifted[1]),unflagged_moved=unname(shifted[2]),
                            flagged_share=if (flagged_total>0) unname(shifted[1])/flagged_total else NA_real_,
                            all_share=sum(shifted)/(sum(original)+sum(shifted)))
        stopifnot(abs(check$all_share-lr$total_share)<1e-9,(flagged_total==0 && shifted[1]==0) || abs(check$flagged_share-lr$flagged_share)<1e-9)
        checks[[length(checks)+1L]] <- check
      }
      U <- u; V <- v
      U[endpoints] <- u[endpoints]-moved; V[endpoints] <- v[endpoints]+moved
      stopifnot(all(U[endpoints]>= -1e-7),all(V[endpoints]>=0),
                max(abs(as.matrix(U[endpoints]+V[endpoints]-u[endpoints]-v[endpoints])))<1e-7)
      # Pool events and time for the period before direct ESP2013 standardization.
      rate <- function(g) {
        p <- aggregate(g[in_delta,c("person_days",endpoints)],g[in_delta,c("age","standard_n")],sum)
        stopifnot(nrow(p)==16L,all(p$person_days>0))
        w <- p$standard_n/sum(p$standard_n)
        vapply(endpoints,function(ep) sum(w*p[[ep]]/p$person_days)*365.25e5,numeric(1))
      }
      ru <- rate(U); rv <- rate(V); rr <- ru/rv
      summaries[[length(summaries)+1L]] <- data.frame(age_rule=ar$age_rule,link_rule=lr$link_rule,
        flagged_no_dose=unname(ru[1]),flagged_vaccinated=unname(rv[1]),flagged_ratio=unname(rr[1]),
        unflagged_no_dose=unname(ru[2]),unflagged_vaccinated=unname(rv[2]),unflagged_ratio=unname(rr[2]),
        ratio_of_ratios=unname(rr[1]/rr[2]))
      audit[[length(audit)+1L]] <- rbind(cbind(age_rule=ar$age_rule,link_rule=lr$link_rule,U[in_delta,]),
                                          cbind(age_rule=ar$age_rule,link_rule=lr$link_rule,V[in_delta,]))
    }
  }
  result <- do.call(rbind,summaries)
  dir.create(out_dir,recursive=TRUE,showWarnings=FALSE)
  output <- function(d,file) write.csv(d,file.path(out_dir,file),row.names=FALSE,na="")
  output(result,"delta_period_rates.csv")
  output(do.call(rbind,checks),"full_study_calibration.csv")
  cells <- do.call(rbind,audit)
  output(cells[c("age_rule","link_rule","week","age","standard_n","status","person_days",endpoints)],"delta_cells.csv")
  print(result[c("age_rule","link_rule","flagged_ratio","unflagged_ratio")],digits=5,row.names=FALSE)
  invisible(result)
}
file_args <- grep("^--file=",commandArgs(FALSE),value=TRUE)
loc <- if (length(file_args)) sub("^--file=","",file_args[1]) else
  unlist(lapply(sys.frames(),function(f) f$ofile),use.names=FALSE)
here <- if(length(loc)) dirname(normalizePath(tail(loc,1))) else getwd()
run_combined(file.path(here,"step24_inputs"),Sys.getenv("CZ_COMBINED_DELTA_OUT",file.path(here,"results")))
