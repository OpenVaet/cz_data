# Counting-model intervals per 100,000 person-years. These are marginal
# reference intervals for event variation given the selected allocation and time.
# Fay & Feuer (1997), https://wonder.cdc.gov/controller/pdf/FayFeuerConfidenceIntervals.pdf
# Supplying a single count and exposure gives the ordinary equal-tailed Poisson
# gamma limits; unrounded fractional allocations are a working approximation.
rate_interval <- function(deaths,person_days,weights=1) {
  stopifnot(length(deaths)>0, length(deaths)==length(weights),length(deaths)==length(person_days),
    all(is.finite(deaths) & deaths>=0), all(is.finite(person_days) & person_days>0),
    all(is.finite(weights) & weights>0), abs(sum(weights)-1)<1e-9)
  increment <- 365.25*1e5*weights/person_days
  rate <- sum(increment*deaths)
  variance <- sum(increment^2*deaths)
  # At zero deaths, the lower bound is zero; the upper increment still applies.
  lower <- if(rate==0) 0 else qgamma(.025,shape=rate^2/variance,scale=variance/rate)
  unit_max <- max(increment)
  upper_mean <- rate+unit_max
  upper_var <- variance+unit_max^2
  upper <- qgamma(.975,shape=upper_mean^2/upper_var,scale=upper_var/upper_mean)
  data.frame(deaths=sum(deaths),person_years=sum(person_days)/365.25,rate=rate,
             lower95=lower,upper95=upper,poisson_variance=variance)
}

# All numeric rounding occurs at the final display step.
publication_tables <- function(read_result,src) {
  num <- function(x,n=0) formatC(x,digits=n,format="f",big.mark=",")
  one <- function(x) {stopifnot(nrow(x)==1L); x}
  tab <- function(headers,rows,note,widths) list(headers=headers,rows=unname(as.matrix(rows)),note=note,widths=widths)
  ans <- list()
  b <- read_result(3,"annual_population_balance.csv")
  b <- b[match(2020:2023,b$year),]
  stopifnot(identical(b$year,2020:2023),all(b$opening+b$live_births-b$deaths+b$net_migration==b$closing))
  migration <- ifelse(b$net_migration>0,paste0("+",num(b$net_migration)),num(b$net_migration))
  ans$T01 <- tab(c("Year","Opening\nresidents","Births","Deaths","Net\nmigration","Closing\nresidents"),
    cbind(b$year,num(b$opening),num(b$live_births),num(b$deaths),migration,num(b$closing)),
    "ČSÚ annual resident population balance. Opening figures incorporate census revisions.",c(.11,.21,.14,.14,.16,.24))
  means <- read_result(8,"window_mean_weekly_rates.csv")
  means <- means[means$min_age==15,]
  rows <- list()
  for (period in c("Delta","Summer 2022")) for (arm in c("vaccinated","no_dose")) {
    z <- means[means$period==period & means$status==arm,]
    prime <- one(z[z$comparison=="primary",]); grouped <- one(z[z$comparison=="coarsen March",])
    stopifnot(prime$mean_november==grouped$mean_november,prime$comparable_weeks==prime$expected_weeks,
              grouped$comparable_weeks==grouped$expected_weeks)
    status <- if(arm=="vaccinated") "After first dose" else "No dose yet"
    rows[[length(rows)+1L]] <- c(paste(period,status,sep=" · "),num(c(prime$mean_march,grouped$mean_march,grouped$mean_november),1))
  }
  ans$T02 <- tab(c("Window and status","March\nsingle years","March\nfive-year bands","November\nfive-year bands"),
    do.call(rbind,rows),"Ages 15+. Arithmetic means of annualized weekly rates per 100,000 person-years.",c(.36,.19,.22,.23))
  totals <- read_result(9,"annual_pooled_rates.csv")
  cells <- read_result(9,"annual_age_contributions.csv")
  primary <- function(x) x[x$min_age==15 & ((x$release=="november" & x$spec=="primary") |
                            (x$release=="Eurostat" & x$spec=="national")),]
  totals <- primary(totals); cells <- primary(cells)
  freq <- c("no_dose","vaccinated","total","total")
  labels <- c("No dose yet","After first dose","November total","Resident total")
  releases <- c(rep("november",3),"Eurostat")
  summary_rows <- list()
  t03_limits <- list()
  t05_limits <- list()
  bounds <- function(ci) paste0(num(ci$lower95,1),"–",num(ci$upper95,1))
  for (i in 1:4) {
    years <- list()
    for (y in 2022:2023) {
      a <- one(totals[totals$status==freq[i] & totals$release==releases[i] & totals$year==y,])
      z <- cells[cells$status==freq[i] & cells$release==releases[i] & cells$year==y,]
      z <- z[order(z$age),]
      stopifnot(identical(z$age,seq.int(15L,90L,by=5L)), abs(sum(z$contribution)-a$asmr)<1e-6,
                abs(sum(z$deaths)-a$deaths)<1e-5, abs(sum(z$person_days)-a$person_days)<0.001)
      ci <- rate_interval(z$deaths,z$person_days,z$weight)
      stopifnot(abs(ci$rate-a$asmr)<1e-6)
      t03_limits[[length(t03_limits)+1L]] <- data.frame(table_id="T03",iso_year=y,source=a$release,
        age_group="15+",status=a$status,allocation=a$spec,method="Fay-Feuer gamma",ci)
      summary_rows[[length(summary_rows)+1L]] <- c(paste(y,labels[i],sep=" · "),num(a$deaths),
        num(a$person_days/365.25),num(a$asmr,1),bounds(ci))
      years[[as.character(y)]] <- rbind(z[c("deaths","person_days","contribution")],data.frame(deaths=a$deaths,person_days=a$person_days,contribution=a$asmr))
    }
    u <- years[["2022"]]; v <- years[["2023"]]
    pair <- function(x,y,d) paste(num(x,d),num(y,d),sep="  →  ")
    age_labels <- c(paste(seq(15,85,5),seq(19,89,5),sep="–"),"90+","All ages 15+")
    ans[[sprintf("T%02d",i+5L)]] <- tab(c("Age","Deaths","Person-years","ASMR contribution"),
      cbind(age_labels,pair(u$deaths,v$deaths,1),pair(u$person_days/365.25,v$person_days/365.25,0),pair(u$contribution,v$contribution,1)),
      "2022 → 2023. Contributions sum to the annual ASMR before rounding. Deaths per 100,000 standard person-years.",c(.17,.29,.30,.24))
  }
  ans$T03 <- tab(c("Year and population","Deaths","Person-years","ASMR","95% interval"),do.call(rbind,summary_rows),
    paste("Ages 15+. Annual ASMR per 100,000 person-years. November includes known birth bands.",
          "Approximate 95% intervals treat age-specific deaths as Poisson counts. Person-time and age allocation are held fixed.",sep="\n"),
    c(.30,.14,.20,.14,.22))
  ans$T03$limits <- do.call(rbind,t03_limits)
  groups <- c("15-24","25-49","50-59","60-69","70-79","80+")
  official <- read.csv(file.path(src,"csu_g02_u07.csv"))
  sources <- read.csv(file.path(src,"csu_g02_sources.csv"))
  all_ages <- c(0,1,seq(5,95,5))
  stopifnot(!anyDuplicated(official[c("year","sex","age_lower")]),all(official$u07_deaths>=0),
            all(table(official$year,official$sex)==length(all_ages)),setequal(unique(official$age_lower),all_ages))
  official$band <- cut(official$age_lower,breaks=c(0,15,25,50,60,70,80,Inf),right=FALSE,labels=c("0-14",groups))
  counts <- xtabs(u07_deaths~band+year,data=official)
  stopifnot(all(colSums(counts)==sources$all_age_deaths[match(colnames(counts),sources$year)]))
  ans$T04 <- tab(c("Age at death","2020","2021","2022","2023"),
    cbind(chartr("-","–",groups),apply(counts[groups,,drop=FALSE],2,num)),
    "ČSÚ Demographic Yearbooks, Table G.02. Underlying cause U07, both sexes, calendar years.",c(.28,.18,.18,.18,.18))
  annual <- read_result(15,"annual_original_vs_scenario.csv")
  fine <- read_result(15,"weekly_five_year_components.csv")
  fine <- fine[fine$iso_year==2023,]
  weeks <- sprintf("2023-W%02d",1:52); ages <- seq.int(15L,90L,by=5L)
  eligible <- cut(fine$age,breaks=c(15,25,50,60,70,80,Inf),labels=groups,right=FALSE)
  stopifnot(setequal(fine$week,weeks),setequal(fine$age,ages),nrow(fine)==length(ages)*length(weeks),
    !anyDuplicated(fine[c("week","age")]),identical(as.character(eligible),fine$age_group))
  # Retain signed original residuals during aggregation. Rates in T05 use the
  # pooled broad-band numerator and time, not a re-standardized age mix.
  fields <- c("no_dose_days","vaccinated_days","no_dose_deaths","vaccinated_deaths",
              "scenario_no_dose_deaths","scenario_vaccinated_deaths")
  pooled <- aggregate(fine[fields],list(age_group=fine$age_group),sum)
  rows <- list()
  for (g in groups) {
    z <- one(pooled[pooled$age_group==g,])
    a <- annual[annual$iso_year==2023 & annual$age_group==g,]
    stopifnot(nrow(a)==2L,setequal(a$status,c("no_dose","vaccinated")),
      abs(sum(a$original_deaths)-sum(a$scenario_deaths))<1e-7)
    display <- chartr("-","–",g)
    for (allocation in c("original","scenario")) for (arm in c("no_dose","vaccinated")) {
      t <- one(a[a$status==arm,])
      count_col <- paste0(if(allocation=="scenario") "scenario_" else "",arm,"_deaths")
      d <- z[[count_col]]; days <- z[[paste0(arm,"_days")]]
      ci <- rate_interval(d,days)
      stopifnot(abs(d-t[[paste0(allocation,"_deaths")]])<1e-6,abs(days-t$person_days)<.001,
                abs(ci$rate-t[[paste0(allocation,"_rate_per_100k_py")]])<1e-6)
      t05_limits[[length(t05_limits)+1L]] <- data.frame(table_id="T05",iso_year=2023,
        source="November/Eurostat",age_group=g,status=arm,allocation=allocation,method="Pooled gamma",ci)
      display <- c(display,paste0(num(ci$rate,1),"\n(",bounds(ci),")"))
    }
    rows[[length(rows)+1L]] <- display
  }
  ans$T05 <- tab(c("Age","Original\nNo dose","Original\nAt least one dose","15% scenario\nNo dose","15% scenario\nAt least one dose"),
    do.call(rbind,rows),paste("ISO 2023. Rate per 100,000 person-years (approximate 95% counting interval).",
      "Intervals treat allocated deaths as Poisson counts. Person-time, age assignments and the assumed linkage rule are held fixed.",sep="\n"),
    c(.12,.20,.23,.20,.25))
  ans$T05$limits <- do.call(rbind,t05_limits)
  ans
}

draw_table <- function(path,id,title,t) {
  # The generous cell widths preserve the largest appendix values at phone zoom.
  n <- nrow(t$rows); w <- 1820
  multiline <- any(grepl("\n",t$rows,fixed=TRUE))
  row_height <- if(multiline) 103 else 61
  note_lines <- strsplit(t$note,"\n",fixed=TRUE)[[1]]
  h <- 275+row_height*n+36*(length(note_lines)-1L)
  png(path,width=w,height=h,bg="white",res=135)
  on.exit(dev.off())
  grid::pushViewport(grid::viewport(xscale=c(0,w),yscale=c(h,0)))
  u <- function(v) grid::unit(v,"native")
  rect <- function(x,y,width,height,fill)
    grid::grid.rect(x=u(x),y=u(y),width=u(width),height=grid::unit(height/h,"npc"),just=c("left","top"),gp=grid::gpar(col=NA,fill=fill))
  label <- function(x,y,value,size=14,bold=FALSE,col="#152F3A",just="left")
    grid::grid.text(value,x=u(x),y=u(y),just=just,gp=grid::gpar(fontsize=size,fontfamily="sans",fontface=if(bold) "bold" else "plain",col=col))
  rect(0,0,w,76,"#183B49")
  label(43,38,paste("Table",id,"   |   ",title),19,TRUE,"white")
  header_y <- 116; first_y <- 184
  edges <- 40+(w-80)*c(0,cumsum(t$widths))
  rect(40,80,w-80,72,"#E9F1F2")
  for(j in seq_along(t$headers)) {
    x <- if(j==1L) edges[j]+15 else edges[j+1L]-15
    just <- if(j==1L) "left" else "right"
    label(x,header_y,t$headers[j],13,TRUE,just=just)
  }
  for(i in seq_len(n)) {
    y <- first_y+(i-1)*row_height+(row_height-61)/2
    if(i%%2==0 || (startsWith(id,"T0") && identical(t$rows[i,1],"All ages 15+")))
      rect(40,y-floor(row_height/2),w-80,row_height-1,if(t$rows[i,1]=="All ages 15+") "#D8E9EB" else "#F2F6F6")
    for(j in seq_len(ncol(t$rows))) {
      left <- j==1
      label(if(left) edges[j]+15 else edges[j+1]-15,y,t$rows[i,j],14,left || t$rows[i,1]=="All ages 15+",
            just=if(left) "left" else "right")
    }
  }
  footer_y <- first_y+n*row_height+21
  for (i in seq_along(note_lines))
    label(48,footer_y+(i-1L)*36,note_lines[i],12,FALSE,"#405A64")
  grid::popViewport()
}
draw_young_quarters <- function(path,q) {
  png(path,width=1680,height=1220,res=160,bg="white",pointsize=12); on.exit(dev.off())
  par(mfrow=c(2,2),mar=c(2.4,4.5,2.4,1),oma=c(2.8,.5,5.2,.5),las=1,family="sans",col.axis="#435A64",col.lab="#203C47")
  periods <- paste0(rep(2021:2023,each=4),"-Q",1:4)
  groups <- c("15-24","25-49"); cols <- c("#A64713","#096C91")
  for (g in groups) {
    d <- q[q$age_group==g & q$quarter %in% periods,]
    top <- max(d$original_rate_per_100k_py,d$scenario_rate_per_100k_py)
    ticks <- pretty(c(0,top),4)
    for (k in 1:2) {
      plot.new();plot.window(xlim=c(.75,12.25),ylim=range(ticks),xaxs="i",yaxs="i")
      abline(h=ticks,col="#D7E2E5");abline(v=c(4.5,8.5),lty=3,col="#ACBCC0")
      axis(1,at=c(2.5,6.5,10.5),labels=2021:2023,lwd=0);axis(2,at=ticks,lwd=0)
      title(main=if(k==1) "Original allocation" else "15% missed over the study",adj=0,cex.main=1.08)
      if(k==1) mtext(paste("Ages",chartr("-","–",g)),side=2,line=3,las=0,font=2)
      for (arm in 1:2) {
        z <- d[d$status==c("no_dose","vaccinated")[arm],]
        stopifnot(!anyDuplicated(z$quarter),setequal(z$quarter,periods))
        z <- z[match(periods,z$quarter),]
        y <- if(k==1) z$original_rate_per_100k_py else z$scenario_rate_per_100k_py
        lines(1:12,y,type="o",pch=c(16,15)[arm],col=cols[arm],lwd=2,cex=.75)
      }
    }
  }
  mtext("Czechia, 2021–2023 · Quarterly deaths per 100,000 person-years",side=3,outer=TRUE,line=3,adj=.02,font=2,cex=1.18)
  mtext("● No recorded dose",side=3,outer=TRUE,line=1.3,adj=.02,col=cols[1])
  mtext("■ At least one dose",side=3,outer=TRUE,line=1.3,adj=.38,col=cols[2])
  mtext("Each point pools deaths and person-time within a quarter.",side=1,outer=TRUE,line=.8,adj=.02,cex=.86)
}
