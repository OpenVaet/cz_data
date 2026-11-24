# Publication layout adapted from Step 14. Both panels receive the same axis ticks.
draw_age_panel <- function(g, wide, qr, tr, rticks, modelled) {
  arm_labels <- c(no_dose = "No recorded dose", vaccinated = "At least one dose")
  arm_ids <- names(arm_labels)
  quarter <- function(x) paste0(format(x, "%Y"), "-Q", (as.integer(format(x, "%m")) - 1L) %/% 3L + 1L)
  fit_start <- as.Date("2021-01-01")
  w <- wide[wide$age_group == g, ]; w <- w[order(w$week_mid), ]
  q <- qr[qr$age_group == g, ]; fit <- tr[tr$age_group == g, ]
  ink <- "#182B37"; muted <- "#485C66"; grid <- "#DBE3E7"; white <- "#FFFFFF"
  colors <- c(no_dose = "#A64713", vaccinated = "#096C91")
  shade <- "#E8E0F1"; shade_edge <- "#78618F"
  first <- "#177469"; later <- "#8EC7B9"
  par(mar = rep(0,4), xaxs = "i", yaxs = "i", family = "sans", lend = "round")
  plot.new(); plot.window(xlim = c(0,1), ylim = c(0,1))
  label <- function(x, y, s, size=.94, col=ink, adj=0, font=1)
    text(x,y,s,cex=size,col=col,adj=c(adj,.5),font=font)
  xleft <- .128; xright <- .943
  yrs <- range(as.integer(format(w$week_mid,"%Y")))
  jan <- as.Date(sprintf("%d-01-01", yrs + c(0,1)))
  starts <- seq(jan[1], jan[2], by = "3 months")
  X <- function(d) xleft + (xright-xleft)*as.numeric(as.Date(d)-jan[1])/as.numeric(diff(jan))
  mid <- (X(head(starts,-1))+X(tail(starts,-1)))/2
  periods <- quarter(head(starts,-1))
  term <- function(row, dates) {
    row$intercept_at_2021_01_01 + row$annual_change_in_rate_per_100k_py * as.numeric(dates-fit_start)/365.25
  }
  rlo <- min(rticks); rhi <- max(rticks)
  rb <- .465; rt <- .796
  Y <- function(v) rb + (rt-rb)*(v-rlo)/(rhi-rlo)
  # Titles, rate legend and slope readouts.
  label(.026,.961,paste0(if (modelled) "B   15% missed over the study" else "A   Original allocation", "     |     Ages ", g),2.20,font=2)
  label(.027,.918,"Czechia   /   November NZIP with Eurostat resident estimates   /   March 2020 to December 2023",1.14,col=muted)
  points(.032,.862,pch=16,cex=1.15,col=colors[1])
  label(.044,.862,arm_labels[1],1.02,col=colors[1],font=2)
  points(.182,.862,pch=16,cex=1.15,col=colors[2])
  label(.194,.862,arm_labels[2],1.02,col=colors[2],font=2)
  segments(.362,.862,.395,.862,col=ink,lwd=1.35)
  label(.403,.862,if (modelled) "Modelled" else "Estimated",.98)
  segments(.489,.862,.524,.862,lty="13",lwd=2.2,col=ink)
  label(.532,.862,"2015-2019 seasonal baseline",.98)
  segments(.737,.862,.775,.862,lty="64",lwd=2.1,col=ink)
  label(.783,.862,"Trend, Jan 2021 to Jul 2023",.98)
  label(.027,.815,"Mortality rate",.98,font=2)
  label(xleft,.815,"Deaths per 100,000 person-years",.88,col=muted)
  label(xright,.815,"Each baseline uses the group's age mix that week",.86,col=muted,adj=1)
  dates <- X(w$week_mid)
  quarter_lines <- X(starts)
  year_lines <- X(seq(jan[1],jan[2],by="year"))
  segments(quarter_lines,rb,quarter_lines,rt,col="#EDF1F2",lwd=.65)
  segments(year_lines,rb,year_lines,rt,col=grid,lwd=.9)
  segments(xleft,Y(rticks),xright,Y(rticks),col=adjustcolor(grid,alpha.f=.83),lwd=.7)
  label(xleft-.009,Y(rticks),format(rticks,big.mark=",",trim=TRUE),.88,col=muted,adj=1)
  for (arm in arm_ids)
    lines(dates,Y(w[[paste0(arm,"_seasonal_rate")]]),col=colors[arm],lwd=2.2,lty="13")
  for (arm in arm_ids) {
    lines(dates,Y(w[[paste0(arm,"_rate")]]),col=colors[arm],lwd=1.35)
  }
  for (arm in arm_ids) {
    row <- fit[fit$status == arm, ]; xs <- c(row$first_fitted_midweek,row$last_fitted_midweek)
    ys <- Y(term(row,xs))
    if (modelled) clip(xleft, xright, Y(0), rt)
    # White underlay keeps the straight fitted lines visible during waves.
    lines(X(xs),ys,col=adjustcolor(white,alpha.f=.75),lwd=3.0)
    lines(X(xs),ys,col=colors[arm],lwd=2.1,lty="64")
    if (modelled) clip(0,1,0,1)
  }
  if (any(w$negative_no_dose_deaths))
    points(dates[w$negative_no_dose_deaths],rep(Y(0),sum(w$negative_no_dose_deaths)),
           pch=4,col=colors[1],lwd=1.3,cex=.66)
  signed <- function(v, digits=0) {
    n <- abs(round(v,digits)); s <- formatC(n,format="f",digits=digits,big.mark=",")
    ifelse(n == 0,s,ifelse(v < 0,paste0("-",s),paste0("+",s)))
  }
  label(.027,.437,"Fitted change per year",1.0,font=2)
  label(.214,.437,paste0(arm_labels[1],"   ", signed(fit$annual_change_in_rate_per_100k_py[fit$status=="no_dose"],1)),
        1.02,col=colors[1],font=2)
  label(.475,.437,paste0(arm_labels[2],"   ", signed(fit$annual_change_in_rate_per_100k_py[fit$status=="vaccinated"],1)),
        1.02,col=colors[2],font=2)
  label(.735,.437,"Deaths per 100,000 person-years per calendar year",.86,col=muted)

  # Count and share use separate scales in the lower panel. Draw the
  # coverage fill behind the columns and its boundary above the columns.
  db <- .279; dt <- .374
  ticks <- pretty(c(0,max(w$all_doses)),n=3); dmax <- max(ticks,1)
  D <- function(v) db+(dt-db)*v/dmax
  C <- function(v) db+(dt-db)*v/100
  stopifnot(all(is.finite(w$vaccinated_pct) & w$vaccinated_pct >= 0 & w$vaccinated_pct <= 100))
  label(.027,.396,"Doses given",.98,font=2)
  rect(xleft,.388,xleft+.012,.404,col=first,border=NA); label(xleft+.018,.396,"First doses",.90)
  rect(xleft+.118,.388,xleft+.130,.404,col=later,border=NA); label(xleft+.136,.396,"Doses 2-7",.90)
  rect(xleft+.238,.388,xleft+.250,.404,col=shade,border=shade_edge)
  label(xleft+.256,.396,"Share with at least one dose (right axis)",.90,col=shade_edge)
  polygon(c(dates,rev(dates)),c(C(w$vaccinated_pct),rep(db,nrow(w))),col=shade,border=NA)
  segments(quarter_lines,db,quarter_lines,dt,col="#EDF1F2",lwd=.65)
  segments(xleft,D(ticks),xright,D(ticks),col=grid,lwd=.7)
  label(xleft-.009,D(ticks),ifelse(ticks>=1000,paste0(ticks/1000,"k"),as.character(ticks)),.82,col=muted,adj=1)
  xl <- X(w$week_mid-3L); xr <- X(w$week_mid+3L)
  rect(xl,db,xr,D(w$first_doses),col=first,border=NA)
  rect(xl,D(w$first_doses),xr,D(w$all_doses),col=later,border=NA)
  lines(dates,C(w$vaccinated_pct),col=shade_edge,lwd=1.35)
  label(xright+.007,C(c(0,50,100)),c("0%","50%","100%"),.82,col=shade_edge)

  # Columns below use exactly the same calendar scale as the two panels.
  present <- periods %in% q$quarter
  y <- c(.181,.152,.111,.082)
  label(.027,.246,"Quarterly estimates",1.05,font=2)
  for (yr in seq(yrs[1],yrs[2])) {
    sel <- startsWith(periods,as.character(yr))
    label(mean(mid[sel]),.249,as.character(yr),1.06,adj=.5,font=2)
    if ((yr-yrs[1]) %% 2L == 0L)
      rect(min(quarter_lines[which(sel)]),.067,max(quarter_lines[which(sel)+1L]),.231,col="#F1F5F5",border=NA)
  }
  segments(year_lines,.067,year_lines,.233,col=grid,lwd=.9)
  qlab <- paste0("Q",substr(periods,7,7))
  qlab[periods == "2020-Q1"] <- "Q1*"
  label(mid,.215,qlab,.86,col=muted,adj=.5,font=2)
  label(.027,y[1],"Mean size, no dose",.92,col=colors[1])
  label(.027,y[2],"Excess deaths",.92,col=colors[1])
  label(.027,y[3],"Mean size, 1+ dose",.92,col=colors[2])
  label(.027,y[4],"Excess deaths",.92,col=colors[2])
  segments(xleft,.132,xright,.132,col=grid,lwd=.8)
  size_text <- function(v) {
    ifelse(v>=1e6,paste0(formatC(v/1e6,digits=2,format="f"),"m"),
      ifelse(v>=1000,paste0(formatC(v/1000,digits=ifelse(v>=1e5,0,1),format="f"),"k"),
        ifelse(v>0 & v<1,"<1",formatC(round(v),format="f",digits=0))))
  }
  for (j in seq_along(arm_ids)) {
    s <- q[q$status==arm_ids[j], ]; ix <- match(periods[present],s$quarter)
    yy <- y[(2*j-1):(2*j)]
    label(mid[present],yy[1],vapply(s$mean_people[ix],size_text,""),.88,adj=.5)
    label(mid[present],yy[2],ifelse(s$has_exposure[ix],signed(s$excess_deaths[ix]),"--"),.89,
          col=colors[j],adj=.5,font=2)
  }
  foot <- "* 2020 Q1 covers four March weeks. Mean size = person-days / days covered. Excess = observed minus expected at 2015-2019 national rates for the same ISO week and five-year age group."
  if (modelled) foot <- sub("observed minus", "modelled minus", foot, fixed=TRUE)
  label(.027,.041,foot,.81,col=muted)
  note <- "Weeks follow their Thursday. Dashed fits are weighted by group person-time. No-dose deaths and person-time are estimated by subtraction. -- indicates no exposure."
  if (modelled) note <- "Weekly transfers conserve resident deaths and hold group person-time fixed. Dashed trends are weighted by person-time. -- indicates no exposure."
  if (any(w$negative_no_dose_deaths))
    note <- paste0(note,"  x = negative weekly residual.")
  label(.027,.018,note,.80,col=muted)
}

