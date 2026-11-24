# Reproduce the 26 smoothed weekly points of the reviewer's Czech three-panel chart.
# Rscript reviewer_weekly_ratios.R
# Set CZ_REVIEWER_BUCKETS to use a local copy of nzipbuckets.csv.xz.
path <- Sys.getenv("CZ_REVIEWER_BUCKETS")
if (!nzchar(path)) {
  path <- tempfile(fileext=".csv.xz")
  download.file("https://sars2.net/f/nzipbuckets.csv.xz", path, mode="wb")
}
con <- xzfile(path, "rt")
x <- read.csv(con,colClasses=c("integer","NULL","integer","NULL","integer","integer","integer","integer"))
close(con)
x$born <- pmin(1990L,pmax(1925L,x$born))
x$dose <- pmin(1L,x$dose)
z <- aggregate(x[c("pop","dead","coviddead")],x[c("born","dose","obsweek")],sum)
weights <- aggregate(pop~born,subset(z,obsweek==106),sum)
weights$w <- weights$pop/sum(weights$pop)
grid <- expand.grid(obsweek=seq_len(max(z$obsweek)),born=weights$born,dose=0:1)
z <- merge(grid,z,by=c("born","dose","obsweek"),all.x=TRUE)
z[is.na(z)] <- 0
z <- merge(z,weights[c("born","w")],by="born")
z <- z[order(z$born,z$dose,z$obsweek),]
z$other <- z$dead-z$coviddead
date <- as.Date("2020-03-05")+(z$obsweek-1L)*7
# Only interior weeks are reported; every moving window has five complete weeks.
smooth5 <- function(s) as.numeric(stats::filter(s,rep(.2,5),sides=2))
for (k in c("pop","coviddead","other")) z[[k]] <- ave(z[[k]],z$born,z$dose,FUN=smooth5)
z$date <- date
z <- subset(z,date>=as.Date("2021-07-05") & date<=as.Date("2022-01-02"))
stopifnot(all(z$pop>0))
z$flagged <- z$w*z$coviddead/z$pop
z$remaining <- z$w*z$other/z$pop
rates <- aggregate(z[c("flagged","remaining")],z[c("date","dose")],sum)
u <- subset(rates,dose==0)
v <- subset(rates,dose==1)
stopifnot(identical(u$date,v$date),nrow(u)==26L)
o <- data.frame(date=u$date,flagged_ratio=u$flagged/v$flagged,remaining_ratio=u$remaining/v$remaining)
o$ratio_of_ratios <- o$flagged_ratio/o$remaining_ratio
out <- Sys.getenv("CZ_SURVEILLANCE_OUT",file.path("data","analysis","21_surveillance_check"))
dir.create(out,recursive=TRUE,showWarnings=FALSE)
write.csv(o,file.path(out,"reviewer_weekly_ratios.csv"),row.names=FALSE)
print(vapply(o[-1],median,0.0))
