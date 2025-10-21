#!/usr/bin/env Rscript
# =====================================================================
# plot_esp_rates.R — Plot ESP-standardized mortality rates by vax status
# =====================================================================
# Usage examples:
#   Rscript scripts/plot_esp_rates.R \
#     --input outputs/weekly_esp_rates_summary.csv \
#     --output outputs/esp_rates_vax_vs_unvax.png
# Or directly from the full per-age CSV:
#   Rscript scripts/plot_esp_rates.R \
#     --input outputs/weekly_death_rates.csv \
#     --output outputs/esp_rates_vax_vs_unvax.png

suppressMessages(library(ggplot2))

iso_week_monday_date <- function(iso) {
  # Accepts "YYYY-WW" or "YYYYWW" or "YYYY-WWW"
  m <- regexec("^\\s*(\\d{4})[- ]?W?(\\d{1,2})\\s*$", as.character(iso))
  r <- regmatches(as.character(iso), m)[[1]]
  if (length(r) < 3) stop("Expect ISO 'YYYY-WW', got: ", iso)
  year <- as.integer(r[2]); week <- as.integer(r[3])
  jan4 <- as.Date(sprintf("%04d-01-04", year))
  iso_dow <- as.integer(strftime(jan4, "%u")) # 1..7, Mon=1
  week1_monday <- jan4 - (iso_dow - 1)
  week1_monday + (week - 1) * 7
}

# --------------------
# Parse simple CLI args
# --------------------
args <- commandArgs(trailingOnly = TRUE)
opt <- list(input = "outputs/weekly_esp_rates_summary.csv",
            output = "outputs/esp_rates_vax_vs_unvax.png",
            width = 10, height = 5, dpi = 160)
if (length(args)) {
  i <- 1
  while (i <= length(args)) {
    k <- args[i]
    v <- if (i + 1 <= length(args)) args[i + 1] else NA_character_
    if (k %in% c("--input", "-i"))  { opt$input <- v; i <- i + 2; next }
    if (k %in% c("--output","-o")) { opt$output <- v; i <- i + 2; next }
    if (k == "--width")  { opt$width  <- as.numeric(v); i <- i + 2; next }
    if (k == "--height") { opt$height <- as.numeric(v); i <- i + 2; next }
    if (k == "--dpi")    { opt$dpi    <- as.numeric(v); i <- i + 2; next }
    i <- i + 1
  }
}

if (!file.exists(opt$input)) stop("Input CSV not found: ", opt$input)

DF <- utils::read.csv(opt$input, stringsAsFactors = FALSE)
need <- c("year","week","esp_rate_unvaccinated","esp_rate_vaccinated")
if (!all(need %in% names(DF))) stop("Input must have columns: ", paste(need, collapse=", "))

# If input is per-age-group, aggregate by week
agg <- stats::aggregate(cbind(esp_rate_unvaccinated, esp_rate_vaccinated) ~ year + week, data = DF, sum)

# Make ISO-week Monday date
agg$date <- as.Date(mapply(function(y, w) iso_week_monday_date(sprintf("%04d-%02d", y, w)), agg$year, agg$week))
agg <- agg[order(agg$date), ]

plot_df <- rbind(
  data.frame(date = agg$date, rate = agg$esp_rate_unvaccinated, group = "Unvaccinated"),
  data.frame(date = agg$date, rate = agg$esp_rate_vaccinated,   group = "Vaccinated")
)

p <- ggplot(plot_df, aes(x = date, y = rate, color = group)) +
  geom_line(linewidth = 0.7) +
  labs(title = "ESP-standardized mortality rates by vaccination status",
       subtitle = "Weekly (ISO) — all eligible age groups combined",
       x = "Week (ISO, Monday)", y = "Deaths per 100k (ESP-standardized)",
       color = "Status") +
  theme_minimal()

p

dir.create(dirname(opt$output), recursive = TRUE, showWarnings = FALSE)

# Let ggsave infer device from extension (png/pdf/svg)
if (!grepl("\\.(png|pdf|svg)$", tolower(opt$output))) opt$output <- paste0(opt$output, ".png")

ggplot2::ggsave(filename = opt$output, plot = p, width = opt$width, height = opt$height, dpi = opt$dpi)
cat(sprintf("Saved plot to %s\n", opt$output))
