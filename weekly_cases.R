suppressPackageStartupMessages({
  library(dplyr)
  library(knitr)
  library(kableExtra)
  library(ggplot2)
  library(ISOweek)
  library(tidyr)
  library(lubridate)
  library(scales)
  library(RColorBrewer)
  library(htmltools)
  library(tibble)
  library(readr)
})

# -------------------------------------------------------------------
# Paths & URLs
# -------------------------------------------------------------------
covid_file <- "data/nakazeni_reinfekce_tyden.csv"
covid_url  <- "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakazeni-reinfekce-tyden.csv"

# New header
covid_new_header <- paste0(
  '"id","week_from","week_to","week","total_new_cases","primary_infections","first_reinfections","second_reinfections","third_and_subsequent_reinfections"'
)


dir.create("data", showWarnings = FALSE, recursive = TRUE)

# -------------------------------------------------------------------
# Ensure the COVID cases data file is present (download if missing) + replace header
# -------------------------------------------------------------------
if (!file.exists(covid_file)) {
  options(timeout = 1200)
  download.file(covid_url, covid_file, mode = "wb")
  cat("Covid data file downloaded from source.\n")

  # Replace the first line with the provided header
  covid_lines <- readLines(covid_file, warn = FALSE, encoding = "UTF-8")
  if (length(covid_lines) > 0) {
    covid_lines[1] <- covid_new_header
    writeLines(covid_lines, covid_file, useBytes = TRUE)
    cat("Covid data file header replaced.\n")
  } else {
    stop("Error on download; cannot replace header.")
  }
} else {
  cat("Covid data file found locally.\n")
}

# -------------------------------------------------------------------
# Read the CZ CSV into a dataframe
# -------------------------------------------------------------------
covid <- read.csv(covid_file, header = TRUE, stringsAsFactors = FALSE)

print(covid)

to_iso_monday_flex <- function(x) {
  x0 <- ifelse(is.na(x), NA_character_, trimws(x))
  x0[!nzchar(x0)] <- NA_character_
  x1 <- toupper(x0)
  rx <- "^\\s*(\\d{4})[-\\s]?W?(\\d{1,2})(?:[-\\s]?([1-7]))?\\s*$"
  ok <- grepl(rx, x1)
  out <- rep(NA_character_, length(x1))
  if (any(ok)) {
    m <- regmatches(x1, regexec(rx, x1))
    for (i in which(ok)) {
      p  <- m[[i]]
      yr <- suppressWarnings(as.integer(p[2]))
      wk <- suppressWarnings(as.integer(p[3]))
      d  <- if (length(p) >= 4 && nchar(p[4])) suppressWarnings(as.integer(p[4])) else 1L
      if (!is.na(yr) && !is.na(wk) && wk >= 1L && wk <= 53L && d >= 1L && d <= 7L) {
        out[i] <- sprintf("%04d-W%02d-%d", yr, wk, d)
      }
    }
  }
  ISOweek::ISOweek2date(out)
}

covid <- covid %>%
  mutate(
    total_new_cases = as.numeric(total_new_cases),
    week = as.character(week),
    week_date = to_iso_monday_flex(week)
  ) %>%
  arrange(week_date)

ggplot(covid, aes(x = week_date, y = total_new_cases)) +
  geom_col() +
  scale_x_date(
    breaks = seq(min(covid$week_date, na.rm = TRUE),
                 max(covid$week_date, na.rm = TRUE),
                 by = "8 weeks"),
    labels = function(x) strftime(x, "%G-%V")
  ) +
  scale_y_continuous(labels = scales::label_number(big.mark = " ")) +
  labs(
    title = "Czechia — Weekly COVID-19 Cases",
    x = "ISO Week (YYYY-WW)",
    y = "Total new cases"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())


