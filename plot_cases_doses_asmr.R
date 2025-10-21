# ============================================================
# Overlay plot: Cases & Doses (left axis, 0..850,000)
#                ASMR (right axis, 0..1,350)
# Window: 2020-W10 to last ISO week of 2023
# ============================================================
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(lubridate)
  library(ISOweek)
  library(ggplot2)
  library(scales)
  library(tibble)
})

# ---- Inputs (produced earlier) ----------------------------------------------
f_asmr  <- "out/asmr_weekly_15plus_wide_imputed.csv"   # cols: iso_date, asmr_our_imputed
f_doses <- "out/weekly_covid_doses_wide.csv"           # cols: week_date, total_doses_mzcr
f_cases <- "out/weekly_covid_cases_wide.csv"           # cols: week_date, aggregated_daily

# ---- Load weekly series ------------------------------------------------------
asmr <- read_csv(f_asmr,  show_col_types = FALSE) %>%
  transmute(week_date = as.Date(iso_date), asmr_our_imputed)

doses <- read_csv(f_doses, show_col_types = FALSE) %>%
  transmute(week_date = as.Date(week_date), total_doses_mzcr)

cases <- read_csv(f_cases, show_col_types = FALSE) %>%
  transmute(week_date = as.Date(week_date), aggregated_daily)

# ---- Target ISO-week window --------------------------------------------------
start_date <- ISOweek::ISOweek2date("2020-W10-1")
# pick a date in the last ISO week of 2023, then convert to that Monday
end_week_str <- ISOweek::ISOweek(as.Date("2023-12-28"))
end_date     <- ISOweek::ISOweek2date(paste0(end_week_str, "-1"))

weeks_grid <- tibble(week_date = seq.Date(start_date, end_date, by = "week"))

# ---- Join to complete weekly grid (fill counts with 0; ASMR stays NA) -------
df <- weeks_grid %>%
  left_join(cases, by = "week_date") %>%
  left_join(doses, by = "week_date") %>%
  left_join(asmr,  by = "week_date") %>%
  mutate(
    aggregated_daily = replace_na(aggregated_daily, 0),
    total_doses_mzcr = replace_na(total_doses_mzcr, 0)
  ) %>%
  arrange(week_date)

asmr_peak <- max(df$asmr_our_imputed, na.rm = TRUE)
asmr_over <- sum(df$asmr_our_imputed > RIGHT_MAX, na.rm = TRUE)
nonfinite <- sum(!is.finite(df$asmr_our_imputed))

message(sprintf("ASMR peak = %.1f; RIGHT_MAX = %.1f; weeks > RIGHT_MAX = %d; non-finite = %d",
                asmr_peak, RIGHT_MAX, asmr_over, nonfinite))


stopifnot(nrow(df) > 0)

# ---- Axes + scaling ----------------------------------------------------------
LEFT_MAX  <- 850000   # Cases & Doses
RIGHT_MAX <- 1550     # ASMR
coef <- LEFT_MAX / RIGHT_MAX  # scale ASMR up to left-axis space

# ---- Cosmetics helpers -------------------------------------------------------
colors_map <- c(
  "Cases (daily agg.)" = "#1f77b4",
  "Doses (MZCR)"       = "#d62728",
  "ASMR 15+ (our)"     = "#D55E00"
)

year_lines <- tibble(x = seq.Date(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "year")) %>%
  mutate(x = floor_date(x, unit = "week", week_start = 1)) %>%
  filter(x >= start_date, x <= end_date)

# ---- Plot -------------------------------------------------------------------
p <- ggplot(df, aes(x = week_date)) +
  # subtle year separators
  geom_vline(data = year_lines, aes(xintercept = x),
             linetype = "dotted", color = "grey75", linewidth = 0.35) +
  # left-axis series (counts)
  geom_line(aes(y = aggregated_daily, color = "Cases (daily agg.)"), linewidth = 0.9) +
  geom_line(aes(y = total_doses_mzcr, color = "Doses (MZCR)"),       linewidth = 0.9, alpha = 0.9) +
  # right-axis series (scaled)
  geom_line(aes(y = asmr_our_imputed * coef, color = "ASMR 15+ (our)"),
            linewidth = 1.1) +
  scale_color_manual(values = colors_map, name = NULL) +
  scale_x_date(
    limits = c(start_date, end_date),
    date_breaks = "8 weeks", date_labels = "%G-W%V",
    expand = expansion(mult = c(0.003, 0.01))
  ) +
  scale_y_continuous(
    name = "Cases & Doses per week",
    limits = c(0, LEFT_MAX),
    labels = label_number(big.mark = " "),
    expand = expansion(mult = c(0, 0.02)),
    sec.axis = sec_axis(
      ~ . / coef,
      name  = "ASMR (per 100,000 per week)",
      breaks = seq(0, RIGHT_MAX, by = 250),
      labels = label_number(accuracy = 1)
    )
  ) +
  labs(
    title = "Weekly COVID — Cases & Doses (left) vs ASMR (right) — Czechia",
    subtitle = "Window: 2020-W10 to last ISO week of 2023 | ASMR scaled linearly to the right axis",
    x = "ISO week (Monday)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title   = element_text(face = "bold"),
    legend.position = "top",
    legend.box       = "horizontal",
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.25, color = "grey88"),
    panel.grid.major.y = element_line(linewidth = 0.25, color = "grey88")
  )

print(p)

# ---- Save -------------------------------------------------------------------
dir.create("visual/tri_overlay", recursive = TRUE, showWarnings = FALSE)
ggsave("visual/tri_overlay/mzcr_cases_doses_asmr_dual_axis_2020W10_2023.png", p,
       width = 12.5, height = 6.8, dpi = 300, bg = "white")

message(
  "Rows: ", nrow(df),
  " | Range: ", format(min(df$week_date), "%Y-%m-%d"),
  " → ", format(max(df$week_date), "%Y-%m-%d"),
  " | Left max: ", LEFT_MAX,
  " | Right max: ", RIGHT_MAX
)

# ============================================================
# Print details on weeks with missing data (BEFORE filling NAs)
# Requires: weeks_grid, cases, doses, asmr
# ============================================================

# Join onto the complete weekly grid without filling yet
df_raw <- weeks_grid %>%
  left_join(cases, by = "week_date") %>%
  left_join(doses, by = "week_date") %>%
  left_join(asmr,  by = "week_date") %>%
  arrange(week_date)

miss_tbl <- df_raw %>%
  mutate(
    iso_week       = strftime(week_date, "%G-W%V"),
    missing_cases  = is.na(aggregated_daily),
    missing_doses  = is.na(total_doses_mzcr),
    missing_asmr   = is.na(asmr_our_imputed)
  )

tot_weeks <- nrow(miss_tbl)

cat("\n=== Missing-data summary over window ", format(min(miss_tbl$week_date), "%Y-%m-%d"),
    " → ", format(max(miss_tbl$week_date), "%Y-%m-%d"), " (", tot_weeks, " weeks) ===\n", sep = "")

sum_cases <- sum(miss_tbl$missing_cases)
sum_doses <- sum(miss_tbl$missing_doses)
sum_asmr  <- sum(miss_tbl$missing_asmr)

cat("Cases missing: ", sum_cases, " (", round(100*sum_cases/tot_weeks, 1), "%)\n", sep = "")
cat("Doses missing: ", sum_doses, " (", round(100*sum_doses/tot_weeks, 1), "%)\n", sep = "")
cat("ASMR  missing: ", sum_asmr,  " (", round(100*sum_asmr /tot_weeks, 1), "%)\n", sep = "")

# Union of any missing
union_missing <- miss_tbl %>%
  filter(missing_cases | missing_doses | missing_asmr) %>%
  select(week_date, iso_week,
         aggregated_daily, total_doses_mzcr, asmr_our_imputed,
         missing_cases, missing_doses, missing_asmr)

cat("\n-- Weeks with ANY missing field (", nrow(union_missing), " rows) --\n", sep = "")
print(head(union_missing, 20))  # print first 20; change as needed

# ---- Build weekly TESTS to compute positivity -------------------------------
daily_file_path <- "data/nakazeni_vyleceni_umrti_testy.csv"
daily_url       <- "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakazeni-vyleceni-umrti-testy.csv"
daily_header    <- paste0(
  'date,',
  'cumulativeInfected,',
  'cumulativeRecovered,',
  'cumulativeDeaths,',
  'cumulativePcrTests,',
  'cumulativeAntigenTests,',
  'dailyInfected,',
  'dailyRecovered,',
  'dailyDeaths,',
  'dailyPcrTests,',
  'dailyAntigenTests,',
  'cumulativePrimaryInfections,',
  'cumulativeReinfections,',
  'dailyPrimaryInfections,',
  'dailyReinfections'
)

to_num <- function(x) suppressWarnings(as.numeric(x))

if (!file.exists(daily_file_path)) {
  options(timeout = 1200)
  download.file(daily_url, daily_file_path, mode = "wb")
  dl <- readLines(daily_file_path, warn = FALSE, encoding = "UTF-8")
  stopifnot(length(dl) > 0)
  dl[1] <- daily_header
  writeLines(dl, daily_file_path, useBytes = TRUE)
}

df_daily <- read.csv(daily_file_path, header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(
    date = lubridate::ymd(date),
    pcr  = to_num(dailyPcrTests),
    ag   = to_num(dailyAntigenTests),
    pri  = to_num(dailyPrimaryInfections),
    rei  = to_num(dailyReinfections),
    inf  = to_num(dailyInfected)
  )

sum_pri_rei <- ifelse(is.na(df_daily$pri) & is.na(df_daily$rei),
                      NA_real_,
                      rowSums(cbind(df_daily$pri, df_daily$rei), na.rm = TRUE))
daily_total_cases <- ifelse(is.na(sum_pri_rei), df_daily$inf, sum_pri_rei)
daily_total_tests <- rowSums(cbind(df_daily$pcr, df_daily$ag), na.rm = TRUE)

weekly_tests <- df_daily %>%
  transmute(date,
            tests = daily_total_tests,
            cases = daily_total_cases) %>%
  mutate(week_date = floor_date(date, unit = "week", week_start = 1)) %>%
  group_by(week_date) %>%
  summarise(
    weekly_tests = sum(tests, na.rm = TRUE),
    weekly_cases_official = sum(cases, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Join tests into master weekly grid & compute positivity ----------------
df <- df %>%                       # <- your earlier df (weeks_grid + cases + doses + asmr)
  left_join(weekly_tests, by = "week_date") %>%
  mutate(
    weekly_tests   = replace_na(weekly_tests, 0),
    # positivity from our weekly cases (aggregated_daily) divided by official weekly tests
    positivity_pct = if_else(weekly_tests > 0, 100 * aggregated_daily / weekly_tests, NA_real_)
  )

stopifnot(nrow(df) > 0)

# ---- Axes + scaling ----------------------------------------------------------
LEFT_MAX  <- 850000   # Cases & Doses (columns + cases line)
RIGHT_MAX <- 1550     # ASMR (right axis)
coef <- LEFT_MAX / RIGHT_MAX          # ASMR -> left-space
pos_coef <- LEFT_MAX / 100            # Positivity 0..100 -> left-space

# Diagnostics AFTER limits are defined
asmr_peak <- max(df$asmr_our_imputed, na.rm = TRUE)
asmr_over <- sum(df$asmr_our_imputed > RIGHT_MAX, na.rm = TRUE)
nonfinite <- sum(!is.finite(df$asmr_our_imputed))
message(sprintf("ASMR peak = %.1f; RIGHT_MAX = %.1f; weeks > RIGHT_MAX = %d; non-finite = %d",
                asmr_peak, RIGHT_MAX, asmr_over, nonfinite))

# ---- Cosmetics helpers -------------------------------------------------------
colors_map <- c(
  "Cases (daily agg.)" = "#1f77b4",
  "ASMR 15+ (our)"     = "#D55E00",
  "Positivity (0–100%)"= "black"
)
fill_map <- c("Doses (MZCR)" = "#d62728")

year_lines <- tibble(x = seq.Date(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "year")) %>%
  mutate(x = floor_date(x, unit = "week", week_start = 1)) %>%
  filter(x >= start_date, x <= end_date)

# ---- Plot (doses = columns, cases = dashed line, positivity = long-dash) ----
p <- ggplot(df, aes(x = week_date)) +
  # year separators
  geom_vline(data = year_lines, aes(xintercept = x),
             linetype = "dotted", color = "grey75", linewidth = 0.35) +
  # doses as columns (left axis)
  geom_col(aes(y = total_doses_mzcr, fill = "Doses (MZCR)"), alpha = 0.45, width = 6) +
  # cases (left axis, dashed)
  geom_line(aes(y = aggregated_daily, color = "Cases (daily agg.)"),
            linewidth = 0.9, linetype = "dashed") +
  # positivity (scaled to left space; long-dash black)
  geom_line(aes(y = positivity_pct * pos_coef, color = "Positivity (0–100%)"),
            linewidth = 0.9, linetype = "longdash", alpha = 0.9, na.rm = TRUE) +
  # ASMR (right axis; scaled to left space)
  geom_line(aes(y = asmr_our_imputed * coef, color = "ASMR 15+ (our)"),
            linewidth = 1.1, na.rm = TRUE) +
  scale_fill_manual(values = fill_map, name = NULL) +
  scale_color_manual(values = colors_map, name = NULL) +
  scale_x_date(
    limits = c(start_date, end_date),
    date_breaks = "8 weeks", date_labels = "%G-W%V",
    expand = expansion(mult = c(0.003, 0.01))
  ) +
  scale_y_continuous(
    name = "Cases & Doses per week",
    limits = c(0, LEFT_MAX),
    labels = label_number(big.mark = " "),
    expand = expansion(mult = c(0, 0.02)),
    sec.axis = sec_axis(
      ~ . / coef,
      name  = "ASMR (per 100,000 per week)",
      breaks = seq(0, RIGHT_MAX, by = 250),
      labels = label_number(accuracy = 1)
    )
  ) +
  labs(
    title = "Weekly COVID — Doses (columns), Cases (dashed, left) & ASMR (right)",
    subtitle = "Positivity shown as long-dash black line on its own 0–100% scale (internally scaled; no axis). Window: 2020-W10 to last ISO week of 2023",
    x = "ISO week (Monday)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title   = element_text(face = "bold"),
    legend.position = "top",
    legend.box       = "horizontal",
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.25, color = "grey88"),
    panel.grid.major.y = element_line(linewidth = 0.25, color = "grey88")
  ) +
  guides(
    color = guide_legend(order = 1),
    fill  = guide_legend(order = 0)
  )

print(p)

# ---- Save -------------------------------------------------------------------
dir.create("visual/tri_overlay", recursive = TRUE, showWarnings = FALSE)
ggsave("visual/tri_overlay/mzcr_doses_columns_cases_dashed_asmr_right_pos_longdash.png", p,
       width = 12.5, height = 6.8, dpi = 300, bg = "white")
