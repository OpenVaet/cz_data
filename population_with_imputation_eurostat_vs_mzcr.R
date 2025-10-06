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
})

# -------------------------------------------------------------------
# Paths & URLs
# -------------------------------------------------------------------
eu_file <- "data/demo_pjan_linear_2_0.csv"
eu_url  <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/demo_pjan/1.0?compress=false&format=csvdata&formatVersion=2.0&lang=en&labels=name"

dir.create("data", showWarnings = FALSE, recursive = TRUE)

# ===================================================================
# Ensures Eurostat data file is present (download if missing)
# ===================================================================
if (!file.exists(eu_file)) {
  options(timeout = 1200)
  download.file(eu_url, eu_file, mode = "wb")
  cat("Eurostat yearly population by single ages file downloaded.\n")
} else {
  cat("Eurostat yearly population by single ages file found locally.\n")
}

# Read
eu <- read.csv(eu_file, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

print(eu)

# The defaultview is WEEKLY and uses colon-labelled headers; normalize them
eu2 <- eu %>%
  rename(
    time      = `TIME_PERIOD`,
    value     = `OBS_VALUE`
  ) %>%
  mutate(
    geo_code = sub(":.*$", "", geo),
    geo_name = sub("^.*:\\s*", "", geo)
  ) %>%
  filter(geo_code == "CZ") %>%                             # Czechia only
  mutate(value = suppressWarnings(as.numeric(value))) %>%  # numeric deaths
  filter(!is.na(value)) %>%
  filter(age != "TOTAL")                              # <-- remove Total age

print(eu2)

eu3 <- eu2 %>%
  dplyr::filter(time == 2024, age != "UNK") %>%
  dplyr::transmute(
    # pull the code token regardless of colon order, e.g. "Less than 1 year: Y_LT1" or "Y1: 1 year"
    age_code = stringr::str_extract(age, "Y_LT1|Y_GE\\d+|Y\\d+"),
    sex      = sub(":.*$", "", sex),   # T / M / F
    time     = as.integer(time),
    value    = as.numeric(value)
  ) %>%
  dplyr::mutate(
    age = dplyr::case_when(
      age_code == "Y_LT1" ~ 0L,
      grepl("^Y_GE\\d+$", age_code) ~ as.integer(sub("^Y_GE", "", age_code)),
      grepl("^Y\\d+$", age_code)    ~ as.integer(sub("^Y",    "", age_code)),
      TRUE ~ NA_integer_
    )
  ) %>%
  dplyr::filter(!is.na(age)) %>%
  dplyr::select(age, sex, time, value)

# quick check
dplyr::glimpse(eu3)
head(eu3)

# -------------------------------------------------------------------
# Path
# -------------------------------------------------------------------
file_path <- "data/mzcr_no_or_first_infection_with_imputation.csv"

# -------------------------------------------------------------------
# Ensure the CZ data file is present
# -------------------------------------------------------------------
if (!file.exists(file_path)) {
  stop("Error; run 'download_and_data_integrity.R' first.")
} else {
  cat("CZ enhanced data file found locally.\n")
}

# -------------------------------------------------------------------
# Read the CZ CSV into a dataframe
# -------------------------------------------------------------------
df <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)

# -------------------------------------------------------------------
# Handle YearOfBirth end range and apply filtering
# -------------------------------------------------------------------
df_aug <- df %>%
  mutate(
    year_of_birth_end = suppressWarnings(as.integer(sub(".*-", "", YearOfBirth)))
  )
print(df_aug)

# =========================
# Compare df vs eu3 (2024)
# =========================

# Helper: 5-year group label
mk_group5 <- function(a) {
  gs <- (a %/% 5) * 5
  ge <- gs + 4L
  sprintf("%02d–%02d", gs, ge)
}

# --- Eurostat 2024 aggregated to 5-year age groups ---
max_eu_age <- max(eu3$age, na.rm = TRUE)

eu_2024_5 <- eu3 %>%
  # compute total-by-age (prefer T if present, else M+F)
  group_by(age) %>%
  summarise(
    pop_total = if (any(sex == "T")) sum(value[sex == "T"], na.rm = TRUE)
                else sum(value[sex %in% c("M","F")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(bin_start = (age %/% 5) * 5) %>%
  group_by(bin_start) %>%
  summarise(`EuroStat 2024` = sum(pop_total, na.rm = TRUE), .groups = "drop") %>%
  mutate(`Age Group` = sprintf("%02d–%02d", bin_start, bin_start + 4L))

# --- Safe ISO-week → Date converter (returns NA for junk) -------------------
to_iso_monday_safe <- function(x) {
  x0 <- ifelse(is.na(x), NA_character_, trimws(x))
  x0[!nzchar(x0)] <- NA_character_
  x1 <- toupper(x0)

  rx <- "^\\s*(\\d{4})(?:-?W|\\s*W)?(\\d{1,2})(?:[-\\s]?([1-7]))?\\s*$"
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
  ISOweek::ISOweek2date(out)  # NA-safe
}

# --- Keep only people alive on 2024-01-01 -----------------------------------
df_alive_2024 <- df_aug %>%
  mutate(death_week_date = to_iso_monday_safe(DateOfDeathInHealthcareFacility)) %>%
  filter(is.na(death_week_date) | death_week_date >= as.Date("2024-01-01")) %>%
  select(-death_week_date)

# =========================
# Compare df_alive_2024 vs eu3 (2024)
# =========================

# (Eurostat 2024 already aggregated earlier into eu_2024_5:
#  columns: bin_start, `Age Group`, `EuroStat 2024`)

max_eu_age <- max(eu3$age, na.rm = TRUE)
REF_YEAR   <- 2024L

# --- Split: known YOB ranges (for binning) vs unknown YOB (to keep totals) ---
df_known_yob <- df_alive_2024 %>%
  filter(!is.na(year_of_birth_start), !is.na(year_of_birth_end))

df_unknown_yob <- df_alive_2024 %>%
  filter(is.na(year_of_birth_start) | is.na(year_of_birth_end))

unknown_total <- nrow(df_unknown_yob)

# --- Compute possible ages for known YOB ranges on 2024-01-01 ----------------
mzcr_age_ranges <- df_known_yob %>%
  transmute(
    min_age = pmax(0L, REF_YEAR - year_of_birth_end - 1L),  # birthday may be after Jan 1
    max_age = pmax(0L, REF_YEAR - year_of_birth_start)      # birthday may be Jan 1 or earlier
  ) %>%
  mutate(
    min_age = pmin(min_age, max_eu_age),
    max_age = pmin(max_age, max_eu_age)
  ) %>%
  filter(min_age <= max_age)

# --- Map each record to a 5y age group by min_age and by max_age (counts) ----
mzcr_min_counts <- mzcr_age_ranges %>%
  mutate(bin_start = (min_age %/% 5) * 5) %>%
  count(bin_start, name = "MZCR Min. Age") %>%
  mutate(`Age Group` = sprintf("%02d–%02d", bin_start, bin_start + 4L))

mzcr_max_counts <- mzcr_age_ranges %>%
  mutate(bin_start = (max_age %/% 5) * 5) %>%
  count(bin_start, name = "MZCR Max. Age") %>%
  mutate(`Age Group` = sprintf("%02d–%02d", bin_start, bin_start + 4L))

# --- Join with Eurostat and add an "Unknown" row (YearOfBirth NA) ------------
out_tbl <- full_join(mzcr_min_counts, mzcr_max_counts, by = c("bin_start","Age Group")) %>%
  full_join(eu_2024_5, by = c("bin_start","Age Group")) %>%
  arrange(bin_start) %>%
  select(`Age Group`, `MZCR Min. Age`, `MZCR Max. Age`, `EuroStat 2024`) %>%
  tidyr::replace_na(list(`MZCR Min. Age` = 0L, `MZCR Max. Age` = 0L, `EuroStat 2024` = 0))

# append Unknown totals (no Eurostat counterpart)
unknown_row <- tibble(
  `Age Group`      = "Unknown",
  `MZCR Min. Age`  = unknown_total,
  `MZCR Max. Age`  = unknown_total,
  `EuroStat 2024`  = NA_real_
)

out_tbl <- bind_rows(out_tbl, unknown_row)

# --- Add differences to the table -------------------------------------------
out_tbl <- out_tbl %>%
  mutate(
    `Diff (Min - EU)` = `MZCR Min. Age` - `EuroStat 2024`,
    `Diff (Max - EU)` = `MZCR Max. Age` - `EuroStat 2024`
  )

# Keep a print-friendly version (hide bin_start if it exists)
out_tbl_print <- out_tbl %>%
  select(-dplyr::any_of("bin_start"))

knitr::kable(
  out_tbl_print,
  caption = "Czechia — MZCR min/max vs Eurostat 2024 (5-year groups) with differences"
) %>%
  kableExtra::kable_styling(full_width = FALSE)



# Exclude "Unknown" for plotting; order by bin_start if available
plot_df <- out_tbl %>%
  dplyr::filter(`Age Group` != "Unknown", !is.na(`EuroStat 2024`)) %>%
  { if ("bin_start" %in% names(.)) dplyr::arrange(., bin_start) else . } %>%
  dplyr::mutate(
    `Age Group` = factor(`Age Group`, levels = unique(`Age Group`)),
    diff_min = `Diff (Min - EU)`,
    label_col = ifelse(diff_min >= 0, "#007a1f", "black"),
    label_txt = scales::comma(diff_min),
    label_y   = pmax(`MZCR Min. Age`, `EuroStat 2024`, 1) * 1.05
  )

# Long form for side-by-side bars
bars_df <- plot_df %>%
  dplyr::select(`Age Group`, `MZCR Min. Age`, `EuroStat 2024`) %>%
  tidyr::pivot_longer(
    cols = c(`MZCR Min. Age`, `EuroStat 2024`),
    names_to = "series", values_to = "count"
  )

# Ensure legend/order is MZCR first, Eurostat second
bars_df <- bars_df %>%
  dplyr::mutate(series = factor(series, levels = c("MZCR Min. Age", "EuroStat 2024")))

# Plot
p_comp <- ggplot(bars_df, aes(x = `Age Group`, y = count, fill = series)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_text(
    data = plot_df,
    aes(x = `Age Group`, y = label_y, label = label_txt, color = label_col),
    inherit.aes = FALSE, size = 3.6, fontface = "bold"
  ) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si(""))) +
  scale_fill_manual(
    values = c(
      "MZCR Min. Age" = "#2B6CB0",  # soft dark blue
      "EuroStat 2024" = "#9B2C2C"   # soft dark red
    ),
    name = NULL
  ) +
  scale_color_identity() +
  labs(
    title = "Czechia - MZCR (Min Age) vs Eurostat 2024 by 5-year group",
    subtitle = "Label = MZCR Min. Age - Eurostat 2024 (green = positive, red = negative)",
    x = NULL, y = "Population"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

print(p_comp)

# Optional save
dir.create("visual/compare_min_vs_eurostat", showWarnings = FALSE, recursive = TRUE)
ggsave("visual/compare_min_vs_eurostat/mzcr_min_vs_eurostat_2024.png",
       p_comp, width = 12, height = 6, dpi = 300, bg = "white")


# --- Sum of offsets (same ones shown as labels in the plot) ------------------
total_offset <- sum(plot_df$diff_min, na.rm = TRUE)

cat(
  "Sum of offsets (MZCR Min − Eurostat) across plotted age groups: ",
  ifelse(total_offset >= 0, "+", ""),
  scales::comma(total_offset),
  "\n",
  sep = ""
)

# (Optional) show it in the plot subtitle too
p_comp <- p_comp +
  labs(
    subtitle = paste0(
      "Label = MZCR Min. Age - Eurostat 2024 (green offsets = positive, black = negative). ",
      "Total offset: ",
      ifelse(total_offset >= 0, "+", ""),
      scales::comma(total_offset)
    )
  )

print(p_comp)

# =========================
# Sex-specific comparison with custom age groups
# Bands: 0-14, 15-29, 30-49, 50-64, 65+
# =========================

# --- Define custom bands -----------------------------------------------------
bands_breaks  <- c(-1, 14, 29, 49, 64, Inf)   # -1 to include age 0
bands_labels  <- c("0-14", "15-29", "30-49", "50-64", "65+")
bands_levels  <- bands_labels                 # enforce axis order

# --- Eurostat 2024 aggregated to custom bands by sex ------------------------
eu_2024_bins_sex <- eu3 %>%
  dplyr::filter(sex %in% c("M","F")) %>%
  dplyr::mutate(
    age_group = cut(age, breaks = bands_breaks, labels = bands_labels, right = TRUE)
  ) %>%
  dplyr::group_by(sex, age_group) %>%
  dplyr::summarise(`EuroStat 2024` = sum(value, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(`Age Group` = factor(as.character(age_group), levels = bands_levels)) %>%
  dplyr::select(sex, `Age Group`, `EuroStat 2024`)

# --- MZCR: alive on 2024-01-01 + known YOB + known sex -> min-age band ------
df_known_yob_sex <- df_alive_2024 %>%
  dplyr::filter(Gender %in% c(1, 2),
                !is.na(year_of_birth_start), !is.na(year_of_birth_end)) %>%
  dplyr::mutate(
    sex = ifelse(Gender == 1, "M", "F"),
    min_age = pmax(0L, REF_YEAR - year_of_birth_end - 1L),
    max_age = pmax(0L, REF_YEAR - year_of_birth_start)
  ) %>%
  dplyr::mutate(
    min_age = pmin(min_age, max_eu_age),
    max_age = pmin(max_age, max_eu_age)
  ) %>%
  dplyr::filter(min_age <= max_age)

mzcr_min_bins_sex <- df_known_yob_sex %>%
  dplyr::mutate(
    age_group = cut(min_age, breaks = bands_breaks, labels = bands_labels, right = TRUE),
    `Age Group` = factor(as.character(age_group), levels = bands_levels)
  ) %>%
  dplyr::count(sex, `Age Group`, name = "MZCR Min. Age")

# --- Join + compute differences ---------------------------------------------
out_tbl_bins_sex <- dplyr::full_join(
  mzcr_min_bins_sex, eu_2024_bins_sex, by = c("sex","Age Group")
) %>%
  dplyr::mutate(
    `MZCR Min. Age` = tidyr::replace_na(`MZCR Min. Age`, 0L),
    `EuroStat 2024` = tidyr::replace_na(`EuroStat 2024`, 0),
    `Diff (Min - EU)` = `MZCR Min. Age` - `EuroStat 2024`,
    sex_label = dplyr::recode(sex, M = "Male", F = "Female")
  ) %>%
  dplyr::arrange(sex_label, `Age Group`)

# --- (Optional) table print --------------------------------------------------
knitr::kable(
  out_tbl_bins_sex %>%
    dplyr::select(sex_label, `Age Group`, `MZCR Min. Age`, `EuroStat 2024`, `Diff (Min - EU)`),
  caption = "Czechia — MZCR (Min Age) vs Eurostat 2024 by custom age bands and sex"
) %>%
  kableExtra::kable_styling(full_width = FALSE)

# --- Prep for plotting -------------------------------------------------------
plot_df_bins_sex <- out_tbl_bins_sex %>%
  dplyr::mutate(
    diff_min = `Diff (Min - EU)`,
    label_col = ifelse(diff_min >= 0, "#007a1f", "black"),
    label_txt = scales::comma(diff_min),
    label_y   = pmax(`MZCR Min. Age`, `EuroStat 2024`, 1) * 1.05
  )

bars_df_bins_sex <- plot_df_bins_sex %>%
  dplyr::select(sex_label, `Age Group`, `MZCR Min. Age`, `EuroStat 2024`) %>%
  tidyr::pivot_longer(
    cols = c(`MZCR Min. Age`, `EuroStat 2024`),
    names_to = "series", values_to = "count"
  ) %>%
  dplyr::mutate(
    series = factor(series, levels = c("MZCR Min. Age", "EuroStat 2024")),
    `Age Group` = factor(`Age Group`, levels = bands_levels)
  )

# --- Plot (faceted by sex) ---------------------------------------------------
p_comp_bins_sex <- ggplot(bars_df_bins_sex, aes(x = `Age Group`, y = count, fill = series)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_text(
    data = plot_df_bins_sex,
    aes(x = `Age Group`, y = label_y, label = label_txt, color = label_col),
    inherit.aes = FALSE, size = 3.6, fontface = "bold"
  ) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si(""))) +
  scale_fill_manual(
    values = c("MZCR Min. Age" = "#2B6CB0", "EuroStat 2024" = "#9B2C2C"),
    name = NULL
  ) +
  scale_color_identity() +
  labs(
    title = "Czechia — MZCR (Min Age) vs Eurostat 2024 by custom age bands",
    subtitle = "Faceted by sex. Label = MZCR Min. Age − Eurostat 2024 (green = positive, black = negative)",
    x = NULL, y = "Population"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "top"
  ) +
  facet_wrap(~ sex_label, ncol = 1, scales = "free_y")

print(p_comp_bins_sex)

# --- Optional save -----------------------------------------------------------
dir.create("visual/compare_min_vs_eurostat_by_sex_custom_bands", showWarnings = FALSE, recursive = TRUE)
ggsave("visual/compare_min_vs_eurostat_by_sex_custom_bands/mzcr_min_vs_eurostat_2024_by_sex_custom_bands.png",
       p_comp_bins_sex, width = 12, height = 9, dpi = 300, bg = "white")

# --- Optional: total offsets per sex ----------------------------------------
totals_custom_by_sex <- plot_df_bins_sex %>%
  dplyr::group_by(sex_label) %>%
  dplyr::summarise(total_offset = sum(diff_min, na.rm = TRUE), .groups = "drop")
print(totals_custom_by_sex)


