library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(ISOweek)

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

# Unknown-age (UNK) population in Czechia, 2010–2024 : Verify we have none.
unk_tbl <- eu2 %>%
  mutate(year = as.integer(time)) %>%
  filter(geo_code == "CZ",
         year >= 2010, year <= 2025,
         age %in% c("UNK","UKN")) %>%         # accept either spelling, keep UNK
  # keep Total (both sexes) if available; otherwise sum across sexes
  group_by(year, has_total = any(sex %in% c("T","TOT","Total"))) %>%
  filter(if (first(has_total)) sex %in% c("T","TOT","Total") else TRUE) %>%
  summarise(pop_unknown_age = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(year)

knitr::kable(unk_tbl, caption = "Czechia – population with age UNK, 2010–2024 (Eurostat demo_pjan)")

# | year|has_total | pop_unknown_age|
# |----:|:---------|---------------:|
# | 2010|TRUE      |               0|
# | 2011|TRUE      |               0|
# | 2012|TRUE      |               0|
# | 2013|TRUE      |               0|
# | 2014|TRUE      |               0|
# | 2015|TRUE      |               0|
# | 2016|TRUE      |               0|
# | 2017|TRUE      |               0|
# | 2018|TRUE      |               0|
# | 2019|TRUE      |               0|
# | 2020|TRUE      |               0|
# | 2021|TRUE      |               0|
# | 2022|TRUE      |               0|
# | 2023|TRUE      |               0|
# | 2024|TRUE      |               0|

eu3 <- eu2 %>%
  dplyr::filter(time >= 2010, age != "UNK") %>%
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

# Inspect age structure on census years.
age_totals <- eu3 %>%
  dplyr::filter(time %in% c(2011, 2021)) %>%
  dplyr::group_by(time, age) %>%
  dplyr::summarise(
    total = if (any(sex == "T")) sum(value[sex == "T"], na.rm = TRUE)
            else sum(value[sex %in% c("M","F")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(time, age)

# Wide comparison table: Age | 2011 | 2021
age_totals_wide <- age_totals %>%
  tidyr::pivot_wider(
    names_from = time, values_from = total, names_prefix = "y"
  ) %>%
  dplyr::arrange(age)

# Print
knitr::kable(
  age_totals_wide,
  caption = "Czechia — population by single age (Total), 2011 vs 2021 (Eurostat demo_pjan)"
) %>%
  kableExtra::kable_styling(full_width = FALSE)

# How many distinct ages per year?
age_totals %>%
  dplyr::count(time, name = "n_distinct_ages") %>%
  knitr::kable(caption = "Number of distinct ages present per year")

##########################################################################################
# -------> 
##########################################################################################
# Czechia — population by single age (Total), 2011 vs 2021 (Eurostat demo_pjan)
# age y2011   y2021
# 0   119110  107478
# 1   121127  110593
# 2   122775  113057
# 3   118248  113934
# 4   108676  113163
# 5   102848  111101
# 6   98087   110325
# 7   94249   107618
# 8   93347   109476
# 9   91481   109605
# 10  90730   117320
# 11  89349   118496
# 12  90187   119932
# 13  90670   115511
# 14  90881   106770
# 15  96487   103131
# 16  107381  98478
# 17  121723  95157
# 18  123055  95275
# 19  130338  94541
# 20  132081  94581
# 21  131687  92513
# 22  137335  94195
# 23  136492  95024
# 24  139293  95663
# 25  142653  101844
# 26  143673  112607
# 27  144448  126202
# 28  147970  127157
# 29  149303  134601
# 30  158265  135476
# 31  174770  133618
# 32  180456  138035
# 33  183225  136564
# 34  187956  138455
# 35  191321  141149
# 36  193309  141786
# 37  181092  142188
# 38  164130  145060
# 39  154642  146311
# 40  148768  154855
# 41  143090  170814
# 42  136315  176474
# 43  136453  179251
# 44  138098  184080
# 45  143498  187535
# 46  150068  189349
# 47  144775  177225
# 48  130431  160284
# 49  127278  151200
# 50  124828  144969
# 51  123499  139234
# 52  133295  132073
# 53  143395  132304
# 54  147809  133446
# 55  149441  138205
# 56  149865  143811
# 57  150687  138222
# 58  153198  123598
# 59  153422  120081
# 60  150195  116907
# 61  145648  115073
# 62  149457  123517
# 63  153807  131475
# 64  147592  135030
# 65  117896  135073
# 66  121730  134411
# 67  116888  133982
# 68  101726  134434
# 69  95990   133218
# 70  92398   128592
# 71  80122   122843
# 72  75453   124408
# 73  69650   125923
# 74  66815   118017
# 75  65269   92190
# 76  63984   93567
# 77  62208   87499
# 78  62630   74564
# 79  59225   68179
# 80  56848   63552
# 81  50485   53068
# 82  46341   47419
# 83  40987   41470
# 84  36935   37634
# 85  32557   34129
# 86  28565   30848
# 87  24681   27461
# 88  19948   24682
# 89  15773   20734
# 90  11029   17368
# 91  7191    13033
# 92  3120    10070
# 93  2384    7458
# 94  1924    5465
# 95  1774    3728
# 96  1678    2602
# 97  1093    1847
# 98  728     1081
# 99  412     666

# Distinct sex values present (sanity check)
eu3 %>%
  dplyr::filter(time %in% c(2011, 2021)) %>%
  dplyr::distinct(time, sex) %>%
  dplyr::arrange(time, sex) %>%
  knitr::kable(caption = "Distinct sex codes present in 2011 & 2021")

# Totals by sex
sex_totals <- eu3 %>%
  dplyr::filter(time %in% c(2011, 2021)) %>%
  dplyr::group_by(time, sex) %>%
  dplyr::summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(sex_label = dplyr::recode(sex, "T" = "Total", "F" = "Female", "M" = "Male")) %>%
  tidyr::pivot_wider(names_from = time, values_from = total, names_prefix = "y") %>%
  dplyr::arrange(factor(sex_label, levels = c("Total", "Female", "Male"))) %>%
  dplyr::select(Sex = sex_label, y2011, y2021)

knitr::kable(
  sex_totals,
  caption = "Czechia — population totals by sex, 2011 vs 2021 (Eurostat demo_pjan)"
) %>%
  kableExtra::kable_styling(full_width = FALSE)

##########################################################################################
# -------> 
##########################################################################################
# Sex y2011 y2021
# Total 10486199  10494217
# Female  5339077 5323405
# Male  5147122 5170812

###############################################################
## Cohort aging heatmap for Czechia
###############################################################

# --- Build CZ population frame: Year, Age, Population (Total sex) ---
pop <- eu3 %>%
  dplyr::group_by(Year = time, Age = age) %>%
  dplyr::summarise(
    Population = if (any(sex == "T")) sum(value[sex == "T"], na.rm = TRUE)
                 else sum(value[sex %in% c("M","F")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Year, Age)

# --- Params tuned for CZ scale ---
HIGHLIGHT_THRESHOLD <- 25000L  # outline cells when |Δ| > 25k (CZ-sized cohorts)

# --- Years & bounds ---
yrs     <- sort(unique(pop$Year))
max_age <- max(pop$Age, na.rm = TRUE)

# --- Cohort deltas at single ages: Δ(N,A) = Pop(N,A) - Pop(N-1,A-1) ---
delta_long <- pop %>%
  dplyr::mutate(Year_prev = Year - 1, Age_prev = Age - 1) %>%
  dplyr::left_join(
    pop %>% dplyr::select(Year, Age, Population) %>%
      dplyr::rename(Year_prev = Year, Age_prev = Age, Pop_prev = Population),
    by = c("Year_prev", "Age_prev")
  ) %>%
  dplyr::transmute(Year, Age, delta = Population - Pop_prev)  # NA for first year or Age==0

# --- 5-year age groups (00–04, 05–09, …, top group capped at max_age) ---
mk_group5 <- function(A) {
  gs <- (A %/% 5) * 5
  ge <- pmin(gs + 4, max_age)
  sprintf("%02d–%02d", gs, ge)
}

# Grouped population by Year x AgeGroup5
pop_group_year <- pop %>%
  dplyr::mutate(AgeGroup5 = mk_group5(Age),
                grp_start  = (Age %/% 5) * 5) %>%
  dplyr::group_by(AgeGroup5, grp_start, Year) %>%
  dplyr::summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")

# Grouped cohort deltas by Year x AgeGroup5 (sum single-age deltas in the group)
delta_group_year <- delta_long %>%
  dplyr::mutate(AgeGroup5 = mk_group5(Age),
                grp_start  = (Age %/% 5) * 5) %>%
  dplyr::group_by(AgeGroup5, grp_start, Year) %>%
  dplyr::summarise(Delta = sum(delta, na.rm = TRUE), .groups = "drop")

# --- Display params (math unchanged) ---
OUT_DIR  <- "visual/cz_pop_aging"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

YEAR_MIN  <- min(yrs, na.rm = TRUE)
YEAR_MAX  <- max(yrs, na.rm = TRUE)
year_span <- paste0(YEAR_MIN, "-", YEAR_MAX)

# Merge totals + Δ (keep first year even if Δ is NA)
df5 <- pop_group_year %>%
  dplyr::left_join(delta_group_year, by = c("AgeGroup5","grp_start","Year")) %>%
  dplyr::filter(Year >= YEAR_MIN, Year <= YEAR_MAX)

# Order groups youngest → oldest
group_levels <- df5 %>% dplyr::distinct(AgeGroup5, grp_start) %>%
  dplyr::arrange(grp_start) %>% dplyr::pull(AgeGroup5)
df5 <- df5 %>% dplyr::mutate(AgeGroup5 = factor(AgeGroup5, levels = group_levels))

# Compact label formatters (SI suffixes)
fmt_si     <- scales::label_number(accuracy = 0.1, scale_cut = scales::cut_si(""))
fmt_si_int <- scales::label_number(accuracy = 1,   scale_cut = scales::cut_si(""))

# Two-line labels: Population (line 1) + Δ (line 2, signed if present)
df5 <- df5 %>%
  dplyr::mutate(
    pop_lbl   = fmt_si(Population),
    delta_lbl = ifelse(is.na(Delta), "", paste0(ifelse(Delta > 0, "+", ""), fmt_si_int(Delta))),
    label     = ifelse(delta_lbl == "", pop_lbl, paste0(pop_lbl, "\nΔ ", delta_lbl))
  )

# Clamp for color mapping only (labels use true values)
q98   <- stats::quantile(abs(df5$Delta), 0.98, na.rm = TRUE)
limit <- max(HIGHLIGHT_THRESHOLD, q98, na.rm = TRUE)
df5 <- df5 %>%
  dplyr::mutate(
    delta_clamped = pmax(pmin(Delta, limit), -limit),
    extreme       = !is.na(Delta) & abs(Delta) > HIGHLIGHT_THRESHOLD
  )

# Legend limits & readable text color per cell
lim <- max(abs(df5$delta_clamped), na.rm = TRUE)
df5 <- df5 %>%
  dplyr::mutate(text_col = dplyr::case_when(
    is.na(delta_clamped) ~ "black",
    abs(delta_clamped) > 0.55 * lim ~ "white",
    TRUE ~ "black"
  ))

# --- Plot (legend bottom, white background) ---
p5 <- ggplot2::ggplot(df5, ggplot2::aes(x = Year, y = AgeGroup5, fill = delta_clamped)) +
  ggplot2::geom_tile(color = "grey80", linewidth = 0.25, na.rm = FALSE) +
  ggplot2::geom_tile(
    data = subset(df5, extreme),
    ggplot2::aes(x = Year, y = AgeGroup5),
    fill = NA, color = "black", linewidth = 0.45
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = label, color = text_col),
    size = 3.2, lineheight = 0.92
  ) +
  ggplot2::scale_color_identity() +
  ggplot2::scale_x_continuous(breaks = seq(YEAR_MIN, YEAR_MAX, by = 2)) +
  ggplot2::scale_fill_gradient2(
    name = "Δ (sum over ages in group of Pop[N,A] – Pop[N−1,A−1])",
    low = "#b30000", mid = "#f2f2f2", high = "#007a1f",
    midpoint = 0, limits = c(-lim, lim),
    labels = scales::label_number(scale_cut = scales::cut_si("")),
    na.value = "grey95"
  ) +
  ggplot2::labs(
    title = "Cohort Aging — 5-Year Age Groups (Czechia)",
    subtitle = paste0(
      "Years ", year_span, ". Cells outlined when |Δ| > ",
      scales::comma(HIGHLIGHT_THRESHOLD),
      ". Red = negative Δ, Green = positive Δ."
    ),
    x = NULL, y = "Age Group (5y)"
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      barwidth = grid::unit(6, "in"),
      barheight = grid::unit(0.25, "in")
    )
  ) +
  ggplot2::theme_minimal(base_size = 14) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold", size = 18),
    plot.subtitle = ggplot2::element_text(size = 13),
    axis.text.x = ggplot2::element_text(size = 12),
    axis.text.y = ggplot2::element_text(size = 13),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = ggplot2::element_text(size = 15, face = "bold"),
    legend.text  = ggplot2::element_text(size = 14),
    legend.key.height = grid::unit(0.9, "cm"),
    legend.key.width  = grid::unit(0.7, "cm"),
    legend.background     = ggplot2::element_rect(fill = "white", color = NA),
    legend.box.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background      = ggplot2::element_rect(fill = "white", color = NA),
    plot.background       = ggplot2::element_rect(fill = "white", color = NA),
    plot.margin = ggplot2::margin(12, 16, 12, 16)
  )

# --- Save (wide, print-friendly; white background) ---
ggplot2::ggsave(file.path(OUT_DIR, paste0("CZ_cohort_5y_groups_pop_plus_delta_", year_span, "_wide16x8.pdf")),
       p5, width = 16, height = 8, units = "in", bg = "white")
ggplot2::ggsave(file.path(OUT_DIR, paste0("CZ_cohort_5y_groups_pop_plus_delta_", year_span, "_wide16x8.png")),
       p5, width = 16, height = 8, units = "in", dpi = 300, bg = "white")

p5
