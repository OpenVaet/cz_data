suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(lubridate)
  library(scales)
  library(knitr)
  library(kableExtra)
})

# Load Eurostat data
eu_file <- "data/demo_pjan_linear_2_0.csv"
eu <- read.csv(eu_file, stringsAsFactors = FALSE, check.names = FALSE)

eu_cz <- eu %>%
  rename(time = `TIME_PERIOD`, value = `OBS_VALUE`) %>%
  mutate(geo_code = sub(":.*$", "", geo)) %>%
  filter(geo_code == "CZ", time == 2024, age != "TOTAL", age != "UNK") %>%
  mutate(
    age_code = stringr::str_extract(age, "Y_LT1|Y_GE\\d+|Y\\d+"),
    sex = sub(":.*$", "", sex),
    age_numeric = case_when(
      age_code == "Y_LT1" ~ 0L,
      grepl("^Y_GE\\d+$", age_code) ~ as.integer(sub("^Y_GE", "", age_code)),
      grepl("^Y\\d+$", age_code) ~ as.integer(sub("^Y", "", age_code)),
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(!is.na(age_numeric), sex %in% c("M", "F"), !is.na(value)) %>%
  select(age = age_numeric, sex, value) %>%
  mutate(value = as.numeric(value))

# Load imputed dataset
df <- read.csv("data/mzcr_no_or_first_infection_with_imputation.csv", stringsAsFactors = FALSE)

cat(sprintf("Loaded %d records from imputed file\n", nrow(df)))

# Filter alive on 2024-01-01
df_alive <- df %>%
  mutate(week_date_of_death = as.Date(week_date_of_death)) %>%
  filter(is.na(week_date_of_death) | week_date_of_death >= as.Date("2024-01-01")) %>%
  mutate(
    # use the same convention as the Perl weights
    max_age = pmax(0L, 2024L - year_of_birth_end)
  ) %>%
  filter(max_age >= 0)

# Age bands (matching pre-imputation bins)
bands_breaks <- c(-1, 14, 29, 49, 64, Inf)
bands_labels <- c("0-14", "15-29", "30-49", "50-64", "65+")

# Aggregate MZCR with *max_age*
mzcr_agg <- df_alive %>%
  mutate(age_group = cut(max_age, breaks = bands_breaks, labels = bands_labels, right = TRUE)) %>%
  count(sex, age_group, name = "MZCR")


cat(sprintf("Filtered to %d records alive on 2024-01-01\n", nrow(df_alive)))

# Age bands (matching pre-imputation)
bands_breaks <- c(-1, 14, 29, 49, 64, Inf)
bands_labels <- c("0-14", "15-29", "30-49", "50-64", "65+")

# Aggregate MZCR (using max_age like pre-imputation)
mzcr_agg <- df_alive %>%
  mutate(age_group = cut(max_age, breaks = bands_breaks, labels = bands_labels, right = TRUE)) %>%
  count(sex, age_group, name = "MZCR")

# Aggregate Eurostat
eu_agg <- eu_cz %>%
  mutate(age_group = cut(age, breaks = bands_breaks, labels = bands_labels, right = TRUE)) %>%
  group_by(sex, age_group) %>%
  summarise(Eurostat = sum(value, na.rm = TRUE), .groups = "drop")

# Compare
comparison <- full_join(mzcr_agg, eu_agg, by = c("sex", "age_group")) %>%
  mutate(
    MZCR = replace_na(MZCR, 0L),
    Eurostat = replace_na(Eurostat, 0),
    Difference = MZCR - Eurostat,
    Pct_Diff = 100 * Difference / Eurostat,
    sex_label = recode(sex, M = "Male", F = "Female")
  ) %>%
  arrange(sex_label, age_group)

# Print comparison
cat("\n=== COMPARISON: MZCR Imputed vs Eurostat 2024 ===\n\n")
kable(comparison %>% select(sex_label, age_group, MZCR, Eurostat, Difference, Pct_Diff),
      digits = 1,
      col.names = c("Sex", "Age Group", "MZCR", "Eurostat", "Difference", "% Diff")) %>%
  kable_styling(full_width = FALSE) %>%
  print()

# Plot
plot_df <- comparison %>%
  mutate(
    label_col = ifelse(Difference >= 0, "#007a1f", "black"),
    label_y = pmax(MZCR, Eurostat, 1) * 1.05
  )

bars <- plot_df %>%
  select(sex_label, age_group, MZCR, Eurostat) %>%
  pivot_longer(cols = c(MZCR, Eurostat), names_to = "series", values_to = "count") %>%
  mutate(series = factor(series, levels = c("MZCR", "Eurostat")))

p <- ggplot(bars, aes(x = age_group, y = count, fill = series)) +
  geom_col(position = position_dodge(0.7), width = 0.65) +
  geom_text(data = plot_df, aes(x = age_group, y = label_y, label = comma(Difference), color = label_col),
            inherit.aes = FALSE, size = 3.6, fontface = "bold") +
  scale_y_continuous(labels = label_number(scale_cut = cut_si(""))) +
  scale_fill_manual(values = c(MZCR = "#2B6CB0", Eurostat = "#9B2C2C"), name = NULL) +
  scale_color_identity() +
  labs(
    title = "MZCR Imputed vs Eurostat 2024",
    subtitle = "Adjusted unknown cohort population",
    x = NULL, 
    y = "Population"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.x = element_blank(), legend.position = "top") +
  facet_wrap(~ sex_label, ncol = 1, scales = "free_y")

print(p)

dir.create("visual/imputed_vs_eurostat", showWarnings = FALSE, recursive = TRUE)
ggsave("visual/imputed_vs_eurostat/comparison.png", p, width = 12, height = 9, dpi = 300, bg = "white")

cat("\n=== TOTAL BY SEX ===\n")
totals <- comparison %>% 
  group_by(sex_label) %>% 
  summarise(
    MZCR = sum(MZCR), 
    Eurostat = sum(Eurostat), 
    Difference = sum(Difference),
    Pct_Diff = 100 * Difference / Eurostat
  )
print(totals)

cat("\n=== OVERALL TOTAL ===\n")
overall <- data.frame(
  MZCR = sum(comparison$MZCR),
  Eurostat = sum(comparison$Eurostat),
  Difference = sum(comparison$Difference)
) %>%
  mutate(Pct_Diff = 100 * Difference / Eurostat)
print(overall)

