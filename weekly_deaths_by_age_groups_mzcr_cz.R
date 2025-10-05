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
file_path <- "data/Otevrena-data-NR-26-30-COVID-19-prehled-populace-2024-01.csv"
file_url  <- "https://data.mzcr.cz/data/distribuce/402/Otevrena-data-NR-26-30-COVID-19-prehled-populace-2024-01.csv"

# New header applied to the CZ file after download
cz_new_header <- paste0(
  '"ID","Infection","Gender","YearOfBirth","DateOfPositivity","DateOfResult","Recovered","Death","Symptom","TestType","Date_First_Dose","Date_Second_Dose","Date_Third_Dose","Date_Fourth_Dose","Date_Fifth_Dose","Date_Sixth_Dose","Date_Seventh_Dose","VaccinationProductCode_First_Dose","VaccinationProductCode_Second_Dose","VaccinationProductCode_Third_Dose","VaccinationProductCode_Fourth_Dose","VaccinationProductCode_Fifth_Dose","VaccinationProductCode_Sixth_Dose","VaccinationProductCode_Seventh_Dose","PrimaryReasonForHospitalizationCOVID","binary_Hospitalization","min_Hospitalization","days_Hospitalization","max_Hospitalization","binary_ICU","min_ICU","days_ICU","max_ICU","binary_StandardWard","min_StandardWard","days_StandardWard","max_StandardWard","binary_Oxygen","min_Oxygen","days_Oxygen","max_Oxygen","binary_HFNO","min_HFNO","days_HFNO","max_HFNO","binary_MechanicalVentilation_ECMO","min_MechanicalVentilation_ECMO","days_MechanicalVentilation_ECMO","max_MechanicalVentilation_ECMO","Mutation","DateOfDeathInHealthcareFacility","Long_COVID","DCCI"'
)

dir.create("data", showWarnings = FALSE, recursive = TRUE)

# -------------------------------------------------------------------
# Ensure the CZ data file is present (download if missing) + replace header
# -------------------------------------------------------------------
if (!file.exists(file_path)) {
  options(timeout = 1200)
  download.file(file_url, file_path, mode = "wb")
  cat("CZ data file downloaded from source.\n")

  # Replace the first line with the provided header
  cz_lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  if (length(cz_lines) > 0) {
    cz_lines[1] <- cz_new_header
    writeLines(cz_lines, file_path, useBytes = TRUE)
    cat("CZ data file header replaced.\n")
  } else {
    stop("Error on download; cannot replace header.")
  }
} else {
  cat("CZ data file found locally.\n")
}

# -------------------------------------------------------------------
# Read the CZ CSV into a dataframe
# -------------------------------------------------------------------
df <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)

# -------------------------------------------------------------------
# Filter once: keep only Infection == 1 or NA
# -------------------------------------------------------------------
df_base <- df %>%
  filter(is.na(Infection) | Infection == 1)

# -------------------------------------------------------------------
# Handle YearOfBirth ranges and apply filtering
# -------------------------------------------------------------------
df_aug <- df_base %>%
  mutate(
    Gender = as.character(Gender),
    YearOfBirth_start = suppressWarnings(as.integer(sub("-.*", "", YearOfBirth))),
    has_first_dose = !is.na(Date_First_Dose) & nzchar(trimws(Date_First_Dose)),
    has_death = !is.na(DateOfDeathInHealthcareFacility) & nzchar(trimws(DateOfDeathInHealthcareFacility))
  )
print(df_aug)

# -------------------------------------------------------------------
# Weekly deaths by age group
# -------------------------------------------------------------------
weekly_by_age <- df_aug %>%
  filter(has_death) %>%
  mutate(
    death_week_raw = trimws(DateOfDeathInHealthcareFacility),
    death_week_raw = na_if(death_week_raw, ""),

    # normalize to canonical ISO week "YYYY-Www"
    iso_week = case_when(
      !is.na(death_week_raw) & grepl("^\\d{4}-W\\d{2}$", death_week_raw) ~ death_week_raw,
      !is.na(death_week_raw) & grepl("^\\d{4}-\\d{1,2}$", death_week_raw) ~ {
        yy <- sub("^([0-9]{4}).*$", "\\1", death_week_raw)
        ww <- suppressWarnings(as.integer(sub("^.*-([0-9]{1,2})$", "\\1", death_week_raw)))
        ifelse(!is.na(ww) & ww >= 1 & ww <= 53, sprintf("%s-W%02d", yy, ww), NA_character_)
      },
      TRUE ~ NA_character_
    ),

    # anchor date = Monday of ISO week (for plotting on a date axis)
    week_start = suppressWarnings(ISOweek2date(ifelse(is.na(iso_week), NA, paste0(iso_week, "-1")))),

    # ISO year of death (more robust than substr in case of NA)
    iso_year = suppressWarnings(as.integer(format(week_start, "%G"))),

    # Age handling:
    # - Unknown YearOfBirth -> NA age, will map to "Unknown" group
    # - Known YearOfBirth   -> compute age at death from ISO year
    age_at_death = ifelse(is.na(YearOfBirth_start) | is.na(iso_year),
                          NA_real_,
                          iso_year - YearOfBirth_start),

    # Build age groups incl. "Unknown"
    age_group = dplyr::case_when(
      is.na(YearOfBirth_start) ~ "Unknown",
      !is.na(age_at_death) & age_at_death <= 15 ~ "0–15",
      age_at_death <= 24 ~ "15-24",
      age_at_death <= 49 ~ "25–49",
      age_at_death <= 59 ~ "50–59",
      age_at_death <= 69 ~ "60–69",
      age_at_death <= 79 ~ "70–79",
      age_at_death > 79 ~ "80+",
      TRUE ~ "Unknown"
    )
  ) %>%
  filter(!is.na(week_start)) %>%
  # keep plausible ages when known
  filter(is.na(YearOfBirth_start) | (age_at_death >= 0 & age_at_death <= 110)) %>%
  mutate(
    age_group = factor(
      age_group,
      levels = c("Unknown","0–15","15-24","25–49","50–59","60–69","70–79","80+")
    )
  ) %>%
  group_by(week_start, age_group) %>%
  summarise(deaths = n(), .groups = "drop") %>%
  complete(week_start, age_group, fill = list(deaths = 0)) %>%
  arrange(week_start, age_group)

# -------------------------------------------------------------------
# Plot
# -------------------------------------------------------------------

age_levels <- c("Unknown","0–15","15-24","25–49","50–59","60–69","70–79","80+")
weekly_by_age <- weekly_by_age %>%
  mutate(age_group = factor(age_group, levels = age_levels))

# Build a soft-blue palette: dark for Unknown (bottom) → light for 80+ (top)
pal8 <- brewer.pal(8, "Blues")        # light → dark
fill_vals <- c(
  "Unknown" = pal8[8],  # darkest
  "0–15"    = pal8[7],
  "15-24"   = pal8[6],
  "25–49"   = pal8[5],
  "50–59"   = pal8[4],
  "60–69"   = pal8[3],
  "70–79"   = pal8[2],
  "80+"     = pal8[1]   # lightest
)

ggplot(weekly_by_age, aes(x = week_start, y = deaths, fill = age_group)) +
  geom_area(position = position_stack(reverse = TRUE)) +
  geom_line(
    aes(group = age_group),
    linewidth = 0.25, colour = "black",
    position = position_stack(reverse = TRUE),
    show.legend = FALSE
  ) +
  scale_fill_manual(values = fill_vals, name = "Age group") +
  scale_x_date(
    breaks = scales::breaks_width("5 weeks"),              # tick every 5 weeks
    labels = function(d) ISOweek::ISOweek(d),              # label as YYYY-Www
    expand = expansion(mult = c(0.005, 0.01))
  ) +
  labs(
    title = "MZCR.cz - Weekly COVID-19 deaths by age group (by date of death)",
    subtitle = "Czech Republic - ISO weeks; 'Unknown' = missing year of birth",
    x = NULL, y = "Deaths (weekly total)"
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal(base_size = 18) +                          # larger base font
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
    axis.text.y  = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12),
    plot.title   = element_text(face = "bold", size = 22),
    plot.subtitle= element_text(size = 14)
  )

# -------------------------------------------------------------------
# Contingency table: Year of Birth Known vs Unknown × Dead/Alive
# -------------------------------------------------------------------
# 0) Force both dimensions to have 2 levels (prevents 1-col/1-row collapse)
df_aug <- df_aug %>%
  mutate(
    yob_known_factor = factor(!is.na(YearOfBirth_start), levels = c(FALSE, TRUE), labels = c("Unknown","Known")),
    has_death_factor = factor(has_death, levels = c(FALSE, TRUE), labels = c("No","Yes"))
  )

# 1) Contingency table (always 2×2 now)
contingency <- table(
  `Year of birth` = df_aug$yob_known_factor,
  `Dead`          = df_aug$has_death_factor
)

# 2) Counts with totals (your original approach)
contingency_df <- as.data.frame.matrix(contingency)
contingency_df$Total <- rowSums(contingency_df)
contingency_df <- rbind(
  contingency_df,
  Total = colSums(contingency_df)
)
contingency_df <- contingency_df %>% rownames_to_column("Group")

# 3) Row percentages with totals
percent_mat <- round(100 * sweep(contingency, 1, rowSums(contingency), "/"), 1)
percent_df  <- as.data.frame.matrix(percent_mat)
percent_df$Total <- 100
percent_df <- rbind(
  percent_df,
  Total = round(100 * colSums(contingency) / sum(contingency), 1)
)
percent_df <- percent_df %>% rownames_to_column("Group")

# 4) Chi-square (fallback to Fisher if a row/col sum is zero)
use_fisher <- any(rowSums(contingency) == 0) || any(colSums(contingency) == 0)
if (use_fisher) {
  test_name <- "Fisher’s exact test"
  test <- fisher.test(contingency)
  stat_txt <- "—"
  df_txt <- "—"
  p_txt <- if (test$p.value < 1e-4) "< 0.0001" else formatC(test$p.value, format="f", digits=4)
} else {
  test_name <- "Chi-square test of independence"
  test <- suppressWarnings(chisq.test(contingency, correct = FALSE))
  stat_txt <- sprintf("%.2f", unname(test$statistic))
  df_txt <- sprintf("%d", unname(test$parameter))
  p_txt <- if (test$p.value < 1e-4) "< 0.0001" else formatC(test$p.value, format="f", digits=4)
}

# 5) Pretty HTML tables
counts_tbl <- kable(
  contingency_df,
  format = "html",
  align = "lccc",
  caption = "Contingency table (counts): Year of birth Known/Unknown × Death (Yes/No)",
  col.names = c("Group", "Dead: No", "Dead: Yes", "Total"),
  escape = FALSE
) |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) |>
  row_spec(0, bold = TRUE) |>
  column_spec(1, bold = TRUE)

pct_tbl <- kable(
  percent_df,
  format = "html",
  align = "lccc",
  caption = "Row percentages (%): proportions within Known/Unknown",
  col.names = c("Group", "Dead: No", "Dead: Yes", "Total"),
  escape = FALSE
) |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) |>
  row_spec(0, bold = TRUE) |>
  column_spec(1, bold = TRUE)


# 6) Light CSS + layout
css <- HTML('
<style>
  .wrap {font-family: system-ui, Segoe UI, Roboto, Arial, sans-serif; margin: 0 auto; max-width: 1100px; padding: 1.25rem;}
  .title {font-size: 1.6rem; font-weight: 700; margin-bottom: .25rem;}
  .subtitle {color: #555; margin-bottom: 1rem;}
  .grid {display: grid; grid-template-columns: 1fr 1fr; gap: 1.25rem; align-items: start;}
  @media (max-width: 980px) {.grid {grid-template-columns: 1fr;}}
  table {border-radius: 12px; overflow: hidden;}
  .card {background: #fff; border: 1px solid #e8e8e8; border-radius: 14px; padding: 1rem 1.25rem; box-shadow: 0 1px 2px rgba(0,0,0,.04); margin-bottom: 1rem;}
  .card-title {font-weight: 700; margin-bottom: .5rem;}
  .statline {display: flex; gap: 1.25rem; flex-wrap: wrap;}
  .statline .label {display:block; color:#666; font-size:.85rem;}
  .statline .value {display:block; font-weight:700; font-size:1.1rem;}
  .note {color:#666; font-size:.9rem; margin-top:.4rem;}
  .footer {color:#666; font-size:.85rem; margin-top: 1rem;}
  .kable-table caption {caption-side: top !important; font-weight: 600; padding-bottom: .35rem;}
</style>
')

header <- div(class = "title", "Mortality by Year-of-Birth Status (Czech COVID-19 dataset)")
subhdr <- div(class = "subtitle",
              HTML(sprintf("N = %s persons; generated on %s",
                           format(sum(contingency), big.mark = " "),
                           format(Sys.Date(), "%Y-%m-%d"))))

test_card <- HTML(sprintf('
  <div class="card">
    <div class="card-title">%s</div>
    <div class="statline">
      <div><span class="label">χ²</span><span class="value">%s</span></div>
      <div><span class="label">df</span><span class="value">%s</span></div>
      <div><span class="label">p-value</span><span class="value">%s</span></div>
    </div>
    <div class="note">Association between death (Yes/No) and year-of-birth status (Known/Unknown).</div>
  </div>', test_name, stat_txt, df_txt, p_txt))

body <- div(
  class = "grid",
  div(class = "kable-table", HTML(counts_tbl)),
  div(class = "kable-table", HTML(pct_tbl))
)

footer <- div(class = "footer",
  HTML("Rows: <i>Year of birth status</i> (Unknown/Known). Columns: <i>Death</i> (Yes/No). Percentages are within rows.")
)

page <- tags$html(
  tags$head(tags$meta(charset = "utf-8"), css, tags$title("Death × Year-of-Birth Status")),
  tags$body(
    div(class = "wrap", header, subhdr, test_card, body, footer)
  )
)

# 7) Show + save
browsable(page)
dir.create("outputs", showWarnings = FALSE)
save_html(page, file = "outputs/death_yob_known_unknown.html")
message('Saved to outputs/death_yob_known_unknown.html')