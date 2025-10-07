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
# Path
# -------------------------------------------------------------------
file_path <- "data/mzcr_no_or_first_infection.csv"

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

# Small normalization used later (factors for the 2×2 table)
df_aug <- df %>%
  mutate(
    gender = as.character(gender),
    yob_known_factor = factor(!is.na(year_of_birth_end),
                              levels = c(FALSE, TRUE),
                              labels = c("Unknown","Known")),
    has_death_factor = factor(has_death,
                              levels = c(FALSE, TRUE),
                              labels = c("No","Yes"))
  )

# -------------------------------------------------------------------
# Weekly deaths by age group
# -------------------------------------------------------------------
age_levels <- c("Unknown","0–15","15-24","25–49","50–59","60–69","70–79","80+")

weekly_by_age <- df %>%
  # rely on precomputed has_death + week_date_of_death
  filter(has_death) %>%
  mutate(
    week_start = as.Date(week_date_of_death),                 # already Monday of ISO week in the enhanced data
    iso_year   = suppressWarnings(as.integer(format(week_start, "%G"))),

    # Age grouping; use provided age_at_death and fall back to Unknown if YOB missing
    age_group = dplyr::case_when(
      is.na(year_of_birth_end) ~ "Unknown",
      !is.na(age_at_death) & age_at_death <= 15 ~ "0–15",
      !is.na(age_at_death) & age_at_death <= 24 ~ "15-24",
      !is.na(age_at_death) & age_at_death <= 49 ~ "25–49",
      !is.na(age_at_death) & age_at_death <= 59 ~ "50–59",
      !is.na(age_at_death) & age_at_death <= 69 ~ "60–69",
      !is.na(age_at_death) & age_at_death <= 79 ~ "70–79",
      !is.na(age_at_death) & age_at_death > 79  ~ "80+",
      TRUE ~ "Unknown"
    )
  ) %>%
  filter(!is.na(week_start)) %>%
  # keep plausible ages when known
  filter(is.na(year_of_birth_end) | (age_at_death >= 0 & age_at_death <= 110)) %>%
  mutate(age_group = factor(age_group, levels = age_levels)) %>%
  group_by(week_start, age_group) %>%
  summarise(deaths = n(), .groups = "drop") %>%
  complete(week_start, age_group, fill = list(deaths = 0)) %>%
  arrange(week_start, age_group)

# -------------------------------------------------------------------
# Weekly totals and % Unknown (Unknown / Total * 100)
# -------------------------------------------------------------------
# % Unknown by week + two linear trends on specified ranges
# -------------------------------------------------------------------
weekly_unknown <- weekly_by_age %>%
  dplyr::group_by(week_start) %>%
  dplyr::summarise(
    total_deaths   = sum(deaths),
    unknown_deaths = sum(deaths[age_group == "Unknown"]),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    pct_unknown = dplyr::if_else(total_deaths > 0,
                                 100 * unknown_deaths / total_deaths,
                                 NA_real_)
  )

# Ranges (inclusive, Mondays of ISO weeks)
rng1_start <- ISOweek::ISOweek2date("2020-W10-1")
rng1_end   <- ISOweek::ISOweek2date("2021-W15-1")
rng2_start <- ISOweek::ISOweek2date("2021-W16-1")
rng2_end   <- ISOweek::ISOweek2date("2023-W52-1")

fit_trend <- function(dat, start_date, end_date, seg_name) {
  seg <- dat %>%
    dplyr::filter(week_start >= start_date, week_start <= end_date) %>%
    dplyr::filter(!is.na(pct_unknown)) %>%
    dplyr::arrange(week_start)

  # Diagnostics
  cat(sprintf("[%-28s] rows in range: %d", seg_name, nrow(seg)),
      if (nrow(seg) > 0) sprintf(" | %s → %s\n",
        format(min(seg$week_start), "%Y-%m-%d"),
        format(max(seg$week_start), "%Y-%m-%d")) else "\n")

  if (nrow(seg) < 2) return(dplyr::tibble())  # need ≥2 points to fit a line

  seg <- seg %>% dplyr::mutate(x_num = as.numeric(week_start))
  mdl <- lm(pct_unknown ~ x_num, data = seg)

  seg %>%
    dplyr::mutate(
      pct_trend = as.numeric(predict(mdl, newdata = seg)),
      idx       = dplyr::row_number(),
      label_flag = (idx %% 5) == 0,
      label_pct  = scales::percent(pct_trend/100, accuracy = 0.1)
    ) %>%
    dplyr::select(week_start, total_deaths, pct_trend, idx, label_flag, label_pct)
}

# -- RE-COMPUTE the two segments with names ---------------------------
trend1_df <- fit_trend(weekly_unknown, rng1_start, rng1_end, "2020W10–2021W15")
trend2_df <- fit_trend(weekly_unknown, rng2_start, rng2_end, "2021W16–2023W52")

# Scale % to primary axis units so we can add a secondary Y for %
K <- max(weekly_unknown$total_deaths, na.rm = TRUE) / 100
if (!is.finite(K) || K <= 0) K <- 1

trend1_df <- trend1_df %>% dplyr::mutate(y_plot = pct_trend * K)
trend2_df <- trend2_df %>% dplyr::mutate(y_plot = pct_trend * K)

# -------------------------------------------------------------------
# Plot
# -------------------------------------------------------------------
pal8 <- brewer.pal(8, "Blues")
fill_vals <- c(
  "Unknown" = pal8[8],
  "0–15"    = pal8[7],
  "15-24"   = pal8[6],
  "25–49"   = pal8[5],
  "50–59"   = pal8[4],
  "60–69"   = pal8[3],
  "70–79"   = pal8[2],
  "80+"     = pal8[1]
)

p <- ggplot(weekly_by_age, aes(x = week_start, y = deaths, fill = age_group)) +
  geom_area(position = position_stack(reverse = TRUE)) +
  geom_line(
    aes(group = age_group),
    linewidth = 0.25, colour = "black",
    position = position_stack(reverse = TRUE),
    show.legend = FALSE
  ) +
  geom_line(
    data = trend1_df, inherit.aes = FALSE,
    aes(x = week_start, y = y_plot),
    linewidth = 1.1, colour = "black", linetype = "solid"
  ) +
  geom_point(
    data = dplyr::filter(trend1_df, idx %in% c(1, max(idx))), inherit.aes = FALSE,
    aes(x = week_start, y = y_plot),
    colour = "black", size = 2.2, shape = 16
  ) +
  geom_text(
    data = dplyr::filter(trend1_df, label_flag), inherit.aes = FALSE,
    aes(x = week_start, y = y_plot, label = label_pct),
    colour = "black", size = 3.2, fontface = "bold", vjust = -2
  ) +
  geom_line(
    data = trend2_df, inherit.aes = FALSE,
    aes(x = week_start, y = y_plot),
    linewidth = 1.1, colour = "black", linetype = "dashed"
  ) +
  geom_point(
    data = dplyr::filter(trend2_df, idx %in% c(1, max(idx))), inherit.aes = FALSE,
    aes(x = week_start, y = y_plot),
    colour = "black", size = 2.2, shape = 1, stroke = 1.1
  ) +
  geom_text(
    data = dplyr::filter(trend2_df, label_flag), inherit.aes = FALSE,
    aes(x = week_start, y = y_plot, label = label_pct),
    colour = "black", size = 3.2, fontface = "bold", vjust = -0.6
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_si("")),
    sec.axis = sec_axis(~ . / K, name = "% Unknown age (weekly)",
                        labels = function(v) paste0(round(v), "%"))
  ) +
  scale_fill_manual(values = fill_vals, name = "Age group") +
  scale_x_date(
    breaks = scales::breaks_width("5 weeks"),
    labels = function(d) ISOweek::ISOweek(d),
    expand = expansion(mult = c(0.005, 0.01))
  ) +
  labs(
    title = "MZCR.cz - Weekly deaths by age group",
    subtitle = "Two linear trends of % of Deaths with Unknown age compared to the weekly deaths: 2020W10–2021W15 (solid) and 2021W16–2023W52 (dashed).",
    caption = "Czech Republic - ISO weeks; 'Unknown' = missing year of birth; Age determined on Min. age band.",
    x = NULL, y = "Deaths (weekly total)"
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
    axis.text.y  = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12),
    plot.title   = element_text(face = "bold", size = 22),
    plot.subtitle= element_text(size = 14)
  )


print(p)


# -------------------------------------------------------------------
# Contingency table: Year of Birth Known vs Unknown × Dead/Alive
# -------------------------------------------------------------------
# 1) Contingency table (always 2×2 now)
contingency <- table(
  `Year of birth` = df_aug$yob_known_factor,
  `Dead`          = df_aug$has_death_factor
)

# 2) Counts with totals
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

# 4) Chi-square (fallback to Fisher if needed) -- unchanged
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

# =========================
# Weekly share of unknown-YOB deaths (relative to ALL unknown-YOB deaths)
# =========================
weekly_unknown_out <- weekly_unknown %>%
  dplyr::ungroup() %>%  # ensure no grouping carries over
  dplyr::mutate(
    total_unknown_deaths_all = sum(unknown_deaths, na.rm = TRUE),
    pct_of_total_unknown = dplyr::if_else(
      total_unknown_deaths_all > 0,
      100 * unknown_deaths / total_unknown_deaths_all,
      NA_real_
    ),
    pct_unknown = round(pct_unknown, 3),
    pct_of_total_unknown = round(pct_of_total_unknown, 3)
  ) %>%
  dplyr::select(
    week_start,
    total_deaths,
    unknown_deaths,
    pct_unknown,
    pct_of_total_unknown
  )

# Quick sanity checks
print(sum(weekly_unknown_out$pct_of_total_unknown, na.rm = TRUE))  # ~ 100
print(range(weekly_unknown_out$unknown_deaths, na.rm = TRUE))      # should vary
print(utils::head(unique(weekly_unknown_out$pct_of_total_unknown)))# should vary

# Save to CSV
dir.create("data", showWarnings = FALSE, recursive = TRUE)
write.csv(weekly_unknown_out, "data/weekly_deaths_with_missing_yob.csv", row.names = FALSE)
cat("Wrote weekly summary with share of unknown deaths to data/weekly_deaths_with_missing_yob.csv\n")
