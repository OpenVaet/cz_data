library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(ISOweek)
library(forcats)

# -------------------------------------------------------------------
# Paths & URLs
# -------------------------------------------------------------------
ecdc_file <- "data/ecdc_doses_data.csv"
ecdc_url  <- "https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv"

dir.create("data", showWarnings = FALSE, recursive = TRUE)

# ===================================================================
# Ensures ECDC dose data file is present (download if missing)
# ===================================================================
if (!file.exists(ecdc_file)) {
  options(timeout = 1200)
  download.file(ecdc_url, ecdc_file, mode = "wb")
  cat("ECDC weekly dose data file downloaded.\n")
} else {
  cat("ECDC weekly dose data file found locally.\n")
}

# Read
ecdc <- read.csv(ecdc_file, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

print(ecdc)

# The defaultview is WEEKLY and uses colon-labelled headers; normalize them
ecdc2 <- ecdc %>%
  filter(ReportingCountry == "CZ") %>%                     # Czechia only
  mutate(FirstDose = suppressWarnings(as.numeric(FirstDose))) %>%  # numeric deaths
  filter(TargetGroup == "ALL")                              # Sustains only "all age groups" %>%

print(ecdc2)

# --------------------------
# Weekly totals by Vaccine (CZ, TargetGroup == ALL)
# --------------------------

# Make sure numeric (some fields come as character)
dose_cols <- c("FirstDose","SecondDose",
               "DoseAdditional1","DoseAdditional2","DoseAdditional3",
               "DoseAdditional4","DoseAdditional5")

ecdc2 <- ecdc %>%
  dplyr::filter(ReportingCountry == "CZ", TargetGroup == "ALL") %>%
  dplyr::mutate(
    dplyr::across(all_of(dose_cols),
                  ~ suppressWarnings(as.numeric(.))),
    # normalize Vaccine label (avoid NA/empty)
    Vaccine = ifelse(is.na(Vaccine) | !nzchar(trimws(Vaccine)),
                     "Unknown", trimws(Vaccine)),
    # ISO week (Monday) for plotting on a date axis
    week_date = ISOweek::ISOweek2date(paste0(YearWeekISO, "-1"))
  )

# Row total per record, then sum by week & vaccine
weekly_by_vax <- ecdc2 %>%
  dplyr::mutate(doses_total = rowSums(dplyr::across(all_of(dose_cols)), na.rm = TRUE)) %>%
  dplyr::group_by(YearWeekISO, week_date, Vaccine) %>%
  dplyr::summarise(total_doses = sum(doses_total, na.rm = TRUE), .groups = "drop")

# Order vaccines by total volume
vax_totals <- weekly_by_vax %>%
  dplyr::group_by(Vaccine) %>%
  dplyr::summarise(total = sum(total_doses), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(total))

top_k <- 12
keep_vax <- vax_totals %>%
  dplyr::slice_head(n = top_k) %>%
  dplyr::pull(Vaccine) %>%
  as.character()   # <-- ensure character for fct_other()

# Collapse small categories into "Other", re-aggregate
weekly_by_vax_collapsed <- weekly_by_vax %>%
  dplyr::mutate(
    Vaccine = forcats::fct_other(
      as.character(Vaccine),   # convert current column to character
      keep = keep_vax,
      other_level = "Other"
    )
  ) %>%
  dplyr::group_by(YearWeekISO, week_date, Vaccine) %>%
  dplyr::summarise(total_doses = sum(total_doses), .groups = "drop") %>%
  dplyr::mutate(Vaccine = forcats::fct_relevel(Vaccine, c(keep_vax, "Other")))

# Choose a palette dynamically (Set2 ≤8, Set3 ≤12, else fallback to hue)
n_cols <- nlevels(weekly_by_vax_collapsed$Vaccine)
pal <- if (n_cols <= 8) RColorBrewer::brewer.pal(n_cols, "Set2") else
       if (n_cols <= 12) RColorBrewer::brewer.pal(n_cols, "Set3") else
       scales::hue_pal()(n_cols)

p_ecdc <- ggplot(weekly_by_vax_collapsed, aes(x = week_date, y = total_doses, fill = Vaccine)) +
  geom_col() +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si("")), expand = expansion(mult = c(0, .05))) +
  scale_fill_manual(values = pal, name = "Vaccine") +
  labs(
    title = "COVID-19 vaccine doses by ISO week — Czechia (ECDC)",
    subtitle = paste0("Sum of First, Second, and Additional (1–5) doses per vaccine; top ", top_k, " shown, others grouped"),
    x = NULL, y = "Doses administered"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

print(p_ecdc)

# (Optional) save
dir.create("visual/ecdc_weekly", recursive = TRUE, showWarnings = FALSE)
ggsave("visual/ecdc_weekly/ecdc_cz_weekly_by_vaccine.png",
       p_ecdc, width = 12, height = 6, dpi = 300, bg = "white")
