# Load necessary libraries
library(httr)
library(stringi)
library(dplyr)
library(knitr)
library(htmltools)
library(rmarkdown)
library(ggplot2)
library(flextable)
library(scales) 
library(lubridate)

# Breakdown by brand.
doses_1_by_brand <- read.csv('data/doses_1_by_brand.csv', stringsAsFactors = FALSE)
print(doses_1_by_brand)
doses_1_by_brand_table <- flextable(doses_1_by_brand) %>%
  set_header_labels(
    "first_dose_type" = "First Dose Brand",
    "total_recipients" = "Total Recipents"
  ) %>%
  theme_zebra() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 14, part = "all") %>%
  padding(padding = 2) %>%
  autofit() %>%
  set_caption("Table 1: Doses 1 by Brand to December 31, 2022")
save_as_html(doses_1_by_brand_table, path = "doses_1_by_brand.html")

# Breakdown by brand summarized.
doses_1_by_brand_summarized <- read.csv('data/doses_1_by_brand_summarized.csv', stringsAsFactors = FALSE)
print(doses_1_by_brand_summarized)
doses_1_by_brand_summarized_table <- flextable(doses_1_by_brand_summarized) %>%
  set_header_labels(
    "first_dose_type" = "First Dose Brand (Synth.)",
    "total_recipients" = "Total Recipents"
  ) %>%
  theme_zebra() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 14, part = "all") %>%
  padding(padding = 2) %>%
  autofit() %>%
  set_caption("Table 2 : Doses 1 by Synthetized Brand to December 31, 2022")
save_as_html(doses_1_by_brand_summarized_table, path = "doses_1_by_brand_summarized.html")

# Breakdown by population-year-birth_sex.csv
population_year_birth_sex <- read.csv("data/population_year_birth_sex.csv")
print(population_year_birth_sex)

# Group the data by 5-year intervals
population_year_birth_sex$year_group <- cut(population_year_birth_sex$year_birth, breaks = seq(1910, 2025, by = 5), right = FALSE, include.lowest = TRUE, labels = paste(seq(1910, 2020, by = 5), seq(1914, 2024, by = 5), sep = "-"))

# Aggregate population by 5-year intervals
aggregated_population_year_birth_sex <- population_year_birth_sex %>%
  group_by(year_group) %>%
  summarize(population = sum(population))

# Create the ggplot for total population
ggplot(aggregated_population_year_birth_sex, aes(x = year_group, y = population)) +
  geom_col(fill = "skyblue") + # Create the columns
  geom_text(aes(label = population), vjust = -0.5, size = 5) + # Add values on top of columns, size adjusted
  labs(title = "CZ - Population Distribution by 5-Year Groups of Birth Year",
       x = "Year of Birth (Grouped by 5 Years)",
       y = "Population") +
  theme_minimal() + # Use a minimal theme for cleaner look
  theme(
    plot.title = element_text(size = 22, face = "bold"), # Title font size
    axis.title.x = element_text(size = 18), # X-axis title font size
    axis.title.y = element_text(size = 18), # Y-axis title font size
    axis.text.x = element_text(size = 18, angle = 90, hjust = 1), # X-axis text font size and rotation
    axis.text.y = element_text(size = 18) # Y-axis text font size
  )

# Breakdown by unvaccinated population-year-birth_sex.csv
unvaccinated_population_year_birth_sex <- read.csv("data/unvaccinated_population_year_birth_sex.csv")
print(unvaccinated_population_year_birth_sex)

# Group the data by 5-year intervals
unvaccinated_population_year_birth_sex$year_group <- cut(unvaccinated_population_year_birth_sex$year_birth, breaks = seq(1910, 2025, by = 5), right = FALSE, include.lowest = TRUE, labels = paste(seq(1910, 2020, by = 5), seq(1914, 2024, by = 5), sep = "-"))

# Aggregate population by 5-year intervals
aggregated_unvaccinated_population_year_birth_sex <- unvaccinated_population_year_birth_sex %>%
  group_by(year_group) %>%
  summarize(population = sum(population))

# Create the ggplot for unvaccinated population
ggplot(aggregated_unvaccinated_population_year_birth_sex, aes(x = year_group, y = population)) +
  geom_col(fill = "skyblue") + # Create the columns
  geom_text(aes(label = population), vjust = -0.5, size = 5) + # Add values on top of columns, size adjusted
  labs(title = "CZ - Unvaccinated Population Distribution by 5-Year Groups of Birth Year",
       x = "Year of Birth (Grouped by 5 Years)",
       y = "Population") +
  theme_minimal() + # Use a minimal theme for cleaner look
  scale_y_continuous(labels = comma) + # Format y-axis labels
  theme(
    plot.title = element_text(size = 22, face = "bold"), # Title font size
    axis.title.x = element_text(size = 18), # X-axis title font size
    axis.title.y = element_text(size = 18), # Y-axis title font size
    axis.text.x = element_text(size = 18, angle = 90, hjust = 1), # X-axis text font size and rotation
    axis.text.y = element_text(size = 18) # Y-axis text font size
  )

# Breakdown by vaccinated population-year-birth_sex.csv
vaccinated_population_year_birth_sex <- read.csv("data/vaccinated_population_year_birth_sex.csv")
print(vaccinated_population_year_birth_sex)

# Group the data by 5-year intervals
vaccinated_population_year_birth_sex$year_group <- cut(vaccinated_population_year_birth_sex$year_birth, breaks = seq(1910, 2025, by = 5), right = FALSE, include.lowest = TRUE, labels = paste(seq(1910, 2020, by = 5), seq(1914, 2024, by = 5), sep = "-"))

# Aggregate population by 5-year intervals
aggregated_vaccinated_population_year_birth_sex <- vaccinated_population_year_birth_sex %>%
  group_by(year_group) %>%
  summarize(population = sum(population))

# Create the ggplot for vaccinated population
ggplot(aggregated_vaccinated_population_year_birth_sex, aes(x = year_group, y = population)) +
  geom_col(fill = "skyblue") + # Create the columns
  geom_text(aes(label = population), vjust = -0.5, size = 5) + # Add values on top of columns, size adjusted
  labs(title = "CZ - Vaccinated Population Distribution by 5-Year Groups of Birth Year",
       x = "Year of Birth (Grouped by 5 Years)",
       y = "Population") +
  theme_minimal() + # Use a minimal theme for cleaner look
  scale_y_continuous(labels = comma) + # Format y-axis labels
  theme(
    plot.title = element_text(size = 22, face = "bold"), # Title font size
    axis.title.x = element_text(size = 18), # X-axis title font size
    axis.title.y = element_text(size = 18), # Y-axis title font size
    axis.text.x = element_text(size = 18, angle = 90, hjust = 1), # X-axis text font size and rotation
    axis.text.y = element_text(size = 18) # Y-axis text font size
  )

# Breakdown by brand.
doses_percents_administered_by_year_month <- read.csv('data/doses_percents_administered_by_year_month.csv', stringsAsFactors = FALSE)

# Convert 'dose_year' to character to avoid formatting with commas
doses_percents_administered_by_year_month$dose_year <- as.character(doses_percents_administered_by_year_month$dose_year)

# Sort the table by 'dose_year' and 'dose_month'
doses_percents_administered_by_year_month <- doses_percents_administered_by_year_month %>%
  arrange(as.numeric(dose_year), as.numeric(dose_month))

# Create the flextable
doses_percents_administered_by_year_month_table <- flextable(doses_percents_administered_by_year_month) %>%
  set_header_labels(
    "dose_year" = "Year",
    "dose_month" = "Month",
    "comirnaty" = "Comirnaty",
    "janssen" = "Janssen",
    "novavax" = "Novavax",
    "spikevax" = "Spikevax",
    "astrazeneca" = "Astrazeneca",
    "others" = "Others"
  ) %>%
  theme_zebra() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 14, part = "all") %>%
  padding(padding = 2) %>%
  autofit() %>%
  set_caption("Table 3: Monthly Percent of Doses Administered by Brand")

# Save the table as an HTML file
save_as_html(doses_percents_administered_by_year_month_table, path = "doses_percents_administered_by_year_month.html")

# Breakdown by brand.
first_doses_percents_administered_by_year_month <- read.csv('data/first_doses_percents_administered_by_year_month.csv', stringsAsFactors = FALSE)

# Convert 'dose_year' to character to avoid formatting with commas
first_doses_percents_administered_by_year_month$dose_year <- as.character(first_doses_percents_administered_by_year_month$dose_year)

# Sort the table by 'dose_year' and 'dose_month'
first_doses_percents_administered_by_year_month <- first_doses_percents_administered_by_year_month %>%
  arrange(as.numeric(dose_year), as.numeric(dose_month))

# Create the flextable
first_doses_percents_administered_by_year_month_table <- flextable(first_doses_percents_administered_by_year_month) %>%
  set_header_labels(
    "dose_year" = "Year",
    "dose_month" = "Month",
    "comirnaty" = "Comirnaty",
    "janssen" = "Janssen",
    "novavax" = "Novavax",
    "spikevax" = "Spikevax",
    "astrazeneca" = "Astrazeneca",
    "others" = "Others"
  ) %>%
  theme_zebra() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 14, part = "all") %>%
  padding(padding = 2) %>%
  autofit() %>%
  set_caption("Table 4: Monthly Percent of First Doses Administered by Brand")

# Save the table as an HTML file
save_as_html(first_doses_percents_administered_by_year_month_table, path = "first_doses_percents_administered_by_year_month.html")

# Breakdown by brand-lot.
first_doses_administered_by_lot <- read.csv('data/first_doses_administered_by_lot.csv', stringsAsFactors = FALSE)

# Convert 'dose_year' to character to avoid formatting with commas
first_doses_administered_by_lot$dose_year <- as.character(first_doses_administered_by_lot$dose_year)

# Sort the table by 'dose_year' and 'dose_month'
first_doses_administered_by_lot <- first_doses_administered_by_lot %>%
  arrange(as.numeric(dose_year), as.numeric(dose_month))

# Create the flextable
first_doses_administered_by_lot_table <- flextable(first_doses_administered_by_lot) %>%
  set_header_labels(
    "dose_year" = "Year",
    "dose_month" = "Month",
    "comirnaty" = "Comirnaty",
    "janssen" = "Janssen",
    "novavax" = "Novavax",
    "spikevax" = "Spikevax",
    "astrazeneca" = "Astrazeneca",
    "others" = "Others"
  ) %>%
  theme_zebra() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 14, part = "all") %>%
  padding(padding = 2) %>%
  autofit() %>%
  set_caption("Table X: First Doses Administered by Brand & Lots")

# Save the table as an HTML file
save_as_html(first_doses_administered_by_lot_table, path = "first_doses_administered_by_lot.html")



# Breakdown by lot-brand.
first_doses_administered_by_lot_brand <- read.csv('data/first_doses_administered_by_lot_brand.csv', stringsAsFactors = FALSE)

# Convert 'dose_year' to character to avoid formatting with commas
first_doses_administered_by_lot_brand$dose_year <- as.character(first_doses_administered_by_lot_brand$dose_year)

# Sort the table by 'dose_year' and 'dose_month'
first_doses_administered_by_lot_brand <- first_doses_administered_by_lot_brand %>%
  arrange(as.numeric(dose_year), as.numeric(dose_month))

# Create the flextable
first_doses_administered_by_lot_brand_table <- flextable(first_doses_administered_by_lot_brand) %>%
  set_header_labels(
    "dose_year" = "Year",
    "dose_month" = "Month",
    "comirnaty" = "Comirnaty",
    "janssen" = "Janssen",
    "novavax" = "Novavax",
    "spikevax" = "Spikevax",
    "astrazeneca" = "Astrazeneca",
    "others" = "Others"
  ) %>%
  theme_zebra() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 14, part = "all") %>%
  padding(padding = 2) %>%
  autofit() %>%
  set_caption("Table X: First Doses Administered by Brand & Lots")

# Save the table as an HTML file
save_as_html(first_doses_administered_by_lot_brand_table, path = "first_doses_administered_by_lot_brand.html")
