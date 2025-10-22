#!/usr/bin/env Rscript
# Standalone script to plot ASMR data from CSV output
# Can be run independently after the main analysis

library(tidyverse)
library(lubridate)
library(scales)
library(ggplot2)

# Advanced interactive plot option
create_interactive_asmr_plot <- function() {
  if (!require(plotly)) {
    cat("Installing plotly for interactive plots...\n")
    install.packages("plotly")
    library(plotly)
  }
  
  # Read the ASMR data
  asmr_data <- read.csv("outputs/weekly_asmr.csv", stringsAsFactors = FALSE) %>%
    mutate(
      week_start = as.Date(paste0(year, "-01-01")) + weeks(week - 1),
      week_label = paste0(year, "-W", sprintf("%02d", week))
    )
  
  # Create the plot
  p <- plot_ly(asmr_data, x = ~week_start) %>%
    add_trace(y = ~asmr_unvaccinated, 
              name = 'Unvaccinated',
              type = 'scatter',
              mode = 'lines+markers',
              line = list(color = '#D55E00', width = 2),
              marker = list(size = 4),
              text = ~paste("Week:", week_label,
                           "<br>ASMR:", round(asmr_unvaccinated, 2)),
              hoverinfo = 'text') %>%
    add_trace(y = ~asmr_vaccinated,
              name = 'Vaccinated',
              type = 'scatter', 
              mode = 'lines+markers',
              line = list(color = '#0072B2', width = 2, dash = 'dash'),
              marker = list(size = 4),
              text = ~paste("Week:", week_label,
                           "<br>ASMR:", round(asmr_vaccinated, 2)),
              hoverinfo = 'text') %>%
    add_trace(y = ~asmr_total,
              name = 'Total Population',
              type = 'scatter',
              mode = 'lines+markers',
              line = list(color = '#009E73', width = 2, dash = 'dot'),
              marker = list(size = 4),
              text = ~paste("Week:", week_label,
                           "<br>ASMR:", round(asmr_total, 2)),
              hoverinfo = 'text') %>%
    layout(title = list(text = "Weekly Age-Standardized Mortality Rates (ASMR) by Vaccination Status",
                       font = list(size = 18)),
           xaxis = list(title = "Date",
                       rangeslider = list(visible = TRUE),
                       type = "date"),
           yaxis = list(title = "ASMR per 100,000 person-years",
                       gridcolor = 'rgb(230,230,230)'),
           hovermode = 'closest',
           paper_bgcolor = 'white',
           plot_bgcolor = 'rgb(245,245,245)',
           legend = list(x = 0.02, y = 0.98),
           margin = list(t = 80))
  
  # Save interactive plot
  htmlwidgets::saveWidget(p, "outputs/weekly_asmr_interactive.html", 
                          selfcontained = TRUE,
                          title = "Weekly ASMR Analysis")
  
  cat("Interactive plot saved to outputs/weekly_asmr_interactive.html\n")
  return(p)
}

# Create a heatmap showing weekly patterns
create_asmr_heatmap <- function() {
  # Read the ASMR data
  asmr_data <- read.csv("outputs/weekly_asmr.csv", stringsAsFactors = FALSE) %>%
    mutate(
      # Calculate rate ratio
      rate_ratio = ifelse(asmr_vaccinated > 0, 
                          asmr_unvaccinated / asmr_vaccinated, 
                          NA),
      # Cap extreme values for visualization
      rate_ratio_capped = pmin(rate_ratio, 10, na.rm = TRUE)
    )
  
  # Reshape for heatmap
  heatmap_data <- asmr_data %>%
    select(year, week, asmr_unvaccinated, asmr_vaccinated, asmr_total, rate_ratio_capped) %>%
    pivot_longer(cols = c(asmr_unvaccinated, asmr_vaccinated, asmr_total, rate_ratio_capped),
                names_to = "metric",
                values_to = "value") %>%
    mutate(
      metric = case_when(
        metric == "asmr_unvaccinated" ~ "Unvaccinated ASMR",
        metric == "asmr_vaccinated" ~ "Vaccinated ASMR",
        metric == "asmr_total" ~ "Total ASMR",
        metric == "rate_ratio_capped" ~ "Rate Ratio (Unvax/Vax)",
        TRUE ~ metric
      )
    )
  
  # Create heatmap
  p <- ggplot(heatmap_data, aes(x = week, y = factor(year), fill = value)) +
    geom_tile(color = "white", size = 0.1) +
    facet_wrap(~metric, scales = "free", ncol = 2) +
    scale_fill_gradientn(
      colors = c("navy", "blue", "cyan", "yellow", "orange", "red"),
      name = "Value",
      na.value = "gray90"
    ) +
    scale_x_continuous(breaks = seq(0, 52, 4), expand = c(0, 0)) +
    labs(
      title = "Weekly ASMR Heatmap by Year and Metric",
      subtitle = "Visualizing temporal patterns in mortality rates",
      x = "Week of Year",
      y = "Year"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      strip.text = element_text(face = "bold", size = 10),
      panel.spacing = unit(1, "lines"),
      axis.text = element_text(size = 8),
      legend.position = "bottom",
      legend.key.width = unit(2, "cm")
    )
  
  ggsave("outputs/weekly_asmr_heatmap.png", 
         plot = p, 
         width = 14, 
         height = 10, 
         dpi = 300,
         bg = "white")
  
  cat("Heatmap saved to outputs/weekly_asmr_heatmap.png\n")
  return(p)
}

# Create a cumulative mortality plot
create_cumulative_plot <- function() {
  # Read the data
  asmr_data <- read.csv("outputs/weekly_asmr.csv", stringsAsFactors = FALSE) %>%
    mutate(
      week_start = as.Date(paste0(year, "-01-01")) + weeks(week - 1)
    ) %>%
    arrange(year, week) %>%
    mutate(
      # Calculate cumulative sums
      cumulative_unvaccinated = cumsum(asmr_unvaccinated),
      cumulative_vaccinated = cumsum(asmr_vaccinated),
      cumulative_total = cumsum(asmr_total)
    ) %>%
    pivot_longer(
      cols = starts_with("cumulative"),
      names_to = "group",
      values_to = "cumulative_asmr",
      names_prefix = "cumulative_"
    ) %>%
    mutate(
      group = str_to_title(group)
    )
  
  # Create cumulative plot
  p <- ggplot(asmr_data, aes(x = week_start, y = cumulative_asmr, color = group)) +
    geom_line(size = 1.5, alpha = 0.8) +
    geom_area(aes(fill = group), alpha = 0.2, position = "identity") +
    scale_color_manual(
      values = c(
        "Unvaccinated" = "#D55E00",
        "Vaccinated" = "#0072B2",
        "Total" = "#009E73"
      ),
      name = "Group"
    ) +
    scale_fill_manual(
      values = c(
        "Unvaccinated" = "#D55E00",
        "Vaccinated" = "#0072B2",
        "Total" = "#009E73"
      ),
      guide = "none"
    ) +
    scale_x_date(
      date_breaks = "3 months",
      date_labels = "%b %Y"
    ) +
    scale_y_continuous(
      labels = comma,
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      title = "Cumulative Age-Standardized Mortality Over Time",
      subtitle = "Running total of weekly ASMR per 100,000 person-years",
      x = "Date",
      y = "Cumulative ASMR"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      legend.position = "top",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
  
  ggsave("outputs/weekly_asmr_cumulative.png",
         plot = p,
         width = 12,
         height = 7,
         dpi = 300,
         bg = "white")
  
  cat("Cumulative plot saved to outputs/weekly_asmr_cumulative.png\n")
  return(p)
}

# Create a rate ratio plot with confidence bands
create_rate_ratio_plot <- function() {
  # Read the data
  asmr_data <- read.csv("outputs/weekly_asmr.csv", stringsAsFactors = FALSE) %>%
    mutate(
      week_start = as.Date(paste0(year, "-01-01")) + weeks(week - 1),
      # Calculate rate ratio
      rate_ratio = ifelse(asmr_vaccinated > 0, 
                          asmr_unvaccinated / asmr_vaccinated, 
                          NA),
      # Calculate rolling average for smoothing
      rate_ratio_smooth = zoo::rollmean(rate_ratio, k = 4, fill = NA, align = "center")
    ) %>%
    filter(!is.na(rate_ratio))
  
  # Create the plot
  p <- ggplot(asmr_data, aes(x = week_start)) +
    # Add reference line at 1 (equal mortality)
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50", size = 1) +
    # Add shaded regions for interpretation
    annotate("rect", xmin = min(asmr_data$week_start), xmax = max(asmr_data$week_start),
             ymin = 0, ymax = 1, alpha = 0.1, fill = "blue") +
    annotate("rect", xmin = min(asmr_data$week_start), xmax = max(asmr_data$week_start),
             ymin = 1, ymax = max(asmr_data$rate_ratio, na.rm = TRUE),
             alpha = 0.1, fill = "red") +
    # Add the actual rate ratio
    geom_point(aes(y = rate_ratio), color = "gray40", alpha = 0.5, size = 1) +
    geom_line(aes(y = rate_ratio), color = "gray40", alpha = 0.3) +
    # Add smoothed line
    geom_line(aes(y = rate_ratio_smooth), color = "darkred", size = 1.5) +
    # Add text annotations
    annotate("text", x = min(asmr_data$week_start) + 30, y = 0.5,
             label = "Lower mortality\nin unvaccinated", 
             hjust = 0, color = "blue", fontface = "italic", size = 3.5) +
    annotate("text", x = min(asmr_data$week_start) + 30, 
             y = max(asmr_data$rate_ratio, na.rm = TRUE) * 0.9,
             label = "Higher mortality\nin unvaccinated", 
             hjust = 0, color = "red", fontface = "italic", size = 3.5) +
    scale_y_continuous(
      trans = "log10",
      labels = function(x) format(x, scientific = FALSE),
      breaks = c(0.5, 1, 2, 5, 10, 20)
    ) +
    scale_x_date(
      date_breaks = "3 months",
      date_labels = "%b %Y"
    ) +
    labs(
      title = "Mortality Rate Ratio: Unvaccinated vs Vaccinated",
      subtitle = "Values > 1 indicate higher mortality in unvaccinated population (log scale)",
      x = "Date",
      y = "Rate Ratio (Unvaccinated / Vaccinated)",
      caption = "Gray points: weekly values | Red line: 4-week moving average"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 9, color = "gray50", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
  
  ggsave("outputs/weekly_asmr_rate_ratio.png",
         plot = p,
         width = 12,
         height = 7,
         dpi = 300,
         bg = "white")
  
  cat("Rate ratio plot saved to outputs/weekly_asmr_rate_ratio.png\n")
  return(p)
}

# Main execution
cat("\n", rep("=", 60), "\n", sep = "")
cat("ASMR VISUALIZATION SCRIPT\n")
cat(rep("=", 60), "\n\n", sep = "")

# Check if output file exists
if (!file.exists("outputs/weekly_asmr.csv")) {
  stop("Error: outputs/weekly_asmr.csv not found. Please run the main analysis first.")
}

# Create output directory if it doesn't exist
if (!dir.exists("outputs")) {
  dir.create("outputs", recursive = TRUE)
}

# Function to create the main ASMR plot
create_main_asmr_plot <- function() {
  # Read the ASMR data
  asmr_data <- read.csv("outputs/weekly_asmr.csv", stringsAsFactors = FALSE)
  
  # Convert year-week to date for plotting
  asmr_data <- asmr_data %>%
    mutate(
      week_start = as.Date(paste0(year, "-01-01")) + weeks(week - 1)
    ) %>%
    pivot_longer(
      cols = c(asmr_unvaccinated, asmr_vaccinated, asmr_total),
      names_to = "group",
      values_to = "asmr"
    ) %>%
    mutate(
      group = case_when(
        group == "asmr_unvaccinated" ~ "Unvaccinated",
        group == "asmr_vaccinated" ~ "Vaccinated",
        group == "asmr_total" ~ "Total Population",
        TRUE ~ group
      )
    )
  
  # Calculate peak values for annotation
  peak_unvax <- asmr_data %>%
    filter(group == "Unvaccinated") %>%
    slice_max(asmr, n = 1)
  
  peak_vax <- asmr_data %>%
    filter(group == "Vaccinated") %>%
    slice_max(asmr, n = 1)
  
  # Create the plot
  p <- ggplot(asmr_data, aes(x = week_start, y = asmr, color = group, group = group)) +
    theme_minimal(base_size = 12) +
    geom_line(aes(linetype = group), size = 1.2, alpha = 0.9) +
    geom_point(size = 0.8, alpha = 0.5) +
    geom_smooth(aes(fill = group), method = "loess", span = 0.2, 
                alpha = 0.1, size = 0.5, linetype = "dotted", se = TRUE) +
    scale_color_manual(
      values = c(
        "Unvaccinated" = "#D55E00",
        "Vaccinated" = "#0072B2",
        "Total Population" = "#009E73"
      ),
      name = "Population Group"
    ) +
    scale_fill_manual(
      values = c(
        "Unvaccinated" = "#D55E00",
        "Vaccinated" = "#0072B2",
        "Total Population" = "#009E73"
      ),
      guide = "none"
    ) +
    scale_linetype_manual(
      values = c(
        "Unvaccinated" = "solid",
        "Vaccinated" = "dashed",
        "Total Population" = "dotted"
      ),
      name = "Population Group"
    ) +
    scale_y_continuous(
      labels = function(x) format(x, scientific = FALSE, big.mark = ","),
      expand = expansion(mult = c(0.05, 0.1))
    ) +
    scale_x_date(
      date_breaks = "3 months",
      date_labels = "%b %Y",
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    labs(
      title = "Weekly Age-Standardized Mortality Rates (ASMR) by Vaccination Status",
      subtitle = "Per 100,000 person-years, ESP 2013 standardized",
      x = "Date",
      y = "ASMR per 100,000 person-years",
      caption = paste("Data source: MZCR | Analysis period:", 
                     min(asmr_data$year), "Week", min(asmr_data$week[asmr_data$year == min(asmr_data$year)]),
                     "to", max(asmr_data$year), "Week", max(asmr_data$week[asmr_data$year == max(asmr_data$year)]))
    ) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      axis.title = element_text(size = 11, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      legend.position = "top",
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "gray85", size = 0.3),
      panel.grid.minor = element_line(color = "gray95", size = 0.2),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "gray98", color = NA)
    )
  
  # Add peak annotations if available
  if (nrow(peak_unvax) > 0 && nrow(peak_vax) > 0) {
    p <- p +
      annotate("text", 
               x = peak_unvax$week_start[1], 
               y = peak_unvax$asmr[1] * 1.05,
               label = paste("Peak unvaccinated:\n", round(peak_unvax$asmr[1], 1)),
               size = 3, color = "#D55E00", fontface = "bold") +
      annotate("text",
               x = peak_vax$week_start[1],
               y = peak_vax$asmr[1] * 1.05,
               label = paste("Peak vaccinated:\n", round(peak_vax$asmr[1], 1)),
               size = 3, color = "#0072B2", fontface = "bold")
  }
  
  return(p)
}

# Generate all plots
cat("Generating standard ASMR plot...\n")
main_plot <- create_main_asmr_plot()

# Save the main plot
ggsave("outputs/weekly_asmr_plot.png", 
       plot = main_plot, 
       width = 14, 
       height = 8, 
       dpi = 300,
       bg = "white")

ggsave("outputs/weekly_asmr_plot.pdf", 
       plot = main_plot, 
       width = 14, 
       height = 8,
       device = "pdf")

cat("Main ASMR plot saved to outputs/weekly_asmr_plot.png and .pdf\n")

cat("\nGenerating additional visualizations...\n")

# Generate heatmap
heatmap_plot <- create_asmr_heatmap()

# Generate cumulative plot
cumulative_plot <- create_cumulative_plot()

# Generate rate ratio plot
rate_ratio_plot <- create_rate_ratio_plot()

# Generate interactive plot if possible
tryCatch({
  interactive_plot <- create_interactive_asmr_plot()
}, error = function(e) {
  cat("Note: Could not create interactive plot. Install 'plotly' package if needed.\n")
})

# Print summary statistics with more detail
cat("\n", rep("=", 60), "\n", sep = "")
cat("DETAILED ASMR ANALYSIS\n")
cat(rep("=", 60), "\n\n", sep = "")

asmr_data <- read.csv("outputs/weekly_asmr.csv", stringsAsFactors = FALSE)

# Overall statistics
cat("Overall Statistics:\n")
cat(rep("-", 40), "\n", sep = "")

summary_stats <- asmr_data %>%
  summarise(
    across(c(asmr_unvaccinated, asmr_vaccinated, asmr_total),
           list(
             Mean = ~mean(., na.rm = TRUE),
             Median = ~median(., na.rm = TRUE),
             Min = ~min(., na.rm = TRUE),
             Max = ~max(., na.rm = TRUE),
             SD = ~sd(., na.rm = TRUE),
             IQR = ~IQR(., na.rm = TRUE)
           ), .names = "{.fn}_{.col}")
  ) %>%
  pivot_longer(everything(),
               names_to = c("Statistic", "Group"),
               names_pattern = "(.*)_asmr_(.*)") %>%
  pivot_wider(names_from = Group, values_from = value) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

print(summary_stats, row.names = FALSE)

# Yearly breakdown
cat("\n\nYearly Average ASMR:\n")
cat(rep("-", 40), "\n", sep = "")

yearly_stats <- asmr_data %>%
  group_by(year) %>%
  summarise(
    Unvaccinated = round(mean(asmr_unvaccinated, na.rm = TRUE), 2),
    Vaccinated = round(mean(asmr_vaccinated, na.rm = TRUE), 2),
    Total = round(mean(asmr_total, na.rm = TRUE), 2),
    `Avg Rate Ratio` = round(mean(asmr_unvaccinated / asmr_vaccinated, na.rm = TRUE), 2)
  )

print(yearly_stats, row.names = FALSE)

# Peak mortality weeks
cat("\n\nPeak Mortality Weeks:\n")
cat(rep("-", 40), "\n", sep = "")

peak_unvax <- asmr_data %>%
  slice_max(asmr_unvaccinated, n = 1) %>%
  mutate(Group = "Unvaccinated", Peak_ASMR = round(asmr_unvaccinated, 2)) %>%
  select(Group, year, week, Peak_ASMR)

peak_vax <- asmr_data %>%
  slice_max(asmr_vaccinated, n = 1) %>%
  mutate(Group = "Vaccinated", Peak_ASMR = round(asmr_vaccinated, 2)) %>%
  select(Group, year, week, Peak_ASMR)

peak_total <- asmr_data %>%
  slice_max(asmr_total, n = 1) %>%
  mutate(Group = "Total", Peak_ASMR = round(asmr_total, 2)) %>%
  select(Group, year, week, Peak_ASMR)

peak_weeks <- bind_rows(peak_unvax, peak_vax, peak_total)
print(peak_weeks, row.names = FALSE)

cat("\n", rep("=", 60), "\n", sep = "")
cat("All visualizations completed successfully!\n")
cat(rep("=", 60), "\n", sep = "")