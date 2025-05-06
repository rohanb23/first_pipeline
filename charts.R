# Load required packages
library(tidyverse)
library(readxl)
library(purrr)
library(rlang)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(stringr)


#------------------------
#### GRAPHS AND CHARTS
#-----------------------

# Function to prepare data for stacked bar charts
prepare_stacked_chart_data <- function(aggregate_data, keyword) {
  selected_dfs <- aggregate_data[names(aggregate_data) %>% stringr::str_detect(keyword)]

  combined_data <- dplyr::bind_rows(
    lapply(names(selected_dfs), function(name) {
      df <- selected_dfs[[name]]

      worker_type <- ifelse(stringr::str_detect(name, "Care worker"), "Care Worker", "Senior Care Worker")

      df %>%
        dplyr::select(Period_End, Total...In.Country, Total...Out.of.Country) %>%
        tidyr::pivot_longer(cols = c(Total...In.Country, Total...Out.of.Country),
                            names_to = "Flow_Type",
                            values_to = "Total") %>%
        dplyr::mutate(
          Flow_Type = stringr::str_replace_all(Flow_Type, c("Total...In.Country" = "In Country",
                                                            "Total...Out.of.Country" = "Out of Country")),
          Worker_Type = worker_type
        )
    })
  )

  return(combined_data)
}

# Function to create stacked bar charts
generate_stacked_charts <- function(aggregate_data) {
  apps_data <- prepare_stacked_chart_data(aggregate_data, "Apps")
  grants_data <- prepare_stacked_chart_data(aggregate_data, "Grants")

  apps_dates <- sort(unique(apps_data$Period_End))
  apps_breaks <- apps_dates[seq(1, length(apps_dates), by = 2)]  # every 2nd date

  apps_plot <- ggplot2::ggplot(apps_data, ggplot2::aes(x = Period_End, y = Total, fill = interaction(Flow_Type, Worker_Type))) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2::scale_x_date(
      breaks = apps_breaks,
      date_labels = "%d %b %Y"
    ) +
    ggplot2::labs(title = "Applications: In & Out of Country", x = "Period", y = "Total", fill = "Flow & Worker Type") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))

  grants_dates <- sort(unique(grants_data$Period_End))
  grants_breaks <- apps_dates[seq(1, length(grants_dates), by = 2)]  # every 2nd date

  grants_plot <- ggplot2::ggplot(grants_data, ggplot2::aes(x = Period_End, y = Total, fill = interaction(Flow_Type, Worker_Type))) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2::scale_x_date(
      breaks = grants_breaks,
      date_labels = "%d %b %Y"
    ) +
    ggplot2::labs(title = "Grants: In & Out of Country", x = "Period", y = "Total", fill = "Flow & Worker Type") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))

  return(list(apps_plot = apps_plot, grants_plot = grants_plot))
}


