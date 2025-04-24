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

#' Read all sheets from an Excel file, except "Cover"
#'
#' @param file_path A character string specifying the path to the Excel file.
#' @return A single dataframe with all sheets combined.
#' @import readxl dplyr purrr
#' @export

### SEC 1 - FUNCTIONS TO READ IN DATA

read_my_data <- function(file_path) {
  # Get all sheet names
  sheet_names <- readxl::excel_sheets(file_path)

  # Exclude sheets named "Cover"
  sheet_names <- sheet_names[!sheet_names %in% "Cover"]

  # Read all remaining sheets into a named list
  data_list <- purrr::map(sheet_names, ~ readxl::read_excel(file_path, sheet = .x))

  # Assign sheet names as list names
  names(data_list) <- sheet_names

  return(data_list)  # Returns a list of data frames
}

### SEC 2.1 - FUNCTIONS TO CLEAN DATA

clean_data <- function(data_list, start_date = "2022-02-28") {
  start_date <- as.Date(start_date)

  if (!is.list(data_list) || length(data_list) == 0) {
    stop("Error: data_list must be a non-empty list of dataframes")
  }

  purrr::map(data_list, function(df) {
    df <- df[-(1:3), ]  # Remove the first 4 rows
    colnames(df) <- as.character(df[1, ])
    df <- df[-1, ]  # Remove the row now used as headers
    colnames(df) <- make.names(colnames(df), unique = TRUE)

    df <- df %>%
      mutate(Week.Commencing = as.Date(as.numeric(Week.Commencing), origin = "1899-12-30")) %>%
      mutate(across(where(is.character), ~ suppressWarnings(as.numeric(.))))

    if (!"Week.Commencing" %in% names(df)) {
      stop("Error: 'Week.Commencing' column is missing")
    }

    df %>%
      mutate(
        Period_End = start_date + (floor(as.numeric(difftime(Week.Commencing, start_date, units = "weeks")) / 4) + 1) * 4 * 7 - 1
      ) %>%
      group_by(Period_End) %>%
      summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
      arrange(Period_End) %>%
      slice(-n())
  })
}

library(ggplot2)

# SEC 2.2 FUNCTIONS TO PRODUCE KEY STATS KEEPING CARE/ SENIOR CARE WORKERS SPLIT FROM CLEANED DATA
#CONTAINS 3 FUNCTIONS
# CHECK_TREND = IDENTIFIES VALUES WE WANT
# CREATE_TREND = IDENTIFIES COLUMNS WE WANT
# CALCULATE_TREND = A FUNCTION WHICH BINDS THE TWO FUNCTIONS ABOVE


# Function to calculate trends for all dataframes and flatten the result
calculate_values <- function(aggregate_data) {
  # Helper: check trend values in a single column
  check_trend <- function(df, column_name) {
    if (!"Period_End" %in% names(df)) return(NA)
    if (!(column_name %in% names(df))) return(NA)

    df <- df %>% mutate(Period_End = as.Date(Period_End))
    df <- df[order(df$Period_End), ]

    if (nrow(df) < 3) return(NA)

    recent_value <- tail(df[[column_name]], 1)
    penultimate_value <- tail(df[[column_name]], 2)[1]
    third_to_last_value <- tail(df[[column_name]], 3)[1]

    list(
      recent_value = recent_value,
      penultimate_value = penultimate_value,
      third_to_last_value = third_to_last_value
    )
  }

  # Helper: create trend values for each relevant column
  create_trends <- function(df_name, df) {
    columns <- c("Total...In.Country", "Total...Out.of.Country")

    purrr::map(setNames(columns, columns), ~check_trend(df, .x)) %>%
      setNames(paste0(df_name, " ", names(.)))
  }
  # Step 1: create key_stats
  key_stats_listed <- purrr::map2(names(aggregate_data), aggregate_data, create_trends)
  key_stats <- unlist(key_stats_listed, use.names = TRUE)

  # Internal helper: sum values by keyword
  sum_trend_value <- function(key_stats, type_keyword, column_keyword, value_type = "recent_value") {
    filtered_names <- names(key_stats)[
      stringr::str_detect(names(key_stats), type_keyword) &
        stringr::str_detect(names(key_stats), column_keyword) &
        stringr::str_detect(names(key_stats), value_type)
    ]
    values <- unlist(key_stats[filtered_names])
    sum(values, na.rm = TRUE)
  }

  # Internal helper: generate summary values from key_stats
  calculate_summary_values <- function(key_stats) {
    type_keywords <- c("Apps", "Grants")
    column_keywords <- c("In.Country", "Out.of.Country")
    value_types <- c("recent_value", "penultimate_value", "third_to_last_value")

    combinations <- expand.grid(
      type = type_keywords,
      column = column_keywords,
      value = value_types,
      stringsAsFactors = FALSE
    )

    labels <- paste(combinations$type, combinations$column, dplyr::case_when(
      combinations$value == "recent_value" ~ "Recent",
      combinations$value == "penultimate_value" ~ "Penultimate",
      combinations$value == "third_to_last_value" ~ "Third to Last"
    ), sep = " - ")

    labels <- gsub("\\.", " ", labels)

    results <- purrr::pmap_dbl(
      combinations,
      function(type, column, value) {
        sum_trend_value(key_stats, type, column, value)
      }
    )

    names(results) <- labels
    as.list(results)
  }

  # Step 2: generate summary values
  summary_values <- calculate_summary_values(key_stats)
  }

calculate_summary_trends <- function(summary_values) {
  categories <- c("Apps - In Country", "Apps - Out of Country",
                  "Grants - In Country", "Grants - Out of Country")

  summary_trends <- lapply(categories, function(category) {
    recent <- summary_values[[paste(category, "Recent", sep = " - ")]]
    penultimate <- summary_values[[paste(category, "Penultimate", sep = " - ")]]
    third <- summary_values[[paste(category, "Third to Last", sep = " - ")]]

    # Calculate trends
    trend_rp <- if (!is.na(recent) && !is.na(penultimate)) {
      if (recent > penultimate) "increased" else if (recent < penultimate) "decreased" else "no change"
    } else NA

    pct_rp <- if (!is.na(recent) && !is.na(penultimate) && penultimate != 0) {
      round(((recent - penultimate) / penultimate) * 100, 0)
    } else NA

    trend_pt <- if (!is.na(penultimate) && !is.na(third)) {
      if (penultimate > third) "increased" else if (penultimate < third) "decreased" else "no change"
    } else NA

    pct_pt <- if (!is.na(penultimate) && !is.na(third) && third != 0) {
      round(((penultimate - third) / third) * 100, 0)
    } else NA

    # Return named list for this category
    list(
      trend_recent_vs_penultimate = trend_rp,
      pct_change_recent_vs_penultimate = pct_rp,
      trend_penultimate_vs_third_to_last = trend_pt,
      pct_change_penultimate_vs_third_to_last = pct_pt
    )
  })

  names(summary_trends) <- categories
  return(summary_trends)
}


