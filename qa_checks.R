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
library(readr)

quality_assurance_report <- function(cleaned_data, aggregate_data, output_file = "QA_Report.csv") {

  # Summarise input data (cleaned_data) for each dataset in the list
  input_summary <- map_dfr(cleaned_data, ~ summarise(
    .x,
    Total_Records = n(),
    Missing_Week_Com = sum(is.na(Week.Commencing)),
    Num_Variables = ncol(.x) - 1,
    Total_Sum_All = sum(across(where(is.numeric), sum, na.rm = TRUE))
  ), .id = "Dataset")

  # Summarise aggregated output data (aggregated_data) for each dataset in the list
  output_summary <- map_dfr(aggregate_data, ~ summarise(
    .x,
    Total_Records = n(),
    Missing_Period_End = sum(is.na(Period_End)),
    Num_Variables = ncol(.x) - 1,
    Total_Sum_All = sum(across(where(is.numeric), sum, na.rm = TRUE))
  ), .id = "Dataset")

  # Verify if totals match before and after aggregation
  total_check <- left_join(input_summary, output_summary, by = "Dataset", suffix = c("_Input", "_Output")) %>%
    mutate(
      Records_Difference = Total_Records_Input - Total_Records_Output,
      Total_Sum_Difference = Total_Sum_All_Input - Total_Sum_All_Output
    ) %>%
    select(Dataset, Records_Difference, Total_Sum_Difference)

  # Identify extreme values for each dataset
  extreme_values <- map_dfr(aggregate_data, ~ .x %>%
                              summarise(
                                Min_Value = min(unlist(select(., where(is.numeric))), na.rm = TRUE),
                                Max_Value = max(unlist(select(., where(is.numeric))), na.rm = TRUE),
                                Mean_Value = mean(unlist(select(., where(is.numeric))), na.rm = TRUE),
                                SD_Value = sd(unlist(select(., where(is.numeric))), na.rm = TRUE)
                              ), .id = "Dataset")

  # Create summary QA table
  qa_table <- bind_rows(
    tibble(Section = "Input Data Summary", input_summary),
    tibble(Section = "Aggregated Output Summary", output_summary)
  )

  # Save report as CSV
  write_csv(bind_rows(qa_table, total_check, extreme_values), output_file)


  return(list(QA_Summary = qa_table, Total_Check = total_check, Extreme_Values = extreme_values))
}
