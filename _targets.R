library(targets)
source("functions.R")
source("charts.R")

username <- Sys.getenv("USERNAME")
filepath <- file.path(
  "C:/Users", username,
  "OneDrive - Department of Health and Social Care/Git/first_pipeline/data",        #Change if neccesary
  "2025-03-01-OFF-SENS-DHSC-SOC-6145&6146-Table.xlsx"                               #Change this line
)

list(
  tar_target(data_list,
             read_my_data(filepath)), #reads in weekly data

  tar_target(aggregate_data,
    clean_data(data_list)),       #produces monthly data split by occupation (we want to retain this)

  tar_target(summary_values,
             calculate_values(aggregate_data)), #summary values which include, for example, most recent observation *split* by occupation

  tar_target(summary_trends,
             calculate_summary_trends(summary_values)),  #summary trends across all occupations which include, for example, most recent percentage increase

  tar_target(stacked_charts,
             generate_stacked_charts(aggregate_data)) # Generate the stacked plots previously produced for the MI data reports from the list of aggregated data
)
