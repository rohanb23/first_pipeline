library(targets)
source("functions.R")
source("charts.R")

username <- Sys.getenv("USERNAME")

data_folder <- file.path(
  "C:/Users", username,
  "OneDrive - Department of Health and Social Care/Git/first_pipeline/data"                                                    #Change this line
)

# Match files starting with a date
files <- list.files(data_folder, pattern = "^\\d{4}-\\d{2}-\\d{2}-visa-data\\.xlsx$", full.names = TRUE)
if (length(files) == 0) stop("No visa data files found.")

# Extract the date from the start of the filename
dates <- as.Date(substr(basename(files), 1, 10), format = "%Y-%m-%d")
latest_file <- files[which.max(dates)]

filepath <- latest_file

output_folder <- file.path(
  "C:/Users", username,
  "OneDrive - Department of Health and Social Care/Git/first_pipeline"                                                    #Change this line
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
             generate_stacked_charts(aggregate_data,output_folder)) # Generate the stacked plots previously produced for the MI data reports from the list of aggregated data
)
