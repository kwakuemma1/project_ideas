#' Kwaku Adu
#' 6/27/2023
#' 
#' Purpose: Scripts to clean data for the project
#' Project: Interactively visualize endangered languages

library(tidyverse)

# URL links to datasets
path_languages = 'https://raw.githubusercontent.com/mrbalikci/endangered-languages/master/data.csv'
path_gdp = "https://raw.githubusercontent.com/mrbalikci/endangered-languages/master/GDP.csv"
path_countries = "https://raw.githubusercontent.com/mrbalikci/endangered-languages/master/Countries.csv"

# Read raw data files
# raw_data_languages = read_csv(path_languages) |> janitor::clean_names()
# raw_data_gdp = read_csv(path_gdp) |> janitor::clean_names()
# raw_data_countries = read_csv(path_countries) |> janitor::clean_names()

read_raw_data <- function(path_name){
  
  # Extract path value
  path = unlist(mget(path_name, envir = .GlobalEnv))
  
  print('hey')
  
  # Read data using path value
  the_data = read_csv(path) |> janitor::clean_names()
  
  # Extract suffix of path_name
  suffix = unlist(strsplit(path_name, "_"))[2]
  
  # Use suffix of path_name to name the data
  filename = paste0("raw_data_", suffix)
  
  # Assign the data to the environment
  assign(filename, the_data, envir = .GlobalEnv)
  
  
}

# Collect all path names
all_path_names = ls()[grep("^path_", ls())]


walk(all_path_names, read_raw_data)


