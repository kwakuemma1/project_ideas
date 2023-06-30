#' Kwaku Adu
#' 6/27/2023
#' 
#' Purpose: Scripts to read data for the project
#' Project: Interactively visualize endangered languages

library(tidyverse)

# URL links to datasets
path_languages = 'https://raw.githubusercontent.com/mrbalikci/endangered-languages/master/data.csv'
path_gdp = "https://raw.githubusercontent.com/mrbalikci/endangered-languages/master/GDP.csv"
path_countries = "https://raw.githubusercontent.com/mrbalikci/endangered-languages/master/Countries.csv"



read_raw_data <- function(path_name){
  
  # Extract path value
  path = unlist(mget(path_name, envir = .GlobalEnv))
  
  
  # Read data using path value
  the_data = read_csv(path) |> 
    
    # clean the column names: remove spacing and special characters
    janitor::clean_names()
  
  # Extract suffix of path_name
  suffix = unlist(strsplit(path_name, "_"))[2]
  
  # Use suffix of path_name to name the data
  filename = paste0("raw_data_", suffix)
  
  # Assign the data to the environment
  assign(filename, the_data, envir = .GlobalEnv)
  
  
}

# Collect all path names
all_path_names = ls()[grep("^path_", ls())]


# Read all data and assign to global environment
walk(all_path_names, read_raw_data)


