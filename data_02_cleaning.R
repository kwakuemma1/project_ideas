#' Kwaku Adu
#' 6/30/2023
#' 
#' Purpose: Scripts to clean data for the project
#' Project: Interactively visualize endangered languages


# Data Manipulation
library(tidyverse) # For data manipulation
library(janitor)   # For clean column names, and other utilities
library(glue)      # String manipulation

# Interactive Maps
library(plotly)    # For interactive plots
library(leaflet)   # For interactive maps

# Map packages
library(sf)        # For map shape files
library(tigris)   
library(rnaturalearth)
library(ggspatial)

# Plotting packages
library(scales)
library(patchwork)

# Collect/read the data
source('data_01_collect_data.R')
