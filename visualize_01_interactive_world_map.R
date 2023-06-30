#' Kwaku Adu
#' 6/30/2023
#' 
#' Purpose: Scripts to visualize data for the project
#' Project: Interactively visualize endangered languages


# Data Manipulation
library(tidyverse) # For data manipulation
library(janitor)   # For clean column names, and other utilities
library(glue)      # String manipulation

# Interactive Maps
library(htmltools)
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
library(RColorBrewer)

# Collect/read the data
source('data_02_cleaning.R')


elp_data_order_of_severity = c(
  "At risk",
  "Vulnerable",
  "Endangered",
  "Severely Endangered",
  "Critically Endangered",
  "Threatened",
  "Dormant or Extinct",
  "Awakening",
  "Vitality Unknown"
)

map_data <- elp_data %>% 
  dplyr::select(latitude, longitude, endangerment_degree, confidence_in_degree, name_in_the_language) %>%
  mutate(endangerment_degree = factor(endangerment_degree, levels = elp_data_order_of_severity)) %>%
  mutate(confidence_in_degree = rescale(confidence_in_degree, to = c(5, 10))) %>%
  # drop_na() %>%
  I()

color_pal <- colorFactor(
  palette = c(
    'green',
    'orange',
    '#D73377',
    '#4A0D67', 
    'red',
    '#6F0C17',
    'black',
    '#002642',
    'grey'
    
  ),
  # "YlGnBu",
  domain = map_data$endangerment_degree
)



# Begin Plot --------------------------------------------------------------



interactive_map <- leaflet() %>%
  addTiles()

interactive_map <- interactive_map %>% 
  addCircleMarkers(
    data = map_data,  # Use your data frame
    lng = ~longitude,  # Column name for longitude
    lat = ~latitude,  # Column name for latitude
    color = ~color_pal(endangerment_degree),  # Column name for the categorical variable
    radius = ~confidence_in_degree,  # Size of the markers
    stroke = FALSE,  # Remove stroke around markers
    fillOpacity = 0.7,  # Opacity of the markers
    label = ~name_in_the_language #endangerment_degree  # Display the category label on hover
  )



tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("Classification of Endangered Languages across the World.<br>Size of radius represent degree of belief in classification")
)  

# title <- tags$div(
#   HTML("Classification of Endangered Languages across the World.<br>Size of radius represent degree of belief in classification")
# )  


interactive_map %>%
  addLegend(data = map_data,
            "topright", pal = color_pal, values = ~endangerment_degree,
            title = "Degree of Endangerment",
            opacity = 1
  ) %>%
  addControl(title, position = "topleft", className="map-title")
