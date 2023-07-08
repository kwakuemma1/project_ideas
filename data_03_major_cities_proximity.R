#| message: false
library(tidyverse) # for data manipulation
library(rstanarm) # for stan_glm stan_polr models
library(brms)

# read the data from website into R
raw_data_languages <- read_csv('https://raw.githubusercontent.com/mrbalikci/endangered-languages/master/data.csv') |>
  # clean the column names: remove spacing and special characters
  janitor::clean_names() |>
  
  # convert values of all character columns to lower case
  mutate_if(is.character, tolower) |>
  
  # some rows have multiple countries: create new rows for such rows
  separate_rows(countries, country_codes_alpha_3, sep = ',') |>
  separate_rows(iso639_3_codes, sep = ',') |>
  distinct(id, countries, country_codes_alpha_3,iso639_3_codes, .keep_all = TRUE) |>
  
  # Clean up country names: Remove anything at and after '(' symbol in the Name column
  mutate(countries = str_replace_all(countries, "\\s*\\(.*", "")) |>
  
  # remove white-spaces from the values of all character columns
  mutate_if(is.character, str_squish)



raw_data_countries <- read_csv('https://raw.githubusercontent.com/mrbalikci/endangered-languages/master/Countries.csv') |>
  # clean the column names: remove spacing and special characters
  janitor::clean_names()  |>
  
  # convert values of all character columns to lower case
  mutate_if(is.character, tolower) |>
  
  # remove white-spaces from the values of all character columns
  mutate_if(is.character, str_squish)



combined_data <- raw_data_languages |>
  left_join(
    raw_data_countries,
    by = c("countries" = "name")
  )



locations_data <- combined_data |>
  dplyr::select(id, latitude, longitude) |>
  distinct() |>
  drop_na()


# https://gist.github.com/curran/13d30e855d48cdd6f22acdf0afe27286

path_major_cities <- "https://gist.githubusercontent.com/curran/13d30e855d48cdd6f22acdf0afe27286/raw/0635f14817ec634833bb904a47594cc2f5f9dbf8/worldcities_clean.csv"

raw_major_city <- read_csv(path_major_cities) |>
  dplyr::select(lat, lng) %>%
  distinct() |>
  drop_na()

colnames(raw_major_city) <- c("lat", "lon")


path_capital_city <- "https://gist.githubusercontent.com/ofou/df09a6834a8421b4f376c875194915c9/raw/355eb56e164ddc3cd1a9467c524422cb674e71a9/country-capital-lat-long-population.csv"

raw_captial_city <- read_csv(path_capital_city) |>
  dplyr::select(Latitude,Longitude) %>%
  distinct() |>
  drop_na()

colnames(raw_captial_city) <- c("lat", "lon")


library(sf)
library(geosphere)

# Function to compute proximity to the nearest major city
compute_proximity <- function(df_major_city, latitude, longitude) {
  
  # Convert the input coordinates to a spatial point
  input_point <- data.frame(lat = latitude, lon = longitude)
  input_point <- st_as_sf(input_point, coords = c("lon", "lat"), crs = 4326)
  
  # Convert the cities dataset to spatial points
  cities_points <- st_as_sf(df_major_city, coords = c("lon", "lat"), crs = 4326)
  
  # Compute the distances between the input point and all cities
  distances <- st_distance(input_point, cities_points)
  
  # Find the index of the nearest city
  nearest_city_index <- which.min(distances)
  
  # Return the nearest distance in km
  return(as.numeric(distances[nearest_city_index]))
}

# Example usage
compute_proximity(df_major_city = raw_major_city,latitude = 40.7128, longitude = -74.0060)


# Apply function on location data

locations_data <- locations_data |>
  mutate(
    proximity_to_major_city = pmap_dbl(
      .l = list(
        df_major_city = list(raw_major_city),
        latitude = latitude, 
        longitude = longitude
      ),
      .f = compute_proximity
    )
  )


locations_data <- locations_data |>
  mutate(
    proximity_to_capital_city = pmap_dbl(
      .l = list(
        df_major_city = list(raw_captial_city),
        latitude = latitude, 
        longitude = longitude
      ),
      .f = compute_proximity
    )
  )


vroom::vroom_write(locations_data, 'locations_data_with_proximity.csv', delim = ',')


combined_data <- combined_data |>
  left_join(
    locations_data,
    by = c("id" = "id", "latitude" = "latitude", "longitude"="longitude")
  ) |>
  mutate(
    proximity_to_major_city = proximity_to_major_city/100000,
    proximity_to_capital_city = proximity_to_capital_city/100000
  )


the_levels_of_endangerment = c("vulnerable", 
                               "definitely endangered",
                               "severely endangered",
                               "critically endangered",
                               "extinct")

combined_data <- combined_data |>
  mutate(
    # Creating a numeric version of degree of endangerment
    degree_of_endangerment_numeric = case_when(
      degree_of_endangerment == "vulnerable" ~ 1,
      degree_of_endangerment == "definitely endangered" ~ 2,
      degree_of_endangerment == "severely endangered" ~ 3,
      degree_of_endangerment == "critically endangered" ~ 4,
      degree_of_endangerment == "extinct" ~ 5
    ),
    #creating an ordred factor version of degree of endangerment 
    degree_of_endangerment_factor = factor(degree_of_endangerment,
                                           levels = the_levels_of_endangerment,
                                           ordered = TRUE)
    
  )


covariates <- c(
  # 'gdppc',
  'literacy',
  'infant_mortality',
  'agriculture',
  # 'population',
  'net_migration',
  'proximity_to_capital_city'
  # ,
  # 'latitude',
  # 'longitude',
  # 'number_of_speakers'
)

model_formula_numeric <- as.formula(
  paste0(
    'degree_of_endangerment_numeric ~ ', 
    paste0(covariates, collapse = '+')
  )
)

model_formula_numeric

summary(lm(model_formula_numeric, data = combined_data))

summary(lm(degree_of_endangerment_numeric ~ proximity_to_major_city, data = combined_data))

library(esquisse)

esquisse::esquisser(
  combined_data %>% dplyr::select(proximity_to_capital_city, degree_of_endangerment_factor)
)


combined_data %>% dplyr::select(degree_of_endangerment_numeric, proximity_to_capital_city) %>%
  drop_na() %>%
  plot()

