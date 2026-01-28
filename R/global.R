library(dplyr)
library(readr)
library(sf)
library(terra)

# setwd("C:/Users/nguyenta/Documents/LEILA/working_code_documentation/code/leila_visualization")
#------------------------------------------------------------------------------#
#                         Read database                                        #
#------------------------------------------------------------------------------#
# Read shape files of station and catchments
hydrologische_indikatoren <- read_csv("data/hydrological_indicators.csv", 
                                      show_col_types = FALSE)

stations <- st_transform(st_read("data/CAMELS_DE_gauging_stations.shp", 
                                 quiet = TRUE), 4326) %>% 
  filter(gauge_id %in% hydrologische_indikatoren$gauge_id)

catchments <- st_transform(st_read("data/CAMELS_DE_catchments.shp", 
                                   quiet = TRUE), 4326) %>% 
  filter(gauge_id %in% hydrologische_indikatoren$gauge_id)

# Read catchment attributes
attributes <- read_csv("data/attributes.csv", show_col_types = FALSE) %>%
  rename(Lat = gauge_lat, Long = gauge_lon) %>%
  select(!c("dams_names", "dams_river_names", "dams_purposes")) %>% 
  filter(gauge_id %in% hydrologische_indikatoren$gauge_id)

hydrologische_indikatoren <- attributes %>% 
  select(Lat, Long, gauge_id) %>% 
  left_join(hydrologische_indikatoren, by = "gauge_id")


#Note the values are Hydrogeologische Einheiten in huek250 map from BGR
huek <- rast("data/huek.tif")

#------------------------------------------------------------------------------#
#                     Ultility functions for server                            #
#------------------------------------------------------------------------------#



