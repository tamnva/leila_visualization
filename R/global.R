library(dplyr)
library(readr)
library(sf)
library(terra)

# setwd("C:/Users/nguyenta/Documents/LEILA/working_code_documentation/code/leila_visualization")
#------------------------------------------------------------------------------#
#                         Read database                                        #
#------------------------------------------------------------------------------#
# Read shape files of station and catchments
stations <- st_transform(st_read("data/CAMELS_DE_gauging_stations.shp", 
                                 quiet = TRUE), 4326) 

catchments <- st_transform(st_read("data/CAMELS_DE_catchments.shp", 
                                   quiet = TRUE), 4326) 

# Read catchment attributes
attributes <- read_csv("data/attributes.csv", show_col_types = FALSE) %>%
  rename(Lat = gauge_lat, Long = gauge_lon) 


# Read default hydrological indicators, can be recalcuated from the interface
hydrologische_indikatoren <- read_csv("data/hydrological_indicators.csv", 
                                      show_col_types = FALSE)

hydrologische_indikatoren <- NULL

#Read Hydrogeologische Einheiten (from huek250 map from BGR)
huek <- rast("data/huek.tif")

#------------------------------------------------------------------------------#
#                     Ultility functions for server                            #
#------------------------------------------------------------------------------#



