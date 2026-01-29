library(leaflet)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(sf)
library(terra)
library(tibble)
library(DT)
library(spsComps)


# setwd("C:/Users/nguyenta/Documents/LEILA/working_code_documentation/code/leila_visualization")
#------------------------------------------------------------------------------#
#                         Read database                                        #
#------------------------------------------------------------------------------#
timeseries_camels_combine_file = "data/CAMELS_DE_hydromet_timeseries_combine.csv"

# Read catchment attributes
attributes <- read.csv("data/attributes.csv", header = TRUE, sep = ",") %>%
  as_tibble() %>% rename(lat = gauge_lat, long = gauge_lon)

# Read shape files of station and catchments
stations <- st_transform(st_read("data/CAMELS_DE_gauging_stations.shp", 
                                 quiet = TRUE), 4326) %>%
  mutate(long = st_coordinates(geometry)[1],
        lat = st_coordinates(geometry)[2])

catchments <- st_transform(st_read("data/CAMELS_DE_catchments.shp", 
                                   quiet = TRUE), 4326) 


# Read default hydrological indicators, can be recalcuated from the interface
hydrologische_indikatoren <- read_csv("data/hydrological_indicators.csv", 
                                      show_col_types = FALSE)

hydrologische_indikatoren <- NULL

#Read Hydrogeologische Einheiten (from huek250 map from BGR)
huek <- rast("data/huek.tif")

#------------------------------------------------------------------------------#
#                     Ultility functions for server                            #
#------------------------------------------------------------------------------#



