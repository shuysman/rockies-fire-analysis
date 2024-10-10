## Extract daily VPD data from gridMET for fire ignition sites
library(tidyverse)
library(sf)
library(ncdf4)
library(raster)
library(cmsafops)

url <- "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_vpd_1979_CurrentYear_CONUS.nc#fillmismatch"

ds <- nc_open(url)

#(sites <- read.csv('./rockies_fire_points.csv') %>% 
#     st_as_sf(coords = c("Lon", "Lat"), crs = 4326))

sites <- read.csv('./rockies_fire_points.csv')

selpoint.multi("daily_mean_vapor_pressure_deficit", nc=ds, outpath = "./gridmet-out", lon1 = sites$Lon, lat1 = sites$Lat, station_names = sites$Event_ID, format = "csv", verbose = TRUE)
