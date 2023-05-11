## Extract historical daily tp  data from gridMET for fire ignition sites
library(tidyverse)
library(sf)
library(ncdf4)
library(raster)
library(cmsafops)


## Gridmet variable names used by urls
gridmet_vars = c("pr",            ## Precipitation (mm)
                 "tmmx",          ## Tmax (K)
                 "tmmn",          ## Tmin (K)
                 "rmax",          ## RH Max(%)
                 "rmin",          ## RH Min (%)
                 "vpd")           ## VPD kPa


## Gridmet variable labels used internally on ncdf4 files
gridmet_labels <- c("precipitation_amount",
                    "daily_maximum_temperature",
                    "daily_minimum_temperature",
                    "daily_maximum_relative_humidity",
                    "daily_minimum_relative_humidity",
                    "daily_mean_vapor_pressure_deficit")



sites <- read.csv('./Southern_rockies_fire_points.csv')

##for (i in 1:length(gridmet_vars)) {
for (i in 6) {
    url <- paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_", gridmet_vars[i], "_1979_CurrentYear_CONUS.nc#fillmismatch")
    ds <- nc_open(url)
    selpoint.multi(gridmet_labels[i], nc=ds, outpath = paste0("./gridmet-",gridmet_vars[i]), lon1 = sites$Lon, lat1 = sites$Lat, station_names = sites$Event_ID, format = "csv", verbose = TRUE)
}


