## This script develops projections using climate futures downloaded from the NPS gridded water balance data set thredds server.  It is based on raster, and cuases my machine to run out of disk space on long runs.  I rewrote it using terra, which performs faster and does not use any disk space.  This script is kept as an example of how to accomplish this with the raster library.

library(ncdf4)
library(raster)
library(sf)
library(tidyverse)
##library(zoo) ## rollapply()
library(ggmap)
library(rgdal)
library(leaflet)
library(leaflet.extras)

rasterOptions(chunksize = 1e+10, maxmemory = 4e+10)
              

fire_season_start <- 93
fire_season_end <- 289

rolling_window <- 7

threshold <- 0.10

ecdf_regression <- function (x) {
    ## ECDF function generated from dryness_script.Rmd.  This function
    ## was generated for 7 day rolling windows and forested cover
    ## type.  To use a different length for rolling calculations and
    ## different cover types a different regression function should be
    ## used here.
    e <- exp(1)
    return(e^(-10.38 + 29.98*x + -45.25*x^2 + 25.65*x^3))
}

##curve(ecdf_regression(x))

start.time <- Sys.time()

out_path <- "/home/steve/Downloads/thredds/out/"
##nc_data_path <- "/home/steve/Downloads/thredds/"
models <- c('NorESM1-M', 'MRI-CGCM3','MIROC-ESM-CHEM', 'MIROC5','IPSL-CM5A-LR','inmcm4','HadGEM2-CC365','CSIRO-Mk3-6-0','CNRM-CM5','CanESM2', 'BNU-ESM','CCSM4', 'GFDL-ESM2G')
##models <- c("NorESM1-M")
##scenarios <- c('rcp85', 'rcp45')
scenarios <- c("rcp45")

years <- seq(2024, 2050)
##years2 <- seq(2051, 2099)

##ncfiles <- list.files(path = nc_data_path, pattern="*.nc")

lcc_crs <- st_crs("+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
gye_boundary <- st_read("/home/steve/OneDrive/whitebark/gyeboundary/GYE_boundary_dd.shp")
gye_boundary_lcc <- st_transform(gye_boundary, lcc_crs)

beginCluster(n=12)
for (model in models) {
    print(model)
    for (scenario in scenarios) {
        print(scenario)
        all_years_brick <- brick()
        for (year in years) {
            ncpath <- paste0("/home/steve/Downloads/thredds/Deficit_", model, "_", scenario, "_", year, "subset.nc")
            print(ncpath)

            x <- brick(ncpath)

            ##x <- crop(b, ext)

            ## Using raster::movingFun instead of zoo:rollapply() here, need to test to see if results are the same
            x2 <- clusterR(x,
                           fun = calc,
                           args = list(fun = function (x) movingFun(x, n = rolling_window, fun = sum, type = "to", circular = FALSE)),
                           export = 'rolling_window')

            x3 <- clusterR(x2,
                           fun = calc,
                           args = list(fun = function (x) percent_rank(x)))

            fire_risk <- clusterR(x3,
                                  fun = calc,
                                  args = list(fun = function (x) if_else(ecdf_regression(x) >= threshold, 1, 0)),
                                  export = c('threshold', 'ecdf_regression'))

            fire_risk_only_fire_season_days <- raster::calc(subset(fire_risk, seq(fire_season_start, fire_season_end)), sum, na.rm = TRUE)
            all_years_brick <- addLayer(all_years_brick, fire_risk_only_fire_season_days)
            crs(all_years_brick) <- crs(x)
        }

        fire_risk_all_years <- clusterR(all_years_brick,
                                        fun = calc,
                                        args = list(fun = function (x) mean(x, na.rm = TRUE)))

        writeRaster(fire_risk_all_years, paste0(out_path, "Deficit_", model, "_", scenario, "_", years[1], "-", tail(years, 1), ".nc"))

        removeTmpFiles(h=1.5)
    }
}
endCluster()

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken


ggplot(as.data.frame(fire_risk_all_years, xy = TRUE)) +
    geom_raster(mapping = aes(x = x, y = y, fill = layer)) +
 ##   geom_sf(data = gye_boundary_lcc, alpha = 0.15, fill = NA, lwd = 2) +
    theme_bw() +
    scico::scale_fill_scico(palette = "bilbao") +
    ggtitle("Days above fire danger threshold")
