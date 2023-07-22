library(dplyr)
##library(purrr)
library(ggplot2)
library(parallel)
library(iterators)
##library(foreach)
library(lubridate)
library(zoo)
library(terra)


terraOptions(verbose = TRUE,
             memfrac = 0.9)

rolling_window <- 7
threshold <- 0.1
fire_season_start <- 93
fire_season_end <- 289
## ncpath <- "/home/steve/Downloads/thredds/Deficit_NorESM1-M_rcp45_2024subset.nc"
##in_path <- "/media/smithers/shuysman/data/nps_gridded_wb/gye/forecasts/"
home_path <- Sys.getenv("HOME")
in_path <- paste0(home_path, "/data/gye/forecasts")
##hist_path <- "/media/smithers/shuysman/data/nps_gridded_wb/gye/historical/"
hist_path <- paste0(home_path, "/data/gye/historical/rolling_sum/")
##out_path <- "/media/smithers/shuysman/data/out/fire/"
out_path <- "./out/"
## nc_data_path <- "/home/steve/Downloads/thredds/"

## models <- c('NorESM1-M', 'MRI-CGCM3','MIROC-ESM-CHEM', 'MIROC5','IPSL-CM5A-LR','inmcm4','HadGEM2-CC365','CSIRO-Mk3-6-0','CNRM-CM5','CanESM2', 'BNU-ESM','CCSM4', 'GFDL-ESM2G')
models <- c("NorESM1-M", "MRI-CGCM3", "MIROC5", "IPSL-CM5A-LR", "inmcm4", "HadGEM2-CC365", "CSIRO-Mk3-6-0", "CNRM-CM5", "CanESM2", "BNU-ESM", "CCSM4", "GFDL-ESM2G") ## MIROC-ESM-CHEM >2070 rcp45 futures are missing
##model <- c("NorESM1-M")

scenarios <- c("rcp85", "rcp45")
##scenario <- c("rcp45")

## years <- seq(2024, 2050)
years <- seq(2023, 2099)

ecdf_regression <- function(x) {
    ## ECDF function generated from dryness_script.Rmd.  This function
    ## was generated for 7 day rolling windows and forested cover
    ## type.  To use a different length for rolling calculations and
    ## different cover types a different regression function should be
    ## used here.
    e <- exp(1)
    return(e^(-10.38 + 29.98 * x + -45.25 * x^2 + 25.65 * x^3))
}

## rank_day_historical <- function(wb_day) {
##     ## Add layer for single day to historical data set, calculate percent_rank and return percent rankile for input day
##     ## We want to rank each day individually against the history, to see how dry each day in projection is relative
##     ## to the history
##     t <- time(wb_day)
##     r <- c(wb_day, wbdata_historical)
##     r <- terra::app(r, fun = function(x) percent_rank(x), cores = cl)
##     r_day <- subset(r, 1)
##     time(r_day) <- t
##     return(r_day)
## }

## rolling_sum_ncpaths <- function(ncpaths) {
##     ## r <- foreach(i = 1:length(ncpaths), .combine = c) %dopar% {
##     ##     terra::roll(terra::rast(ncpaths[i]), n = rolling_window, fun = sum, type = "to", circular = FALSE)
##     ## }
##     ## return(r)
##     r <- rast()
##     for (ncpath in ncpaths) {
##         r2 <- terra::rast(ncpath)
##         r2_smooth <- terra::roll(r2, n = rolling_window, fun = sum, type = "to", circular = FALSE)
##         r <- c(r, r2_smooth)
##     }
##     return(r)
## }

cl <- makeCluster(48)
##doParallel::registerDoParallel(cl)
clusterExport(cl, "ecdf_regression")
clusterExport(cl, "percent_rank")
clusterExport(cl, "if_else")
clusterExport(cl, "fire_season_start")
clusterExport(cl, "fire_season_end")
clusterExport(cl, "threshold")
clusterExport(cl, "rolling_window")
clusterExport(cl, "rollapply")

start.time <- Sys.time()

ncpaths_historical_smoothed <- list.files(path = hist_path, pattern = "Deficit.*.nc", full.names = TRUE)
wbdata_historical_smoothed <- rast(ncpaths_historical_smoothed) %>%
    subset(year(time(.)) <= 2021) %>%
    subset(yday(time(.)) >= fire_season_start) %>%
    subset(yday(time(.)) <= fire_season_end)

for (model in models) {
    print(model)
    for (scenario in scenarios) {
        print(scenario)
        for (year in years) {
            print(year)
           
            ncpath <- paste0(in_path, "Deficit_", model, "_", scenario, "_", year, "subset.nc")
            wbdata_future <- terra::rast(ncpath) %>% subset(year(time(.)) >= 2022)
            
            r <- subset(wbdata_future, year(time(wbdata_future)) == year)

            ##wbdata_future_smoothed <- terra::roll(wbdata_future, n = rolling_window, fun = sum, type = "to", circular = FALSE) %>% subset(any(day(time(.)) >= fire_season_start, day(time(.)) <= fire_season_end))

            wbdata_future_smoothed <- terra::app(r, fun = function (x) rollapply(x, rolling_window, sum, by = 1, partial = FALSE, fill = NA, align = "right"), cores = cl)
            
            terra::time(wbdata_future_smoothed) <- terra::time(r)
            
            wbdata_future_smoothed <- wbdata_future_smoothed %>%
                subset(yday(time(.)) >= fire_season_start) %>%
                subset(yday(time(.)) <= fire_season_end)

            r_smoothed_plus_historical <- c(wbdata_future_smoothed, wbdata_historical_smoothed)
            r_percentiles <- terra::app(r_smoothed_plus_historical,
                                        fun = function(x) percent_rank(x), cores = cl
                                        )

            terra::time(r_percentiles) <- terra::time(r_smoothed_plus_historical)
            ranked_year <- subset(r_percentiles, year(time(r_percentiles)) == year)
            
            ##wbdata_future_smoothed <- terra::roll(wbdata_future, n = rolling_window, fun = sum, type = "to", circular = FALSE)
            ##wbdata_future_smoothed <- rolling_sum_ncpaths(ncpaths_future)
            
            ##wbdata_future_smoothed <- wbdata_future_smoothed %>%
            ##    subset(any(day(time(.)) >= fire_season_start, day(time(.)) <= fire_season_end))
            
            ## wbdata_percentiles <- terra::app(wbdata_smoothed,
            ##     fun = function(x) percent_rank(x), cores = cl
            ## )
            
            above_fire_risk <- terra::app(ranked_year, fun = function(x) if_else(ecdf_regression(x) >= threshold, 1, 0), cores = cl)
            terra::time(above_fire_risk) <- terra::time(ranked_year)

            days_above_fire_risk <- terra::tapp(above_fire_risk, index = "years", fun = "sum", na.rm = TRUE)

            mean_days_above_fire_risk <- mean(days_above_fire_risk, na.rm = TRUE)

            writeCDF(days_above_fire_risk, filename = paste0(out_path, "Days_Above_Fire_Risk_Deficit_all_days_", model, "_", scenario, "_", year, ".nc"), overwrite = TRUE)
            writeCDF(mean_days_above_fire_risk, filename = paste0(out_path, "Mean_Days_Above_Fire_Risk_Deficit_", model, "_", scenario, "_", years[1], "-", tail(years, 1), ".nc"), overwrite = TRUE)
        }
    }
}

stopCluster(cl)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken


## ggplot(as.data.frame(days_above_fire_risk_all_years, xy = TRUE)) +
##     geom_raster(mapping = aes(x = x, y = y, fill = mean)) +
##     ##   geom_sf(data = gye_boundary_lcc, alpha = 0.15, fill = NA, lwd = 2) +
##     theme_bw() +
##     scico::scale_fill_scico(palette = "bilbao") +
##     ggtitle("Days above fire danger threshold")
