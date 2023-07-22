library(dplyr)
##library(purrr)
library(ggplot2)
library(parallel)
library(iterators)
library(foreach)
library(lubridate)
library(zoo)
library(terra)

## Precalculate historical and future rolling sum files with rolling_sums_future.R and rolling_sums_historical.R scripts first
## Set future_path, and hist_path to the dirs containing these files.
## Same rolling window should be used to run both scripts

terraOptions(verbose = TRUE,
             memfrac = 0.9)

cores <- 12
rolling_window <- 7
threshold <- 0.1
fire_season_start <- 93
fire_season_end <- 289
## ncpath <- "/home/steve/Downloads/thredds/Deficit_NorESM1-M_rcp45_2024subset.nc"
##in_path <- "/media/smithers/shuysman/data/nps_gridded_wb/gye/forecasts/"
home_path <- Sys.getenv("HOME")
future_path <- paste0(home_path, "/data/gye/forecasts/rolling_sum/")
##hist_path <- "/media/smithers/shuysman/data/nps_gridded_wb/gye/historical/"
hist_path <- paste0(home_path, "/data/gye/historical/rolling_sum/")
##out_path <- "/media/smithers/shuysman/data/out/fire/"
out_path <- "./out/"
## nc_data_path <- "/home/steve/Downloads/thredds/"

## models <- c('NorESM1-M', 'MRI-CGCM3','MIROC-ESM-CHEM', 'MIROC5','IPSL-CM5A-LR','inmcm4','HadGEM2-CC365','CSIRO-Mk3-6-0','CNRM-CM5','CanESM2', 'BNU-ESM','CCSM4', 'GFDL-ESM2G')
models <- c("NorESM1-M", "MRI-CGCM3", "MIROC5", "IPSL-CM5A-LR", "inmcm4", "HadGEM2-CC365", "CSIRO-Mk3-6-0", "CNRM-CM5", "CanESM2", "BNU-ESM", "CCSM4", "GFDL-ESM2G") ## MIROC-ESM-CHEM >2070 rcp45 futures are missing

scenarios <- c("rcp85", "rcp45")

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

cl <- makeCluster(cores)
doParallel::registerDoParallel(cl)

start.time <- Sys.time()

ncpaths_historical_smoothed <- list.files(path = hist_path, pattern = "Deficit.*.nc", full.names = TRUE)

foreach(model = iter(models)) %:%
    foreach(scenario = iter(scenarios)) %:%
    foreach(year = iter(years),
            .inorder = TRUE,
            .export = c("rolling_window", "fire_season_start", "fire_season_end", "ecdf_regression"), ### Probably don't need these exports
            .packages = c("terra", "lubridate", "dplyr")
            ) %dopar% {
                print(model)
                print(scenario)
                print(year)
                
                wbdata_historical_smoothed <- rast(ncpaths_historical_smoothed) %>%
                    subset(year(time(.)) <= 2021) %>%
                    subset(yday(time(.)) >= fire_season_start) %>%
                    subset(yday(time(.)) <= fire_season_end)
                
                ncpath_future_smoothed <- paste0(future_path, "Deficit_rolling_sum_", rolling_window, "_", model, "_", scenario, "_", year, ".nc")
                wbdata_future_smoothed <- terra::rast(ncpath_future_smoothed)

                wbdata_future_smoothed <- wbdata_future_smoothed %>%
                    subset(yday(time(.)) >= fire_season_start) %>%
                    subset(yday(time(.)) <= fire_season_end)

                ## Loop through each day in the year, and rank each day separately against the historical deficit rolling sums
                ## Return a SpatRaster with each layer having that day's percentiles compared to historical values
                ## "How extreme is this day compared to the history of the pixel?"
                ranked_year <- rast()
                for (day in fire_season_start:fire_season_end) {
                    future_day <- wbdata_future_smoothed %>% subset(yday(time(.)) == day)
                    future_day_plus_historical <- c(future_day, wbdata_historical_smoothed)
                    percentiles <- terra::app(future_day_plus_historical,
                                            fun = function(x) percent_rank(x), cores = 1
                                            )
                    terra::time(r_percentiles) <- terra::time(future_day_plus_historical)
                    ranked_day <- subset(percentiles, yday(time(percentiles)) == yday)
                    ranked_year <- c(ranked_year, ranked_day)
                }

                out_file_ranks <- paste0(out_path, "Percentile_Rank_Deficit_all_days_", model, "_", scenario, "_", year, ".nc")
                writeCDF(ranked_year, filename = out_file_ranks, overwrite = TRUE)
                
                fire_risk <- terra::app(ranked_year, fun = function(x) ecdf_regression(x), cores = 1)

                out_file_fire_risk <- paste0(out_path, "Fire_Risk_Deficit_all_days_", model, "_", scenario, "_", year, ".nc")
                writeCDF(days_above_fire_risk, filename = out_file_fire_risk, overwrite = TRUE)

                out_file_fire_risk
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
