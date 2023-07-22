library(dplyr)
##library(purrr)
library(ggplot2)
library(parallel)
library(iterators)
library(foreach)
library(lubridate)
library(zoo)
library(terra)


terraOptions(verbose = TRUE,
             memfrac = 0.9)

cores <- 8
rolling_window <- 7
threshold <- 0.1
fire_season_start <- 93
fire_season_end <- 289
## ncpath <- "/home/steve/Downloads/thredds/Deficit_NorESM1-M_rcp45_2024subset.nc"
##in_path <- "/media/smithers/shuysman/data/nps_gridded_wb/gye/forecasts/"
home_path <- Sys.getenv("HOME")
in_path <- paste0(home_path, "/data/gye/forecasts/")
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

                ncpath <- paste0(in_path, "Deficit_", model, "_", scenario, "_", year, "subset.nc")
                wbdata_future <- terra::rast(ncpath)
                
                wbdata_future_smoothed <- terra::roll(wbdata_future, n = rolling_window, fun = sum, type = "to", circular = FALSE)

                terra::time(wbdata_future_smoothed) <- terra::time(wbdata_future)
                
                wbdata_future_smoothed <- wbdata_future_smoothed %>%
                    subset(yday(time(.)) >= fire_season_start) %>%
                    subset(yday(time(.)) <= fire_season_end)

                r_smoothed_plus_historical <- c(wbdata_future_smoothed, wbdata_historical_smoothed)
                r_percentiles <- terra::app(r_smoothed_plus_historical,
                                            fun = function(x) percent_rank(x), cores = 1
                                            )

                terra::time(r_percentiles) <- terra::time(r_smoothed_plus_historical)
                ranked_year <- subset(r_percentiles, year(time(r_percentiles)) == year)
                
                fire_risk <- terra::app(ranked_year, fun = function(x) ecdf_regression(x), cores = 1)

                out_file <- paste0(out_path, "Fire_Risk_Deficit_all_days_", model, "_", scenario, "_", year, ".nc")
                writeCDF(days_above_fire_risk, filename = out_file, overwrite = TRUE)

                out_file
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
