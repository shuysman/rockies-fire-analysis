library(dplyr)
library(stringr)
library(ggplot2)
library(parallel)
library(lubridate)
library(zoo)
library(terra)

## Precalculate historical and future rolling sum files with rolling_sums_future.R and rolling_sums_historical.R scripts first
## Set future_path, and hist_path to the dirs containing these files.
## Same rolling window should be used to run both scripts

terraOptions(verbose = TRUE,
             memfrac = 0.9)

args <- commandArgs(TRUE)
future_file <- args[1]
split <- str_split(future_file, "_")[[1]]
model <- split[6]
scenario <- split[7]
year <- str_split(split[8], "\\.")[[1]][1]

print(model)
print(scenario)
print(year)

cores <- 32
rolling_window <- 7
threshold <- 0.1
fire_season_start <- 93
fire_season_end <- 289
## ncpath <- "/home/steve/Downloads/thredds/Deficit_NorESM1-M_rcp45_2024subset.nc"
##in_path <- "/media/smithers/shuysman/data/nps_gridded_wb/gye/forecasts/"
home_path <- Sys.getenv("HOME")
##future_path <- paste0(home_path, "/data/gye/forecasts/rolling_sum/")
##future_path <- paste0("/media/smithers/shuysman/data/nps_gridded_wb/gye/forecasts/rolling_sum/")
##hist_path <- "/media/smithers/shuysman/data/nps_gridded_wb/gye/historical/rolling_sum/"
hist_path <- paste0(home_path, "/data/gye/historical/rolling_sum/")
##out_path <- "/media/smithers/shuysman/data/out/fire/"
out_path <- "./out/"
## nc_data_path <- "/home/steve/Downloads/thredds/"

## models <- c('NorESM1-M', 'MRI-CGCM3','MIROC-ESM-CHEM', 'MIROC5','IPSL-CM5A-LR','inmcm4','HadGEM2-CC365','CSIRO-Mk3-6-0','CNRM-CM5','CanESM2', 'BNU-ESM','CCSM4', 'GFDL-ESM2G')
##models <- c("NorESM1-M", "MRI-CGCM3", "MIROC5", "IPSL-CM5A-LR", "inmcm4", "HadGEM2-CC365", "CSIRO-Mk3-6-0", "CNRM-CM5", "CanESM2", "BNU-ESM", "CCSM4", "GFDL-ESM2G") ## MIROC-ESM-CHEM >2070 rcp45 futures are missing

##scenarios <- c("rcp85", "rcp45")

ecdf_regression <- function(x) {
    ## ECDF function generated from dryness_script.Rmd.  This function
    ## was generated for 7 day rolling windows and forested cover
    ## type.  To use a different length for rolling calculations and
    ## different cover types a different regression function should be
    ## used here.
    e <- exp(1)
    return(e^(-10.38 + 29.98 * x + -45.25 * x^2 + 25.65 * x^3))
}

start.time <- Sys.time()

cl <- makeCluster(cores)
clusterExport(cl, "percent_rank")
clusterExport(cl, "ecdf_regression")

ncpaths_historical_smoothed <- list.files(path = hist_path, pattern = "Deficit.*.nc", full.names = TRUE)

wbdata_historical_smoothed <- rast(ncpaths_historical_smoothed) %>%
    subset(year(time(.)) <= 2021) %>%
    subset(yday(time(.)) >= fire_season_start) %>%
    subset(yday(time(.)) <= fire_season_end)

wbdata_future_smoothed <- terra::rast(future_file)

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
                              fun = function(x) percent_rank(x), cores = cl
                              )
    terra::time(percentiles) <- terra::time(future_day_plus_historical)
    ranked_day <- percentiles %>%
        subset(year(time(.)) == year) %>%
        subset(yday(time(.)) == day)
    ranked_year <- c(ranked_year, ranked_day)
}

out_file_ranks <- paste0(out_path, "Percentile_Rank_Deficit_all_days_", model, "_", scenario, "_", year, ".nc")
writeCDF(ranked_year, filename = out_file_ranks, overwrite = TRUE)

fire_risk <- terra::app(ranked_year, fun = function(x) ecdf_regression(x), cores = cl)

out_file_fire_risk <- paste0(out_path, "Fire_Risk_Deficit_all_days_", model, "_", scenario, "_", year, ".nc")
writeCDF(days_above_fire_risk, filename = out_file_fire_risk, overwrite = TRUE)

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
