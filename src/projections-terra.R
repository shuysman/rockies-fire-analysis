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
model <- args[1]
scenario <- args[2]

##future_file <- "/media/smithers/shuysman/data/nps_gridded_wb/gye/forecasts/rolling_sum/Deficit_rolling_sum_7_BNU-ESM_rcp45_2033.nc"
##model <- "BNU-ESM"
##scenario <- "rcp45"
##year <- 2033

print(model)
print(scenario)

##cores <- 12
rolling_window <- 7
##threshold <- 0.1
## fire_season_start <- 93
## fire_season_end <- 289
## fire_season_len <- fire_season_end - fire_season_start
## ncpath <- "/home/steve/Downloads/thredds/Deficit_NorESM1-M_rcp45_2024subset.nc"
##in_path <- "/media/smithers/shuysman/data/nps_gridded_wb/gye/forecasts/"
home_path <- Sys.getenv("HOME")
future_path <- paste0(home_path, "/data/nps_gridded_wb/gye/forecasts/rolling_sum/")
##future_path <- paste0("/media/smithers/shuysman/data/nps_gridded_wb/gye/forecasts/rolling_sum/")
##hist_path <- "/media/smithers/shuysman/data/nps_gridded_wb/gye/historical/rolling_sum/"
##quants_path <- "/home/steve/OneDrive/nothern-rockies-dryness/out/Deficit_historical_quants_0.79_0.845_0.875_.nc"
hist_path <- paste0(home_path, "/data/nps_gridded_wb/gye/historical/rolling_sum/")
##out_path <- "/media/smithers/shuysman/data/out/fire/"
out_path <- "./out/"
quants_path <- paste0(out_path, "/Deficit_historical_quants_0.35_0.5_0.7_.nc")
## nc_data_path <- "/home/steve/Downloads/thredds/"
quants <- c(0.35, 0.5, 0.7)


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
  return(e^(-4.1839624 + 5.5952288 * x + 0.2504363 * x^2 + -1.6207789 * x^3))
  }

start.time <- Sys.time()

future_files <- Sys.glob(paste0(future_path, "Deficit_rolling_sum_", rolling_window, "_", model, "_", scenario, "_*.nc"))

wbdata_future_smoothed <- terra::rast(future_files)

## wbdata_future_smoothed <- wbdata_future_smoothed %>%
##     subset(yday(time(.)) >= fire_season_start) %>%
##     subset(yday(time(.)) <= fire_season_end)


wbdata_historical_quants <- terra::rast(quants_path) %>% clamp(lower = 0)
names(wbdata_historical_quants) <- quants


above_quants_0.35 <- terra::compare(wbdata_future_smoothed, subset(wbdata_historical_quants, 1), ">", falseNA = TRUE)
above_quants_0.5 <- terra::compare(wbdata_future_smoothed, subset(wbdata_historical_quants, 2), ">", falseNA = TRUE)
above_quants_0.7 <- terra::compare(wbdata_future_smoothed, subset(wbdata_historical_quants, 3), ">", falseNA = TRUE)

s_0.35 <- terra::tapp(above_quants_0.35, index = "years", fun = "sum", na.rm = TRUE)
writeCDF(s_0.35, filename = paste0(out_path, "Annual_sum_days_above_fire_risk_threshold_0.35_", model, "_", scenario, ".nc"))
s_0.5 <- terra::tapp(above_quants_0.5, index = "years", fun = "sum", na.rm = TRUE)
writeCDF(s_0.5, filename = paste0(out_path, "Annual_sum_days_above_fire_risk_threshold_0.5_", model, "_", scenario, ".nc"))
s_0.7 <- terra::tapp(above_quants_0.7, index = "years", fun = "sum", na.rm = TRUE)
writeCDF(s_0.7, filename = paste0(out_path, "Annual_sum_days_above_fire_risk_threshold_0.7_", model, "_", scenario, ".nc"))

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken
