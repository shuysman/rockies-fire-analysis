library(dplyr)
library(stringr)
library(ggplot2)
library(parallel)
library(lubridate)
library(zoo)
library(terra)

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
##quants_path <- paste0(home_path, "/data/gye/historical/rolling_sum/Deficit_historical_quants_0.35_0.5_0.7_.nc")
hist_path <- paste0(home_path, "/data/nps_gridded_wb/gye/historical/rolling_sum/")
##out_path <- "/media/smithers/shuysman/data/out/fire/"
out_path <- "./out/"
## nc_data_path <- "/home/steve/Downloads/thredds/"

ncpaths_historical_smoothed <- list.files(path = hist_path, pattern = "Deficit.*.nc", full.names = TRUE)
wbdata_historical_smoothed <- rast(ncpaths_historical_smoothed)


replace_duplicated <- function(x) {
  x[duplicated(x)] <- NA
  return(x)
}

wbdata_historical_smoothed <- wbdata_historical_smoothed %>%
  subset(year(time(.)) <= 2021) %>%
  #### Pixellation correction adjustments
  round(digits = 1) %>%
  subst(0, NA) %>%
  terra::app(function(x) replace_duplicated(x))


quants <- c(0.35, 0.5, 0.7)
############
#### Generate Quants
## ### ecdf_regression(0.350) = 0.1039
## ### ecdf_regression(0.500) = 0.2173
## ### ecdf_regression(0.700) = 0.496
quants_string <- paste0(quants, "_", collapse = "")
wbdata_historical_quants_file <-  paste0(out_path, "Deficit_historical_quants_", quants_string, ".nc")

start.time <- Sys.time()
wbdata_historical_quants <- terra::quantile(wbdata_historical_smoothed, probs = quants, na.rm = TRUE)
end.time <- Sys.time()
names(wbdata_historical_quants) <- quants
writeCDF(wbdata_historical_quants, filename = wbdata_historical_quants_file, overwrite = TRUE)
time.taken <- round(end.time - start.time,2)
time.taken
#############
