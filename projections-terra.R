library(dplyr)
library(stringr)
library(ggplot2)
library(parallel)
library(lubridate)
library(zoo)
library(terra)
library(tidyterra)

## Precalculate historical and future rolling sum files with rolling_sums_future.R and rolling_sums_historical.R scripts first
## Set future_path, and hist_path to the dirs containing these files.
## Same rolling window should be used to run both scripts

terraOptions(verbose = TRUE,
             memfrac = 0.9)

args <- commandArgs(TRUE)
model <- args[1]
scenario <- args[2]

##future_file <- "/media/smithers/shuysman/data/nps_gridded_wb/gye/forecasts/rolling_sum/Deficit_rolling_sum_7_BNU-ESM_rcp45_2033.nc"

## model <- "BNU-ESM"
## scenario <- "rcp45"
## year <- 2033

print(model)
print(scenario)

##cores <- 12
rolling_window <- 7
##threshold <- 0.1
fire_season_start <- 93
fire_season_end <- 289
fire_season_len <- fire_season_end - fire_season_start
## ncpath <- "/home/steve/Downloads/thredds/Deficit_NorESM1-M_rcp45_2024subset.nc"
##in_path <- "/media/smithers/shuysman/data/nps_gridded_wb/gye/forecasts/"
home_path <- Sys.getenv("HOME")
future_path <- paste0(home_path, "/data/nps_gridded_wb/gye/forecasts/rolling_sum/")
##future_path <- "/media/smithers/shuysman/data/nps_gridded_wb/gye/forecasts/rolling_sum/"
hist_path <- paste0(home_path, "/data/nps_gridded_wb/gye/historical/rolling_sum/")
##hist_path <- "/media/smithers/shuysman/data/nps_gridded_wb/gye/historical/rolling_sum/"
##quants_path <- "/home/steve/OneDrive/nothern-rockies-dryness/out/Deficit_historical_quants_0.79_0.845_0.875_.nc"
quants_path <- paste0(home_path, "/data/nps_gridded_wb/gye/historical/rolling_sum/Deficit_historical_quants_0.35_0.425_0.485_.nc")
## quants_path <- "/home/steve/OneDrive/northern-rockies-dryness/out/Deficit_historical_quants_0.35_0.425_0.485_.nc"
## out_path <- "/media/smithers/shuysman/data/out/fire/"
out_path <- paste0(home_path, "/out/fire")
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
    return(e^(-4.1839624 + 5.5952288 * x + 0.2504363 * x^2 + -1.6207789 * x^3))
    ## old formula before percentile algo changes
    ## return(e^(-10.38 + 29.98 * x + -45.25 * x^2 + 25.65 * x^3))
}

clean_timeseries <- function(x) {
    ### Run all steps from custom percent_rank function in dryness_script.Rmd before actual ranking
    x2 <- round(x, 1)
    x2[x2 == 0] <- NA
    x2[duplicated(x2)] <- NA
    ##ranks <- data.frame(x = x2, pct = percent_rank(x2))
    ##inds <- match(round(x, 1), ranks$x)
    ##x3 <- ranks$pct[inds]
    ##x3 <- replace_na(x3, 0)
    return(x2)
}

start.time <- Sys.time()

## ncpaths_historical_smoothed <- list.files(path = hist_path, pattern = "Deficit.*.nc", full.names = TRUE)

## wbdata_historical_smoothed <- rast(ncpaths_historical_smoothed) %>%
##     subset(year(time(.)) <= 2021) %>%
##     subset(yday(time(.)) >= fire_season_start) %>%
##     subset(yday(time(.)) <= fire_season_end) %>%
##     terra::app(fun = clean_timeseries, cores = 8)

future_files <- Sys.glob(paste0(future_path, "Deficit_rolling_sum_", rolling_window, "_", model, "_", scenario, "_*.nc"))

wbdata_future_smoothed <- terra::rast(future_files) %>%
  subset(yday(time(.)) >= fire_season_start) %>%
  subset(yday(time(.)) <= fire_season_end)

## quants <- c(0.79, 0.845, 0.875)
quants <- c(0.35, 0.425, 0.485)

############
#### Generate Quants
#### Takes a long time to run, so run once and cache the quants file
### Old formula:
### ecdf_regression(0.790) = 0.1012792
### ecdf_regression(0.845) = 0.1525024
### ecdf_regression(0.875) = 0.2001806
### New percentile algo:
### ecdf_regression(0.89) = 0.1058749
### ecdf_regression(0.915) = 0.1503866
### ecdf_regression(0.933) = 0.2036546
### 
## quants_string <- paste0(quants, "_", collapse = "")
## wbdata_historical_quants_file <-  paste0(out_path, "Deficit_historical_quants_", quants_string, ".nc")

## start.time <- Sys.time()
## wbdata_historical_quants <- terra::quantile(wbdata_historical_smoothed, probs = quants, na.rm = TRUE, filename = wbdata_historical_quants_file, overwrite = TRUE)
## end.time <- Sys.time()
## time.taken <- round(end.time - start.time,2)
## time.taken
#############

wbdata_historical_quants <- terra::rast(quants_path)

names(wbdata_historical_quants) <- quants

above_quants_0.1 <- terra::compare(wbdata_future_smoothed, subset(wbdata_historical_quants, 1), ">")
above_quants_0.15 <- terra::compare(wbdata_future_smoothed, subset(wbdata_historical_quants, 2), ">")
above_quants_0.2 <- terra::compare(wbdata_future_smoothed, subset(wbdata_historical_quants, 3), ">")

s_0.1 <- terra::tapp(above_quants_0.1, index = "years", fun = "sum", na.rm = TRUE, filename = paste0(out_path, "Annual_sum_days_above_fire_risk_threshold_0.1_", model, "_", scenario, ".nc"), overwrite = TRUE)
s_0.15 <- terra::tapp(above_quants_0.15, index = "years", fun = "sum", na.rm = TRUE, filename = paste0(out_path, "Annual_sum_days_above_fire_risk_threshold_0.15_", model, "_", scenario, ".nc"), overwrite = TRUE)
s_0.2 <- terra::tapp(above_quants_0.2, index = "years", fun = "sum", na.rm = TRUE, filename = paste0(out_path, "Annual_sum_days_above_fire_risk_threshold_0.2_", model, "_", scenario, ".nc"), overwrite = TRUE)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

## ggplot() +
##   geom_spatraster(data = mean(s_0.1))

## plot(mean(s_0.1))

## ggplot() +
##   geom_spatraster(data = s_0.1$y_2050)


## test_thresh <- wbdata_future_smoothed %>%
##     subset(year(time(.)) == 2033) %>%
##     terra::compare(subset(wbdata_historical_quants, 1), ">")

## s_test_thresh <- sum(test_thresh)

## plot(s_test_thresh)

## plot(max(wbdata_future_smoothed %>%
##     subset(year(time(.)) == 2033)))



## ggplot() +
##     geom_spatraster(data = wbdata_historical_quants["0.485"]) +
##     scale_fill_hypso_c(palette = "colombia") 


## test0.1 <- rast("./out/Annual_sum_days_above_fire_risk_threshold_0.1_BNU-ESM_rcp45.nc")
## test0.2 <- rast("./out/Annual_sum_days_above_fire_risk_threshold_0.2_BNU-ESM_rcp45.nc")


## ggplot() +
##     geom_spatraster(data = mean(test0.1)) +
##     scale_fill_hypso_c(palette = "colombia") +
##     ggtitle("BNU-ESM rcp4.5 Mean annual days above threshold 2023-2099 (10% of historical fires), new %ile algorithm")


## ggplot() +
##     geom_spatraster(data = mean(test0.2)) +
##     scale_fill_hypso_c(palette = "colombia") +
##     ggtitle("BNU-ESM rcp4.5 Mean annual days above threshold 2023-2099 (20% of historical fires), new %ile algorithm")
