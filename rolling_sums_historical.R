### Calculate rolling sums of historical Deficit data from NPS gridded
### water balance data set.  We want to precalculate it hear, then
### feed it into the projections-terra.R script because it is a
### computationally intensive part of the script
### Make sure  rolling_window used here match settings used in projections script

library(dplyr)
##library(purrr)
library(ggplot2)
library(parallel)
library(iterators)
library(foreach)
library(lubridate)
library(terra)

terraOptions(verbose = TRUE,
             memfrac = 0.9)

rolling_window <- 7
threshold <- 0.1
fire_season_start <- 93
fire_season_end <- 289
home_path <- Sys.getenv("HOME")
hist_path <- paste0(home_path, "/data/gye/historical/")
##hist_path <- "/media/smithers/shuysman/data/nps_gridded_wb/gye/historical/"
##out_path <- "/media/smithers/shuysman/data/out/fire/"
out_path <- "./out/"

cl <- makeCluster(24)
doParallel::registerDoParallel(cl)
clusterExport(cl, "rolling_window")
clusterEvalQ(cl, library("lubridate"))
clusterEvalQ(cl, library("terra"))


start.time <- Sys.time()

ncpaths_historical <- list.files(path = hist_path, pattern = "Deficit.*.nc", full.names = TRUE)
##wbdata_historical <- rast(ncpaths_historical) %>% subset(year(time(.)) <= 2021)
##wbdata_historical_smoothed <-  

foreach(ncpath = iter(ncpaths_historical)) %dopar% {
    print(ncpath)
    r <- rast(ncpath)
    year <- year(time(ncpath))
    rolled <- terra::roll(r, n = rolling_window, fun = sum, type = "to", circular = FALSE)

    filename <- paste0("Deficit_", year, "rolling_sum_", rolling_window, "_day.nc")

    writeCDF(rolled, filename = paste0(out_path, filename))
}

stopCluster(cl)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken
