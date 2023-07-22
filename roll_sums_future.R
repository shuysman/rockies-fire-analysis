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

cores <- 16
rolling_window <- 7
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

cl <- makeCluster(cores)
doParallel::registerDoParallel(cl)

start.time <- Sys.time()

foreach(model = iter(models)) %:%
    foreach(scenario = iter(scenarios)) %:%
    foreach(year = iter(years),
            .inorder = TRUE,
            .packages = c("terra", "lubridate", "dplyr")
            ) %dopar% {
                
                ncpath <- paste0(in_path, "Deficit_", model, "_", scenario, "_", year, "subset.nc")
                
                wbdata_future <- terra::rast(ncpath)
                
                wbdata_future_smoothed <- terra::roll(wbdata_future, n = rolling_window, fun = sum, type = "to", circular = FALSE)

                terra::time(wbdata_future_smoothed) <- terra::time(wbdata_future)

                out_file <- paste0(out_path, "Deficit_rolling_sum_", rolling_window, "_",  model, "_", scenario, "_", year, ".nc")
                writeCDF(wbdata_future_smoothed, filename = out_file, overwrite = TRUE)

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
