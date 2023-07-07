library(dplyr)
library(ggplot2)
library(parallel)
library(terra)

rolling_window = 7
threshold <- 0.1
fire_season_start <- 93
fire_season_end <- 289
##ncpath <- "/home/steve/Downloads/thredds/Deficit_NorESM1-M_rcp45_2024subset.nc"
out_path <- "/home/steve/Downloads/thredds/out/"
##nc_data_path <- "/home/steve/Downloads/thredds/"

##models <- c('NorESM1-M', 'MRI-CGCM3','MIROC-ESM-CHEM', 'MIROC5','IPSL-CM5A-LR','inmcm4','HadGEM2-CC365','CSIRO-Mk3-6-0','CNRM-CM5','CanESM2', 'BNU-ESM','CCSM4', 'GFDL-ESM2G')
models <- c('NorESM1-M', 'MRI-CGCM3', 'MIROC5','IPSL-CM5A-LR','inmcm4','HadGEM2-CC365','CSIRO-Mk3-6-0','CNRM-CM5','CanESM2', 'BNU-ESM','CCSM4', 'GFDL-ESM2G') ## MIROC-ESM-CHEM >2070 rcp45 futures are missing
##models <- c("NorESM1-M")

scenarios <- c('rcp85', 'rcp45')
##scenarios <- c("rcp45")

##years <- seq(2024, 2050)
years <- seq(2051, 2099)

ecdf_regression <- function (x) {
    ## ECDF function generated from dryness_script.Rmd.  This function
    ## was generated for 7 day rolling windows and forested cover
    ## type.  To use a different length for rolling calculations and
    ## different cover types a different regression function should be
    ## used here.
    e <- exp(1)
    return(e^(-10.38 + 29.98*x + -45.25*x^2 + 25.65*x^3))
}

cl <- makeCluster(12)
clusterExport(cl, "ecdf_regression")
clusterExport(cl, "percent_rank")
clusterExport(cl, "if_else")
clusterExport(cl, "fire_season_start")
clusterExport(cl, "fire_season_end")
clusterExport(cl, "threshold")

start.time <- Sys.time()
for (model in models) {
    print(model)
    for (scenario in scenarios) {
        print(scenario)
        all_years <- rast()
        
        for (year in years) {
            ncpath <- paste0("/home/steve/Downloads/thredds/Deficit_", model, "_", scenario, "_", year, "subset.nc")
            print(ncpath)
            
            wbdata <- terra::rast(ncpath)

            wbdata_smoothed <- terra::roll(wbdata, n = rolling_window, fun = sum, type = "to", circular = FALSE)

            wbdata_percentiles <- terra::app(subset(wbdata_smoothed,
                                                    seq(fire_season_start, fire_season_end)),
                                             fun = function (x) percent_rank(x), cores = cl)
            
            fire_risk <- terra::app(wbdata_percentiles, fun = function (x) ecdf_regression(x), cores = cl)
            
            above_fire_risk <- terra::app(fire_risk, fun = function(x) if_else(x >= threshold, 1, 0), cores = cl)

            days_above_fire_risk <- sum(above_fire_risk, na.rm = TRUE)

            all_years <- c(all_years, days_above_fire_risk)
        }

        days_above_fire_risk_all_years <- mean(all_years, na.rm = TRUE)

        writeCDF(all_years, filename = paste0(out_path, "Days_Above_Fire_Risk_Deficit_", model, "_", scenario, "_", years[1], "-", tail(years, 1), ".nc"), overwrite = TRUE)
        writeCDF(days_above_fire_risk_all_years, filename = paste0(out_path, "Mean_Days_Above_Fire_Risk_Deficit_", model, "_", scenario, "_", years[1], "-", tail(years, 1), ".nc"), overwrite = TRUE)
    }
}
stopCluster(cl)
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken


ggplot(as.data.frame(days_above_fire_risk_all_years, xy = TRUE)) +
    geom_raster(mapping = aes(x = x, y = y, fill = mean)) +
 ##   geom_sf(data = gye_boundary_lcc, alpha = 0.15, fill = NA, lwd = 2) +
    theme_bw() +
    scico::scale_fill_scico(palette = "bilbao") +
    ggtitle("Days above fire danger threshold")
