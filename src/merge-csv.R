## Merge together csvs for fire ignition analysis
## fire-tp and fire-wb data dirs from Mike Tercek
## gridmet-vpd data pulled from gridmet thredds server, using maca-vpd.R script

library(tidyverse)
library(parallel)

sites <- read_csv('./rockies_fire_points.csv')
out_dir <- "./out/"
t_base <- 5.5 ## tbase in degrees C for GDD calculations
skip <- sites$Event_ID %in% c("WY4352911071419870423") ## file missing for this site, need to get Mike to rerun, or pull myself,
##  but this site is not a wildfire site so probably not relevant to analysis

f_to_c <- function(t_f) {
    return((t_f - 32) * 5/9)
}

generate_wb <- function(site) {
    tp <- read_csv(paste('./fire_tp/', site, '_historical.csv', sep=''), show_col_types = FALSE)
    wb <- read_csv(paste('./fire_wb/', site, '_water_balance_historical.csv', sep=''), show_col_types = FALSE)
    vpd <- read_delim(delim=";", paste('./gridmet-vpd/', site, '.csv', sep=''), show_col_types = FALSE) %>% filter(!row_number() >= 15707) ## cut off extra rows, Mike's data only goes to 2021-12-31, but vpd data we pulled is more recent so row # mismatched

    out_df <- bind_cols(tp, wb, vpd) %>%
        select(Date = "Date...1",
               P = "Precip (in)",
               TmaxF = "Tmax (F)",
               TminF = "Tmin (F)",
               RHmax = "RHmax (%)",
               RHmin = "RHmin(%)",
               TavgF = "Tavg (F)",
               D = "Deficit in",
               AET = "AET in",
               SOIL = "soil_water in",
               DRO = "runoff in",
               RAIN = "rain in",
               PACK = "accumswe in",
               PET = "PET in",
               VPD = "result") %>% ## vpd in kPa
        mutate(P = P * 25.4, # in to mm
               D = D * 25.4,
               AET = AET * 25.4,
               SOIL = SOIL * 25.4,
               DRO = DRO * 25.4,
               RAIN = RAIN * 25.4,
               PACK = PACK * 25.4,
               PET = PET * 25.4,
               RH = (RHmax + RHmin) /2,
               Tmax = f_to_c(TmaxF),
               Tmin = f_to_c(TminF),
               T = f_to_c(TavgF),
               "/\\SOIL" = SOIL - lag(SOIL, default = 0),
               GDD1 = (f_to_c(TmaxF) + f_to_c(TminF)) / 2 - t_base,
               GDD = ifelse(GDD1 >= 0, GDD1, 0)) %>% ## GDD cannot be negative, so minimum is 0
        select(-GDD1, -TmaxF, -TminF, -TavgF, -RHmax, -RHmin) 
               

    write.csv(out_df, paste(out_dir, site, ".csv", sep=""))
}

# Remove !skip after getting missing site from Mike
for (site in sites$Event_ID[!skip]) {
    generate_wb(site)
}


system.time(save1 <- mclapply(sites$Event_ID[!skip], generate_wb, mc.cores = 8))
