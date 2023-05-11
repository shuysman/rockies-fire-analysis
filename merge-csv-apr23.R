## Merge together csvs for fire ignition analysis
## ap23 data dirs historical waterbalance data from Mike Tercek
## gridmet-vpd data pulled from gridmet thredds server, using maca-vpd.R script
## Version for apr23 files Mike sent, which did not include tp variables

library(tidyverse)
library(parallel)

sites <- read_csv('./Southern_rockies_fire_points.csv')
out_dir <- "./out/ap23/"
t_base <- 5.5 ## tbase in degrees C for GDD calculations

f_to_c <- function(t_f) {
    return((t_f - 32) * 5/9)
}

k_to_c <- function(t_k) {
    return((t_k - 273.15))
}

generate_wb <- function(site) {
    ##tp <- read_csv(paste('./fire_tp/', site, '_historical.csv', sep=''), show_col_types = FALSE)
    wb <- read_csv(paste('./ap23/', site, '_water_balance_historical.csv', sep=''), show_col_types = FALSE)
    pr <- read_delim(delim=";", paste('./gridmet-pr/', site, '.csv', sep=''), skip = 1, show_col_types = FALSE, col_names = c("Date", "P", "Lon", "Lat")) %>% filter(!row_number() >= 15707) ## cut off extra rows, Mike's data only goes to 2021-12-31, but vpd data we pulled is more recent so row # mismatched
    tmmx <- read_delim(delim=";", paste('./gridmet-tmmx/', site, '.csv', sep=''), skip = 1, show_col_types = FALSE, col_names = c("Date", "TmaxK", "Lon", "Lat")) %>% filter(!row_number() >= 15707) ## cut off extra rows, Mike's data only goes to 2021-12-31, but vpd data we pulled is more recent so row # mismatched
    tmmn <- read_delim(delim=";", paste('./gridmet-tmmn/', site, '.csv', sep=''), skip = 1, show_col_types = FALSE, col_names = c("Date", "TminK", "Lon", "Lat")) %>% filter(!row_number() >= 15707) ## cut off extra rows, Mike's data only goes to 2021-12-31, but vpd data we pulled is more recent so row # mismatched
    rmax <- read_delim(delim=";", paste('./gridmet-rmax/', site, '.csv', sep=''), skip = 1, show_col_types = FALSE, col_names = c("Date", "RHmax (%)", "Lon", "Lat")) %>% filter(!row_number() >= 15707) ## cut off extra rows, Mike's data only goes to 2021-12-31, but vpd data we pulled is more recent so row # mismatched
    rmin <- read_delim(delim=";", paste('./gridmet-rmin/', site, '.csv', sep=''), skip = 1, show_col_types = FALSE, col_names = c("Date", "RHmin(%)", "Lon", "Lat")) %>% filter(!row_number() >= 15707) ## cut off extra rows, Mike's data only goes to 2021-12-31, but vpd data we pulled is more recent so row # mismatched
    vpd <- read_delim(delim=";", paste('./gridmet-vpd/', site, '.csv', sep=''), skip = 1, show_col_types = FALSE, col_names = c("Date", "VPD", "Lon", "Lat")) %>% filter(!row_number() >= 15707) ## cut off extra rows, Mike's data only goes to 2021-12-31, but vpd data we pulled is more recent so row # 
    
    out_df <- bind_cols(pr, tmmx, tmmn, rmax, rmin, wb, vpd) %>%
        dplyr::select(Date = "Date...1",
               P = "P",
               TmaxK = "TmaxK",
               TminK = "TminK",
               RHmax = "RHmax (%)",
               RHmin = "RHmin(%)",
               D = "Deficit in",
               AET = "AET in",
               SOIL = "soil_water in",
               DRO = "runoff in",
               RAIN = "rain in",
               PACK = "accumswe in",
               PET = "PET in",
               VPD = "VPD") %>%
        mutate(##P = P * 25.4, # in to mm
               D = D * 25.4,
               AET = AET * 25.4,
               SOIL = SOIL * 25.4,
               DRO = DRO * 25.4,
               RAIN = RAIN * 25.4,
               PACK = PACK * 25.4,
               PET = PET * 25.4,
               RH = (RHmax + RHmin) /2,
               Tmax = k_to_c(TmaxK),
               Tmin = k_to_c(TminK),
               T = (Tmax + Tmin) / 2,
               "/\\SOIL" = SOIL - lag(SOIL, default = 0),
               GDD1 = T - t_base,
               GDD = ifelse(GDD1 >= 0, GDD1, 0)) %>% ## GDD cannot be negative, so minimum is 0
        dplyr::select(-GDD1, -TmaxK, -TminK, -RHmax, -RHmin) 
               

    write.csv(out_df, paste(out_dir, site, ".csv", sep=""))
}

for (site in sites$Event_ID) {
    generate_wb(site)
}


system.time(save1 <- mclapply(sites$Event_ID, generate_wb, mc.cores = 8))
