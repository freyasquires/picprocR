# Read and bind in a list of minute averaged scaled data created by scale_data 
# function. Produces data frame.

read_scaled_picarro_data <- function(picarro_data_dir){
  picarro_data_list <- list.files(picarro_data_dir, pattern = ".csv", full.names = T)
  purrr::map_dfr(picarro_data_list, read_picarro_data_worker)
  #map(picarro_data_list, read_picarro_cal_worker)
}

read_picarro_data_worker <- function(file){
  
  df <- read.csv(file)
  
  df$date <- lubridate::ymd_hms(df$date)
  
  #return
  df
}

# Average data
average_picarro <- function(df, avg.time){
  
  df_mn <- df %>% 
    dplyr::mutate(co2_scaled = ifelse(badflag != 0, NA, co2_scaled),
                  co2_scaled = ifelse(calflag != 0, NA, co2_scaled),
                  CO2_dry = ifelse(badflag != 0, NA, CO2_dry),
                  CO2_dry = ifelse(calflag != 0, NA, CO2_dry),
                  CO2_wet = ifelse(badflag != 0, NA, CO2_wet),
                  CO2_wet = ifelse(calflag != 0, NA, CO2_wet),
                  ch4_scaled = ifelse(badflag != 0, NA, ch4_scaled),
                  ch4_scaled = ifelse(calflag != 0, NA, ch4_scaled),
                  CH4_dry = ifelse(badflag != 0, NA, CH4_dry),
                  CH4_dry = ifelse(calflag != 0, NA, CH4_dry),
                  CH4_wet = ifelse(badflag != 0, NA, CH4_wet),
                  CH4_wet = ifelse(calflag != 0, NA, CH4_wet),
                  H2O = ifelse(badflag != 0, NA, H2O),
                  H2O = ifelse(calflag != 0, NA, H2O)) %>% 
    dplyr::select(date,
                  CO2_dry,
                  co2_scaled,
                  CO2_wet,
                  CH4_dry,
                  ch4_scaled,
                  CH4_wet,
                  H2O) %>% 
    openair::timeAverage(statistic = "mean", avg.time = avg.time)
  
  df_sd <- df %>% 
    dplyr::mutate(co2_scaled = ifelse(badflag != 0, NA, co2_scaled),
                  co2_scaled = ifelse(calflag != 0, NA, co2_scaled),
                  CO2_dry = ifelse(badflag != 0, NA, CO2_dry),
                  CO2_dry = ifelse(calflag != 0, NA, CO2_dry),
                  CO2_wet = ifelse(badflag != 0, NA, CO2_wet),
                  CO2_wet = ifelse(calflag != 0, NA, CO2_wet),
                  ch4_scaled = ifelse(badflag != 0, NA, ch4_scaled),
                  ch4_scaled = ifelse(calflag != 0, NA, ch4_scaled),
                  CH4_dry = ifelse(badflag != 0, NA, CH4_dry),
                  CH4_dry = ifelse(calflag != 0, NA, CH4_dry),
                  CH4_wet = ifelse(badflag != 0, NA, CH4_wet),
                  CH4_wet = ifelse(calflag != 0, NA, CH4_wet),
                  H2O = ifelse(badflag != 0, NA, H2O),
                  H2O = ifelse(calflag != 0, NA, H2O)) %>% 
    dplyr::select(date,
                  CO2_dry,
                  co2_scaled,
                  CO2_wet,
                  CH4_dry,
                  ch4_scaled,
                  CH4_wet,
                  H2O) %>% 
    openair::timeAverage(statistic = "sd", avg.time = avg.time) %>% 
    dplyr::rename(CO2_sd = CO2_dry,
                  CH4_sd = CH4_dry,
                  CO2w_sd = CO2_wet,
                  ch4w_sd = CH4_wet,
                  co2_scaled_sd = co2_scaled,
                  ch4_scaled_sd = ch4_scaled,
                  H2O_sd = H2O)
  
  df_av <- dplyr::left_join(df_mn, df_sd, "date")
  
}