
flag_data <- function(cal, dat, baddates_csv_fp, fn){
  #add flags to data frames--------------------------------------- 
  #one for calibration and one for 'bad' data where nonsensical data has 
  #been read in

  # Pad the cal file so there is a continuous time series and forward fill the
  # start and stop times of each calibration so every row has a start and stop
  # time. Also flag 100 s before the cal and 300 s after.
  
  cal_pad <- cal %>% 
    mutate(date = date - 100,
           StartTime = StartTime - 100, 
           StopTime = StopTime + 300) %>% 
    add_row(date = dat[[1]]$date[1], 
            .before = 1) %>% 
    add_row(date = dat[[length(dat)]]$date[nrow(dat[[length(dat)]])],
            .after = nrow(cal)) %>% 
    pad_ts(datecol = "date", 1) %>% 
    fill(c(StartTime, StopTime), .direction = "downup") %>% 
    select(date, StartTime, StopTime)
  
  # Merge the padded cal ts with the data and make calflag == 1 if the date is 
  # between cal start and stop times.
  dat <- map(dat, left_join, cal_pad, by = "date")
  dat <- dat %>% map(~mutate(.x, 
                             calflag = ifelse(between(date, StartTime, StopTime), 
                                              1, 0)))
  
  # Add multiport valve position information in 
  cal_mpv <- cal %>%
    rename(mpv = Multiway) %>%
    add_row(date = dat[[length(dat)]]$date[nrow(dat[[length(dat)]])],
            .after = nrow(cal)) %>% 
    pad_ts(datecol = "date", 1) %>%
    fill(c(StartTime, StopTime, mpv), .direction = "down") %>%
    select(date, StartTime, StopTime, mpv)
  
  # Select the rows where cal is happening only
  cal_mpv <- cal_mpv[between(cal_mpv$date, cal_mpv$StartTime, cal_mpv$StopTime),] %>% 
    select(date, 
           mpv)
  
  # Join to dat
  dat <- map(dat, left_join, cal_mpv, by = "date")
  
  # Tidy dat
  dat <- dat %>% 
    map(~select(.x, -StartTime, -StopTime))

  # data quality flag (based on how date read in and some thresholds)
  dat <- dat %>% 
    map(~mutate(.x, badflag = ifelse(is.na(date), 1, 0)))

  # add some flags based on non-physical values caused normally by corrupted file
  dat <- dat %>% 
    map(~mutate(.x, badflag = ifelse(CO2_dry < 5, 1, badflag))) %>% 
    map(~mutate(.x, badflag = ifelse(CO2_dry > 1000, 1, badflag))) %>% 
    map(~mutate(.x, badflag = ifelse(CH4_dry < 0.5, 1, badflag))) %>% 
    map(~mutate(.x, badflag = ifelse(CH4_dry > 100, 1, badflag)))
  
  # flag periods where dates are manually bad
  bd <- read.csv(baddates_csv_fp) %>% 
    mutate(startdate = dmy(startdate),
           enddate = dmy(enddate))
  
  # Create a sequence of dates
  bd <- bd %>% 
    mutate(baddates = map2(startdate, enddate, ~seq(.x, .y, by = "1 day")))
  
  baddates <- bd$baddates %>% 
    unlist() %>% 
    as_date()
  
  # Flag cals based on bad dates
  dat <- dat %>% 
    map(~mutate(.x, badflag = ifelse(lubridate::date(date) %in% baddates, 1, badflag))) 
  
  # Extra flagging not picked up by above --------------------------------------
  # 16-17 June 2022. The calibration started before midnight on 16th June - I think
  # because a new file was created for the 17th the calibration has been interrupted.
  # No cal file for that day has been created.
  # Flag the partial cal and remove it.
  
  if("Picarro_data_20220616.csv" %in% names(dat)){

    dat$Picarro_data_20220616.csv <- dat$Picarro_data_20220616.csv %>%
      mutate(badflag = ifelse(between(date,
                                      ymd_hms("2022-06-16 23:43:31"),
                                      ymd_hms("2022-06-16 23:59:59")),
                              1, badflag)
      )
  }
  
  if("Picarro_data_20220617.csv" %in% names(dat)){
    
    dat$Picarro_data_20220617.csv <- dat$Picarro_data_20220617.csv %>%
      mutate(badflag = ifelse(between(date,
                                      ymd_hms("2022-06-17 00:00:00"),
                                      ymd_hms("2022-06-17 00:56:26")),
                              1, badflag)
      )
  }
  
  #Data loss statistics-----------------------------------------------------
  #How many data points have been lost to reading errors/file corruption?
  #sum up badflag per file
  bad_flag_datasumlist <- dat %>% 
    map(~sum(.x$badflag, na.rm = T))
  
  cal_flag_datasumlist <- dat %>% 
    map(~sum(.x$calflag, na.rm = T))
  
  #create a data frame with filename and number of data points flagged
  bad_flag_datasum <- bind_rows(bad_flag_datasumlist, .id = "column_label") %>% 
    pivot_longer(cols = everything(), 
                 names_to = "file",
                 values_to = "count_data_lost") %>% 
    mutate(date = ymd(paste(substr(file, 14, 21))))
  
  #create a data frame with filename and number of cal points flagged
  cal_flag_datasum <- bind_rows(cal_flag_datasumlist, .id = "column_label") %>% 
    pivot_longer(cols = everything(), 
                 names_to = "file",
                 values_to = "count_data_cal") %>% 
    mutate(date = ymd(paste(substr(file, 14, 21))))
  
  #how much data being thrown out per month?
  monthly_data_lost <- bad_flag_datasum %>% 
    group_by(month = floor_date(date, "month")) %>% 
    summarise(n_lost_dps = sum(count_data_lost))
  
  #how many data points flagged as cals?
  monthly_cal_dps <- cal_flag_datasum %>% 
    group_by(month = floor_date(date, "month")) %>% 
    summarise(n_cal_dps = sum(count_data_cal))
  
  #this is crap way to do this
  flag_summary <- data.frame(month = monthly_data_lost$month) %>% 
    left_join(monthly_data_lost, "month") %>% 
    left_join(monthly_cal_dps, "month")
  
  write.csv(flag_summary, file = paste0(out, fn, "_flag_summary.csv"), row.names = F)
  
  #return list of data frames with flag column in
  return(dat)
  
}
