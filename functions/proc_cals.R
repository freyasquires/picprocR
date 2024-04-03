# Function to calculate cal slopes and intercepts 
# Outputs pdf of calibration values

calc_cal_factors <- function(cal, cylinder_info, fn, z_score_threshold = 3,
                             cal_method, baddates_csv_fp){

  # Read in cylinder information .csv
  
  if(file.exists(cylinder_info)) {
    cyl_concs <- read.csv(cylinder_info) %>% 
      mutate(startdate = dmy_hm(startdate),
             enddate = dmy_hm(enddate))
  } else{
    stop("Calibration cylinder information file not found.")
  }
  
  #Add cylinder information to the cal file - this will remove rows where cal
  #file has corrupted (any mpvs that's not in cylinder_info file are dropped)
  cal <- left_join(cal %>% rename(mpv = Multiway), 
                    cyl_concs, 
                    by = "mpv", 
                    relationship = "many-to-many") %>% 
    filter(between(date, startdate, enddate))
  
  # Make values nan where dates appear in 'baddates' file so bad data isn't used
  # to calculate cal slopes
  bd <- read.csv(baddates_csv_fp) %>% 
    mutate(startdate = dmy(startdate),
           enddate = dmy(enddate))
  
  # Create a sequence of dates
  bd <- bd %>% 
    mutate(baddates = map2(startdate, enddate, ~seq(.x, .y, by = "1 day")))
  
  baddates <- bd$baddates %>% 
    unlist() %>% 
    as_date()
  
  # Flag cals based on baddates and if flag is NaN. 
  cal <- cal %>% 
    mutate(CO2cal_dry = ifelse(filedate %in% baddates, NA, CO2cal_dry)) %>% 
    mutate(CH4cal_dry = ifelse(filedate %in% baddates, NA, CH4cal_dry))
  
  # Calculate the cal factors to be applied - calculation different depending 
  # on method
  
  if(cal_method == "linear"){
    # Run linear model for every cal - returns intercept and slope
    co2_lm <- cal %>% 
      #group_by(filedate) %>% 
      nest(data = -filedate) %>% 
      mutate(model = map(data, ~lm(CO2cal_dry ~ co2_cal_ppm, data = .)), 
             tidied = map(model, tidy)) %>% 
      unnest(tidied)
    
    # Rename 
    co2_lm$term[co2_lm$term == "co2_cal_ppm"] <- "co2_slope"
    co2_lm$term[co2_lm$term == "(Intercept)"] <- "co2_intercept"
    
    # Create date column for interpolating below from 'StopTime'
    co2_lm$date <- ymd_hms(NA)
    for (i in 1:nrow(co2_lm)) {
      co2_lm$date[i] <- ymd_hms(co2_lm$data[[i]][[nrow(co2_lm$data[[i]]),6]])
    }
    
    #and again for CH4...
    ch4_lm <- cal %>% 
      nest(data = -filedate) %>% 
      mutate(model = map(data, ~lm(CH4cal_dry*1000 ~ ch4_cal_ppb, data = .)), 
             tidied = map(model, tidy)) %>% 
      unnest(tidied)
    
    ch4_lm$term[ch4_lm$term == "ch4_cal_ppb"] <- "ch4_slope"
    ch4_lm$term[ch4_lm$term == "(Intercept)"] <- "ch4_intercept"
    
    ch4_lm$date <- ymd_hms(NA)
    for (i in 1:nrow(ch4_lm)) {
      ch4_lm$date[i] <- ch4_lm$data[[i]][[nrow(ch4_lm$data[[i]]),6]]
    }
    
    # Output the lm runs as a list of data frames- this is easier to interpolate 
    # work with compared to the lm objects. 
    
    co2_lm_df <- co2_lm %>% 
      select(filedate,
             date,
             term,
             estimate,
             std.error,
             statistic,
             p.value) %>% 
      pivot_wider(id_cols = c(date, filedate),
                  names_from = term,
                  values_from = c(estimate, std.error, statistic, p.value)) %>% 
      as.data.frame()
    
    ch4_lm_df <- ch4_lm %>% 
      select(filedate,
             date,
             term,
             estimate,
             std.error,
             statistic,
             p.value) %>% 
      pivot_wider(id_cols = c(date, filedate),
                  names_from = term,
                  values_from = c(estimate, std.error, statistic, p.value)) %>% 
      as.data.frame()
    
    # Calculate Z score which you can use to filter calibrations. A Z score of 3 
    # means that the value is 3 standard deviations away from the mean. 
    # Add user defined Z score threshold here to flag cal values by. Default value
    # is 3.
    
    co2_lm_df <- co2_lm_df %>% 
      mutate(z_score_slope = (estimate_co2_slope-mean(estimate_co2_slope, na.rm = T))/sd(estimate_co2_slope, na.rm = T),
             z_score_int = (estimate_co2_intercept-mean(estimate_co2_intercept, na.rm = T))/sd(estimate_co2_intercept, na.rm = T),
             z_score_thresh = z_score_threshold)
    
    ch4_lm_df <- ch4_lm_df %>% 
      mutate(z_score_slope = (estimate_ch4_slope-mean(estimate_ch4_slope, na.rm = T))/sd(estimate_ch4_slope, na.rm = T),
             z_score_int = (estimate_ch4_intercept-mean(estimate_ch4_intercept, na.rm = T))/sd(estimate_ch4_intercept, na.rm = T),
             z_score_thresh = z_score_threshold)
  }
  
  if(cal_method == "target"){
  # Calculate scaling factor - divide measured value by cylinder value
    co2_sf_df <- cal %>% 
      mutate(estimate_co2_intercept = 0,
             co2_slope = CO2cal_dry/co2_cal_ppm,
             estimate_co2_slope = NA,
             std.error_co2_intercept = NA,
             std.error_co2_slope = NA,
             statistic_co2_intercept = NA,
             statistic_co2_slope = NA,
             p.value_co2_intercept = NA,
             p.value_co2_slope = NA,
             z_score_slope = NA,
             z_score_int = NA,
             z_score_thresh = z_score_threshold) %>% 
      select(date,
             filedate,
             estimate_co2_intercept, 
             estimate_co2_slope, 
             std.error_co2_intercept,
             std.error_co2_slope,
             statistic_co2_intercept,
             statistic_co2_slope,
             p.value_co2_intercept,
             p.value_co2_slope,
             z_score_slope,
             z_score_int,
             z_score_thresh,
             co2_slope,
             cyl_n
             )
    
    # Average the slopes calculated for each cylinder - average per filedate
    # These will be used to create cal interp if using this method, but want to 
    # plot individual cylinders too.
    # Don't want to use cylinder CB09892 - pressure is too low to trust 
    # Omit cals that are 0 from the average
    
    co2_sf_df <- co2_sf_df %>% 
      mutate(co2_slope = ifelse(co2_slope < 0.95, NA, co2_slope),
             co2_slope = ifelse(co2_slope > 1.05, NA, co2_slope)) %>% 
      group_by(filedate) %>% 
      mutate(estimate_co2_slope = mean(co2_slope[cyl_n != "CB09892"], na.rm = T)) %>% 
      ungroup() %>% 
      mutate(z_score_slope = (estimate_co2_slope-mean(estimate_co2_slope, na.rm = T))/sd(estimate_co2_slope, na.rm = T))
    
    ch4_sf_df <- cal %>% 
      mutate(estimate_ch4_intercept = 0,
             ch4_slope = CH4cal_dry*1000/ch4_cal_ppb,
             estimate_ch4_slope = NA,
             std.error_ch4_intercept = NA,
             std.error_ch4_slope = NA,
             statistic_ch4_intercept = NA,
             statistic_ch4_slope = NA,
             p.value_ch4_intercept = NA,
             p.value_ch4_slope = NA,
             z_score_slope = NA,
             z_score_int = NA,
             z_score_thresh = z_score_threshold) %>% 
      select(date,
             filedate,
             estimate_ch4_intercept, 
             estimate_ch4_slope, 
             std.error_ch4_intercept,
             std.error_ch4_slope,
             statistic_ch4_intercept,
             statistic_ch4_slope,
             p.value_ch4_intercept,
             p.value_ch4_slope,
             z_score_slope,
             z_score_int,
             z_score_thresh,
             cyl_n,
             ch4_slope
      )
    
    ch4_sf_df <- ch4_sf_df %>% 
      mutate(ch4_slope = ifelse(ch4_slope < 0.95, NA, ch4_slope),
             ch4_slope = ifelse(ch4_slope > 1.05, NA, ch4_slope)) %>% 
      group_by(filedate) %>% 
      mutate(estimate_ch4_slope = mean(ch4_slope[cyl_n != "CB09892"], na.rm = T)) %>% 
      ungroup() %>% 
      mutate(z_score_slope = (estimate_ch4_slope-mean(estimate_ch4_slope, na.rm = T))/sd(estimate_ch4_slope, na.rm = T))
    
  }

  #plot cal factor data ------------
  if(cal_method == "linear") {
    co2_slope <- ggplot(co2_lm_df) + 
      geom_errorbar(aes(date, ymin = estimate_co2_slope - 2*std.error_co2_slope, 
                        ymax = estimate_co2_slope + 2*std.error_co2_slope)) + 
      geom_point(aes(date, estimate_co2_slope, colour = ifelse(abs(z_score_slope) > z_score_thresh, 'red', 'dodgerblue'))) + 
      scale_colour_manual(labels = c("Within 3 SD", "Outlier"), values=c('dodgerblue', 'red'), name = NULL) + 
      ylab(parse(text = "`CO2 Slope \u00B12 Std Err`")) + 
      scale_x_datetime(date_labels = "%d/%m/%Y") +
      theme_bw() + 
      theme(axis.title.x = element_blank())
    
    co2_intercept <- ggplot(co2_lm_df) + 
      geom_errorbar(aes(date, ymin = estimate_co2_intercept - 2*std.error_co2_intercept, 
                        ymax = estimate_co2_intercept + 2*std.error_co2_intercept)) + 
      geom_point(aes(date, estimate_co2_intercept, colour = ifelse(abs(z_score_int) > z_score_thresh, 'red', 'dodgerblue'))) + 
      scale_colour_manual(labels = c("Within 3 SD", "Outlier"), values=c('dodgerblue', 'red'), name = NULL) + 
      ylab(parse(text = "`CO2 Intercept \u00B12 Std Err`")) + 
      scale_x_datetime(date_labels = "%d/%m/%Y") +
      theme_bw() + 
      theme(axis.title.x = element_blank())
    
    ch4_slope <- ggplot(ch4_lm_df) +
      geom_errorbar(aes(date, ymin = estimate_ch4_slope - 2*std.error_ch4_slope, 
                        ymax = estimate_ch4_slope + 2*std.error_ch4_slope)) + 
      geom_point(aes(date, estimate_ch4_slope, colour = ifelse(abs(z_score_slope) > z_score_thresh, 'red', 'dodgerblue'))) + 
      scale_colour_manual(labels = c("Within 3 SD", "Outlier"), values=c('dodgerblue', 'red'), name = NULL) + 
      ylab(parse(text = "`CH4 Slope \u00B12 Std Err`")) + 
      scale_x_datetime(date_labels = "%d/%m/%Y") +
      theme_bw() + 
      theme(axis.title.x = element_blank())
    
    ch4_intercept <- ggplot(ch4_lm_df) + 
      geom_errorbar(aes(date, ymin = estimate_ch4_intercept - 2*std.error_ch4_intercept, 
                        ymax = estimate_ch4_intercept + 2*std.error_ch4_intercept)) + 
      geom_point(aes(date, estimate_ch4_intercept, colour = ifelse(abs(z_score_int) > z_score_thresh, 'red', 'dodgerblue'))) + 
      scale_colour_manual(labels = c("Within 3 SD", "Outlier"), values=c('dodgerblue', 'red'), name = NULL) + 
      ylab(parse(text = "`CH4 Intercept \u00B12 Std Err`")) + 
      scale_x_datetime(date_labels = "%d/%m/%Y") +
      theme_bw() + 
      theme(axis.title.x = element_blank())
    
    # New plots omitting the values that have Z score higher than user threshold
    co2_slope_filtered <- ggplot(co2_lm_df %>% 
                                   mutate(estimate_co2_slope = ifelse(abs(z_score_slope) > z_score_thresh, NA, estimate_co2_slope))) + 
      geom_errorbar(aes(date, ymin = estimate_co2_slope - 2*std.error_co2_slope, 
                        ymax = estimate_co2_slope + 2*std.error_co2_slope)) + 
      geom_point(aes(date, estimate_co2_slope, colour = ifelse(abs(z_score_slope) > z_score_thresh, 'red', 'dodgerblue'))) + 
      scale_colour_manual(labels = c("Within 3 SD", "Outlier"), values=c('dodgerblue', 'red'), name = NULL) + 
      ylab(parse(text = "`CO2 Slope \u00B12 Std Err`")) + 
      scale_x_datetime(date_labels = "%d/%m/%Y") +
      theme_bw() + 
      theme(axis.title.x = element_blank())
    
    co2_intercept_filtered <- ggplot(co2_lm_df %>% 
                                       mutate(estimate_co2_intercept = ifelse(abs(z_score_int) > z_score_thresh, NA, estimate_co2_intercept))) + 
      geom_errorbar(aes(date, ymin = estimate_co2_intercept - 2*std.error_co2_intercept, 
                        ymax = estimate_co2_intercept + 2*std.error_co2_intercept)) + 
      geom_point(aes(date, estimate_co2_intercept, colour = ifelse(abs(z_score_int) > z_score_thresh, 'red', 'dodgerblue'))) + 
      scale_colour_manual(labels = c("Within 3 SD", "Outlier"), values=c('dodgerblue', 'red'), name = NULL) + 
      ylab(parse(text = "`CO2 Intercept \u00B12 Std Err`")) + 
      scale_x_datetime(date_labels = "%d/%m/%Y") +
      theme_bw() + 
      theme(axis.title.x = element_blank())
    
    ch4_slope_filtered <- ggplot(ch4_lm_df %>% 
                                   mutate(estimate_ch4_slope = ifelse(abs(z_score_slope) > z_score_thresh, NA, estimate_ch4_slope))) + 
      geom_errorbar(aes(date, ymin = estimate_ch4_slope - 2*std.error_ch4_slope, 
                        ymax = estimate_ch4_slope + 2*std.error_ch4_slope)) + 
      geom_point(aes(date, estimate_ch4_slope, colour = ifelse(abs(z_score_slope) > z_score_thresh, 'red', 'dodgerblue'))) + 
      scale_colour_manual(labels = c("Within 3 SD", "Outlier"), values=c('dodgerblue', 'red'), name = NULL) + 
      ylab(parse(text = "`CH4 Slope \u00B12 Std Err`")) + 
      scale_x_datetime(date_labels = "%d/%m/%Y") +
      theme_bw() + 
      theme(axis.title.x = element_blank())
    
    ch4_intercept_filtered <- ggplot(ch4_lm_df %>% 
                                       mutate(estimate_ch4_intercept = ifelse(abs(z_score_int) > z_score_thresh, NA, estimate_ch4_intercept))) +
      geom_errorbar(aes(date, ymin = estimate_ch4_intercept - 2*std.error_ch4_intercept, 
                        ymax = estimate_ch4_intercept + 2*std.error_ch4_intercept)) + 
      geom_point(aes(date, estimate_ch4_intercept, colour = ifelse(abs(z_score_int) > z_score_thresh, 'red', 'dodgerblue'))) + 
      scale_colour_manual(labels = c("Within 3 SD", "Outlier"), values=c('dodgerblue', 'red'), name = NULL) + 
      ylab(parse(text = "`CH4 Intercept \u00B12 Std Err`")) + 
      scale_x_datetime(date_labels = "%d/%m/%Y") +
      theme_bw() + 
      theme(axis.title.x = element_blank())
    
    # Set hard limits within a reasonable range because Z score doesn't capture 
    # all outliers
    co2_slope_filtered_2 <- ggplot(co2_lm_df) + 
      geom_errorbar(aes(date, ymin = estimate_co2_slope - 2*std.error_co2_slope, 
                        ymax = estimate_co2_slope + 2*std.error_co2_slope)) + 
      geom_point(aes(date, estimate_co2_slope), colour = "seagreen") + 
      ylab(parse(text = "`CO2 Slope \u00B12 Std Err`")) + 
      scale_x_datetime(date_labels = "%d/%m/%Y") +
      ylim(c(0.985, 1.005)) + 
      theme_bw() + 
      theme(axis.title.x = element_blank())
    
    co2_intercept_filtered_2 <- ggplot(co2_lm_df) + 
      geom_errorbar(aes(date, ymin = estimate_co2_intercept - 2*std.error_co2_intercept, 
                        ymax = estimate_co2_intercept + 2*std.error_co2_intercept)) + 
      geom_point(aes(date, estimate_co2_intercept), colour = "seagreen") + 
      ylab(parse(text = "`CO2 Intercept \u00B12 Std Err`")) + 
      scale_x_datetime(date_labels = "%d/%m/%Y") +
      ylim(c(-2.5, 2.5)) +
      theme_bw() + 
      theme(axis.title.x = element_blank())
    
    ch4_slope_filtered_2 <- ggplot(ch4_lm_df) + 
      geom_errorbar(aes(date, ymin = estimate_ch4_slope - 2*std.error_ch4_slope, 
                        ymax = estimate_ch4_slope + 2*std.error_ch4_slope)) + 
      geom_point(aes(date, estimate_ch4_slope), colour = "seagreen") + 
      ylab(parse(text = "`CH4 Slope \u00B12 Std Err`")) + 
      scale_x_datetime(date_labels = "%d/%m/%Y") +
      ylim(c(0.995, 1.005)) + 
      theme_bw() + 
      theme(axis.title.x = element_blank())
    
    ch4_intercept_filtered_2 <- ggplot(ch4_lm_df) +
      geom_errorbar(aes(date, ymin = estimate_ch4_intercept - 2*std.error_ch4_intercept, 
                        ymax = estimate_ch4_intercept + 2*std.error_ch4_intercept)) + 
      geom_point(aes(date, estimate_ch4_intercept), colour = "seagreen") + 
      ylab(parse(text = "`CH4 Intercept \u00B12 Std Err`")) + 
      scale_x_datetime(date_labels = "%d/%m/%Y") +
      ylim(c(-35, 35)) +
      theme_bw() + 
      theme(axis.title.x = element_blank())
    
    
    #Arrange the plots on a page and save as a pdf
    pdf(file = paste0(out, fn, "_cal_plots.pdf"), width = 7.5, height = 11, paper = "a4")
    print(ggpubr::ggarrange(co2_slope, co2_intercept, ch4_slope, ch4_intercept, ncol = 1, align = "v"))
    print(ggpubr::ggarrange(co2_slope_filtered, co2_intercept_filtered, ch4_slope_filtered, ch4_intercept_filtered, ncol = 1, align = "v"))
    print(ggpubr::ggarrange(co2_slope_filtered_2, co2_intercept_filtered_2, ch4_slope_filtered_2, ch4_intercept_filtered_2, ncol = 1, align = "v"))
    dev.off()
  }
  
  if(cal_method == "target"){
    co2_slope <- ggplot(co2_sf_df) + 
      geom_point(aes(date, co2_slope, colour = cyl_n)) + 
      ylab(parse(text = "`CO2 Slope (targets)`")) + 
      scale_x_datetime(date_labels = "%d/%m/%Y") +
      theme_bw() + 
      theme(axis.title.x = element_blank())
    
    ch4_slope <- ggplot(ch4_sf_df) +
      geom_point(aes(date, ch4_slope, colour = cyl_n)) + 
      ylab(parse(text = "`CH4 Slope (targets)`")) + 
      scale_x_datetime(date_labels = "%d/%m/%Y") +
      theme_bw() + 
      theme(axis.title.x = element_blank())
    
    co2_slope2 <- ggplot(co2_sf_df) + 
      geom_point(aes(date, co2_slope, colour = cyl_n)) + 
      ylab(parse(text = "`CO2 Slope (targets)`")) + 
      scale_x_datetime(date_labels = "%d/%m/%Y") +
      ylim(c(0.995, 1.005)) +
      theme_bw() + 
      theme(axis.title.x = element_blank())
    
    ch4_slope2 <- ggplot(ch4_sf_df) +
      geom_point(aes(date, ch4_slope, colour = cyl_n)) + 
      ylab(parse(text = "`CH4 Slope (targets)`")) + 
      scale_x_datetime(date_labels = "%d/%m/%Y") +
      ylim(c(0.995, 1.005)) +
      theme_bw() + 
      theme(axis.title.x = element_blank())
    
    co2_slope_av <- ggplot(co2_sf_df) + 
      geom_point(aes(date, estimate_co2_slope)) + 
      ylab(parse(text = "`Average CO2 Slope (targets)`")) + 
      scale_x_datetime(date_labels = "%d/%m/%Y") +
      ylim(c(0.995, 1.005)) +
      theme_bw() + 
      theme(axis.title.x = element_blank())
    
    ch4_slope_av <- ggplot(ch4_sf_df) +
      geom_point(aes(date, estimate_ch4_slope)) + 
      ylab(parse(text = "`Average CH4 Slope (targets)`")) + 
      scale_x_datetime(date_labels = "%d/%m/%Y") +
      ylim(c(0.995, 1.005)) +
      theme_bw() + 
      theme(axis.title.x = element_blank())
    
    pdf(file = paste0(out, fn, "_cal_plots.pdf"), width = 7.5, height = 11, paper = "a4")
    print(ggpubr::ggarrange(co2_slope, ch4_slope, ncol = 1, align = "v"))
    print(ggpubr::ggarrange(co2_slope2, ch4_slope2, ncol = 1, align = "v"))
    print(ggpubr::ggarrange(co2_slope_av, ch4_slope_av, ncol = 1, align = "v"))
    dev.off()
    
  }
  
  # Return cal factors from function
  if(cal_method == "linear"){
    return(list(co2_cal_factors = co2_lm_df, ch4_cal_factors = ch4_lm_df))
    
  }else if(cal_method == "target"){
      return(list(co2_cal_factors = co2_sf_df, 
                  ch4_cal_factors = ch4_sf_df))
    
    }else if (cal_method == "fixed"){
        return(list(co2_cal_factors = co2_fixed_values, ch4_cal_factors = ch4_fixed_values))
      
      }else{
        return(NULL)
        }
      
}

# Filter calibrations based on 'bad dates' list and statistics from the cals
# themselves. Applied to the output of calc_cal_factors function (a list of 2
# dataframes.)

flag_cals <- function(cal_list, baddates_csv_fp, fn, cal_method){
  
  if(cal_method %in% c("linear", "target")){
    # Initialise flag col, set as 0
    
    cal_list <- cal_list %>% 
      map(~mutate(.x, badflag = 0))
    
    # # Remove any additional problem dates (where instrument was reported as not
    # # working properly so bad values are not included in the cal slopes)
    # 
    # bd <- read.csv(baddates_csv_fp) %>% 
    #   mutate(startdate = dmy(startdate),
    #          enddate = dmy(enddate))
    # 
    # # Create a sequence of dates
    # bd <- bd %>% 
    #   mutate(baddates = map2(startdate, enddate, ~seq(.x, .y, by = "1 day")))
    # 
    # baddates <- bd$baddates %>% 
    #   unlist() %>% 
    #   as_date()
    # 
    # # Flag cals based on baddates and if flag is NaN. 
    # cal_list <- cal_list %>% 
    #   map(~mutate(.x, badflag = ifelse(filedate %in% baddates, 1, badflag))) %>% 
    #   map(~mutate(.x, badflag = ifelse(is.na(badflag), 1, badflag)))
    
    if(cal_method == "linear"){
      # Filter cals based on Z value and standard error
      # If intercept standard error is greater than 50, flag
      # If slope standard error is greater than 1, flag  
      
      # Make badflag 1 when fit SE is above threshold and z score is above threshold
      # Make badflag 1 when SE is nan - this is because there isn't a curve (i.e. one point cal)
      # 1 point cals should be processed using 'target' method
      
      cal_list <- cal_list %>% 
        map(~mutate_at(.x, vars(5), list(badflag = ~ifelse(. >5, 1, badflag)))) %>% 
        map(~mutate_at(.x, vars(6), list(badflag = ~ifelse(. >1, 1, badflag)))) %>% 
        map(~mutate_at(.x, vars(5), list(badflag = ~ifelse(is.na(.), 1, badflag)))) %>% 
        map(~mutate_at(.x, vars(6), list(badflag = ~ifelse(is.na(.), 1, badflag)))) %>% 
        map(~mutate_at(.x, vars(11), list(badflag = ~ifelse(abs(.) > z_score_thresh, 1, badflag)))) %>% 
        map(~mutate_at(.x, vars(12), list(badflag = ~ifelse(abs(.) > z_score_thresh, 1, badflag))))
      
    } else if (cal_method == "target") {
      
      # Haven't got standard error for fit so for now just flag on Z score.
      # Don't calculate an intercept so just use slope
      # Flag for low slope values - these have been calculated using bad data, 
      # not captured by other methods. Need to improve on this in future.
      
      cal_list <- cal_list %>%
        map(~mutate_at(.x, vars(11), list(badflag = ~ifelse(abs(.) > z_score_thresh, 1, badflag)))) %>%
        map(~mutate_at(.x, vars(4), list(badflag = ~ifelse(. < 0.9, 1, badflag)))) %>%
        map(~mutate_at(.x, vars(4), list(badflag = ~ifelse(. > 1.1, 1, badflag)))) %>% 
        map(~mutate_at(.x, vars(4), list(badflag = ~ifelse(is.na(.), 1, badflag))))
      
    }
    
    
    # Write files to .csv (target cals have extra column because of inclusion of
    # none averaged slope)
    
    if(cal_method %in% c("linear", "fixed")){
      write.csv(cal_list[['co2_cal_factors']][1:14], 
              file = paste0(out, fn, "_cal_factors_co2.csv"), 
              row.names = F)
      
      write.csv(cal_list[['ch4_cal_factors']][1:14], 
                file = paste0(out, fn, "_cal_factors_ch4.csv"), 
                row.names = F)
    }
    
    if(cal_method == "target"){
      # Output the calibration results as a csv
      write.csv(cal_list[['co2_cal_factors']][1:15], 
                file = paste0(out, fn, "_cal_factors_co2.csv"), 
                row.names = F)
      
      write.csv(cal_list[['ch4_cal_factors']][1:15], 
                file = paste0(out, fn, "_cal_factors_ch4.csv"), 
                row.names = F)
    }
   
    return(cal_list)
    
  }
  
  # If fixed values used - no need to do anything, return original list
  
  if(cal_method == "fixed"){
    return(cal_list)
  }
}

# Interpolate cals, ignoring those that are flagged
# Applied to flagged list of cal factors

interp_cal_series <- function(cal_list_f, dat, cal_method){
  
  if(cal_method == "fixed"){
    # No need to run the interpolation step, return input
    return(cal_list_f)
  }
  
  if(cal_method %in% c("linear", "target")){
    # If badflag == 1, make cal slope and intercept NA - syntax is horrible
    cal_list_f <- cal_list_f %>% 
      map(~mutate_at(.x, vars(3), list(~ifelse(badflag == 1, NA, .)))) %>% 
      map(~mutate_at(.x, vars(4), list(~ifelse(badflag == 1, NA, .))))
    
    # Create a timeseries of CO2 cal factors
    co2_cal_interp <- cal_list_f[['co2_cal_factors']][1:14]
    
    # If first date from dat happens before first cal date add that date, 
    # so interpolation happens for entire time series
    
    if(co2_cal_interp$date[1] > dat[[1]]$date[1]){
      
      co2_cal_interp <- co2_cal_interp %>% add_row(date = dat[[1]]$date[1], 
                                                   filedate = ymd(substr(names(dat[1]), 14, 21)),
                                                   .before = 1)
    }
    
    
    # And at the end...
    if(co2_cal_interp$date[nrow(co2_cal_interp)] < dat[[length(dat)]]$date[nrow(dat[[length(dat)]])]){
      
    co2_cal_interp <- co2_cal_interp %>% add_row(date = dat[[length(dat)]]$date[nrow(dat[[length(dat)]])], 
                                                 filedate = ymd(substr(names(dat[length(dat)]), 14, 21)),
                                                 .after = nrow(co2_cal_interp))
    }
    # Pad and interpolate
    co2_cal_interp <- pad_ts(co2_cal_interp, datecol = "date", interval = 1)
    gc()
    
    # The slopes and intercepts set for 2018 - 2021 are the mean values for May 
    # 2022 - September 2023 calibrations.
    co2_cal_interp <- noxpro::InterpSeries(co2_cal_interp, 
                                           cols = c("estimate_co2_intercept", "estimate_co2_slope"))
    
    # If all points are NA no interpolation will happen - force column to be NA
    if(!"estimate_co2_intercept_interp" %in% names(co2_cal_interp)){
      co2_cal_interp <- co2_cal_interp %>% 
        mutate(estimate_co2_intercept_interp = NA)
    }
    
    if(!"estimate_co2_slope_interp" %in% names(co2_cal_interp)){
      co2_cal_interp <- co2_cal_interp %>% 
        mutate(estimate_co2_slope_interp = NA)
    }
    
    co2_cal_interp <- co2_cal_interp %>% 
      select(date, 
             co2_int = estimate_co2_intercept_interp,
             co2_slope = estimate_co2_slope_interp)
    
    
    #CH4
    ch4_cal_interp <- cal_list_f[['ch4_cal_factors']][1:14]
    
    # Check dates so interpolation happens for entire time series
    if(ch4_cal_interp$date[1] > dat[[1]]$date[1]){
    
    ch4_cal_interp <- ch4_cal_interp %>% add_row(date = dat[[1]]$date[1], 
                                                 filedate = ymd(substr(names(dat[1]), 14, 21)),
                                                 .before = 1)
    }
    
    # And at the end...
    if(ch4_cal_interp$date[nrow(ch4_cal_interp)] < dat[[length(dat)]]$date[nrow(dat[[length(dat)]])]){
      
    ch4_cal_interp <- ch4_cal_interp %>% add_row(date = dat[[length(dat)]]$date[nrow(dat[[length(dat)]])], 
                                                 filedate = ymd(substr(names(dat[length(dat)]), 14, 21)),
                                                 .after = nrow(ch4_cal_interp))
    }
    
    ch4_cal_interp <- pad_ts(ch4_cal_interp, datecol = "date", interval = 1)
    gc()
    
    ch4_cal_interp <- noxpro::InterpSeries(ch4_cal_interp, 
                                           cols = c("estimate_ch4_intercept", "estimate_ch4_slope"))
    
    if(!"estimate_ch4_intercept_interp" %in% names(ch4_cal_interp)){
      ch4_cal_interp <- ch4_cal_interp %>% 
        mutate(estimate_ch4_intercept_interp = NA)
    }
    
    if(!"estimate_ch4_slope_interp" %in% names(ch4_cal_interp)){
      ch4_cal_interp <- ch4_cal_interp %>% 
        mutate(estimate_ch4_slope_interp = NA)
    }
    
    ch4_cal_interp <- ch4_cal_interp %>% 
      select(date, 
             ch4_int = estimate_ch4_intercept_interp,
             ch4_slope = estimate_ch4_slope_interp)
    
    return(list(co2_cal_interp = co2_cal_interp, ch4_cal_interp = ch4_cal_interp))
    
  } else {
    print("Cal method not understood.")
  }
}

scale_data <- function(dat, cal_interp_list, scaled_file_dir, cal_method){
  
  #initialise list
  dat_scaled = list()
  
  for(i in 1:length(dat)){
    filename <- names(dat[i])
    df <- dat[[i]]
    filedate <- ymd(substr(filename, 14, 21))
    
    if(cal_method %in% c("linear", "target")){
      
    # Join the interpolated data to the raw data for scaling
    # Files sometimes start a few minutes before midnight so filter the cal interp
    # ts to include filedate and day before
    df_j <- df %>% 
      left_join(cal_interp_list[['co2_cal_interp']], by = "date") %>% 
      left_join(cal_interp_list[['ch4_cal_interp']], by = "date")
    }
    
    if(cal_method == "fixed"){
      df_j <- df %>% 
        mutate(co2_int = cal_interp_list[['co2_cal_factors']][[2]],
               co2_slope =  cal_interp_list[['co2_cal_factors']][[1]],
               ch4_int = cal_interp_list[['ch4_cal_factors']][[2]],
               ch4_slope = cal_interp_list[['ch4_cal_factors']][[1]])
    }
    
    # Apply the calibration
    df_j$co2_scaled <- (df_j$CO2_dry-df_j$co2_int)/df_j$co2_slope
    df_j$ch4_scaled <- (df_j$CH4_dry-(df_j$ch4_int/1000))/df_j$ch4_slope
    
    # Average the concentration values to 1 minute means
    df_1min <- df_j %>%
      select(date,
             CO2_dry,
             CO2_wet,
             CH4_dry,
             CH4_wet,
             co2_scaled,
             ch4_scaled,
             H2O,
             co2_slope,
             co2_int,
             ch4_slope,
             ch4_int
             ) %>%
      mutate(date = floor_date(date, "minute")) %>%
      group_by(date) %>%
      summarise_all(mean, na.rm = T)

    # Take the max value of the flags in that minute
    df_1min_f <- df_j %>%
      select(date,
             badflag) %>%
      mutate(date = floor_date(date, "minute")) %>%
      group_by(date) %>%
      summarise_all(max, na.rm = T)

    # Take the modal value for cal flag and valve state
    df_1min_calf <- df_j %>%
      select(date,
             calflag,
             mpv) %>%
      mutate(date = floor_date(date, "minute")) %>%
      group_by(date) %>%
      summarise_all(getmode)

    # Add the flags back to the data
    df_1min <- df_1min %>%
      left_join(df_1min_f,
                "date") %>%
      left_join(df_1min_calf,
                "date")

    write.csv(df_1min, paste0(scaled_file_dir, filename), row.names = F)
    
    dat_scaled[[i]] <- df_1min
    names(dat_scaled)[i] <- filename
    gc()
  }
  return(dat_scaled)
}
