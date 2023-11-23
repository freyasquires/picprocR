# Process Picarro data
# load set-up script and functions
source("C:/Users/frees/Desktop/GHG DATA PROCESSING/input/neumayer_setup.R")
source(utils)
source(load_data)
source(flag_data)
source(process_cals)

# Read in the files
dat <- load_data(data_dir)
cal <- load_cal(cal_dir)
gc()

# Create calibration and basic data quality flags
dat <- flag_data(cal, dat,
                 baddates_csv_fp = baddates_csv_fp,
                 fn = fn)

gc()

# Cal plots and calculate cal slopes and intercepts
cal_lm <- calc_cal_factors(cal, 
                           cylinder_info, 
                           fn = fn,
                           z_score_threshold = 3,
                           cal_method = cal_method)

# Filter cals 
cal_lm_f <- flag_cals(cal_lm, 
                      baddates_csv_fp, 
                      fn = fn,
                      cal_method = cal_method)


cal_interp <- interp_cal_series(cal_lm_f, 
                                dat, 
                                cal_method = cal_method)
gc()

#Scale the data
dat_scaled <- scale_data(dat = dat, 
                         cal_interp_list = cal_interp, 
                         scaled_file_dir = scaled_file_dir,
                         cal_method = cal_method)

print(praise::praise("${Exclamation}! This ${adjective} data has been processed ${adverb_manner}!"))

