# Load required packages
library(stringr)
library(dplyr)
library(magrittr)
library(lubridate)
library(purrr)
library(ggplot2)
library(readr)
library(tidyr)
library(broom)

# Picarro data processing set-up script
# set root directory
root <- "C:/Users/frees/Desktop/GHG DATA PROCESSING/"

# raw data file paths
cal_dir <- paste0(root, "raw_data/calib/")

data_dir <-  paste0(root,"raw_data/data/")

# functions file path
utils <- paste0(root, "functions/picarro_readers.R")
load_data <- paste0(root, "functions/load_data.R")
flag_data <- paste0(root, "functions/flag_data.R")
process_cals <- paste0(root, "functions/proc_cals.R")

# set output directory
out <-  paste0(root,"output/")

if(!file.exists(out)){
  dir.create(out)
}

# Cylinder concentrations information
cylinder_info <- paste0(root, "input/neumayer_cal_cylinders.csv")

#Bad dates csv
baddates_csv_fp <- paste0(root, "input/neumayer_baddates.csv")

#Output directory
scaled_file_dir <- paste0(out, "scaled/")

if(!file.exists(scaled_file_dir)){
  dir.create(scaled_file_dir)
}

# Set how calibrations should be applied e.g. linear 3 point cal, target cal or  
# fixed slope and intercept. 
# Options are: "linear", "target" or "fixed"
cal_method <- "target" 

# If using fixed values specify here:
co2_fixed_values = c(slope = 0.98, intercept = 0)
ch4_fixed_values = c(slope = 0.98, intercept = 0)

# Set basic file name
fn <- "neu_target_test_20231123"