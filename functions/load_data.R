#function to load data from the Picarro at Neumayer
#creates a calibration data frame
#creates a list of data frames of raw data

load_data <- function(data_dir){
  #list files--------------------------------------------------
  data_files <- list.files(data_dir, pattern = ".csv", full.names = T)
  
  #read in --------------------------------------------------------
  dat <- read_picarro_data(data_files)
  
  #which files are problem files? ---------------------------------
  #read in some files as a list and name the list elements using the file names...
  names(dat) <- basename(data_files)
  
  #which have a "Read Error" msg? Test by seeing if class is character.
  error_files <- dat %>%
    keep(~is.character(.x)) %>%
    names()
  
  null_files <- dat %>% 
    keep(~is.null(.x)) %>% 
    names()
  
  dat <- dat[!names(dat) %in% c(error_files, null_files)] 
  
  return(dat)
  
}

load_cal <- function(cal_dir){
  
  #list files--------------------------------------------------
  cal_files <- list.files(cal_dir, pattern = ".csv", full.names = T)
  
  #read in --------------------------------------------------------

  cal <- read_picarro_cal(cal_files)
  
  #return
  return(cal)
}
