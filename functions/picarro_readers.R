#Neumayer data readers

read_picarro_cal <- function(picarro_cal_list){
  purrr::map_dfr(picarro_cal_list, read_picarro_cal_worker)
}

read_picarro_cal_worker <- function(file){
  
  file <- file
  
  #skip file if it is empty...
  if (!file.size(file) == 0) {
    
    df <- read.csv(file)
    
    #return date from file name and add to start and end times
    filedate <- basename(file) %>% 
      stringr::str_remove(".csv") %>% 
      stringr::str_split(pattern = "_") %>% 
      sapply(magrittr::extract2, 3) %>% 
      lubridate::ymd()
    
    df$filedate <- filedate
    
    #combine date and time
    #2018 calibration files have hms format
    #2019 have ymd hms format
    
    df$StartTime = tryCatch({
      lubridate::ymd_hms(df$StartTime)
    },
    warning = function(w) {
      as.POSIXct(lubridate::ymd(df$filedate) + lubridate::hms(df$StartTime))
    })
    
    df$StopTime = tryCatch({
      lubridate::ymd_hms(df$StopTime)
    },
    warning = function(w) {
      as.POSIXct(lubridate::ymd(df$filedate) + lubridate::hms(df$StopTime))
    })
    
    #sometimes the calibration goes over midnight and if adding file date the 
    #stop time will be before the start time
    for (i in 1:nrow(df)) {
      if (df$StopTime[i] < df$StartTime[i]) {
        df$StopTime[i] <- df$StopTime[i] + 86400
      }
    }
    
    #sometimes calibrations continue after midnight and may have wrong date 
    #if adding file date
    #start on 2 - the first start stop time should be correct? who knows...
    for (i in 2:nrow(df)) {
      
      if (df$StartTime[i] < df$StartTime[i-1]) {
        df$StartTime[i] <- df$StartTime[i] + 86400
      }
      
      if (df$StopTime[i] < df$StopTime[i-1]) {
        df$StopTime[i] <- df$StopTime[i] + 86400
      }
      
    }
    
    #remove PicTime and DateTimecolumn which is in some files! Not used and 
    #confusing!
    if ("PicTime" %in% names(df)) {
      df <- df %>% dplyr::select(-PicTime)
    }
    
    if ("DateTime" %in% names(df)) {
      df <- df %>% dplyr::select(-DateTime)
    }
    
    #make date column which can be used for merging later.
    #Set this to cal start time
    df$date <- df$StartTime
    
    #return
    df
    
  }
}

read_picarro_data <- function(picarro_data_list){
  purrr::map(picarro_data_list, safer_read_picarro_data_worker)
  #map(picarro_data_list, read_picarro_cal_worker)
}

read_picarro_data_worker <- function(file){
  
  if (!file.size(file) == 0) {
    df <- readr::read_csv(file, 
                   col_select = 1:6,
                   col_types = "cnnnn")
    
    #can't coerce some columns to numbers using read_csv (don't know why)  
    #this tends to be the last column
    if(any(sapply(df[2:6], is.character))){
      
      df[2:6] <- sapply(df[2:6],as.numeric)
    
    }
    
    # Most times the files have headers but occasionally not due to file writing. 
    # This just makes sure the column names are what they should be. 
    df <- df %>% setNames(nm = c("PicTime",
                                 "CO2_wet",
                                 "CO2_dry",
                                 "CH4_wet",
                                 "CH4_dry",
                                 "H2O"))
 
    df$date <- lubridate::ymd_hms(df$PicTime) %>%
      lubridate::round_date(unit = "second") 
    
    df <- dplyr::select(df, 
                        -PicTime)
    
    #return
    df
    
  }
}

safer_read_picarro_data_worker <- purrr::possibly(read_picarro_data_worker, 
                                                  otherwise = "Read Error")

#create a time series to join to (irregular time series in Picarro data)
pad_ts <- function(df, datecol, interval){
  
  ts <- df[[datecol]] #changed this 08/12/23 - not sure why this has stopped working? Maybe tbl vs df???
  padded_ts <- seq(min(ts, na.rm = T), max(ts, na.rm = T), interval)
  
  padded_ts <- data.frame(padded_ts)
  names(padded_ts) <- datecol
  
  dplyr::left_join(padded_ts, df, datecol)
  
}

#tidy up files with broken lines
picarro_tidy <- function(file){
  
  #use readlines to read in file and create a large character object from each 
  #line
  datRaw <- readLines(file)
  
  #get line lengths
  lineLengths <- unlist(lapply(datRaw,nchar))
  mytable <- table(lineLengths)
  
  #pull out the most common line length - assume that this is the correct line 
  #length when the file writing has been successful
  modalLine <- as.numeric(names(mytable[mytable == max(mytable)]))
  
  #store header
  header <- datRaw[1] %>% 
    stringr::str_split(",") %>% 
    unlist()
  
  #select lines which have line length equal to most common line length - this 
  #is slow! So don't read all files like this...
  dat <- datRaw[which(lineLengths == modalLine)] %>% 
    stringr::str_split(",") %>% 
    purrr::reduce(rbind) %>% 
    data.frame() %>% 
    tibble::tibble() %>% 
    setNames(header)
  
  dat[,2:6] <- sapply(dat[,2:6],as.numeric)
  
  dat
}

getmode <- function(v, na.rm = T) {
  if(na.rm){
    v = v[!is.na(v)]
  }
  
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
