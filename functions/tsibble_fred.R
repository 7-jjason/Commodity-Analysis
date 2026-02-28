# tsibble_fred 

tsibble_fred <- function(series_id, name = NA, ...){
  
  # packages
  require(fredr)
  require(lubridate)
  
  # get series from fred
  xx <- fredr(series_id, ...)  
 
  
  # determine frequency
 
  series <- xx$value     # set x to be the "value" column from xx
  time <- xx$date
  
  ## Determine Frequency
  
  time.diff <- time[2] - time[1]
  
  # weekly frequency
  if (time.diff == 7){
    time <- yearweek(time)
  }
  
  # monthly frequency 
  if (25 < time.diff & time.diff <= 33){
    time <- yearmonth(time) 
  }
  
  # quarterly frequency
  if (70 < time.diff & time.diff <= 100){
    time <- yearquarter(time)
  }
  
  # yearly frequency
  if (300 < time.diff & time.diff <= 400){
    time <- year(time)
  }
  
  # Create tsibble
  
  x <- tsibble(date = time, series = series, index = date)
  if(!is.na(name)){
    names(x)[2] <- name
  }
  
  return(x)  
}   
# usage

# series_id <- "GDP" 
# start <- "2010-01-01" need to format as as.Date

# usgdp <- tsibble_fred(series_id,  observation_start = as.Date("1990-01-01"))






