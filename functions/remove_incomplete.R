remove_incomplete <- function(data, date, freq) {
  
  freq = switch(as.character(freq),
                "365" = as.Date,
                "52" = tsibble::yearweek,
                "12" = tsibble::yearmonth,
                "4" = tsibble::yearquarter,
                "1" = lubridate::year,
                stop("Invalid new_freq"))
  
  current <- freq(Sys.Date())
  
  data <- data |>
    filter(date < current)
  
  return(data)
}
