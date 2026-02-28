
change_freq <- function(data, date = 1, by = 1, new_freq, trans_type) {
  
  # capture bare names as strings
  date <- rlang::as_name(rlang::enquo(date))
  by <- rlang::as_name(rlang::enquo(by))
  
  # set to tsibble
  if(tsibble::is_tsibble(data) == FALSE) {
    data <- tsibble::as_tsibble(data)
  } 
  
  # set aggregation function
  trans <- switch(trans_type,
                  "mean" = function(x) mean(x, na.rm = TRUE),
                  "median" = function(x) median(x, na.rm = TRUE),
                  "mode" = calculate_mode,
                  "sum" = function(x) sum(x, na.rm = TRUE),
                  "max" = function(x) max(x, na.rm = TRUE),
                  "min" = function(x) min(x, na.rm = TRUE),
                  "eop" = function(x) dplyr::last(x),
                  "bop" = function(x) dplyr::first(x),
                  stop("Invalid trans_type"))
  
  # set frequency function
  freq = switch(as.character(new_freq),
                "52" = tsibble::yearweek,
                "12" = tsibble::yearmonth,
                "4" = tsibble::yearquarter,
                "1" = lubridate::year,
                stop("Invalid new_freq"))
  
  current <- Sys.Date()
  
  data <- data |>
    tsibble::index_by(new_freq = freq(!!sym(date))) |>
    dplyr::summarize(!!sym(by) := trans(!!sym(by))) |>
    dplyr::filter(new_freq < freq(current)) |>
    dplyr::rename(!!sym(date) := new_freq) 
  
  return(data)
}
