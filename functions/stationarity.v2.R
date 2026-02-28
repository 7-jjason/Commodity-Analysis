stationarity <- function(data, date_col, no_trans, lags) {
  # Remove columns from stationary transformation
  cols_to_trans <- setdiff(names(data), c(date_col, no_trans))
  
  data <- data |>
    as.data.frame() |>
    dplyr::mutate(dplyr::across(all_of(cols_to_trans), 
                  ~ if (all(. > 0, na.rm = TRUE)) {
                    log(.) - dplyr::lag(log(.), lags)
                  } else {
                    . - dplyr::lag(., lags)
                  })
    )
  return(data)
}
