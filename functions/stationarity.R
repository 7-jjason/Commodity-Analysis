stationarity <- function(data, date_col, lags) {
  data <- data |>
    as.data.frame() |>
    dplyr::mutate(dplyr::across(-!!rlang::sym(date_col), 
                  ~ if(all(. > 0, na.rm = TRUE)) {
                    log(.) - dplyr::lag(log(.), lags)
                  } else {
                    . - dplyr::lag(., lags)
                  })
    )
  return(data)
}