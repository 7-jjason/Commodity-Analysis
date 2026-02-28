portion_date <- function(data, date, prop = 1) {
  
  date <- rlang::as_name(rlang::enquo(date))
  
  row_count <- nrow(data)
  rows_used <- round(row_count * prop) 
  output <- data |>
    head(rows_used) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::pull(!!sym(date))|>
    as.Date() |>
    as.character()
  
  return(output)
}