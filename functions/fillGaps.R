
fillGaps <- function(data, fill_col = 2, fill_type = "downup") {
  
  # no use in using has_gaps - some series include the gaps with NA values.
  # this doesn't register as a gap and won't fill those NA values.
  
  # sets as tsibble
  if(!tsibble::is_tsibble(data)) {
    data <- tsibble::as_tsibble(data)
  } 
  
  # removes NA values at the beginning of the series
  while (is.na(dplyr::first(dplyr::pull(data[, fill_col])))) {
    data <- data[-1,]
  }
  
  # if given an integer or default, then this changes fill_col to that column name
  if(is.numeric(fill_col)) {
    fill_col <- names(data)[fill_col]
  }
  
  # actually fill gaps
  data <- data |> 
    tsibble::fill_gaps() |>
    tidyr::fill(all_of(fill_col), .direction = fill_type)
  
  return(data)
}
