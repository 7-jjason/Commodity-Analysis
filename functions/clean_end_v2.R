
clean_end <- function(data, cols = NULL) {
  
  # capture bare names as strings
  cols <- rlang::enquo(cols)
  
  if (rlang::quo_is_null(cols)) {
    # default is set to use all columns
    cols <- names(data)
  } else {
    # get given column names
    cols <- rlang::eval_tidy(cols)
  }
  
  while(nrow(data) > 0 && !complete.cases(last(data[, cols]))) {
    row <- nrow(data)
    data <- data[-row,]
  }
  return(data)
}


  