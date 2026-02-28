
clean_beginning <- function(data, cols = NULL) {
  
  # capture bare names as strings
  cols <- rlang::enquo(cols)
  
  if (rlang::quo_is_null(cols)) {
    # default is set to use all columns
    cols <- names(data)
  } else {
    # get given column names
    cols <- rlang::eval_tidy(cols)
  }
  
  # find first completed row for specified columns / all columns
  while (nrow(data) > 0 && !complete.cases(first(data[, cols]))) {
    data <- data[-1,]
  }
  return(data)
}
