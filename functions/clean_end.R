
clean_end <- function(data) {
  while(complete.cases(last(data)) == FALSE) {
    row <- nrow(data)
    data <- data[-row,]
  }
  return(data)
}
