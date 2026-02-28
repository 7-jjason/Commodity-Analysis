
clean_beginning <- function(data) {
  while (complete.cases(first(data)) == FALSE) {
    data <- data[-1,]
  }
  return(data)
}
