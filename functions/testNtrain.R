testNtrain <- function(data, train_proportion) {
  matTrain <- lapply(data, function(x) {
    n <- floor(nrow(x) * train_proportion)
    head(x, n)
  })
  matTest <- lapply(data, function(x) {
    n <- nrow(x) - floor(nrow(x) * train_proportion)
    tail(x, n)
  })
  return(list(train = matTrain, test = matTest))
}