
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
} # https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
