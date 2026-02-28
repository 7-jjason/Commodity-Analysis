
multi_join <- function(...){
  # tsibbles need to have same index name
  mylist <- list(...)
  x <- mylist[[1]]
  for(i in 1:(length(mylist)-1)) {
    x <- full_join(x, mylist[[i+1]])
  }
  return(x)
}
