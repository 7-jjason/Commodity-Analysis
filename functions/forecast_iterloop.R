


forecast_iter <- function(data, 
                          variables,
                          est.start, 
                          est.end, 
                          fc.start, 
                          fc.end,
                          h,
                          lag){
  # define windows 
  data.est <- filter_index(data, est.start ~ est.end)
  data.fc <- filter_index(data, fc.start ~ fc.end)
  
  # appends total forecasted amount - amount already forecasted, correct?
  data.fc <- append_row(data.fc, 
                        h - nrow(data.fc))
  
  for (i in 1:h) {
    # fit model, iterating 1 date forward from the beginning with each loop
    fit <- model(data.est[i:nrow(data.est)], 
                 fable::VAR(vars(data[[variables]]) ~ AR(lag)))
    # forecast 1 step ahead
    fct <- forecast(fit, h = 1)
    # store missing value column positions
    ind.missing <- which(is.na(data.fc[i,]))
    # wouldn't work without as.list(unlist()). 
    # also removed -1 because it called on the column to the left of what was 
    # wanted. I might be misinterpreting, but I thought that the moving average 
    # meant that the estimate would remove an old value with the addition of 
    # each new value, so I put indexed in the the model function itself
    data.fc <- fct$.mean[ind.missing]
    # extend estimate with newly forecasted row/period
    data.est <- full_join(data.est, data.fc[i,])
  }
  return(data.est)
}
























