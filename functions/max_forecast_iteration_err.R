forecast_iteration_err <- function(model, data.est, data.fc, h){
  require(fable)
  
  # Utility function to re-estimate the var on new data
  reest_var <- function(model, new_data){
    # pull results from model
    results <- model[[1]][[1]]
    # pull order of AR component
    order <- as.numeric(results$fit$spec["p"])
    # pull names of endogenous variables
    varnames <- colnames(results$fit$fits) %>% lapply(sym)
    # pull names of exogenous variables
    temp <- rownames(results$fit$coef)
    pos.const <- which(temp == 'constant')
    if (length(pos.const) == 0) {
      xvarnames <- temp[(pos.const+1):length(temp)]  %>% lapply(sym)     
      model_new <- model(new_data,
                         VAR(vars(!!!varnames) ~ AR(order) + xreg(!!!xvarnames)))
    } else if(pos.const == length(temp)){
      model_new <- model(new_data, VAR(vars(!!!varnames) ~ AR(order)))  
    } else {
      xvarnames <- temp[(pos.const+1):length(temp)]  %>% lapply(sym)     
      model_new <- model(new_data,
                         VAR(vars(!!!varnames) ~ AR(order) + xreg(!!!xvarnames)))
    }
    return(model_new)
  }
  
  #h <- nrow(data.fc)
  # Create forecast object for entire horizon to be filled in 
  fc <- forecast(model, new_data = data.fc)
  
  varnames <- colnames(model[[1]][[1]]$fit$model$fitted.values)
  
  
  #Initialize Model
  model.i <- model
  # Initialize estimation sample
  data.est.i <- data.est
  
  
  ## Start Iteration
  for (i in 1:h){
    #Create a one step ahead forecast
    fc.i <- forecast(model.i, new_data = data.fc[i,])
    
    # turn into list
    ls.fc.i <- as.list(fc.i$.mean)
    names(ls.fc.i) <- varnames
    
    # replace missing values in data.fc with forecast
    data.fc[i, ] <- replace_na(data.fc[i,], ls.fc.i)
    
    #Append data.est (estimation sample) with the newest data
    data.est.i <- full_join(data.est.i, data.fc[i,])
    tail(data.est.i)
    #Re-estimate model with new data
    model.i <- reest_var(model.i, data.est.i)
    
  }
  # Overwrite values in fable forecast object
  fc$.mean <- data.fc[,-1]
  return(fc)
}
