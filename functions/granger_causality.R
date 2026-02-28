# #############################################################################
# # Granger Causality # 
# #############################################################################

# pass grangercauslity a data frame without dates

granger_causality <- function(data, order = 2) {

  require(lmtest)
  
  # check data type for data.frame
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  
  # initialize list
  results <- list()
  # pull column names
  vars <- colnames(data)
  # clean data
  data <- na.omit(data)

  # run all pairs
  for (i in 1:(length(vars) - 1)) {
    for (j in (i+1):length(vars)) {
      x_name <- vars[i]
      y_name <- vars[j]
      
      # build formula
      formula_xy <- as.formula(paste(y_name, "~", x_name))
      formula_yx <- as.formula(paste(x_name, "~", y_name))
      
      # test pairs
      test_xy <- grangertest(formula_xy, order = order, data = data)
      test_yx <- grangertest(formula_yx, order = order, data = data)
    
      # store results
      results[[paste(x_name, "->", y_name)]] <- test_xy
      results[[paste(y_name, "->", x_name)]] <- test_yx
    }
  }
  
  results_df <- data.frame(
    test = names(results),
    p_value = map_dbl(results, ~ .x[2, "Pr(>F)"]),
    f_stat = map_dbl(results, ~ .x[2, "F"]),
    df = map_dbl(results, ~ abs(.x[2, "Df"]))
  )
  
  results_df <- results_df %>%
    mutate(
      sig_level = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01  ~ "**",
        p_value < 0.05  ~ "*",
        p_value < 0.10  ~ ".",
        TRUE            ~ "ns"  # not significant
      )
    )
  
  return(results_df)
}









