
# columns in quotations
cansim_fill_vector <- function(data, Date, fill_col, VECTOR, sleep_time) {
  for (i in 1:nrow(data)) {
    if (is.na(data[[i, "fill_col"]])) {
      
      # call vector from cansim
      temp_df <- get_cansim_vector(data[[i, "VECTOR"]]) 
      
      # get missing date
      temp_date <- data[[i, "Date"]]
      
      # extract product name
      filt_df <- temp_df |>
        mutate(Date = case_when(
          Date >= as.Date("2020-01-01") & Date < as.Date("2021-01-01") ~ as.Date("2020-07-01"),
          Date >= as.Date("2021-01-01") & Date < as.Date("2022-01-01") ~ as.Date("2021-07-01"),
          Date >= as.Date("2022-01-01") & Date < as.Date("2023-01-01") ~ as.Date("2022-07-01"),
          Date >= as.Date("2023-01-01") & Date < as.Date("2024-01-01") ~ as.Date("2023-07-01"),
          Date >= as.Date("2024-01-01") & Date < as.Date("2025-01-01") ~ as.Date("2024-07-01"),
          Date >= as.Date("2025-01-01") & Date < as.Date("2026-01-01") ~ as.Date("2025-07-01"),
          TRUE ~ Date
        )) |>
        filter(Date == as.Date(temp_date)) 
     
       # check if results exist and store
      if (nrow(filt_df) > 0) {
        data[i, "fill_col"] <- as.character(filt_df$fill_col[1])
      } else {
        print(paste("No match found for row", i))
      }
      
      Sys.sleep(sleep_time)
      
    }
  }
  return(data)
}






