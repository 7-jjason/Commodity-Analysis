extract_split <- function(data, commodity_pattern, split_name = "split 1") {
  x <- lapply(names(data), function(date_name) {
    date_list <- data[[date_name]]
    idx <- grep(commodity_pattern, names(date_list))
    
    if (length(idx) > 0) {
      df <- date_list[[idx[1]]][[split_name]]
      df$date <- date_name
      df
    } else {
      NULL
    }
  }) |> bind_rows()
  x |> 
    relocate(date, .before = 1) |>
    mutate(date = str_replace_all(date, "_", "-"),
           date = ymd(date)) |>
    rename("Recency" = "Type",
           "Statistic" = "type")
}