### MODE ###

mode = function(df, col, group, bin_size) {
  df[[group]] <- as.character(df[[group]]);
  df |>
    # group by group; .data lets us refer to variables with strings
    # tidy-eval
    group_by(.data[[group]]) |>
    summarise(
      mode = {
        grouped <- .data[[col]]
        # create bins
        breaks <- seq(from = min(grouped), 
                      to = max(grouped), 
                      by = bin_size);
        # apply bins to column of interest
        intervals <- cut(grouped, 
                         breaks = breaks);
        # create frequency table
        freq <- table(intervals);
        # calculate bounds of binned modes
        bounds <- which.max(freq);
        # calculate mean from bounds
        (breaks[bounds] + breaks[bounds + 1]) / 2;
      }
    )
}

