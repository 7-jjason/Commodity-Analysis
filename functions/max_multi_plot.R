# max sties' multiplot

multi_plot <- function(data, col = "steelblue", ...){
  #var.index <- tsibble::index(data)
  data <- rename(data, index = tsibble::index(data))
  data.raw.long <- pivot_longer(data, !index)  
  ggplot(data.raw.long, aes(x = index, y = value, group = name)) + 
    geom_line(linewidth = 1, col = col, ...) +
    facet_grid(vars(name), scales = "free_y") + 
    theme(legend.position = "None") +
    ylab("") + xlab("")
  #+ scale_color_brewer(palette="Set1")
}
