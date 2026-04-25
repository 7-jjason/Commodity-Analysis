# Get Data
# updated_data <- readRDS("data/cftc_petroleum_dynamically_updated.rds")

# Get Data by Commodity and Split
nat_gas_nyme <- extract_split(updated_data, 
                              "NAT GAS NYME - NEW YORK MERCANTILE EXCHANGE", 
                              "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")
henry_hub <- extract_split(updated_data,
                           "HENRY HUB - NEW YORK MERCANTILE EXCHANGE",
                           "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")
hendry_hub_basis <- extract_split(updated_data,
                                  "HENRY HUB BASIS - ICE FUTURES ENERGY DIV",
                                  "split 1")
hendry_hub_index <- extract_split(updated_data,
                                  "HENRY HUB INDEX - ICE FUTURES ENERGY DIV",
                                  "split 1")
waha_basis <- extract_split(updated_data,
                            "WAHA FIN BASIS - ICE FUTURES ENERGY DIV",
                            "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")
hsc_basis <- extract_split(updated_data,
                           "HSC FIN BASIS - ICE FUTURES ENERGY DIV",
                           "split 1")
propane_nyme <- extract_split(updated_data,
                              "PROPANE - NEW YORK MERCANTILE EXCHANGE",
                              "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")


# See All Commodities Across Time
lapply(updated_data, names)
# See Unique Commodities
unique(unlist(lapply(updated_data, names)))

# Prepare Plot Data

plot_data <- multi_join(
  henry_hub |> mutate(Group = "HH"),
  nat_gas_nyme |> mutate(Group = "NG"),
  propane_nyme |> mutate(Group = "PR")
) |>
  filter(
    Statistic == "Changes in Commitments from Last Week (2500 MMBtus)",
    date >= as.Date("2025-11-01")
  )


ggplot() +
  geom_line(
    data = plot_data,
    aes(
      x = date,
      y = Values,
      colour = Group,
      group = Group
    ),
    linewidth = 1
  ) + 
  labs(
    # need to change the commodity each time - can fix it but first have to 
    # include the commodity name in the df somehow
    title = paste0("Change in Commitments from Last Week, Published ", 
                   format(max(plot_data$date, na.rm = TRUE), "%B %d, %Y"),
                   ".\nHenry Hub; Natural Gas NYME"),
    x = "",
    y = "Number of Contracts",
  ) +
  scale_x_yearmonth(breaks = plot_data$date,
                    labels = as.character(plot_data$date)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(~Variables, scales = "free_y", ncol = 3) 
