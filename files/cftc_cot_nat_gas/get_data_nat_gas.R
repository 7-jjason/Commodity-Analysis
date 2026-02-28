source("Functions/extract_split.R")
source("Functions/max_multi_join.R")

# Get Data
updated_data_ng <- readRDS("Energy Analysis/data/cftc_nat_gas_dynamically_updated.rds")

# Get Data by Commodity and Split
nat_gas_nyme <- extract_split(updated_data_ng, 
                              "NAT GAS NYME - NEW YORK MERCANTILE EXCHANGE", 
                              "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

henry_hub <- extract_split(updated_data_ng,
                           "HENRY HUB - NEW YORK MERCANTILE EXCHANGE",
                           "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

nat_gas_ice_ld1 <- extract_split(updated_data_ng,
                                 "NAT GAS ICE LD1 - ICE FUTURES ENERGY DIV",
                                 "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

# See All Commodities Across Time
lapply(updated_data_ng, names)
# See Unique Commodities
unique(unlist(lapply(updated_data_ng, names)))

# Prepare Plot Data
plot_data_nat_gas <- multi_join(
  henry_hub |> mutate(Group = "HH"),
  nat_gas_nyme |> mutate(Group = "NG"),
  nat_gas_ice_ld1 |> mutate(Group = "PR")
) |>
  filter(
    Statistic == "Changes in Commitments from Last Week (2500 MMBtus)",
    date >= as.Date("2025-11-01")
  )


ggplot() +
  geom_line(
    data = plot_data_nat_gas,
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
    y = "x2500 MMBtus",
  ) +
  scale_x_yearmonth(breaks = plot_data$date,
                    labels = as.character(plot_data$date)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(~Variables, 
             scales = "free_y", 
             ncol = 3,
             labeller = label_wrap_gen(width = 30)) 
