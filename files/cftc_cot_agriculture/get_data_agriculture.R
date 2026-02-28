source("Functions/extract_split.R")
source("Functions/max_multi_join.R")

# Get Data
updated_data_ag <- readRDS("Energy Analysis/data/cftc_ag_dynamically_updated.rds")

# Get Data by Commodity and Split
wheat_srw <- extract_split(updated_data_ag, 
                           "WHEAT-SRW - CHICAGO BOARD OF TRADE", 
                           "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

wheat_hrw <- extract_split(updated_data_ag,
                           "WHEAT-HRW - CHICAGO BOARD OF TRADE",
                           "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

corn <- extract_split(updated_data_ag,
                      "CORN - CHICAGO BOARD OF TRADE",
                      "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

oats <- extract_split(updated_data_ag,
                      "OATS - CHICAGO BOARD OF TRADE",
                      "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

lean_hogs <- extract_split(updated_data_ag,
                           "LEAN HOGS - CHICAGO MERCANTILE EXCHANGE",
                           "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

live_cattle <- extract_split(updated_data_ag,
                             "LIVE CATTLE - CHICAGO MERCANTILE EXCHANGE",
                             "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

feeder_cattle <- extract_split(updated_data_ag,
                               "FEEDER CATTLE - CHICAGO MERCANTILE EXCHANGE",
                               "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

milk_3 <- extract_split(updated_data_ag,
                        "MILK, Class III - CHICAGO MERCANTILE EXCHANGE",
                        "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

milk_nonfat_dry <- extract_split(updated_data_ag,
                                 "NON FAT DRY MILK - CHICAGO MERCANTILE EXCHANGE",
                                 "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

milk_4 <- extract_split(updated_data_ag,
                        "CME MILK IV - CHICAGO MERCANTILE EXCHANGE",
                        "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

soybeans <- extract_split(updated_data_ag,
                          "SOYBEANS - CHICAGO BOARD OF TRADE",
                          "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

soybean_oil <- extract_split(updated_data_ag,
                             "SOYBEAN OIL - CHICAGO BOARD OF TRADE",
                             "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

soybean_meal <- extract_split(updated_data_ag,
                              "SOYBEAN MEAL - CHICAGO BOARD OF TRADE",
                              "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")


# See All Commodities Across Time
# lapply(updated_data_ag, names)
# See Unique Commodities
# unique(unlist(lapply(updated_data_ag, names)))

# Prepare Plot Data
plot_data_grain <- multi_join(
  wheat_srw |> mutate(Group = "Wheat SRW"),
  wheat_hrw |> mutate(Group = "Wheat HRW"),
  corn |> mutate(Group = "Corn"),
  oats |> mutate(Group = "Oats")
) |>
  filter(
    Statistic == "Changes in Commitments from Last Week (Contracts of Varying Amounts by Commodity)",
    date >= as.Date("2025-11-01")
  )

plot_data_livestock <- multi_join(
  lean_hogs |> mutate(Group = "Live Hogs"),
  live_cattle |> mutate(Group = "Live Cattle"),
  feeder_cattle |> mutate(Group = "Feeder Cattle")
) |>
  filter(
    Statistic == "Changes in Commitments from Last Week (Contracts of Varying Amounts by Commodity)",
    date >= as.Date("2025-11-01")
  )

plot_data_milk <- multi_join(
  milk_nonfat_dry |> mutate(Group = "Non-Fat Milk"),
  milk_3 |> mutate(Group = "Milk III"),
  milk_4 |> mutate(Group = "Milk IV")
) |>
  filter(
    Statistic == "Changes in Commitments from Last Week (Contracts of Varying Amounts by Commodity)",
    date >= as.Date("2025-11-01")
  )

plot_data_soybean <- multi_join(
  soybeans |> mutate(Group = "Bean"),
  soybean_meal |> mutate(Group = "Meal"),
  soybean_oil |> mutate(Group = "Oil")
) |>
  filter(
    Statistic == "Changes in Commitments from Last Week (Contracts of Varying Amounts by Commodity)",
    date >= as.Date("2025-11-01")
  )

# Sub here
plot_data <- plot_data_soybean

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
    y = "x2500 MMBtus",
  ) +
  scale_x_date(breaks = plot_data$date,
               labels = as.character(plot_data$date)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(~Variables, scales = "free_y", ncol = 3) 

