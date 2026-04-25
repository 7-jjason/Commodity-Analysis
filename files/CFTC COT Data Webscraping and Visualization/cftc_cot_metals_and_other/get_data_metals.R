# source("Functions/extract_split.R")

# Get Data
updated_data_metal <- readRDS("Energy Analysis/data/cftc_other_dynamically_updated.rds")

# PALLADIUM
palladium <- extract_split(updated_data_metal, 
                           "PALLADIUM - NEW YORK MERCANTILE EXCHANGE", 
                           "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

# PLATINUM
platinum <- extract_split(updated_data_metal,
                          "PLATINUM - NEW YORK MERCANTILE EXCHANGE",
                          "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

# SILVER
silver <- extract_split(updated_data_metal,
                        "SILVER - COMMODITY EXCHANGE INC.",
                        "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

# GOLD
gold <- extract_split(updated_data_metal,
                      "GOLD - COMMODITY EXCHANGE INC.",
                      "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

# MINI GOLD
mini_gold <- extract_split(updated_data_metal,
                           "MICRO GOLD - COMMODITY EXCHANGE INC.",
                           "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

# COPPER
copper <- extract_split(updated_data_metal,
                        "COPPER- #1 - COMMODITY EXCHANGE INC.",
                        "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

# ALUMINUM
aluminum <- extract_split(updated_data_metal,
                          "ALUMINIUM EURO PREM DUTY-PAID - COMMODITY EXCHANGE INC.",
                          "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

# STEEL
steel <- extract_split(updated_data_metal,
                       "STEEL-HRC - COMMODITY EXCHANGE INC.",
                       "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")


# See All Commodities Across Time
# lapply(updated_data_metal, names)
# See Unique Commodities
# unique(unlist(lapply(updated_data_metal, names)))

# Prepare Plot Data

plot_data_met <- multi_join(
  # palladium |> mutate(Group = "Palladium"),
  # platinum |> mutate(Group = "Platinum"),
  # silver |> mutate(Group = "Silver"),
  # gold |> mutate(Group = "Gold"),
  # mini_gold |> mutate(Group = "Mini Gold"),
  copper |> mutate(Group = "Copper"),
  # aluminum |> mutate(Group = "Aluminum"),
  # steel |> mutate(Group = "Steel")
  soybeans |> mutate(Group = "Soybeans")
) |>
  filter(
    Statistic == "Changes in Commitments from Last Week (Contracts of Varying Amounts by Commodity)",
    date >= as.Date("2025-11-01")
  )


ggplot() +
  geom_line(
    data = plot_data_met,
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
  facet_wrap(~Variables,
             scales = "free_y",
             ncol = 3,
             labeller = label_wrap_gen(width = 30)) 
