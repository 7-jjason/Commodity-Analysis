
library(tidyverse)
source("Functions/extract_split.R")
source("Functions/max_multi_join.R")

# Get Data
updated_data_pet <- readRDS("data/cftc_petroleum_dynamically_updated.rds")

# WTI
wti <- extract_split(updated_data_pet,
                     "WTI FINANCIAL CRUDE OIL - NEW YORK MERCANTILE EXCHANGE",
                     "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

# WTI LIGHT SWEET
wti_ls <- extract_split(updated_data_pet,
                        "CRUDE OIL, LIGHT SWEET-WTI - ICE FUTURES EUROPE",
                        "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

# BRENT
brent <- extract_split(updated_data_pet,
                       "BRENT LAST DAY - NEW YORK MERCANTILE EXCHANGE",
                       "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

# WTI/BRENT CALENDAR
wti_brent_time <- extract_split(updated_data_pet,
                                "WTI-BRENT CALENDAR - NEW YORK MERCANTILE EXCHANGE",
                                "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

# WCS/WTI
wcs_wti <- extract_split(updated_data_pet, 
                         "CRUDE DIFF-WCS HOUSTON/WTI 1ST - ICE FUTURES ENERGY DIV",
                         "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

# RBOB
rbob <- extract_split(updated_data_pet,
                      "GASOLINE RBOB - NEW YORK MERCANTILE EXCHANGE",
                      "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

# GASOLINE CRACK - RBOB/BRENT
gas_crack <- extract_split(updated_data_pet,
                           "GASOLINE CRK-RBOB/BRENT 1st - ICE FUTURES ENERGY DIV",
                           "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

# ETHANOL
ethanol <- extract_split(updated_data_pet,
                         "ETHANOL - NEW YORK MERCANTILE EXCHANGE",
                         "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")

# NAT GAS
updated_data_ng <- readRDS("data/cftc_nat_gas_dynamically_updated.rds")
nat_gas_ice_ld1 <- extract_split(updated_data_ng,
                                 "NAT GAS ICE LD1 - ICE FUTURES ENERGY DIV",
                                 "split 1") |>
  pivot_longer(cols = -c(date, Statistic, Recency),
               names_to = "Variables",
               values_to = "Values")


# See All Commodities Across Time
# lapply(updated_data_pet, names)
# See Unique Commodities
# unique(unlist(lapply(updated_data_pet, names)))

# Prepare Plot Data 
plot_data_pet <- multi_join(
  wti |> filter(Statistic == "Changes in Commitments from Last Week (100 CUBIC METERS)",
                    date >= as.Date("2025-11-01")) |>
    mutate(Group = "WTI"),
  brent |> filter(Statistic == "Changes in Commitments from Last Week (100 CUBIC METERS)",
                    date >= as.Date("2025-11-01")) |>
    mutate(Group = "Brent"),
  rbob |> filter(Statistic == "Changes in Commitments from Last Week (100 CUBIC METERS)",
                    date >= as.Date("2025-11-01")) |>
    mutate(Group = "RBOB"),
  # ethanol |> filter(Statistic == "Changes in Commitments from Last Week (100 CUBIC METERS)",
  #                   date >= as.Date("2025-11-01")) |>
  #   mutate(Group = "Ethanol"),
  nat_gas_ice_ld1 |> filter(Statistic == "Changes in Commitments from Last Week (2500 MMBtus)",
                            date >= as.Date("2025-11-01")) |>
    mutate(Group = "Natural Gas\n(scaled /10)",
           Values = Values / 10)
)

ggplot() +
  geom_path(
    data = plot_data_pet,
    aes(
      x = date,
      y = Values,
      colour = Group,
      group = Group
    ),
    linewidth = 1
  ) + 
  labs(
    title = paste0("Change in Commitments from Last Week, Published ", 
                   max(plot_data_pet$date, na.rm = TRUE)),
    x = "",
    y = "Number of Contracts",
  ) +
  scale_x_date(date_breaks = "1 week",
               date_labels = "%Y-%b-%d") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        plot.caption = element_text(colour = "grey40",
                                    hjust = 0)) + 
  facet_wrap(~Variables, 
             scales = "free_y", 
             ncol = 3,
             labeller = label_wrap_gen(width = 30)) 
