# This will use WRDS LSEG (formerly Refinitiv) Datastream Futures Series data 
# on commodities available on Stocktrak. 

# Stocktrak Commodities:
# Chicago Ethanol (Platts) Futures (Globex) x
# Crude Oil (Light-Sweet) (Globex) x
# Crude Oil Brent (Globex) x
# Heating Oil (Globex) x
# Natural Gas (Globex) x
# RBOB Gasoline (Globex) x
# Aluminum (Globex)
# e-Mini Gold (Globex) x
# Gold (Globex) x
# High Grade Copper (Globex) x
# Palladium (Globex) x
# Platinum (Globex) x
# Silver (Globex) x
# Corn (Globex) x
# Oats (Globex) x
# Soybean Meal (Globex) x
# Soybean Oil (Globex) x
# Soybeans (Globex) x
# Wheat (Globex) x
# Cheese (Globex) x
# Lumber (Globex) x
# Milk (Globex) x
# Live Cattle (Globex) x
# Feeder Cattle (Globex) x
# Lean Hogs (Globex) x
# Pork Cutout (Globex) x

# CalcSeriesCode
# ClsCode - Refinitiv generated ID for a Datastream class code
# DSMnem - Unique Datastream mnemonic for a futures continuous series
# CalcSeriesName - Description of the series
# ISOCurrCode - ISO currency code for the currency upon which the series is calculated
# ISOCurrDesc Desc_ - Full name or description of Code + Type_
# RollMethodCode - Roll method used for the calculation
# Rollmethoddesc Desc_ - Full name or desiption of Code + Type_
# PositionFwdCode - Position forward. If a contract trades four quarters, a 
#                   continuous series may show data from the most recent month,
#                   second most recent month, third most recent month, and so on.
#                   This field indicates which position is used.
# Positionofwddesc Desc_ - Full name of description of Code + Type_
# CalcMthCode - Calculation month code. May be S (traded for all months) or T
#               (traded for specific months)
# trdmonths - list of specific months traded
# Date_ - Market date
# Open_ - the open price value is exchange calculated, is the first trade price,
#         or if there were no trades on the market date, then this is the 
#         settlement price.
# High - ''
# Low - ''
# Volume - Number of contracts traded
# Settlement - settlement price as published by the exchange. If no figure is 
#              published by the exchange then the last trade is used.
# OpenInterest - number of contracts not closed.
# cmonth - Underlying Contract Month
# yd - Yield
# up - Unadjusted Price

# Using DATASTREAM_CALCSERIESCODE
# This data is for the contract n months ahead, depending on the observation time. 

# Load Packages and Functions

library(tidyverse)
library(stringr)
library(ggnewscale)
library(scales)
library(data.table)
library(patchwork)
setwd("/Users/josephjason/Documents/Forecasting/R/projects")
source("Commodity Analysis/functions/nin.R")

# LOAD DATA
# as of feb 7/8 2026
csv <- read.csv("Commodity Analysis/files/historical_futures_analysis/data/commodity_continuous_series_lseg_apr_4_2026.csv")
csv <- filter(
  csv,
  CalcSeriesName %nin% c("CMX-GOLD 100 OZ TRcm1",
                         "CMX-GOLD 100 OZ TRcm4",
                         "ICE-BRENT CRUDE OIL TRc12",
                         "ICE-HEATING OIL CONTINUOUS LTDT",
                         "ICE-NATURAL GAS QTR TRc1",
                         "TOCOM-PALLADIUM TRc6",
                         "NYMEX-RBOB GASOLINE CONT. 2ND LTDT",
                         "NYMEX-RBOB GASOLINE CONTINUOUS LTDT")
)

# CLEAN DATA
csv_clean <- csv |>
  select(
    date = Date_,
    id = CalcSeriesCode,
    name = CalcSeriesName,
    months_ahead = PositionFwdCode,
    open = Open_,
    high = High,
    low = Low,
    settlement = Settlement,
    volume = Volume,
    open_interest = OpenInterest
  ) |>
  distinct() |>
  mutate(
    date = as.Date(date),
    commodity = case_when(
      grepl("CORN", name) ~ "Corn",
      grepl("OATS", name) ~ "Oats",
      grepl("SOYABEAN MEAL", name) ~ "Soybean Meal",
      grepl("SOYABEAN OIL", name) ~ "Soybean Oil",
      grepl("SOYBEANS COMP.", name) ~ "Soybeans",
      grepl("WHEAT", name) ~ "Wheat",
      grepl("CHEESE", name) ~ "Cheese",
      grepl("FEEDER CATTLE", name) ~ "Feeder Cattle",
      grepl("LEAN HOGS", name) ~ "Lean Hogs",
      grepl("LIVE CATTLE", name) ~ "Live Cattle",
      grepl("LUMBER", name) ~ "Lumber",
      grepl("MILK", name) ~ "Milk",
      grepl("PORK", name) ~ "Port Cutout",
      grepl("ALUMINIUM", name) ~ "Aluminium",
      grepl("MICRO GOLD", name) ~ "Micro Gold",
      grepl("GOLD 100", name) ~ "Gold",
      grepl("COPPER", name) ~ "Copper",
      grepl("SILVER", name) ~ "Silver",
      grepl("ETHANOL", name) ~ "Ethanol",
      grepl("BRENT", name) ~ "Brent Crude Oil",
      grepl("HEATING OIL", name) ~ "Heating Oil",
      grepl("NATURAL GAS", name) ~ "Natural Gas",
      grepl("LIGHT CRUDE OIL", name) ~ "Light Crude Oil",
      grepl("ULSD", name) ~ "ULSD",
      grepl("PALLADIUM", name) ~ "Palladium",
      grepl("PLATINUM", name) ~ "Platinum",
      grepl("RBOB", name) ~ "RBOB",
      TRUE ~ NA_character_
    ),
    extracted_months_ahead = as.numeric(str_extract(name, "(?<=TRc|trc|TRcm|CS0)\\d|\\d(?=TH|ND|RD)")),
    extracted_months_ahead = ifelse(is.na(extracted_months_ahead), 1, extracted_months_ahead),
    contract_date = date %m+% months(as.numeric(extracted_months_ahead)),
    .before = id,
    month_num = month(date),
    month_fct = factor(month(date), labels = month.abb)
  ) |>
  arrange(commodity, date, contract_date) |>
  mutate(nearest_contract = min(extracted_months_ahead), 
         total_contracts = length(unique(extracted_months_ahead)),
         .by = commodity) |>
  mutate(contract_year = year(contract_date), 
         contract_month = month(contract_date),
         .after = contract_date,
         year_group = ceiling(contract_year / 5) * 5) |>
  as.data.table()


contracts <- unique(csv_clean$name)
commodities <- unique(csv_clean[, .(commodity, nearest_contract, total_contracts)])

colSums(is.na(csv_clean))

# Fill missing dates and fill other missing values with last available values
csv_clean <- csv_clean[, {
  all_dates <- seq(min(date), max(date), by = "day")
  .SD[.(all_dates), on = .(date), roll = TRUE]
}, by = .(commodity, extracted_months_ahead)]

colSums(is.na(csv_clean))

# plot many
for (i in seq_along(commodities)) {
  commodity_grp <- i
  plot <- ggplot(csv_clean |>
                 group_by(date) |>
                 filter(
                   commodity == commodity_grp,
                   date >= as.Date("2020-01-01"),
                   date %in% c(ceiling_date(date, "month") - days(1)
                   #ceiling_date(date, "month") - days(15)
                )
              ) |>
                mutate(year_fct = factor(year(date)))
  ) +
    geom_line(
      aes(
        x = contract_date,
        y = settlement,
        group = date,
        colour = month_fct
      ),
      linewidth = 2
    ) + 
    scale_colour_viridis_d(name = "Observation    \n     Month") + 
    new_scale_colour() +
    geom_point( # change point colour to match the observation year
      data = \(x) x |>
        group_by(date) |>
        filter(
          commodity == commodity_grp,
          date >= as.Date("2020-01-01")
        ) |>
        slice_min(contract_date, n = 1),
      aes(
        x = contract_date,
        y = settlement,
        colour = year_fct
      ),
      size = 4,
      shape = 18
    ) +
    geom_vline(
      data = \(x) x |>
        mutate(year_ln = floor_date(date, "year")) |>
        group_by(year_ln) |>
        slice_min(date, n = 1),
      aes(xintercept = date),
      linetype = "dashed",
      alpha = 0.5,
      colour = "gray30"
    ) + 
    scale_colour_viridis_d(name = "Observation    \n     Year") +
    labs(
      title = paste0(commodity_grp, " Historical Commodity Forward Curves (Monthly)."),
      subtitle = "Vertical lines mark year transitions in the observation dates (the contracts are anchored to the front futures contract and not to the spot price, so the dashed \nlines are aligned to the first futures contract at that observation date).",
      x = "Contract Month",
      y = "Settlement Price",
      colour = "Month"
    ) +
    scale_x_date(
      date_breaks = "3 months",
      date_labels = "%b\n%Y"
    ) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 8),
                       labels = scales::label_dollar(accuracy = 0.01)) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      plot.subtitle = element_text(margin = margin(b = 10)),
      legend.text = element_text(vjust = 1,
                                 hjust = 1,
                                 margin = margin(l = 8)),
      legend.key.width = unit(1.5, "cm")
    )
  print(plot)
}

dev.off()


# Plot Single Commodity
# change for the plot
contracts
commodities
commodity_grp <- "Lumber"

ggplot(csv_clean |>
         group_by(date) |>
         filter(
           commodity == commodity_grp,
           date >= as.Date("2007-02-03"),
           date <= as.Date("2020-02-03"),
           date %in% c(ceiling_date(date, "month") - days(1)
                       # ceiling_date(date, "month") - days(15)
                       )
         ) |>
         mutate(
           year_fct = factor(year(date))
         )
       ) +
  geom_line(
    aes(
      x = contract_date,
      y = settlement,
      group = date,
      colour = month_fct
    ),
    linewidth = 2
  ) + 
  scale_colour_viridis_d(name = "Observation    \n     Month") + 
  new_scale_colour() +
  geom_point( # change point colour to match the observation year
    data = \(x) x |>
      group_by(date) |>
      filter(
        commodity == commodity_grp,
        date >= as.Date("2010-01-01")
      ) |>
      slice_min(contract_date, n = 1),
    aes(
      x = contract_date,
      y = settlement,
      colour = year_fct
    ),
    size = 4,
    shape = 18
  ) +
  # geom_vline(
  #   data = \(x) x |>
  #     mutate(year_ln = floor_date(date, "year")) |>
  #     group_by(year_ln) |>
  #     slice_min(date, n = 1),
  #   aes(xintercept = date),
  #   linetype = "dashed",
  #   alpha = 0.5,
  #   colour = "gray20"
  # ) + 
  scale_colour_viridis_d(name = "Observation    \n     Year") +
  labs(
    title = paste0(commodity_grp, " Historical Commodity Forward Curves."),
    # subtitle = "Vertical lines mark year transitions in the observation dates (the contracts are anchored to the front futures contract and not to the spot price, so the dashed \nlines are aligned to the first futures contract at that observation date).",
    subtitle = paste0("The ", commodity_grp, " forward curves are composed of ", 
                      commodities[commodity == commodity_grp, total_contracts],
                      " contracts, and the first contract of the forward curve is ",
                      commodities[commodity == commodity_grp, nearest_contract],
                      " month", ifelse(commodities[commodity == commodity_grp, nearest_contract] == 1, "", "s"), " ahead of the observation date."),
    x = "Contract Month",
    y = "Close Price",
    colour = "Month",
    caption = "Data from LSEG. Graph by Joseph Jason @github.com/7-jjason."
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 8),
                     labels = scales::label_dollar(accuracy = 0.01)) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.subtitle = element_text(margin = margin(b = 10)),
    legend.text = element_text(vjust = 1,
                               hjust = 1,
                               margin = margin(l = 8)),
    legend.key.width = unit(1.5, "cm"),
    axis.line.x.bottom = element_line(colour = "grey50",
                                      linewidth = 0.25),
    axis.line.y.left = element_line(colour = "grey50",
                                    linewidth = 0.25),
    plot.caption = element_text(colour = "grey50",
                                hjust = 0)
  )








# Plot Two Commodities 
# error occurs if using a group that has any observation dates with
# only one contract - print these as points and then filter them out
# from the line section

# if nearest month = 1, data is a continuous index instead of continuously 
# being positioned n months ahead of the observation date.
commodities 
# all contracts
contracts 
# init
commodity_grps <- c("Soybeans", "Soybean Meal")
filter_from <- as.Date("2010-01-01")
filter_to <- as.Date("2020-06-01")
conversion_factor <- 1    # Scale factor for the second commodity in 
                          #   commodity_grps, start at 1 and adjust.
                          # If not scaling correctly, switch order of
                          #   commodity_grps (higher price, lower price).
                          # 1 - viridis, 2 - magma.
# or
scaler <- csv_clean |>
  filter(
    commodity %in% commodity_grps,
    date >= filter_from & date <= filter_to
  ) |>
  group_by(commodity) |>
  reframe(
    med_val = median(settlement, na.rm = TRUE)
  )

med_1 <- scaler[scaler$commodity == commodity_grps[[1]], "med_val"] |> pull()
med_2 <- scaler[scaler$commodity == commodity_grps[[2]], "med_val"] |> pull()
conversion_factor <- med_1/med_2

    
# plot
ggplot() +
  geom_smooth(
    data = csv_clean |>
      filter(
        commodity %in% commodity_grps,
        date >= filter_from & date <= filter_to,
        date %in% c(ceiling_date(date, "month") - days(1))
      ) |>
      pivot_wider(
        names_from = commodity, values_from = settlement,
        id_cols = c(date, contract_date)
      ) |>
      mutate(inter_commodity_basis = .data[[commodity_grps[1]]] - .data[[commodity_grps[2]]],
             Spread = "Spread") |>
      drop_na(inter_commodity_basis),
    aes(
      x = contract_date,
      y = inter_commodity_basis,
      colour = Spread
    ),
    method = "loess",
    se = FALSE,
    linewidth = 1,
    alpha = 0.2
  ) +
  scale_color_manual(
    values = c("Spread" = "blue"),
    name = "Spread - Loess",
    labels = c("Spread" = "")  # This is what shows for the line
  ) +
  new_scale_colour() +
  geom_line(
    data = csv_clean |>
      filter(
        commodity %in% commodity_grps,
        date >= filter_from & date <= filter_to,
        date %in% c(ceiling_date(date, "month") - days(1))
      ) |>
      pivot_wider(
        names_from = commodity, values_from = settlement,
        id_cols = c(date, contract_date, month_fct)
      ) |>
      mutate(inter_commodity_basis = .data[[commodity_grps[1]]] - .data[[commodity_grps[2]]],
             Spread = "Spread") |>
      drop_na(inter_commodity_basis),
    aes(
      x = contract_date,
      y = inter_commodity_basis,
      colour = month_fct,
      group = date
    ),
    linewidth = 1
    # alpha = 0.2
  ) +
  scale_colour_viridis_d(name = paste0("Spread - Observation Month\n(", commodity_grps[[1]], " - ", commodity_grps[[2]], ")"),
                         option = "rocket") +
  new_scale_colour() +
  geom_point(
    data = csv_clean |>
      filter(
        commodity == commodity_grps[1],
        date <= filter_to & date <= filter_to,
        # date %in% c(ceiling_date(date, "month") - days(1))
      ) |>
      group_by(commodity, date) |>
      mutate(single_pt = n() == 1) |>
      ungroup() |>
      filter(single_pt),
    aes(
      x = contract_date,
      y = settlement,
      colour = month_fct
    ),
    size = 2,
    shape = 18
  ) +
  geom_line(
    data = csv_clean |>
             filter(
               commodity == commodity_grps[1],
               date >= filter_from & date <= filter_to,
               date %in% c(ceiling_date(date, "month") - days(1))
             ) |>
      # group_by(date, contract_date) |>
      group_by(commodity, date) |>
      mutate(single_pt = n() == 1) |>
      ungroup() |>
      filter(!single_pt) |>
      group_by(date),
    aes(
      x = contract_date,
      y = settlement,
      group = date,
      colour = month_fct
    ),
    linewidth = 1
  ) +
  scale_colour_viridis_d(name = paste0(commodity_grps[1], " - Observation Month"),
                         option = "viridis",
                         begin = 0.4) + 
  new_scale_colour() +
  geom_point(
    data = csv_clean |>
      filter(
        commodity == commodity_grps[2],
        date >= filter_from & date <= filter_to,
        date %in% c(ceiling_date(date, "month") - days(1))
      ) |>
      group_by(commodity, date) |>
      mutate(single_pt = n() == 1) |>
      ungroup() |>
      filter(single_pt) |>
      mutate(settlement = settlement * conversion_factor),
    aes(
      x = contract_date,
      y = settlement,
      colour = month_fct
    ),
    size = 2,
    shape = 18
  ) +
  geom_line(
    data = csv_clean |>
      filter(
        commodity == commodity_grps[2],
        date >= filter_from & date <= filter_to,
        date %in% c(ceiling_date(date, "month") - days(1))
      ) |>
      group_by(commodity, date) |>
      mutate(single_pt = n() == 1) |>
      ungroup() |>
      filter(!single_pt) |>
      mutate(settlement = settlement * conversion_factor),
    aes(
      x = contract_date,
      y = settlement,
      group = date,
      colour = month_fct
    ),
    linewidth = 1
  ) +
  scale_colour_viridis_d(name = paste0(commodity_grps[2], " Observation - Month"),
                         option = "cividis",
                         end = 0.85) + 
  new_scale_colour() +
  geom_vline(
    xintercept = {
      date_range <- csv_clean |>
        filter(commodity %in% commodity_grps, date >= filter_from & date <= filter_to) |>
        summarise(
          min_date = min(date, na.rm = TRUE),
          max_date = max(contract_date, na.rm = TRUE) 
        )
      seq(
        from = as.Date(paste0(year(date_range$min_date), "-01-01")),
        to = as.Date(paste0(year(date_range$max_date) + 1, "-01-01")),
        by = "year"
      )
    },
    linetype = "dashed",
    alpha = 0.7,
    colour = "gray20"
  ) +
  labs(
    title = paste0(commodity_grps[1],
                   " and ", 
                   commodity_grps[2], 
                   " Historical Commodity Forward Curves, Monthly."),
    subtitle = paste0("The ", tolower(commodity_grps[1]), " forward curves are composed of ", 
                      commodities[commodity == commodity_grps[1], total_contracts],
                      " contracts, and the first contract of the forward curve is ",
                      commodities[commodity == commodity_grps[1], nearest_contract],
                      " month", ifelse(commodities[commodity == commodity_grp[1], nearest_contract] == 1, "", "s"), " ahead of the observation date.",
                      "\nThe ", tolower(commodity_grps[2]), " forward curves are composed of ", 
                      commodities[commodity == commodity_grps[2], total_contracts],
                      " contracts, and the first contract of the forward curve is ",
                      commodities[commodity == commodity_grps[2], nearest_contract],
                      " month", ifelse(commodities[commodity == commodity_grp[2], nearest_contract] == 1, "", "s"), " ahead of the observation date."),
    caption = "Data from LSEG; Graph by Joseph Jason @https://github.com/7-jjason.",
    x = "Futures Contract Month",
    y = "Contract Price"
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b\n%Y",
    limits = c(filter_from, filter_to)
  ) +
  scale_y_continuous(name = paste0(commodity_grps[1], 
                                   " Futures Close Price and Inter-Commodity Spread"),
                     breaks = breaks_pretty(n = 8),
                     labels = label_dollar(accuracy = 0.01),
                     sec.axis = sec_axis(
                       transform = ~. / conversion_factor,
                       name = paste0(commodity_grps[2],
                                     " Futures Close Price"),
                       breaks = breaks_pretty(n = 8),
                       labels = label_dollar(accuracy = 0.01),
                     )) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(
      margin = margin(t = 10),
      # size = 16
      ),
    axis.title.y.left = element_text(
      margin = margin(r = 10),
      # size = 16
      ),
    axis.title.y.right = element_text(
      margin = margin(l = 10),
      # size = 16
      ),
    # axis.text.x = element_text(size = 14),
    # axis.text.y = element_text(size = 14),
    plot.subtitle = element_text(
      margin = margin(b = 10),
      # size = 16
      ),
    plot.caption = element_text(
      hjust = 0,
      colour = "grey50",
      # size = 16
      ),
    plot.title = element_text(size = 18),
    legend.text = element_text(
      vjust = 1,
      hjust = 1,
      margin = margin(l = 10),
      # size = 16
      ),
    # legend.title = element_text(size = 16),
    legend.key.width = unit(1.5, "cm"),
    panel.border = element_rect(
      colour = "grey20", 
      fill = NA, 
      linewidth = 0.25)
  )
 







