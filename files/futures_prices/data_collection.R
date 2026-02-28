# Load Packages
library(quantmod)
library(data.table)
library(dplyr)
library(ggrepel)

commodity_contracts <- list(
  gold = list(
    "GCG26.CMX", # Feb 26
    "GCH26.CMX", # Mar 26
    "GCJ26.CMX", # Apr 26
    "GCK26.CMX", # May 26
    "GCM26.CMX", # Jun 26
    "GCN26.CMX", # Jul 26
    "GCQ26.CMX", # Aug 26
    "GCU26.CMX", # Sept 26
    "GCV26.CMX", # Oct 26
    "GCX26.CMX", # Nov 26
    "GCZ26.CMX"  # Dec 26
  ),
  natural_gas = list(
    "NGH26.NYM", # Mar 26
    "NGJ26.NYM", # Apr 26
    "NGK26.NYM", # May 26
    "NGM26.NYM", # Jun 26
    "NGN26.NYM", # Jul 26
    "NGQ26.NYM", # Aug 26
    "NGU26.NYM", # Sept 26
    "NGV26.NYM", # Oct 26
    "NGX26.NYM", # Nov 26
    "NGZ26.NYM"  # Dec 26
  ),
  crude_oil = c(
    "CLH26.NYM", # Mar 26
    "CLJ26.NYM", # Apr 26
    "CLK26.NYM", # May 26
    "CLM26.NYM", # Jun 26
    "CLN26.NYM", # Jul 26
    "CLQ26.NYM", # Aug 26
    "CLU26.NYM", # Sept 26
    "CLV26.NYM", # Oct 26
    "CLX26.NYM", # Nov 26
    "CLZ26.NYM"  # Dec 26
  ),
  oats = c(
    "ZOH26.CBT", # Mar 26
    "ZOK26.CBT", # May 26
    "ZON26.CBT", # Jul 26
    "ZOU26.CBT", # Sep 26
    "ZOZ26.CBT"  # Dec 26
  ),
  soybeans = c(
    "ZSH26.CBT", # Mar 26
    "ZSK26.CBT", # May 26
    "ZSN26.CBT", # Jul 26
    "ZSQ26.CBT", # Aug 26
    "ZSU26.CBT", # Sept 26
    "ZSX26.CBT"  # Nov 26
  )
)



# Get Data
commodity_prices <- NULL
for (commodity in commodity_contracts) {
  for (contract in commodity) {
    prices <- getSymbols(contract,
                         src = "yahoo",
                         auto.assign = FALSE) |>
      as.data.table(keep.rownames = "Date")
    
    # Select close and volume variables
    prices <- prices[, grep("Date|Close|Volume", names(prices)), with = FALSE]
    
    # Merge
    if (is.null(commodity_prices)) {
      commodity_prices <- prices
    } else {
      commodity_prices <- merge(commodity_prices,
                                prices, 
                                by = "Date",
                                all = TRUE)
    }
  }
}


### 1 ###
# Correlation Table
input_tbl <- commodity_prices[, grep("Close", names(commodity_prices)), with = FALSE]
cor_tbl <- cor(as.matrix(input_tbl), use = "complete.obs")

# Pivot Longer to Plot
input_tbl <- cor_tbl |>
  as.data.frame() |>
  mutate(var_1 = rownames(cor_tbl),
         .before = 1) |>
  remove_rownames() |>
  pivot_longer(cols = -var_1,
               names_to = "var_2",
               values_to = "prices")

# Plot Heatmap
ggplot(input_tbl, 
       aes(x = var_1,
           y = var_2,
           fill = prices)) + 
  geom_tile() + 
  labs(
    title = "Commodity Futures Correlation Heatmap",
    x = "",
    y = "",
    fill = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 90,
                               vjust = 0.5)
  )

### 2 ###
# 
ts_tbl <- commodity_prices[, grep("Close|Date", names(commodity_prices)), with = FALSE]

ts_long <- ts_tbl |>
  pivot_longer(
    cols = -Date,
    names_to = "contract",
    values_to = "price"
  ) |>
  mutate(
    commodity = substr(contract, 1, 2),
    month_code = substr(contract, 3, 3),
    year_code = substr(contract, 4, 5),
    month_num = match(month_code, 
                      LETTERS[c(6, 7, 8, 10, 11, 13, 14, 17, 21, 22, 24, 26)]),
    year = as.numeric(paste0("20", year_code)),
    maturity_date = make_date(year, month_num, 1),
    date_label = if_else(
      lead(commodity, 3) != lead(commodity, 4) | is.na(lead(commodity, 4)),
      Date,
      NA)
  ) |>
  select(
    observation_date = Date,
    maturity_date, 
    commodity,
    price,
    date_label
  ) 

ts_wide <- ts_long |>
  pivot_wider(
    names_from = commodity,
    values_from = price
  ) |>
  arrange(observation_date, maturity_date)
  
# Plot Futures Curves by Commodity
ggplot(data = ts_long |> 
         group_by(observation_date) |>
         filter(commodity == "ZS",
                !is.na(price),
                observation_date >= as.Date("2026-01-15")
                # #cur_group_id() %% 7 == 0
         ) |>
         ungroup()
       ) +
  geom_path(
    aes(
      x = maturity_date,
      y = price,
      group = observation_date,
      colour = observation_date
    ),
    linewidth = 1
  ) +
  geom_point(
    aes(
      x = maturity_date,
      y = price,
      group = observation_date,
      colour = observation_date
    ),
    size = 2
  ) +
  geom_text_repel(
    aes(
      x = maturity_date,
      y = price,
      label = as.character(date_label),
      grroup = observation_date
    ),
    position = position_dodge(width = 0.5),
    point.padding = 0.02,
    box.padding = 1.5
  ) +
  labs(
    title = "Crude Oil Forward Curves across Jan. 2, 2026 - Present",
    x = "Futures Maturities",
    y = "Price ($)",
    colour = "Observation Date"
  ) +
  scale_colour_viridis_c(
    trans = "date"
  ) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b\n%Y")









