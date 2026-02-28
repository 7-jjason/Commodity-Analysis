
library(tidyverse)
library(jsonlite)
library(tsibble)
library(stringr)
library(janitor)

# load data
petroleum_balance_sheet <- read.csv("https://ir.eia.gov/wpsr/table1.csv",
                                    header=FALSE, 
                                    stringsAsFactors=FALSE, 
                                    fileEncoding="latin1")

# pull total petroleum stocks in millions per week data
petroleum_stocks_millions_week <- petroleum_balance_sheet[1:19,] |>
  row_to_names(row_number = 1) |>
  clean_names() |>
  rename_with(~ c(
    "Type",
    paste0("Current Week: ", sub("^x", "", .x[2])),
    paste0("Week Ago: ", sub("^x", "", .x[3])),
    "Week Ago: Difference",
    "Week Ago: Percent Change",
    paste0("Year Ago: ", sub("^x", "", .x[6])),
    "Year Ago: Difference",
    "Year Ago: Percent Change"
  )) |>
  mutate(
    across(-Type, ~str_replace_all(., ",", "")),
    across(-Type, ~as.numeric(.))
  )

# clean petroleum supply in thousands per day data
petroleum_supply_thousands_day_categories <- petroleum_balance_sheet[20:95,] |>
  filter(row_number() %% 2 == 0)

petroleum_supply_thousands_day_categories_2 <- petroleum_supply_thousands_day_categories[,1:5] |>
  janitor::row_to_names(row_number = 1) %>%
  janitor::clean_names() %>%
  setNames(c(
    paste0("Four-Week Averages Week Ending: ", sub("x", "", names(.)[1])),
    "Four-Week Averages Week Ending: Percent Change",
    paste0("Cumulative Daily Average: ", sub("x", "", names(.)[3])),
    paste0("Cumulative Daily Average: ", sub("x", "", names(.)[4])),
    "Cumulative Daily Average: Percent Change"
  )) %>%
  # Replace any value containing the hex 96 character with NA
  mutate(across(everything(), ~ if_else(grepl("\x96", ., useBytes = TRUE), NA_character_, .)),
         across(everything(), ~str_replace_all(.x, ",", "")),
         across(everything(), ~as.numeric(.))) %>%
  rename_with(~ sub("_2$", "", .x), .cols = 4:5)

petroleum_supply_thousands_day_data <- petroleum_balance_sheet[20:95,] |>
  filter(row_number() %% 2 == 1) |>
  select(-STUB_1) |>
  janitor::row_to_names(row_number = 1) |>
  remove_rownames() |>
  column_to_rownames(var = "STUB_2") %>%
  setNames(c(
    paste0("Current Week: ", names(.)[1]),
    paste0("Week Ago: ", names(.)[2]),
    "Week Ago: Difference",
    paste0("Year Ago: ", names(.)[4]),
    "Year Ago: Difference",
    paste0("Four-Week Averages Week Ending: ", names(.)[6])
  )) 

petroleum_supply_thousands_day_data_1 <- petroleum_supply_thousands_day_data[1:17,] %>%
  `rownames<-`(paste0("Crude Oil Supply: ", str_trim(str_sub(rownames(.), 5))))
petroleum_supply_thousands_day_data_2 <- petroleum_supply_thousands_day_data[18:29,] %>%
  `rownames<-`(paste0("Other Supply: ", str_trim(str_sub(rownames(.), 5))))
petroleum_supply_thousands_day_data_3 <- petroleum_supply_thousands_day_data[30:36,] %>%
  `rownames<-`(paste0("Products Supplied: ", str_trim(str_sub(rownames(.), 5))))
petroleum_supply_thousands_day_data_4 <- petroleum_supply_thousands_day_data[37,] %>%
  `rownames<-`(paste0("Net Imports of Crude and Petroleum Products: ", str_trim(str_sub(rownames(.), 5))))

petroleum_supply_thousands_day_clean <- rbind(petroleum_supply_thousands_day_data_1,
                                              petroleum_supply_thousands_day_data_2,
                                              petroleum_supply_thousands_day_data_3,
                                              petroleum_supply_thousands_day_data_4)

petroleum_supply_thousands_day <- bind_cols(petroleum_supply_thousands_day_clean,
                                            petroleum_supply_thousands_day_categories_2)

# put into format to join
petroleum_supply_thousands_day_update <- petroleum_supply_thousands_day %>%
  select(1) %>%
  rownames_to_column(var = "variable") %>%
  mutate(date = sub("^Current Week: ", "", names(.)[2]),
         date = as.Date(mdy(date)),
         .before = 1) %>%
  rename_with(~ "value", .cols = 3) %>%
  mutate(units = "thousands of barrels per day") 
  
petroleum_stocks_millions_week_update <- petroleum_stocks_millions_week %>%
  select(1) %>%
  rownames_to_column(var = "variable") %>%
  mutate(date = sub("^Current Week: ", "", names(.)[2]),
         date = as.Date(mdy(date)),
         .before = 1) %>%
  rename_with(~ "value", .cols = 3) %>%
  mutate(units = "millions of barrels per week") 

# save data
# get old data
base_path <- "/Users/josephjason/Documents/Forecasting/R/projects"
old_supply <- readRDS(file.path(base_path, "Macroeconomic Analyis/Energy Analysis/data/petroleum_supply_thousands_day.RDS"))
old_stock <- readRDS(file.path(base_path, "Macroeconomic Analyis/Energy Analysis/data/petroleum_stocks_millions_week.RDS"))
# join new data to old
updated_eia_supply <- bind_rows(old_supply,
                                petroleum_supply_thousands_day_update)
updated_eia_stock <- bind_rows(old_stock,
                               petroleum_stocks_millions_week_update)
# save over old files
saveRDS(updated_eia_supply, 
        file.path(base_path, "Macroeconomic Analyis/Energy Analysis/data/petroleum_supply_thousands_day.RDS"))
saveRDS(updated_eia_stock,
        file.path(base_path, "Macroeconomic Analyis/Energy Analysis/data/petroleum_stocks_millions_week.RDS"))
