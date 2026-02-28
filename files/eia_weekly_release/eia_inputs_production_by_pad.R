
library(tidyverse)
library(janitor)
library(tibble)

inputs_production_pad <- read.csv("https://ir.eia.gov/wpsr/table2.csv",
                                  header=FALSE, 
                                  stringsAsFactors=FALSE, 
                                  fileEncoding="latin1")

# clean data - warning is okay
inputs_production_pad_clean <- inputs_production_pad %>%
  row_to_names(row_number = 1) %>%
  setNames(c(
    "group",
    "variable",
    paste0("Current Week: ", names(.)[3]),
    paste0("Week Ago: ", names(.)[4]),
    paste0("Week Ago: ", names(.)[5]),
    paste0("Year Ago: ", names(.)[6]),
    paste0("Year Ago: ", names(.)[7]),
    paste0("Two Years Ago: ", names(.)[8]),
    paste0("Two Years Ago: ", names(.)[9]),
    paste0("Four Week Averages: ", names(.)[10]),
    paste0("Four Week Averages: ", names(.)[11]),
    paste0("Four Week Averages: ", names(.)[12])
  )) %>%
  mutate(across(everything(), ~str_replace_all(., ",", "")),
         across(-(1:2), ~as.numeric(.)))

# hardcode helper list for next step
helper <- c(
  rep("Crude Oil Inputs", 6),
  rep("Gross Inputs", 6),
  rep("Operable Capacity", 6),
  rep("Percent Utilization", 6),
  rep("Finished Motor Gasoline", 7),
  "Reformulated",
  "Conventional",
  "Adjustment",
  rep("Kerosene-Type Jet Fuel", 6),
  rep("Distillate Fuel Oil", 6),
  rep("15 ppm sulfer and Under", 3),
  rep("Residual Fuel Oil", 6),
  rep("Propane/Propylene", 5),
  rep("Fuel Ethanol", 6)
)

# format to be saved
inputs_production_pad_update <- inputs_production_pad_clean %>%
  select(1:3) %>%
  mutate(date = sub("^Current Week: ", "", names(.)[3]),
         date = as.Date(mdy(date)),
         .before = 1) %>%
  rename_with(~ "variable", .cols = 3) %>%
  rename_with(~ "value", .cols = 4) %>%
  mutate(units = "thousands of barrels per day",
         group_2 = helper) 

path_base <- "/Users/josephjason/Documents/Forecasting/R/projects/Macroeconomic Analysis"

# call old data
old <- readRDS(file.path(path_base, "Energy Analysis/data/inputs_production_pad.RDS"))
# update data
updated <- bind_rows(old, inputs_production_pad_update)
# save over old data
saveRDS(updated, file.path(path_base, "Energy Analysis/data/inputs_production_pad.RDS"))

































