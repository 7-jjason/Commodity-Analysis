library(tidyverse)
library(stringr)

# Collect Energy Information Association (EIA) Data
nat_gas_storage_wkly <- read.csv("https://ir.eia.gov/ngs/wngsr.csv",
                                 sep = ",",
                                 header = FALSE,
                                 skip = 4) 

# Get publication date
date <- nat_gas_storage_wkly[3, 2]

cleaned_storage <- nat_gas_storage_wkly |>
  select(!where(anyNA)) |>
  filter(row_number() <= 11) |>
  t() |>
  as.data.frame() |>
  row_to_names(row_number = 1) |>
  clean_names() |>
  unite("variable", 1:3, sep = " ") |>
  mutate(across(everything(), ~trimws(.x)),
         Date = date, 
         .before = 1,
         across(!c(Date, variable), ~gsub(",", "", .x)),
         across(!c(Date, variable), ~as.numeric(.x)),
         Date = dmy(Date),
         variable = tolower(variable)) |>
  rename(date = Date) 

rownames(cleaned_storage) <- NULL

cleaned_storage_long <- cleaned_storage |>
  pivot_longer(cols = 3:10, 
               names_to = "region",
               values_to = "values") 

path <- "/Users/josephjason/Documents/Forecasting/R/projects/Macroeconomic Analysis/Energy Analysis/data/reserve_changes.rds"
if (!file.exists(path)) {
  saveRDS(cleaned_storage_long, path)
} else {
  old <- readRDS(path)
  updated <- rbind(old, cleaned_storage_long)
  saveRDS(updated, path)
}
  

