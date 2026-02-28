# Commodity Futures Trading Commission (CFTC) 
# Disaggregated Futures-and-Options - Combined - Long Format

library(tidyverse)
library(rvest)
library(httr)

# Natural Gas and Products 
electricity_page <- read_html("https://www.cftc.gov/dea/options/electricity_lof.htm")

# Get preformatted text
electricity_preformatted <- electricity_page |>
  html_nodes("pre") |>
  html_text()

# Split into lines
electricity_split <- str_split(electricity_preformatted, "\n")[[1]] 

# Turn into dataframe
electricity_df <- as.data.frame(electricity_split) |>
  # Remove first two lines
  filter(
    row_number() >= 3
    ) |>
  mutate(
    # Mark rows that are solely carriage returns
    is_break = electricity_split == "\r" | trimws(electricity_split) == "",
    # Create a group number that increments at each break (+ 1 just to index from 1)
    group = cumsum(is_break) + 1
    ) 

# Turn into list
electricity_list <- electricity_df |> 
  # Call non-empty rows
  filter(!is_break) %>%
  # Split by group
  split(.$group) |>
  # Remove is_break column
  lapply(function(x) {
    x <- select(x, electricity_split) |>
      mutate(row_n = row_number(),
             type = case_when(
               row_n == 1 ~ "Commodity",
               row_n == 2 ~ "Title and Date",
               row_n %in% c(11:13) ~ "Positions (Contracts of Varying Amounts by Commodity)",
               row_n == 16 ~ "Changes in Commitments from Last Week (Contracts of Varying Amounts by Commodity)",
               row_n %in% c(19:21) ~ "Percent of Open Interest Represented by Each Category of Trader",
               row_n %in% c(24:26) ~ "Number of Traders in Each Category",
               row_n %in% c(33:35) ~ "Percent of Open Interest Held by the Indicated Number of the Largest Traders",
               TRUE ~ NA_character_
             ),
             to_split = case_when(
               type %in% c("Positions (Contracts of Varying Amounts by Commodity)", 
                           "Changes in Commitments from Last Week (Contracts of Varying Amounts by Commodity)",
                           "Percent of Open Interest Represented by Each Category of Trader", 
                           "Number of Traders in Each Category") ~ "split 1",
               type == "Percent of Open Interest Held by the Indicated Number of the Largest Traders" ~ "split 2",
               type %in% c("Commodity", "Title and Date") ~ "Identifier",
               TRUE ~ NA_character_
             )) |>
      na.omit() %>%
      split(.$to_split)
    
    # Select relevant identifier data
    x[[1]] <- x[[1]][[1]] 
    
    str1 <- x[[1]][1] |> strsplit("Code-") # has before and after code
    str2 <- x[[1]][2] |> strsplit("[a-z], ") # has before and after a-z,
    
    # Assign strings, remove carriage returns and extra spaces
    Commodity <- str1[[1]][1] |> gsub("\\r", "", x = _) |> trimws()
    Code <- str1[[1]][2] |> gsub("\\r", "", x = _) |> trimws()
    Title <- str2[[1]][1] |> gsub("\\r", "", x = _) |> trimws()
    Date <- str2[[1]][2] |> gsub("\\r", "", x = _) |> trimws() |> mdy() |> as.Date()
  
    x[[1]] <- data.frame(
      Commodity,
      Date,
      Code, 
      Title
    )
    
    x[[2]] <- x[[2]] |>
      separate_wider_position(
        electricity_split,
        widths = c(
          "Type" = 5,
          1,
          "Open Interest" = 10,
          1,
          "Reportable Positions Producer/Merchant/Processor/User Long" = 10,
          1,
          "Reportable Positions Producer/Merchant/Processor/User Short" = 10,
          1,
          "Reportable Positions Swap Dealers Long" = 10,
          1,
          "Reportable Positions Swap Dealers Short" = 10,
          1,
          "Reportable Positions Swap Dealers Spreading" = 10,
          1,
          "Reportable Positions Managed Money Long" = 10,
          1,
          "Reportable Positions Managed Money Short" = 10,
          1,
          "Reportable Positions Managed Money Spreading" = 10,
          1,
          "Reportable Positions Other Reportables Long" = 10,
          1,
          "Reportable Positions Other Reportables Short" = 10,
          1,
          "Reportable Positions Other Reportables Spreading" = 10,
          1,
          "Nonreportable Positions Long" = 9,
          1,
          "Nonreportable Positions Short" = 9
          ),
        too_many = "drop",
        too_few = "align_start"
      ) |>
      relocate(type, .before = 1) |>
      as.data.frame() |>
      select(-c("row_n", "to_split")) %>%
      # Replace periods "." with NA; remove commas "," and set numeric where possible
      mutate(across(everything(), ~na_if(trimws(.x), ".")), 
             across(-c("type", "Type"), ~str_replace_all(.x, ",", "") |> as.numeric()))

    x[[3]] <- x[[3]] |>
      separate_wider_position(electricity_split,
                              widths = c(
        "Type" = 5,
        18,
        "By Gross Population; 4 or Less Traders Long" = 4,
        7,
        "By Gross Population; 4 or Less Traders Short" = 4,
        7,
        "By Gross Population; 8 or Less Traders Long" = 4,
        7,
        "By Gross Population; 8 or Less Traders Short" = 4,
        7,
        "By Net Population; 4 or Less Traders Long" = 4,
        7,
        "By Net Population; 4 or Less Traders Short" = 4,
        7,
        "By Net Population; 8 or Less Traders Long" = 4,
        7,
        "By Net Population; 8 or Less Traders Short" = 4
      ),
      too_many = "drop",
      too_few = "align_start"
      ) |>
      relocate(type, .before = 1) |>
      as.data.frame() |>
      # Remove extra rows
      select(-c("row_n", "to_split")) %>%
      # Replace periods "." with NA; remove commas "," and set numeric where possible
      mutate(across(everything(), ~na_if(trimws(.x), ".")),
             across(-c("type", "Type"), ~str_replace_all(.x, ",", "") |> as.numeric())) 
    
    x  
    }) 

names(electricity_list) <- sapply(electricity_list, function(x) {
  x$Identifier$Commodity
})

# Find Date from
new_date <- electricity_list[[1]][[1]][2] |>
  pull() |>
  as.Date()

# Load Old Data
old_data <- readRDS("Energy Analysis/data/cftc_electricity_historical_clean.rds")
# Combine with New
updated_data <- c(setNames(list(electricity_list), new_date), old_data)
# Save Combined
saveRDS(updated_data, "Energy Analysis/data/cftc_electricity_dynamically_updated.rds")
