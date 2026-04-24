
# Main Page
html <- read_html("https://www.cftc.gov/MarketReports/CommitmentsofTraders/HistoricalViewable/index.htm")
# Get Links from Main Page
new_urls <- html |>
  html_elements("tr") |>
  html_elements("a") |>
  html_attr("href") |>
  str_replace_all("/MarketReports/CommitmentsofTraders/HistoricalViewable/", "")

# Set Base Link to Concatenate With
base_url <- "https://www.cftc.gov/MarketReports/CommitmentsofTraders/HistoricalViewable/"
# Initialize Storage List
electricity_urls <- list()

# For Each Relevant URL on the Main Page
for (i in 1:length(new_urls)) {
  # Tracks Loop
  message(paste("Processing", i, "of", length(new_urls)))
  # Read the URL and Get HTML Page
  url <- paste0(base_url, new_urls[i])
  html_2 <- read_html(url)
  # Get All Links
  all_urls <- html_2 |>
    html_elements("td") |>
    html_elements("a") |>
    html_attr("href") 
  # Find and Store Relevant electricity and Products Disaggregated Futures-and-
  # Options-Combined Long Format URL
  electricity_urls[[i]] <- all_urls[str_detect(all_urls, "electricity_lof")]
  Sys.sleep(0.1)
}

electricity_urls[[469]] <- electricity_urls[[469]][3]
electricity_urls <- electricity_urls[lengths(electricity_urls) > 0]
  
# Save URLS
saveRDS(electricity_urls, file = "Energy Analysis/data/cftc_electricity_url_list.rds")


# Base URL for CSVs
base_url_2 <- "https://www.cftc.gov"
# Initialize Storage List
electricity_historical <- list()
# Get CSVs
for (i in 1:length(electricity_urls)) {
  # Tracks Loop
  message(paste("Processing", i, "of", length(new_urls)))
  # Skip Empty/NULL URLs
  if (length(electricity_urls[[i]]) == 0 || is.na(electricity_urls[[i]])) {
    message(paste("Skipping", i, "- no URL found"))
    electricity_historical[[i]] <- NA
    next
  }
  # Create CSV URL
  url_2 <- paste0(base_url_2, electricity_urls[i])
  # Get HTML Page from URL
  tryCatch({
    electricity_historical[[i]] <- read_html(url_2) |>
      html_nodes("pre") |>
      html_text()
  }, error = function(e) {
    message(paste("Error on", i, ":", e$message))
    # <<- is to assign in parent scope
    electricity_historical[[i]] <<- NA  
  })
  Sys.sleep(1)
}
# Save Historical Data 
saveRDS(electricity_historical, file = "Energy Analysis/data/cftc_electricity_historical_data.rds")


# Function to Clean Historical Data
clean_electricity_data <- function(electricity_preformatted) {
  # Split into lines
  electricity_split <- str_split(electricity_preformatted, "\n")[[1]] 
  
  # Turn into dataframe
  electricity_df <- as.data.frame(electricity_split) |>
    filter(row_number() >= 3) |>
    mutate(
      is_break = electricity_split == "\r" | trimws(electricity_split) == "",
      group = cumsum(is_break) + 1
    ) 
  
  # Turn into list
  electricity_list <- electricity_df |> 
    filter(!is_break) %>%
    split(.$group) |>
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
      
      x[[1]] <- x[[1]][[1]] 
      
      str1 <- x[[1]][1] |> strsplit("Code-")
      str2 <- x[[1]][2] |> strsplit("[a-z], ")
      
      Commodity <- str1[[1]][1] |> gsub("\\r", "", x = _) |> trimws()
      Code <- str1[[1]][2] |> gsub("\\r", "", x = _) |> trimws()
      Title <- str2[[1]][1] |> gsub("\\r", "", x = _) |> trimws()
      Date <- str2[[1]][2] |> gsub("\\r", "", x = _) |> trimws() |> mdy() |> as.Date()
      
      x[[1]] <- data.frame(Commodity, Date, Code, Title)
      
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
        select(-c("row_n", "to_split")) %>%
        mutate(across(everything(), ~na_if(trimws(.x), ".")),
               across(-c("type", "Type"), ~str_replace_all(.x, ",", "") |> as.numeric())) 
      
      x  
    }) 
  
  # supposed to name the inner commodity names - but fuckcs it all up
  # why does it work in other but not here? idc, im only doing this once
  # names(electricity_list) <- sapply(electricity_list, function(x) {
  #   x$Identifier$Commodity
  # })

}

# Clean Historical Data
electricity_historical_clean <- lapply(seq_along(electricity_historical), function(i) {
  message(paste("Cleaning", i, "of", length(electricity_historical)))
  clean_electricity_data(electricity_historical[[i]])
})

# Name by Date (from the URL or first commodity's date)
url_dates_vector <- electricity_urls |>
  sapply(function(x) {
    # Handle NULL, NA, or character(0)
    if (is.null(x) || length(x) == 0 || all(is.na(x))) {
      return(NA)
    }
    # Get non-NA value
    url <- x[!is.na(x)][1]
    # Extract between "_lof" and ".htm"
    date_str <- str_extract(url, "(?<=_lof).*(?=\\.htm)")
    # Set as Date
    as.Date(date_str, format = "%m%d%y")
  })

# get dates that we can read
url_dates <- as.Date(url_dates_vector, origin = "1970-01-01")
# set dates as names
names(electricity_historical_clean) <- url_dates
# name each inner element by Commodity
electricity_historical_clean <- lapply(electricity_historical_clean, function(outer) {
  names(outer) <- sapply(outer, function(inner) inner$Identifier$Commodity)
  outer
})
# organize by date
electricity_historical_clean <- electricity_historical_clean[order(names(electricity_historical_clean), decreasing = TRUE)]
# Save cleaned data
saveRDS(electricity_historical_clean, file = "Energy Analysis/data/cftc_electricity_historical_clean.rds")

