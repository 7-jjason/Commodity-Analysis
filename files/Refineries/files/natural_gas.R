library(eia)
library(httr)
library(jsonlite)
library(RPostgres)
library(DBI)
library(tidyverse)

api_prompt <- "&api_key="
eia_api_key <- source("files/eia_api_key.R")$value
eia_set_key(eia_api_key)

# Natural Gas - Storage - Weekly - Working Gas in Underground Storage
weekly_working_storage_natgas_url <- "https://api.eia.gov/v2/natural-gas/stor/wkly/data/?frequency=weekly&data[0]=value&start=2010-01-01&end=2026-02-13&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"
response <- GET(paste0(weekly_working_storage_natgas_url, 
                       api_prompt,
                       eia_api_key))
data <- fromJSON(content(response, "text"))
df_1 <- data$response$data

# Natural Gas - Storage - Monthly - Underground Natural Gas Storage Capacity
monthly_storage_capacity_natgas_url <-"https://api.eia.gov/v2/natural-gas/stor/cap/data/?frequency=monthly&data[0]=value&start=1994-01&end=2025-11&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"
response <- GET(monthly_storage_capacity_natgas_url,
                query = list(api_key = eia_api_key))
data <- fromJSON(content(response, "text"))
df_2 <- data$response$data

# Natural Gas - Production - Monthly - Natural Gas Plant Processing
monthly_production_processing_natgas_url <- "https://api.eia.gov/v2/natural-gas/prod/pp/data/?frequency=monthly&data[0]=value&start=1973-01&end=2025-11&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"
response <- GET(monthly_production_processing_natgas_url,
                query = list(api_key = eia_api_key))
data <- fromJSON(content(response, "text"))
df_3 <- data$response$data

# Natural Gas - Imports and Exports/Pipelines - Monthly - U.S. Natural Gas Imports by Point of Entry
monthly_imports_natgas_url <- "https://api.eia.gov/v2/natural-gas/move/poe1/data/?frequency=monthly&data[0]=value&start=1973-01&end=2025-11&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"
response <- GET(monthly_imports_natgas_url,
                query = list(api_key = eia_api_key))
data <- fromJSON(content(response, "text"))
df_4 <- data$response$data

# Natural Gas - Imports and Exports/Pipelines - Monthly - U.S. Natural Gas Exports and Re-Exports by Country
monthly_exports_natgas_url <- "https://api.eia.gov/v2/natural-gas/move/poe2/data/?frequency=monthly&data[0]=value&start=1973-01&end=2025-11&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"
response <- GET(monthly_exports_natgas_url,
                query = list(api_key = eia_api_key))
data <- fromJSON(content(response, "text"))
df_5 <- data$response$data

# Natural Gas -  Consumption/End Use - Monthly - Natural Gas Consumption By End Use
monthly_consumption_natgas_url <- "https://api.eia.gov/v2/natural-gas/cons/sum/data/?frequency=monthly&data[0]=value&start=1973-01&end=2025-11&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"
response <- GET(monthly_consumption_natgas_url,
                query = list(api_key = eia_api_key))
data <- fromJSON(content(response, "text"))
df_6 <- data$response$data

# Natural Gas - Consumption/End Use - Monthly - Heat Content of Natural Gas Consumed
monthly_consumption_heat_natgas_url <- "https://api.eia.gov/v2/natural-gas/cons/heat/data/?frequency=monthly&data[0]=value&start=2012-01&end=2025-11&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"
response <- GET(monthly_consumption_heat_natgas_url,
                query = list(api_key = eia_api_key))
data <- fromJSON(content(response, "text"))
df_7 <- data$response$data


df_1 <- df_1 |>
  mutate(period = as.Date(period),
         value = as.numeric(value))

filter_date <- as.Date("2023-01-01")

# plot df_1
ggplot() +
  geom_path(
    data = df_1 |> 
      filter(period >= filter_date),
    aes(
      x = period,
      y = value,
      colour = `series-description`,
      group = `series-description`
    )
  ) +
  geom_vline(
    xintercept = as.numeric(Sys.Date() - years(0:3)),
    linetype = "dashed",
    colour = "red",
    alpha = 0.5
  ) +
  labs(
    title = "Volume of Working Underground Storage Capacity",
    x = "",
    y = "Volume in Billions of Cubic Feet"
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%Y %b"
  ) +
  scale_y_continuous(
    breaks = seq(min(df_1$value, na.rm = TRUE),
                 max(df_1$value, na.rm = TRUE),
                 (max(df_1$value, na.rm = TRUE) - min(df_1$value, na.rm = TRUE))/12)
  ) +
  theme(
    legend.position = "bottom"
  ) +
  guides(colour = guide_legend(nrow = 4))








# store in postgresql
db <- 'Petroleum'
host_db <- 'localhost'
db_port <- '5432'
db_user <- 'josephjason'
db_password <- 'whyohwhypasswords0.'
drv <- Postgres()
table_name <- 'archived_ais'
temp_table_name <- 'temporary_table'

con <- dbConnect(drv, 
                 dbname = db, 
                 host = host_db, 
                 port = db_port,
                 user = db_user, 
                 password = db_password)

# store data temporarily
dbWriteTable(con, 
             temp_table_name, 
             to_archive_sql,
             temporary = TRUE,
             overwrite = TRUE,
             row.names = FALSE)

# first store - write initial tables completely in - then 
# ALTER TABLE table_name ADD CONSTRAINT unique_cols UNIQUE (col1, col2); on the postgresql side
dbWriteTable(con,
             table_name,
             initial_write,
             overwrite = FALSE,
             row.names = FALSE)

# insert into main table - need to create archived_ais first 
dbExecute(con, "
          insert into archived_ais
          select * from temporary_table
          on conflict () do nothing")

# get data
data <- dbGetQuery(con, paste0("select * from ", table_name))

# close connection
dbDisconnect(con)


















