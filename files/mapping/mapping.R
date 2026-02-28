
# #############################################################################
# # Load Packages # 
# #############################################################################

library(sf)
library(usmap)
library(cancensus)
library(ggplot2)

# #############################################################################
# # Notes #
# #############################################################################

# get monthly refinery throughput canada
# collect refinery throughput data
# map refinery throughput data to refinery geographic data
# regress historical pipeline throughput on historical refinery throughput,
#   padd region, corporation, so I can predict current pipeline throughput

# #############################################################################
# # Get and Map USA and PAD Districts # 
# #############################################################################

states_sf <- us_map(regions = "states") |>
  mutate(subdistrict = case_when(
    full %in% c("Connecticut",
                "Maine",
                "Massachusetts",
                "New Hampshire",
                "Rhode Island",
                "Vermont") ~ "Subdistrict 1A (New England)",
    full %in% c("Delaware",
                "District of Columbia",
                "Maryland",
                "New Jersey", 
                "New York",
                "Pennsylvania") ~ "Subdistrict 1B (Central Atlantic)",
    full %in% c("Florida",
                "Georgia",
                "North Carolina",
                "South Carolina",
                "Virginia",
                "West Virginia") ~ "Subdistrict 1C (Lower Atlantic)",
    full %in% c("Illinois",
                "Indiana",
                "Iowa",
                "Kansas",
                "Kentucky",
                "Michigan",
                "Minnesota",
                "Missouri",
                "Nebraska",
                "North Dakota",
                "South Dakota",
                "Ohio",
                "Oklahoma",
                "Tennessee",
                "Wisconsin") ~ "PAD District 2 (Midwest)",
    full %in% c("Alabama",
                "Arkansas",
                "Louisiana",
                "Mississippi",
                "New Mexico",
                "Texas") ~ "PAD District 3 (Gulf Coast)",
    full %in% c("Colorado",
                "Idaho",
                "Montana",
                "Utah",
                "Wyoming") ~ "PAD District 4 (Rocky Mountain)",
    full %in% c("Alaska",
                "Arizona",
                "California",
                "Hawaii",
                "Nevada",
                "Oregon",
                "Washington") ~ "PAD District 5 (West Coast)",
    full %in% c("Puerto Rico") ~ "PAD District 6 (Caribbean)"
  ),
  district = case_when(
    str_detect(subdistrict, "Subdistrict 1") ~ "PAD District 1 (East Coast)",
    TRUE ~ subdistrict
  ))

# #############################################################################
# # Get and Map Canada # 
# #############################################################################

options(cancensus.api_key='CensusMapper_133cc0037fc8a8593d2d76d8a2e0560b')

canada_map <- get_census(
  dataset = "CA21",
  regions = list("PR" = c("62","61","60","59","48","47",
                          "46","35","24","10","13","12","11")),
  labels = "detailed",
  level = "PR",
  geo_format = "sf",
  use_cache = TRUE
)

canada_sf <- canada_map |>
  dplyr::select(`Shape Area`,
                GeoUID,
                name,
                geometry)

# #############################################################################
# # Get and Map Pipelines # 
# #############################################################################

# US
pipeline_us_sf <- st_read("https://openenergyhub.ornl.gov/api/explore/v2.1/catalog/datasets/petroleumproduct_pipelines_us_eia/exports/geojson?lang=en&timezone=America%2FDenver")
st_geometry_type(pipeline_us_sf, by_geometry = FALSE)

# Canada
pipeline_can_sf <- st_read("data/Canadian_Pipeline.geojson")

# Check and Remove Duplicates
pipeline_sf <- bind_rows(pipeline_us_sf |>
                           select(opername,
                                  pipename,
                                  shape_length,
                                  geometry),
                         pipeline_can_sf |>
                           select(
                             pipename = name_en,
                             opername = ch,
                             shape_length = Shape__Length,
                             geometry
                             )
                         ) 
  

# Test 

# #############################################################################
# # Get and Clean Crude Oil Storage Data for Canada # 
# #############################################################################

can_crude_storage <- rio::import("https://www.cer-rec.gc.ca/en/data-analysis/energy-commodities/crude-oil-petroleum-products/statistics/weekly-crude-run-summary-data/historical-weekly-crude-runs-donnees-sur-les-charges-hebdomadaires-historique.xlsx")

can_crude_storage_clean <- can_crude_storage %>%
  select(2, 6, 8, 12, 14:16, 18:20) %>%
  janitor::row_to_names(row_number = 6) %>%
  rename_with(~ trimws(gsub("/.+$", "", gsub("\\(english\\)", "", ., ignore.case = TRUE)))) %>%
  select(-contains("french")) %>%
  mutate(`Week End` = as.Date(ymd(`Week End`)),
         `Week End Last Year` = as.Date(ymd(`Week End Last Year`)),
         across(-c(`Week End`,
                   `Week End Last Year`,
                   Region),
                ~ round(as.numeric(.), 4)))

# #############################################################################
# # Map Crude Oil Storage Data to Canadian Geography # 
# #############################################################################
# map crude oil storage to Canada's geography
canada_crude_sf <- canada_sf |>
  mutate(
    Region = case_when(
      GeoUID == 35 ~ "Ontario",
      GeoUID %in% c(10, 11, 12, 13, 24) ~ "Quebec & Eastern Canada",
      GeoUID %in% c(46, 47, 48, 59, 60, 61, 62) ~ "Western Canada"
    )
  ) |> 
  full_join(can_crude_storage_clean)
canada_crude_sf_recent <- canada_crude_sf |> 
  group_by(`Region`) |>
  filter(`Week End` == max(`Week End`))

# #############################################################################
# # Get and Clean Refinery Data # 
# #############################################################################

# US
refineries_url_1 <- "https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/Petroleum_Refineries_in_the_US/FeatureServer/0"

# Canada
refineries_url_2 <- "https://services.arcgis.com/jDGuO8tYggdCCnUJ/arcgis/rest/services/US_and_Canadian_Oil_Refineries/FeatureServer/0"
query <- "/query?where=1%3D1&outFields=*&f=geojson"


refineries_1 <- st_read(paste0(refineries_url_1, query)) 
refineries_2 <- st_read(paste0(refineries_url_2, query))

# Join Refinery Lists
refineries_sf <- bind_rows(refineries_1 |>
                             select(FID, Corp, Company, City_Site = Site, Prov_State = State, PADD, Capacity = AD_Mbpd, Latitude, Longitude, geometry),
                           refineries_2 |>
                             select(FID, Corp = Company, Company = Name, City_Site = City, Prov_State, Capacity, Latitude, Longitude, geometry) |>
                             mutate(PADD = NA)) |>
  filter(PADD != 0 | is.na(PADD)) |>
  mutate(id = row_number(),
         .before = 1)

# Check for Duplicates

# check against refinery list
refinery_test <- rio::import("https://www.eia.gov/energyexplained/oil-and-petroleum-products/data/refinery_rank_2024.xls") |>
  janitor::row_to_names(row_number = 1) |>
  mutate(`Barrels per calendar day` = as.numeric(`Barrels per calendar day`))



# #############################################################################
# # Consolidate Mapping Type # 
# #############################################################################

# make mapping type consistent
st_crs(states_sf)
st_crs(canada_sf)
st_crs(pipeline_sf)
st_crs(refineries_sf)

# transform to WGS 84 
states_sf <- st_transform(states_sf, crs = 4326)

# #############################################################################
# # Shift Alaska to Correct Location #
# #############################################################################

# shift alaska to actual location
st_geometry(states_sf)[states_sf$full == "Alaska"] <- 
  st_geometry(states_sf)[states_sf$full == "Alaska"] + c(-30, 38)

# scale up alaska
alaska <- states_sf[states_sf$full == "Alaska", ] # get alaska shape
centroid <- st_centroid(st_union(alaska))         # get centroid
st_geometry(states_sf)[states_sf$full == "Alaska"] <- 
  (st_geometry(alaska) - st_coordinates(centroid)) * 1.8 + st_coordinates(centroid)

# rotate alaska clockwise
alaska <- states_sf[states_sf$full == "Alaska", ] # get new alaska shape
centroid_vec <- st_coordinates(st_centroid(st_union(alaska)))[1, 1:2]
angle <- 24 * pi / 180  # negative = clockwise, positive = counter-clockwise
rot_matrix <- matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)), nrow = 2)
st_geometry(states_sf)[states_sf$full == "Alaska"] <- 
  (st_geometry(alaska) - centroid_vec) * rot_matrix + centroid_vec

# #############################################################################
# # Shift Hawaii to Correct Position #
# #############################################################################


# #############################################################################
# # Shift Puerto Rico to Correct Postition #
# #############################################################################


# #############################################################################
# # Get Weekly PAD District Production Data and Map to USA PADD Map #
# #############################################################################

# pull padd data to map to geographic data
padd_df <- readRDS("data/inputs_production_pad.RDS")

# if PADD, then map to refineries on geographic map
padd_refineries <- padd_df |>
  filter(group == "Refiner Inputs and Utilization ",
         str_detect(variable, "PADD")) |>
  mutate(group = trimws(group),
         PADD = parse_number(variable))

states_padd_sf <- full_join(states_sf |> 
                              mutate(PADD = parse_number(district)),
                            padd_refineries,
                            by = "PADD")

# #############################################################################
# # Plot #
# #############################################################################

# right now - usa fill is utilization of refineries and canada is capacity of storage 
# so should map separtely 
# map refinery utilization either to size of point, alpha of point, or bin it and use shapes
# us utilization is by pad district and canada storage capacity is by ontario, west, east


# plot
ggplot() +
  geom_sf(
    data = canada_crude_sf_recent,
    aes(
      fill = `% of capacity`
    )
  ) +
  geom_sf(
    data = states_padd_sf |> filter(group_2 == "Percent Utilization"),
    aes(
      fill = value
    )
  ) +
  geom_sf(
    data = pipeline_sf,
    alpha = 0.5,
    colour = "orange"
  ) +
  geom_sf(
    data = pipeline_can_sf,
    alpha = 0.5,
    colour = "green"
  ) +
  geom_sf(
    data = refineries_sf,
    size = 0.4,
    alpha = 0.5,
    colour = "red"
  ) +
  theme_minimal()
















