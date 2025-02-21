# Loads public shelters in both wide and long (chronological) format from Google

library(tidyverse)
library(googlesheets4)
library(sf)
library(mapview)


### -------------- CREATE WIDE DATA FROM SHELTERS-ANDREAS GDrive SOURCE

# Load Andreas complete data from Google and create a 1987 version (Wide dataset)
BDGw <- read_sheet("https://docs.google.com/spreadsheets/d/1H8EhFgwhDGKCsM95BTjNwifg5K-oXTv2Q4Qbx84k7ZU/edit?gid=0#gid=0",
                  range = "Shelters", 
                  col_types = "dddccddcddcccccdddcccccdddccddccccc")

# 1987 variant
BDGw <- BDGw %>% 
  filter(!is.na(Final_Longitude_1987) | !is.na(Final_Latitude_1987) ) %>% 
  st_as_sf(coords = c("Final_Longitude_1987", "Final_Latitude_1987"), crs = 4326) %>% 
  dplyr::select(BDnr_1987,year_of_Construction,  Final_type, Final_Pub_Size, 
                Needs_Revisit, Final_Longitude_2024, Final_Latitude_2024, Status_1987, Status_2024)
# saveRDS(BDGw, "output_data/BDG_wide1987.rds")

# 2024 variant
BDGw <- BDGw %>% 
  filter(!is.na(Final_Longitude_2024) & !is.na(Final_Latitude_2024) ) %>% 
  st_as_sf(coords = c("Final_Longitude_2024", "Final_Latitude_2024"), crs = 4326) %>% 
  dplyr::select(BDnr_1987,Year_of_Construction,  Final_type, Final_Pub_Size, 
                Needs_Revisit, Status_1987, Status_2024)
# saveRDS(BDGw, "output_data/BDG_wide2024.rds")


### -------------- LOAD WIDE DATA (SHELTERS-ANDREAS) FROM RDS 

# Load saved rds based on wide data representing 1987 situation 
BDGw <- readRDS("../output_data/BDG_wide1987.rds")
glimpse(BDGw)

# 10 closely undefined shelters - NEED REVISIT
BDGw %>% 
  filter(is.na(Final_type))

# Summaries of capacities in 1987
BDGw %>% 
  group_by(Final_type) %>% 
  summarize(number = n(),
    total_capacity = sum(Final_Pub_Size))


glimpse(BDGw)
mapview(BDGw, zcol = "Year_of_Construction")
        
######################################################################

### -------------- CREATE LONG DATA from Andreas' ALL-SHELTERS

# This dataset still has old FAIMS notes - update from wide shelters-andreas sheet.
# Good for historical "long" data and show through time

BDG <- read_sheet("https://docs.google.com/spreadsheets/d/1C4GEgq4UmEwx_Xi84FiNBrRowmmJHi3S191byjF-hWc/edit?gid=0#gid=0",
                  range = "Ark1",
                  col_types = "ddcdddcdccdc")
BDG  <- BDG %>%
  filter(!is.na(Long) | !is.na(Lat) ) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

BDG <- BDG %>% 
  mutate(decade = case_when(
    Location_startdate< 1940 ~ '1930s',
    Location_startdate< 1950 ~ '1940s',
    Location_startdate< 1960 ~ '1950s',
    Location_startdate< 1970 ~ '1960s',
    Location_startdate< 1980 ~ '1970s',
    Location_startdate< 1990 ~ '1980s',
    Location_startdate< 2000 ~ '1990s',
    Location_startdate< 2010 ~ '2000s',
    Location_startdate< 2020 ~ '2010s',
    Location_startdate< 2030 ~ '2020s'
  )) 

BDG %>%
  mutate(longitude = st_coordinates(.)[,1],
         latitude = st_coordinates(.)[,2]) %>%
  rename(id = ID) %>%
  st_drop_geometry() %>%
  write_csv("../output_data/BDG_long.csv")

saveRDS(BDG, "../output_data/BDG_long.rds")





### ------------- LOAD LONG DATA FROM RDS OR CSV local

BDG <- readRDS("../output_data/BDG_long.rds")

glimpse(BDG)

