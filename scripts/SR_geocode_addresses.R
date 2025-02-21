####################################################

### Geocode filtered sikringsrum data from Ulrik's coordinates 

#######################################################

# libraries
library(tidyverse)
library(opencage)
help("oc_config")
base::interactive() # to interactively put in the API key
oc_config()


### ------------- GET DATA 

# Ulrik's data
sr <- read_csv("../raw_data/BBR_AarhusAll.csv")

# spatialize and get decades
sr <- sr %>% 
  filter(!grepl("GEOMETRY", byg404Koordinat)) %>%  # only valid coordinates
  mutate(decade = case_when(
    byg026Opførelsesår < 1940 ~ '1930s',
    byg026Opførelsesår < 1950 ~ '1940s',
    byg026Opførelsesår < 1960 ~ '1950s',
    byg026Opførelsesår < 1970 ~ '1960s',
    byg026Opførelsesår < 1980 ~ '1970s',
    byg026Opførelsesår < 1990 ~ '1980s',
    byg026Opførelsesår < 2000 ~ '1990s',
    byg026Opførelsesår < 2010 ~ '2000s'
  )) %>% 

  st_as_sf(wkt = "byg404Koordinat", crs = 25832) %>% 

# convert to 4326
 
  st_transform(crs = 4326) %>% 
  mutate(latitude = st_coordinates(.)[,2],
         longitude = st_coordinates(.)[,1])

sr


### ------------- Test the address-geocoding Workflow on first 10 items

# Reverse geocode : get addressses from GPS points
sr_address_only <- oc_reverse(sr$latitude[1:10], sr$longitude[1:10] , abbrv = TRUE,  address_only = TRUE)
sr_address_only

# Recommended test workflow at https://docs.ropensci.org/opencage/articles/output_options.html
library(dplyr, warn.conflicts = FALSE)

sr_oc_add <- sr %>% 
  slice(1:10) %>% 
  select(-kommunekode) %>% 
  mutate(oc_result = oc_reverse(latitude, longitude, abbrv = TRUE,  address_only = TRUE))

sr_oc_add %>% unnest(oc_result)

### ------------- FULL RUN OF THE address-geocoding Workflow


# The workflow at https://docs.ropensci.org/opencage/articles/output_options.html
library(dplyr, warn.conflicts = FALSE)

sr_oc_add <- sr %>% 
  select(-kommunekode) %>% 
  mutate(oc_result = oc_reverse(latitude, longitude, abbrv = TRUE,  address_only = TRUE))

sr_oc_addresses <- sr_oc_add %>% unnest(oc_result)

### ------------- SAVE RESULTS

# save full output
saveRDS(sr_oc_addresses, "../output_data/SR_oc_addresses.rds") # won't save as geojson

# save spatial output
sr_oc_addresses %>% 
  select(id_lokalId:oc_formatted, oc_category, oc_type) %>%  # only select columns save
  st_write("../output_data/SR_oc_addresses.geojson")

# save addresses in CSV
sr_oc_addresses %>% 
  select(id_lokalId:oc_formatted, oc_category, oc_type) %>% 
  st_drop_geometry() %>% 
  write_csv("../output_data/SR_oc_addresses.csv")

### -------------- Test saved results
sr <- readRDS("../output_data/SR_oc_addresses.rds")
sr # crs = 4326
