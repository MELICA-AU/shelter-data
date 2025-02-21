####################################################

### SR data from Ulrik (SR) with sikringsrum

####################################################

# This script creates historical sikringsrum spatial data from SR data provided by Ulrik Nielsen
# It removes empty geometries and renames columns to English, 
# saves data as SR_sikringsrum.geojson
# and creates SR_89 data for historical overview and visualisation
# offers summary mapview and facetted tmaps by decade in undifferentiated dataset

# Libraries 
library(tidyverse)
library(sf)
library(mapview)

### ------------- LOAD BBR data from Ulrik

SR <- read_csv("raw_data/BBR_AarhusAll.csv")

# kommunekode 751 indicates Aarhus kommune
SR %>% 
  group_by(kommunekode) %>% 
  tally() # there are 1895 instances of Aarhus kommune code?


# spatialize under projected SRS and add decades
SR <- SR %>% 
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
  st_as_sf(wkt = "byg404Koordinat", crs = 25832)

### -------------  INSPECT BUILDINGS WITH SR CONSTRUCTION trends

### Attributes
# any NAs in the 'decade'?  27 built in 2000-2004
SR %>% 
  # filter(is.na(decade))  
  filter(decade == '2000s') %>% pull(byg026Opførelsesår)

SR %>% 
  group_by(decade) %>% 
  tally()

### -------------   REMOVE EMPTY GEOMETRIES

### Spatial inspection shows empty geometries
sum(st_is_valid(SR$byg404Koordinat))
sum(st_is_empty(SR$byg404Koordinat))


## 2 ways of eliminating empty values: 1) st_is_empty() or 2) group and eliminate

# First
SR <- SR %>% 
  # filter away valid but empty geometries 
  filter(!st_is_empty(st_as_sf(byg404Koordinat))) %>%
  # rename columns
  rename(ID = id_lokalId, year = byg026Opførelsesår, places = byg069Sikringsrumpladser, 
         code = kommunekode, geometry = byg404Koordinat) %>% 
  # reinitialize renamed geometry column
  st_as_sf(wkt = "geometry", crs = 25832)

sum(st_is_empty(SR$geometry))

# Second
# SR %>% 
#   #group_by(byg404Koordinat) %>% tally()  #13 empty collections
#  filter(grepl("GEOMETRY", byg404Koordinat)) %>% pull(id_lokalId) -> missingGeo
# 
# #now spatialized SR
# SR[SR$id_lokalId%in%missingGeo,]
# SR

### -------------  SAVE 1882 SIKRINGSRUM FROM BBR points

st_write(SR, "output_data/SR.geojson", append = FALSE)


### ------------- SR 1989 - MAPVIEW FOR SANITY CHECK
# quick map
SR <- st_read("output_data/SR.geojson")
SR <- SR %>%
  mutate(decade = case_when(
    decade == '1930s' ~ '1180-1939',
    TRUE ~ decade  # Keep the original value for other cases
  )) 
mapview(SR, zcol = "decade")


SR_89 <- SR %>% 
  dplyr::filter(decade < "1990s") 
SR_89 %>% 
  mapview(zcol = "decade")


##########################################      What next? 
# - geocode - DONE IN SR_geocode_addresses
# - intersect with building footprints, crosscheck with archival records, 
# - calculate total capacity per decade and compare with population trends > Historical trends
# - check for duplicates: are all private shelters unique or are some double-entered,
# - because a building got upgraded, etc.?
