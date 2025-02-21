## Field-visited BDG - statistical overview of insights for IJHA paper
# I use Andreas table wide, which includes dta on 2023 and 2024 fieldwork

library(tidyverse)
library(googlesheets4)
library(sf)
library(mapview)

# 
BDG <- read_rds("../output_data/BDG_long.rds")
BDGw <- read_rds("../output_data/BDG_wide1987.rds")

SR <- st_read("../output_data/SR.geojson")
SR <- SR %>%
  mutate(decade = case_when(
    decade == '1930s' ~ '1180-1939',
    TRUE ~ decade  # Keep the original value for other cases
  )) 

KOB <- read_rds("../output_data/KOB.rds") 

# groundtruthed FAIMS_shelters (209): missing Tranebjerg!
GC23 <- read_sf("../output_data/GC_2023.geojson") 
GC24 <- read_sf("../output_data/GC_20241012.geojson")
mapview(GC23)+mapview(GC24)


### -----------------------  CAPACITIES

# Summaries of capacities
BDGw %>% 
  group_by(Final_type) %>% 
  summarize(number = n(),
            total_capacity = sum(Final_Pub_Size))

BDG %>%
  select(BDnr, Type, Capacity, decade) %>% 
  filter(decade == "1960s" & !is.na(Type) )

BDG %>% 
  filter(BDnr == 131)

BDG %>% 
  filter(Location_startdate == 1944) %>% 
  tally(Capacity)

# Add data Andreas-long sheet to answer questions such as: 
# what is the number of all shelters in decade x? 
# How many shelters are decommissioned when? (add a column)
# update coordinates with FAIMS data 
BDG %>% 
  group_by(decade) %>% 
  summarize(number = n(),
            total_capacity = sum(Capacity, na.rm = T))


SR %>% 
  group_by(decade) %>% 
  summarize(number = n(),
            total_capacity = sum(places))

SR %>% 
  filter(year <1989) %>% 
  tally(places)

KOB %>% 
  group_by(decade) %>% 
  summarize(number = n(),
            total_capacity = sum(Capacity))

2# Total capacity of everything on the ground by 1987 was 294,799 places (assuming accurate numbers; 
# SR only has BBR numbers)
sum(BDGw$Final_Pub_Size, na.rm = T) + 
  sum(SR$places, na.rm = T) + 
  sum(KOB$Capacity, na.rm = T)

### -----------------------  VISITED

# Summary of visited/unvisited (Need googlesheet here)
glimpse(BDGw)
BDGw %>% 
  group_by(FAIMS_verified) %>% 
  tally()

# 46 need revisit (232 were visited and finalized)
BDGw %>% 
 filter(grepl("^Need", Needs_Revisit)) 


# according to Andreas's data , howe many total locations we visited? (202 between the extant and destroyed ones)
BDGw %>% 
  group_by(Status_2024) %>% 
  tally()

# This will work with GSheet, but not with rds
BDGw %>% 
  filter(FAIMS_verified == "Yes") %>% 
  group_by(Status_2024) %>% 
  tally()

BDGw %>% 
  filter(FAIMS_verified == "Yes") %>% 
  filter(Status_2024 == "Moved") 

# Typology of extant shelters
BDGw %>% 
  filter(FAIMS_verified == "Yes") %>% 
  filter(Status_2024 == "Exists") %>% 
  group_by(Final_type) %>% 
  tally()

# Which need to be seen and why? (ca. 20, mostly removed or destroyed)
BDGw %>% 
  st_drop_geometry() %>% 
  group_by(Needs_Revisit) %>% tally() %>% print(n=40)


### -----------------------  CURRENT STATUS, REUSE

# openness
GC23 %>% 
  filter(Accessible == "Open") # 5 in 2023
GC24 %>% 
  filter(accessiblity.of.shelter.during.visit == "Open")  # 3 in 2024 +1 reported by Sahel

GC23 %>% 
  group_by(LanduseAround) %>% 
  tally() %>% 
  arrange(desc(n))

GC24 %>% 
  group_by(`landuse.around`) %>% 
  tally() %>% 
  arrange(desc(n))


# Which of the unvisited, do NOT NEED a visit (ie. they have not been moved acc to Google Earth)
BDGw %>% 
  filter(FAIMS_verified == "No") %>% # 45
  filter(grepl("^Need", Needs_Revisit)) %>%  #42
  mapview() + mapview(s23)
