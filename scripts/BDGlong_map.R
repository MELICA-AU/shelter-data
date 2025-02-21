# Loads public shelters in long (chronological) format from RDS for plotting and temporal assessment

library(tidyverse)
library(googlesheets4)
library(sf)
library(mapview)

### ------------- LOAD LONG DATA FROM RDS OR CSV local

BDG <- readRDS("../output_data/BDG_long.rds")
#BDG <- read_csv("../output_data/BDGlong.csv") # for non-spatial work

glimpse(BDG)


### ------------- PLOT LONG DATA


# Plot BDGs in 1944
BDG %>% 
  filter(Location_startdate==1944) %>% 
  mapview(cex = "Capacity")

# Plot BDGs in 1960s
BDG %>% 
  filter(Location_startdate >1949 & Location_startdate <1970) %>% 
  mapview(cex = "Capacity")

# Plot the first instance of BDG in space (by year of construction)
m <- BDG %>% 
  group_by(BDnr) %>% 
  summarize(Startyear = min(Location_startdate), 
            Capacity = min(Capacity)) %>% 
  mapview(cex = "Capacity", zcol = "Startyear")
m


### ------------- LENGTHEN LONG DATA TO HAVE ANNUAL STATUS (OR BIANNUAL, ETC.)

# Lengthen BDG dataset
expanded_BDG <- BDG %>%
  select(BDnr, Type, Capacity, Location_startdate) %>% 
  group_by(BDnr) %>%
  arrange(Location_startdate) %>%
  mutate(end_year = lead(Location_startdate, default = 2024) - 1) %>%
  ungroup() %>%
  mutate(geometry = if_else(is.na(geometry), lag(geometry), geometry)) %>%
  rowwise() %>%
  mutate(year = list(seq(Location_startdate, end_year, by = 2))) %>%  # by= determines how many years we explore the data over
  unnest(year) %>%
  select(-end_year)

# Inspect one shelter in the lengthened BDG dataset
expanded_BDG %>% 
  filter(BDnr == 101)


# Add 'decade' attribute
expanded_BDG <- expanded_BDG %>% 
  mutate(decade = case_when(
    year < 1940 ~ '1930s',
    year < 1950 ~ '1940s',
    year < 1960 ~ '1950s',
    year < 1970 ~ '1960s',
    year < 1980 ~ '1970s',
    year < 1990 ~ '1980s',
    year < 2000 ~ '1990s',
    year < 2010 ~ '2000s',
    year < 2020 ~ '2010s',
    year < 2030 ~ '2020s'
  )) 



# New shelters established! None in the 1950s
BDG %>% 
  group_by(decade) %>% 
  tally()
BDG %>% st_drop_geometry() %>% 
  pull(decade) %>% unique() %>% sort()

# Total shelter existence/movement events (every 2 years)
expanded_BDG %>% st_drop_geometry() %>% 
  pull(decade) %>% unique() %>% sort()

expanded_BDG %>% 
  group_by(decade) %>% 
  tally()



### -------------   BDG TMAP FACETTED

# Visualize BDG construction and capacity over time - COMPLETE overview
library(tmap)
tmap_options(limits = c(facets.view = 6))  # we want to view 5 periods

tmap_mode(mode = "view")

tm_shape(BDG %>% filter(Location_startdate < 1950))+
  # tm_facets(by = "decade",
  #           ncol = 3)+
  tm_bubbles(size = "Capacity",
             col = "darkgrey")+

tm_shape(expanded_BDG %>% filter(year > 1944 & year < 1999))+
  tm_facets(by = "decade",
            ncol = 3)+
  tm_bubbles(size = "Capacity",
             col = "red")
  
 

# Only changes up to 1989

library(tmap)
tmap_options(limits = c(facets.view = 6))  # we want to view 5 periods

tmap_mode(mode = "view")

tm_shape(BDG %>% filter(decade < "1990s"))+
  tm_facets(by = "decade",
            ncol = 2)+
  tm_bubbles(size = "Capacity")

tmap_mode("plot")



### ------------- CHECK MOVEMENT OF BDG

# Some shelters changed location, in which case they are mentioned multiple times.  
# How many shelter moves do we have on file? (1-3 moves)
movedBDG <- BDG %>% 
  group_by(BDnr) %>% 
  summarize(n = n(),
            year = last(Location_startdate)) %>% 
  filter(n==2)

# 8 shelters moved twice times, three thrice, 134 were moved at least once. 

BDG %>% st_drop_geometry() %>% 
  pull(Location_startdate) %>% unique() %>% sort()


### -------------  MAP MOVEMENT OF BDG

BDG_lines <- movedBDG %>%
  filter(st_geometry_type(geometry) == "MULTIPOINT") %>%
  st_cast("LINESTRING")

library(mapview)
mapview(BDG_lines, zcol = "year") + mapview(BDG, zcol = "Location_startdate")

# majority of shelters are disposed of to fortify the banks of Brabrand!


### ------------- REMOVE BRABRAND SHELTER CEMETERY


# 92 shelters removed to Brabrand!
read_csv("../output_data/BDG_long.csv") %>% 
  filter(!is.na(longitude) | !is.na(latitude) ) %>% 
  filter(latitude == 56.150046 & longitude ==10.107009) %>% 
  distinct(BDnr)


## Filter away the Brabrand location from the csv dataset
BDG_noBrabrand_sf <- read_csv("../output_data/BDG_long.csv")%>% 
  filter(!is.na(longitude) | !is.na(latitude) ) %>% 
  filter(latitude != 56.150046 & longitude !=10.107009) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 

movedBDG_noB <- BDG_noBrabrand_sf %>% 
  group_by(BDnr) %>% 
  summarize(n = n(),
            year = last(Location_startdate)) %>% 
  filter(n==2)

## Let's map the movements besides Brabrand

BDG_noB_lines <- movedBDG_noB %>%
  filter(st_geometry_type(geometry) == "MULTIPOINT") %>%
  st_cast("LINESTRING")

library(mapview)
mapview(BDG_noB_lines, zcol = "year") + 
  mapview(BDG_noBrabrand_sf, zcol = "Location_startdate")

##################################################### 

### -------------  ANIMATE BDG IN TIME  - PROOF OF CONCEPT
# For elaboration see the historical-trends / animation repository

library(gganimate)

?transition_time()
anim_bdg <- ggplot(data = BDG_noBrabrand_sf )+
  geom_sf(aes(color = Location_startdate))+
  theme_bw() +
  transition_time(Location_startdate)+
  labs(subtitle = "Year {round(frame_time,0)}")
anim_bdg 
# Keep the shelters plotted if they were not moved
# Fade out moved shelters' original locations
BDG %>% 
  select(ID, BDnr, Capacity, Location_startdate) %>% 
  arrange(ID)

A <- ggplot(data = BDG )+
  geom_sf(aes(color = Location_startdate))+
  theme_bw() +
  transition_time(Location_startdate)+
  labs(subtitle = "Year {round(frame_time,0)}")

# (In theory) Animate with custom settings to slow down the animation
anim <- animate(A, 
                fps = 10,                # Lower frames per second
                duration = 20,           # Total duration of the animation in seconds
                end_pause = 5)           # Adds a pause at the end of the animation

B <- BDG_noBrabrand_sf %>% 
  rename(year = Location_startdate) %>% 
  ggplot()+
  geom_sf(aes(size = 2, color = year, group = seq_along(year)),show.legend = F)

G2 <- B  +
  transition_states(states = year, state_length = 10, wrap = FALSE)+
  enter_recolor(fill = "#f0f5f9") +
  shadow_mark(past = TRUE, alpha = 1, fill = "#3a6589")

G1 <- B+
  transition_time(time = year, )

BDG_noB_lines

# Animate without rewinding
animate(G2, rewind = FALSE)


#geom_sf(data = BDG_noB_lines, aes(color = n))
mapview(BDG_noB_lines, zcol = "BDnr") + transition_time("Location_startdate")


