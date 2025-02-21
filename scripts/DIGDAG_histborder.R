####################################################

#   GETTING HISTORICAL BORDERS OF CITIES
#

#   DOES NOT WORK ALL THAT WELL YET



### ------------- GET AARHUS BOUNDARY STEDNAVN API

#GET WFS data stednavne

install.packages("ows4R")
library(ows4R)
library(httr) # generic webservice package

wfs <- "https://api.dataforsyningen.dk/digdag_gml2?service=WFS&request=GetCapabilities&token=036d0986e0412d0a474b85d0384ec2e1"
#review in browser

wfs <- "http://schemas.kms.dk/wfs"

wfs <- "https://api.dataforsyningen.dk/digdag_gml2"

url <- parse_url(wfs)
url$query <- list(service = "wfs",
                  version = "2.0.0", # optional
                  request = "GetFeature",
                  typename = "kms:Stednavn",
                  srsName = "EPSG:4326",
                  token ="036d0986e0412d0a474b85d0384ec2e1"
)

request <- build_url(url)
stednavn <- read_sf(request) # trying to get city placenames
head(stednavn)


st_bbox(st_transform(BDG, 25832))
provinces2 <- read_sf(paste0("WFS:", wfs),
                      layer = "Stednavn",
                      bbox = "566847.7,6212708.6,578856.0,6230253.3"
                      # FEATUREID = ""
)

head(provinces2)

prov2 <- read_sf(paste0("WFS:", wfs),
                 query = "SELECT * FROM Stednavn WHERE NavnID=751"
)

#################### Modern cities

## https://dawadocs.dataforsyningen.dk/dok/stednavne

#aarhus <- read_sf("https://api.dataforsyningen.dk/steder?hovedtype=Bebyggelse&undertype=by&primærtnavn=Aarhus&format=geojson")
#st_write(aarhus, "data/DanishCities2024.geojson")
towns <- read_sf("data/DanishCities2024.geojson")
head(towns)
mapview(towns)

aarhus <- towns %>% 
  filter(bebyggelseskode == 11045) 
aarhus %>% 
  mapview()

