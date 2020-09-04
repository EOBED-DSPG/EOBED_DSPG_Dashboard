##### Oregon State University DSPG Team Delta
##### 
##### Team Lead: Thamanna Vasan
##### Team Members: Melvin Ma, Collin Robinson
##### Staff Advisor: Stuart Reitz
##### 
##### File: medianIncomeGraphics.R
##### Author: Collin Robinson
##### Date Created: July 14, 2020
##### Last Update: July 14, 2020


# Heatmap for median income at the county level in both ID and OR, and at the 
# tract level for Malheur County, OR and Canyon, Payette, and Washington Counties
# in ID.

library(ggplot2)
library(dplyr)
library(tidyverse)
library(tigris)
library(acs)
library(stringr)
library(rgdal)
library(sp)
library(leaflet) 
library(rappdirs)
library(sf)
library(maptools)

########## SD Data ##########


cityLng <- c(-117.2382,-116.9949,-116.2023)
cityLat <- c(43.9821,43.8768,43.6150)
cityNames <- c("Vale","Nyssa","Boise")

cityLng2 <- c(-116.9629,-116.9165)
cityLat2 <- c(44.0266,44.0077)
cityNames2 <- c("Ontario","Fruitland")


# Read all School District shapes
SD <- readOGR(
  dsn ="./EDGE_SCHOOLDISTRICT_TL19_SY1819/schooldistrict_sy1819_tl19.shp",
  
)

# Save relevant counties in ID
# If using the first IdTract assignment, switch commented/uncommented lines below

# tmp <- SD
mySD <- SD[SD$STATEFP=="16"|SD$STATEFP=="41",]
mySD <- mySD[mySD$GEOID=="4101020"|mySD$GEOID=="4101350"|mySD$GEOID=="4101500"|mySD$GEOID=="1600510"|mySD$GEOID=="1600570"|
               mySD$GEOID=="1600570"|mySD$GEOID=="1600152"|mySD$GEOID=="1601140"
             |mySD$GEOID=="4106120"|mySD$GEOID=="1600161"|mySD$GEOID=="4106820"|mySD$GEOID=="4106960"|
               mySD$GEOID=="1600158"|mySD$GEOID=="1600012"|mySD$GEOID=="1602070"|mySD$GEOID=="1602130"|
               mySD$GEOID=="1602160"|mySD$GEOID=="1602340"|mySD$GEOID=="1602460"|
               mySD$GEOID=="4109000"|mySD$GEOID=="4109270"|mySD$GEOID=="1602550"|
               mySD$GEOID=="1602580"|mySD$GEOID=="4100014"|mySD$GEOID=="1600600"|
               mySD$GEOID=="1600003"|mySD$GEOID=="1600141"|mySD$GEOID=="1603330"|mySD$GEOID=="1603480",]
mySD <- st_as_sf(mySD)


########## County Data ##########

# Read US county data
counties <- readOGR(
  dsn ="./US_Counties",
  layer = "tl_2019_us_county"
)

# Isolate ID and OR counties
myCounts <- counties[counties$STATEFP=="41"|counties$STATEFP=="16",]
myCounts <- st_as_sf(myCounts)

# Education quality data
SDdata <- read.csv("./College readiness index.csv", header = T)
SDdata$GEOID <- as.character(SDdata$GEOID) # makes type same for geo_join

# Join SD data with census data
SD_merge <- geo_join(mySD, SDdata,"GEOID","GEOID")

# convert to usable numeric rather than character
SD_merge$INDEX <- as.numeric(as.character(SD_merge$INDEX))



# add this in above your map, change the myCounts variable to whichever var you use to store the county-level geocodes
stateM <- aggregate(myCounts[,"STATEFP"], by = list(ID = myCounts$STATEFP), FUN = unique, dissolve = T)
# Adds the state borders inside your map, put as the last added Polygon
# so it is drawn on top of the other shapes. You can play with the weight
# to change the border thickness and the color to change border color.



########## Mapping ##########
minRelevantINDEX <- 0 # set domain minimum for median income gradient

# Combine all data for true total income range
# Domain for color palette
myDomain <- c(minRelevantINDEX:5)

# Color Palette (Gradient)
pal <- colorNumeric(
  palette = c("green","red"),
  domain = myDomain,
  na.color = "gray" # set na color
)

# Map
map3<-leaflet() %>%
  # Map provider
  addProviderTiles("CartoDB.Positron") %>%
  
  # County Layer first (bottom layer)
  addPolygons(data = myCounts,
              # Gradient fill based on domain
              color = "#000000",                     # Gray border line(must be hex)
              fillOpacity = 0.3,                     # Transparency (.8 for big map, .3 for zoomed)
              weight = 1,                            # Thickness of borders
              smoothFactor = 0.2) %>%            
  
  # SD Layer second (top layer)
  addPolygons(data = SD_merge,
              fillColor = ~pal(INDEX),      # Gradient fill based on domain
              color = "#000000",                     # Black border line(must be hex)
              fillOpacity = 1,                       # Make tracts opaque to differentiate
              opacity = 1,                           # Make tract borders opaque
              weight = 1,                            # Thickness of borders
              smoothFactor = 0) %>%
  
  
  
  addPolygons(data = stateM,
              color = "#000000",                     # Black border line(must be hex)
              fillOpacity = 0,                       # Make tracts opaque to differentiate
              opacity = 1,                           # Make tract borders opaque
              weight = 3,                            # Thickness of borders
              smoothFactor = 0) %>%
  
  
  
  
  
  # Legend 
  addLegend(pal = pal,                               # Use given color palette
            values = myDomain,                       # Same domain for legend
            opacity = 1,                             # Make legend opaque
            labels= c("Top 20th", "","","","Bottom 20th"),
            position = "bottomright",                # Legend at bottom right
            title = "Quintiles"
            # Legend Title
  )   # Append $ prefix to legend values

# Show the map
map3

