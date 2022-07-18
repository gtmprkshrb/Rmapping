library(tidyverse)
library(maps)
library(mapproj)
library(leaflet)
library(dplyr)
library(pop)
library(mapdata)
library(ggplot2)
library(ggmap)



#m <- leaflet() %>%
#  addProviderTiles(providers$OpenStreetMap) %>%
#  setView(lng = 78.9629, lat = 20.5937, zoom = 4)
#m





m <- leaflet() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  setView(lng = 78.9629, lat = 20.5937, zoom = 4) 
m

mydata <- readr::read_csv("/Users/arun/Desktop/cities_r2.csv")

ak <- filter(mydata, state_name == "PUNJAB")
ak <- mutate(ak, address = paste(name_of_city, state_name, "India"))
geocodes <- geocode(ak, source = "google")

