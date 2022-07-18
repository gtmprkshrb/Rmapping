library(ggmap)
library(tmaptools)
library(RCurl)
library(jsonlite)
library(tidyverse)
library(leaflet)


register_google(key = "my_key")

geocode("dharwad") %>% leaflet() %>% addTiles() %>% addMarkers()
