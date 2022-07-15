library(ggmap)
library(tmaptools)
library(RCurl)
library(jsonlite)
library(tidyverse)
library(leaflet)


register_google(key = "AIzaSyD5dFf1fbDeRK_TRP4gEsHiwVM5uiB1M7k")

geocode("Pratapgarh") %>% leaflet() %>% addTiles() %>% addMarkers()
