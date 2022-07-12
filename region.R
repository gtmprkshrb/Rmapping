library(tidyverse)
library(maps)
library(mapproj)
library(leaflet)
library(dplyr)
library(pop)
library(mapdata)
library(ggplot2)
library(ggmap)
library(shiny)


leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  setView(lng = 78.9629, lat = 20.5937, zoom = 4) %>%
  setMaxBounds(lng1 = -140, lat1 = -70, lng2 = 155, lat2 = 70 ) %>%
  addMarkers(data = city_info,
             lng = ~Longitude, 
             lat = ~Latitude, 
             popup = paste("<b>",city_info$City,", ", city_info$Country,"</b>","<br>",
                           "<b>ContactName: </b>",city_info$contact_name,"<br>",
                           "<b>Updated: </b>",city_info$inserted_at,"<br>",
                           "<b>RainStatus: </b>",city_info$rain_issue_status,"<br>",
                           "<b>Pincode: </b>",city_info$pincode,"<br>",
                           "<b>Landmark: </b>",city_info$landmark,"<br>"
             ) )



city_info <- read_csv("/Users/arun/Desktop/rain.csv")

