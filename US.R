library(tidyverse)
library(maps)
library(mapproj)
library(leaflet)
library(pop)
library(mapdata)
library(ggplot2)

# mydata <- readr::read_csv("/Users/arun/Downloads/012WayanadRainfallDataHumeCrowdsourced.csv")
# dim(mydata) # shows columns and rows
#head(mydata)

w <- map_data('world')
icj <- map_data('world', region = c('India', 'China', 'Japan'))


ggplot(icj, aes(x = long, y = lat, group = group, fill = region)) + geom_polygon(color = 'black')
+ coord_map('polyconic')
