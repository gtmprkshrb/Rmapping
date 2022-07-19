library(tidyverse)
library(readxl)
library(leaflet.providers)
library(raster)
library(maps)
library(sf)
library(leaflet)
library(dplyr)
library(shiny)
library(bigrquery)

con <- dbConnect(
  bigrquery::bigquery(),
  project = "tides-saas-309509",
  dataset = "917302307943",
  billing = "tides-saas-309509"
)
sql <- "SELECT *  FROM `tides-saas-309509.917302307943.mapping`"
ds <- bq_dataset("tides-saas-309509", "mapping")
tb <- bq_dataset_query(ds,
                       query = sql,
                       billing = "tides-saas-309509"
)
bqdata <- bq_table_download(tb)

ui <- fillPage(
  titlePanel("Glific Mapping"),
  tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
  leafletOutput("mymap", width = "100%", height = "100%")
)

logos <- awesomeIconList(
  "Pothole" = makeAwesomeIcon(
    icon = "fire",
    markerColor = "blue",
    library = "fa"
  ),
  "Tree" = makeAwesomeIcon(
    icon = "tree",
    markerColor = "green",
    library = "fa"
  )
)

IND_2 <- getData("GADM", country = "IND", level = 2)
IND_2@data <-bqdata
server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    
    IND_2 %>%
      leaflet() %>%
      addProviderTiles(providers$Esri.WorldShadedRelief) %>%
      addPolygons(
        weight = 1,
        stroke = TRUE,
        color = "transparent",
        fillOpacity = 0.7,
        dashArray = "3",
        highlight = highlightOptions(
          weight = 2,
          dashArray = "",
          color = "red",
          bringToFront = TRUE
        )
      )%>%
      addAwesomeMarkers(
        lat = ~lat, lng = ~long,
        icon = ~ logos[flow_name],
        popup = paste0(
          "<h4>", IND_2@data$flow_name, "</h4>",
          "<img src = ", IND_2@data$image,
          ' width="100%"  height="100"', ">",
          "<p> Shared by ",
          IND_2@data$name, " on ",
          IND_2@data$inserted_at,
          "</p>"
        ),
        clusterOptions = markerClusterOptions()
      )
    
  })
}

shinyApp(ui, server)