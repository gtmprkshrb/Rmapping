library(maps)
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
sql <- "SELECT contact_name as name, contact_phone, flow_name, CAST(latitude AS FLOAT64) as lat, CAST(longitude AS FLOAT64) as long, '0' as capital  FROM `tides-saas-309509.917302307943.messages` where longitude is not null"
ds <- bq_dataset("tides-saas-309509", "contacts")
tb <- bq_dataset_query(ds,
  query = sql,
  billing = "tides-saas-309509"
)
bqdata <- bq_table_download(tb)


ui <- fillPage(
    titlePanel("Glific Mapping"),
    tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
    leafletOutput("mymap", width = "100%", height = "100%"),
    radioButtons("radio", h3("Select the Flow name"),
    choices = list(
      "Glific" = "Glific", "testcontact" = "testcontact",
      "locationtrial" = "locationtrial"
    ), selected = "Glific"
  )
)

server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    leaflet(bqdata %>%
      dplyr::filter(
        flow_name == input$radio
      )) %>%
      addTiles() %>%
      addMarkers(lat = ~lat, lng = ~long)
  })
}


shinyApp(ui, server)
