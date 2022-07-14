library(maps)
library(sf)
library(leaflet)
library(dplyr)
library(shiny)
library(bigrquery)
library(fontawesome)
library(leaflet.extras)
library(magrittr)

con <- dbConnect(
    bigrquery::bigquery(),
    project = "tides-saas-309509",
    dataset = "917302307943",
    billing = "tides-saas-309509"
)
sql <- "SELECT *  FROM `tides-saas-309509.917302307943.audit`"
ds <- bq_dataset("tides-saas-309509", "audit")
tb <- bq_dataset_query(ds,
    query = sql,
    billing = "tides-saas-309509"
)
bqdata <- bq_table_download(tb)
KGISTalukName <- bqdata %>%
    select(KGISTalukName) %>%
    distinct()
KGISVillageName <- bqdata %>%
    select(KGISVillageName) %>%
    distinct()

ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%; height:100%} "),
    leafletOutput("mymap", height = "100%"),
    absolutePanel(
        style = "background: #dddddd; padding: 10px",
        top = 10, right = 10, draggable = TRUE,
        selectInput(
            "KGISVillageName", "Select the Village Name:",
            append("All", as.list(KGISVillageName$KGISVillageName), ),
            selected = "All",
            multiple = TRUE
        )
    )
)
server <- function(input, output, session) {
    output$mymap <- renderLeaflet({
        filtered_data <- bqdata %>%
            dplyr::filter(
                if ("All" %in% input$KGISVillageName) {
                    KGISVillageName != ""
                } else {
                    KGISVillageName %in% input$KGISVillageName
                }
            )
        leaflet(filtered_data) %>%
            addTiles() %>%
            addHeatmap(lng = ~Longitude, lat = ~Latitude, intensity = 20, max = 100, radius = 20, blur = 10) %>%
            addAwesomeMarkers(
                lat = ~Latitude, lng = ~Longitude,
                clusterOptions = markerClusterOptions()
            )
    })
}

shinyApp(ui, server)
