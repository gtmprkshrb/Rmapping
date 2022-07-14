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
# List of distinct Taluka Names
KGISTalukName <- bqdata %>%
    select(KGISTalukName) %>%
    distinct()

# List of distinct Village Names
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
            # Appending ALL to have a option to load all locations
            append("All", as.list(KGISVillageName$KGISVillageName), ),
            # selecting ALL as default option
            selected = "All",
            multiple = TRUE
        ),
        selectInput(
            "KGISTalukName", "Select the Taluk Name:",
            # Appending ALL to have a option to load all locations
            append("All", as.list(KGISTalukName$KGISTalukName), ),
            # selecting ALL as default option
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
            ) %>%
            dplyr::filter(
                if ("All" %in% input$KGISTalukName) {
                    KGISTalukName != ""
                } else {
                    KGISTalukName %in% input$KGISTalukName
                }
            )
        leaflet(filtered_data) %>%
            # Setting default view to India
            setView(78.9629, 20.5937, zoom = 5) %>%
            addTiles() %>%
            addHeatmap(
                lng = ~Longitude,
                lat = ~Latitude,
                intensity = 20,
                max = 100,
                radius = 20,
                blur = 10
            ) %>%
            addAwesomeMarkers(
                lat = ~Latitude, lng = ~Longitude,
                clusterOptions = markerClusterOptions()
            )
    })
}

shinyApp(ui, server)
