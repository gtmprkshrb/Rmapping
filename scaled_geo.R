library(maps)
library(sf)
library(leaflet)
library(dplyr)
library(shiny)
library(bigrquery)
library(fontawesome)
library(leaflet.extras)
library(leaflet.providers)
library(raster)

con <- dbConnect(
    bigrquery::bigquery(),
    project = "tides-saas-309509",
    dataset = "917302307943",
    billing = "tides-saas-309509"
)
sql <- "SELECT *  FROM `tides-saas-309509.917302307943.cleanscale` limit 500"
ds <- bq_dataset("tides-saas-309509", "cleanscale")
tb <- bq_dataset_query(ds,
    query = sql,
    billing = "tides-saas-309509"
)
bqdata <- bq_table_download(tb)
# List of distinct State Names
State <- bqdata %>%
    select(State) %>%
    distinct()

# List of distinct District Names
District <- bqdata %>%
    select(District) %>%
    distinct()

# List of distinct Category Names
Category <- bqdata %>%
    select(Category) %>%
    distinct()

ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%; height:100%} "),
    leafletOutput("mymap", height = "100%"),
    absolutePanel(
        style = "background: #dddddd; padding: 10px",
        top = 10, right = 10, draggable = TRUE,
        selectInput(
            "District", "Select the District Name:",
            # Appending ALL to have a option to load all locations
            append("All", as.list(District$District), ),
            # selecting ALL as default option
            selected = "All",
            multiple = TRUE
        ),
        selectInput(
            "State", "Select the State Name:",
            # Appending ALL to have a option to load all locations
            append("All", as.list(State$State), ),
            # selecting ALL as default option
            selected = "All",
            multiple = TRUE
        ),
        selectInput(
            "Category", "Select the Category Name:",
            # Appending ALL to have a option to load all locations
            append("All", as.list(Category$Category), ),
            # selecting ALL as default option
            selected = "All",
            multiple = TRUE
        )
    )
)

logos <- awesomeIconList(
    "Pothole" = makeAwesomeIcon(
        icon = "road",
        markerColor = "white",
        library = "fa"
    ),
    "Garbage Collection" = makeAwesomeIcon(
        icon = "trash",
        markerColor = "green",
        library = "fa"
    ),
    "Air Quality" = makeAwesomeIcon(
        icon = "cloud",
        markerColor = "blue",
        library = "fa"
    )
)
IND <- getData("GADM", country = "IND", level = 3)

server <- function(input, output, session) {
    output$mymap <- renderLeaflet({
        filtered_data <- bqdata %>%
            dplyr::filter(
                if ("All" %in% input$District) {
                    District != ""
                } else {
                    District %in% input$District
                }
            ) %>%
            dplyr::filter(
                if ("All" %in% input$State) {
                    State != ""
                } else {
                    State %in% input$State
                }
            ) %>%
            dplyr::filter(
                if ("All" %in% input$Category) {
                    Category != ""
                } else {
                    Category %in% input$Category
                }
            )

        IND@data <- filtered_data
        leaflet(IND) %>%
            # Setting default view to India
            setView(78.9629, 20.5937, zoom = 5) %>%
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
            ) %>%
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
                icon = ~ logos[Category],
                popup = paste0(
                    "<p> <b>Heading: </b>", IND@data$Heading, "</p>",
                    "<img src = ", IND@data$Image,
                    ' width="100%"  height="100"', ">",
                    "<p> <b>Category: </b>",
                    IND@data$Category,
                    "</p>",
                    "<p> <b>Description: </b>",
                    IND@data$Description,
                    "</p>",
                    "<p> <b>State Name: </b>",
                    IND@data$State,
                    "</p>",
                    "<p> <b>District Name: </b>",
                    IND@data$District,
                    "</p>",
                    "<p> <b>Address: </b>",
                    IND@data$Address,
                    "</p>",
                    "<p> <b>Pincode: </b>",
                    IND@data$Pincode,
                    "</p>",
                    "<p> <b>Village Name: </b>",
                    IND@data$VillageName,
                    "</p>",
                    "<p> <b>Village ID: </b>", IND@data$VillageID, "</p>",
                    "<p> <b>WardName: </b>", IND@data$WardName, "</p>",
                    "<p> <b>Longitude: </b>", IND@data$Longitude, "</p>",
                    "<p> <b>Latitude: </b>", IND@data$Latitude, "</p>",
                    "<p> <b>Ward Number: </b>",
                    IND@data$WardNumber,
                    "</p>",
                    "<p> <b>Taluk Name: </b>",
                    IND@data$TalukName,
                    "</p>"
                ),
                clusterOptions = markerClusterOptions()
            )
    })
}

shinyApp(ui, server)
library(maps)
library(sf)
library(leaflet)
library(dplyr)
library(shiny)
library(bigrquery)
library(fontawesome)
library(leaflet.extras)
library(leaflet.providers)
library(raster)

con <- dbConnect(
    bigrquery::bigquery(),
    project = "tides-saas-309509",
    dataset = "917302307943",
    billing = "tides-saas-309509"
)
sql <- "SELECT *  FROM `tides-saas-309509.917302307943.cleanscale` limit 500"
ds <- bq_dataset("tides-saas-309509", "cleanscale")
tb <- bq_dataset_query(ds,
    query = sql,
    billing = "tides-saas-309509"
)
bqdata <- bq_table_download(tb)
# List of distinct State Names
State <- bqdata %>%
    select(State) %>%
    distinct()

# List of distinct District Names
District <- bqdata %>%
    select(District) %>%
    distinct()

# List of distinct Category Names
Category <- bqdata %>%
    select(Category) %>%
    distinct()

ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%; height:100%} "),
    leafletOutput("mymap", height = "100%"),
    absolutePanel(
        style = "background: #dddddd; padding: 10px",
        top = 10, right = 10, draggable = TRUE,
        selectInput(
            "District", "Select the District Name:",
            # Appending ALL to have a option to load all locations
            append("All", as.list(District$District), ),
            # selecting ALL as default option
            selected = "All",
            multiple = TRUE
        ),
        selectInput(
            "State", "Select the State Name:",
            # Appending ALL to have a option to load all locations
            append("All", as.list(State$State), ),
            # selecting ALL as default option
            selected = "All",
            multiple = TRUE
        ),
        selectInput(
            "Category", "Select the Category Name:",
            # Appending ALL to have a option to load all locations
            append("All", as.list(Category$Category), ),
            # selecting ALL as default option
            selected = "All",
            multiple = TRUE
        )
    )
)

logos <- awesomeIconList(
    "Pothole" = makeAwesomeIcon(
        icon = "road",
        markerColor = "white",
        library = "fa"
    ),
    "Garbage Collection" = makeAwesomeIcon(
        icon = "trash",
        markerColor = "green",
        library = "fa"
    ),
    "Air Quality" = makeAwesomeIcon(
        icon = "cloud",
        markerColor = "blue",
        library = "fa"
    )
)
IND <- getData("GADM", country = "IND", level = 3)

server <- function(input, output, session) {
    output$mymap <- renderLeaflet({
        filtered_data <- bqdata %>%
            dplyr::filter(
                if ("All" %in% input$District) {
                    District != ""
                } else {
                    District %in% input$District
                }
            ) %>%
            dplyr::filter(
                if ("All" %in% input$State) {
                    State != ""
                } else {
                    State %in% input$State
                }
            ) %>%
            dplyr::filter(
                if ("All" %in% input$Category) {
                    Category != ""
                } else {
                    Category %in% input$Category
                }
            )

        IND@data <- filtered_data
        leaflet(IND) %>%
            # Setting default view to India
            setView(78.9629, 20.5937, zoom = 5) %>%
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
            ) %>%
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
                icon = ~ logos[Category],
                popup = paste0(
                    "<p> <b>Heading: </b>", IND@data$Heading, "</p>",
                    "<img src = ", IND@data$Image,
                    ' width="100%"  height="100"', ">",
                    "<p> <b>Category: </b>",
                    IND@data$Category,
                    "</p>",
                    "<p> <b>Description: </b>",
                    IND@data$Description,
                    "</p>",
                    "<p> <b>State Name: </b>",
                    IND@data$State,
                    "</p>",
                    "<p> <b>District Name: </b>",
                    IND@data$District,
                    "</p>",
                    "<p> <b>Address: </b>",
                    IND@data$Address,
                    "</p>",
                    "<p> <b>Pincode: </b>",
                    IND@data$Pincode,
                    "</p>",
                    "<p> <b>Village Name: </b>",
                    IND@data$VillageName,
                    "</p>",
                    "<p> <b>Village ID: </b>", IND@data$VillageID, "</p>",
                    "<p> <b>WardName: </b>", IND@data$WardName, "</p>",
                    "<p> <b>Longitude: </b>", IND@data$Longitude, "</p>",
                    "<p> <b>Latitude: </b>", IND@data$Latitude, "</p>",
                    "<p> <b>Ward Number: </b>",
                    IND@data$WardNumber,
                    "</p>",
                    "<p> <b>Taluk Name: </b>",
                    IND@data$TalukName,
                    "</p>"
                ),
                clusterOptions = markerClusterOptions()
            )
    })
}

shinyApp(ui, server)
