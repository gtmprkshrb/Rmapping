library(RPostgres)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(shiny)
library(fontawesome)
library(leaflet.extras)
library(mapview)
library(mapboxapi)
library(ggmap)
library(dotenv)
library(shinycssloaders)
library(shinyWidgets)
library(shinythemes)
library(markdown)
library(readr)
library(DBI)


load_dot_env()

key <- Sys.getenv("GPS_TOKEN")
register_google(key = key)

my_token <- Sys.getenv("MAPBOX_TOKEN")

mapboxapi::mb_access_token(my_token, install = TRUE, overwrite = TRUE)

# Database Name
db <- Sys.getenv("DATABASE")
# Database Host
host_db <- Sys.getenv("DB_HOST")
# Database Port No.
db_port <- Sys.getenv("DB_PORT")
# Database Username
db_user <- Sys.getenv("DB_USER")
# Database Password
db_password <- Sys.getenv("DB_PASSWORD")

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = db,
  host = host_db,
  port = db_port,
  user = db_user,
  password = db_password
)

check_for_update <- function() {
  dbGetQuery(con, 'SELECT MAX(creation) FROM "tabLocations"')
}

location_query <- 'WITH events AS (SELECT title, type, category, status, description, location FROM "tabEvents" WHERE location IS NOT NULL and title IS NOT NULL),
locations AS (SELECT name, CAST(latitude AS FLOAT8) AS lat, CAST(longitude AS FLOAT8) AS long, address, city, state, district FROM "tabLocations")
SELECT * FROM events LEFT JOIN locations ON location = name'

frappe_data <- function() {
  dbGetQuery(con, location_query)
}

# Reading all the data for Assembly level boundaries from Frappe DB
assembly_boundaries <- dbGetQuery(con, 'SELECT json FROM "boundaries" where id = 3')
json_data <- assembly_boundaries$json


# List of distinct category Names
category <- frappe_data() %>%
  dplyr::select(category) %>%
  distinct()

# This we are using in the UI and we are using bootstrap logic here
# along with some CSS
ui_front <- bootstrapPage(
  tags$head(
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1, maximum-scale=1"
    )
  ),
  theme = shinytheme("simplex"),
  div(
    class = "container-fluid",
    leafletOutput("layer_data", width = "100%", height = 600)
  ),
  absolutePanel(
    id = "controls", class = "panel panel-default",
    draggable = TRUE, top = 225, left = "7%", # 125
    right = "auto", bottom = "auto",
    width = 0, height = 0,
    dropdownButton(
      label = "",
      icon = icon("gear"),
      status = "primary",
      circle = TRUE,
      width = 250,
      size = "sm",
      selectInput(
        "category", "Category Name:",
        # Appending ALL to have a option to load all locations
        append("All", as.list(category$category), ),
        # selecting ALL as default option
        selected = "All",
        multiple = TRUE
      ),
      hr(),
      checkboxInput("heat", "Heatmap", FALSE),
      checkboxInput("cluster", "Clustering", TRUE),
      checkboxInput("district", "District Boundaries", FALSE)
    )
  )
)


logos <- awesomeIconList(
  "Pothole" = makeAwesomeIcon(
    icon = "road",
    markerColor = "black"
  ),
  "Garbage Collection" = makeAwesomeIcon(
    icon = "trash",
    markerColor = "green"
  ),
  "Air Quality" = makeAwesomeIcon(
    icon = "cloud",
    markerColor = "blue"
  )
)


geosearch1 <- basicPage(
  HTML(paste0(" <script>
                function initAutocomplete() {

                var autocomplete = new google.maps.places.Autocomplete(document.getElementById('address'),{types: ['geocode']});
                autocomplete.setFields(['address_components', 'formatted_address',  'geometry', 'icon', 'name']);
                autocomplete.addListener('place_changed', function() {
                var place = autocomplete.getPlace();
                if (!place.geometry) {
                return;
                }

                var addressPretty = place.formatted_address;
                var address = '';
                if (place.address_components) {
                address = [
                (place.address_components[0] && place.address_components[0].short_name || ''),
                (place.address_components[1] && place.address_components[1].short_name || ''),
                (place.address_components[2] && place.address_components[2].short_name || ''),
                (place.address_components[3] && place.address_components[3].short_name || ''),
                (place.address_components[4] && place.address_components[4].short_name || ''),
                (place.address_components[5] && place.address_components[5].short_name || ''),
                (place.address_components[6] && place.address_components[6].short_name || ''),
                (place.address_components[7] && place.address_components[7].short_name || '')
                ].join(' ');
                }
                var address_number =''
                address_number = [(place.address_components[0] && place.address_components[0].short_name || '')]
                var coords = place.geometry.location;
                //console.log(address);
                Shiny.onInputChange('jsValue', address);
                Shiny.onInputChange('jsValueAddressNumber', address_number);
                Shiny.onInputChange('jsValuePretty', addressPretty);
                Shiny.onInputChange('jsValueCoords', coords);});}
                </script>
                <script src='https://maps.googleapis.com/maps/api/js?key=", key, "&libraries=places&callback=initAutocomplete' async defer></script>"))
)


ui <- dashboardPage(
  skin = c("green"),
  dashboardHeader(title = "GeoLocation"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tabItem(
      tabName = "Layer",
      ui_front,
      geosearch1
    )
  )
)


server <- function(input, output, session) {
  bqdata <- reactivePoll(10000, session,
    checkFunc = check_for_update,
    valueFunc = frappe_data
  )

  observe({
    proxy <- leafletProxy("layer_data")

    if (input$district) {
      proxy %>% leaflet.extras::addGeoJSONChoropleth(json_data,
        valueProperty = "AREASQMI",
        scale = c("white", "red"),
        mode = "q",
        steps = 4,
        padding = c(0.2, 0),
        labelProperty = "name",
        popupProperty = propstoHTMLTable(
          props = c("name", "description", "altitudeMode", "extrude"),
          table.attrs = list(class = "table table-striped table-bordered"),
          drop.na = TRUE
        ),
        color = "#43a858", weight = 1, fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 2, color = "#9c4e57",
          fillOpacity = 1, opacity = 1,
          bringToFront = TRUE, sendToBack = TRUE
        ),
        pathOptions = pathOptions(
          showMeasurements = TRUE,
          measurementOptions =
            measurePathOptions(imperial = TRUE)
        )
      )
    } else {
      proxy %>% clearGeoJSON()
    }
  })
  # Here we are observing the cluster input
  # If we click on the cluster it tries to cluster all the data points
  # Otherwise it will remove the marker
  observe({
    filtered_data <- bqdata() %>%
      dplyr::filter(
        if ("All" %in% input$category) {
          category != ""
        } else {
          category %in% input$category
        }
      )

    proxy <- leafletProxy("layer_data")
    if (input$cluster) {
      proxy %>% addAwesomeMarkers(
        lat = filtered_data$lat,
        lng = filtered_data$long,
        popup = paste0(
          "<b>Title: </b>", filtered_data$title, "<br>",
          "<b>Type: </b>", filtered_data$type, "<br>",
          "<b>Category: </b>", filtered_data$category, "<br>",
          "<b>Status: </b>", filtered_data$status, "<br>",
          "<b>Description: </b>", filtered_data$description, "<br>",
          "<b>Address: </b>", filtered_data$address, "<br>",
          "<b>City Name: </b>", filtered_data$city, "<br>",
          "<b>State Name: </b>", filtered_data$state, "<br>"
        ),
        clusterOptions = markerClusterOptions()
      )
    } else {
      proxy %>% clearMarkerClusters()
    }
  })

  # Here we are observing the heatmap input
  # If we click on the Heatmap it shows the density of the data points
  # Otherwise it will remove the Heatmap
  observe({
    filtered_data <- bqdata() %>%
      dplyr::filter(
        if ("All" %in% input$category) {
          category != ""
        } else {
          category %in% input$category
        }
      )
    proxy <- leafletProxy("layer_data")
    if (input$heat) {
      proxy %>% addHeatmap(
        lng = filtered_data$long,
        lat = filtered_data$lat,
        intensity = 20,
        max = 100,
        radius = 20,
        blur = 20
      )
    } else {
      proxy %>% clearHeatmap()
    }
  })

  # This we need to auto connect the server.
  session$allowReconnect(TRUE)

  # This is the main map where we render leaflet map
  output$layer_data <- renderLeaflet({
    filtered_data <- bqdata() %>%
      dplyr::filter(
        if ("All" %in% input$category) {
          category != ""
        } else {
          category %in% input$category
        }
      )

    leaflet(filtered_data, options = leafletOptions(zoomControl = FALSE)) %>%
      # Here we have added the support for mapbox and we are using there tiles to render on the map
      addMapboxTiles(
        username = "mapbox",
        style_id = "streets-v11",
        group = "mapbox"
      ) %>%
      addResetMapButton() %>%
      setView(78.9629, 20.5937, zoom = 5) %>%
      # Support for full control
      addFullscreenControl(
        pseudoFullscreen = TRUE,
        position = "bottomright"
      ) %>%
      # This function will keep the zoom in zoom out on the bottom right
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)
    }") %>%
      # This feature will be to search location with the help of Google API
      leaflet.extras::addSearchGoogle(searchOptions(autoCollapse = FALSE, minLength = 8)) %>%
      # This is to add assembly boundaries and to be able to popup the information

      # This is to add control layers on the map
      leaflet::addLayersControl(
        position = "bottomleft",
        baseGroups = c("Light"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
}

shinyApp(ui, server)
