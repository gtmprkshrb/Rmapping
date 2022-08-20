
library(shinydashboard)
library(leaflet)
library(dplyr)
library(shiny)
library(bigrquery)
library(fontawesome)
library(leaflet.extras)
library(mapview)
library(mapboxapi)
library(ggmap)
library(dotenv)
library(rjson)
library(shinycssloaders)
library(shinyWidgets)
library(shinythemes)
library(markdown)


load_dot_env()

json_data <- fromJSON(file="state.json")

district_data <- fromJSON(file="district.json")

ward_data <- fromJSON(file = "ward.json")

key <- Sys.getenv("GPS_TOKEN")
register_google(key = key)

my_token <- Sys.getenv("MAPBOX_TOKEN")

mapboxapi::mb_access_token(my_token, install = TRUE, overwrite = TRUE)

select_boundaries = 
data.frame(boundary = 
c("state_boundaries", "district_boundaries", "ward_boundaries"))


bq_auth(path = "bigquery.json")
sql <- "SELECT *  FROM `tides-saas-309509.917302307943.cleanscale` limit 500"
ds <- bq_dataset("tides-saas-309509", "cleanscale")
tb <- bq_dataset_query(ds,
                       query = sql,
                       billing = "tides-saas-309509"
)
bqdata <- bq_table_download(tb)
State <- bqdata %>%
  dplyr::select(State) %>%
  distinct()

# List of distinct District Names
District <- bqdata %>%
  dplyr::select(District) %>%
  distinct()

# List of distinct Category Names
Category <- bqdata %>%
  dplyr::select(Category) %>%
  distinct()


ui_front <- bootstrapPage(
  tags$head(
    tags$meta(name = "viewport", 
    content="width=device-width, initial-scale=1, maximum-scale=1")
  ),
  theme = shinytheme("simplex"),
  div(class = "container-fluid", 
  leafletOutput("layer_data", width = "100%", height = 600)),
  absolutePanel(
    id = "controls", class = "panel panel-default",
    draggable = TRUE, top = 225, left = "7%", #125 
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
        "Category", "Category Name:",
        # Appending ALL to have a option to load all locations
        append("All", as.list(Category$Category), ),
        # selecting ALL as default option
        selected = "All",
        multiple = TRUE
      ),
      selectInput(
        inputId = "india_boundary", 
        label = "India Boundary",
        choices = select_boundaries
      )
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
                <script src='https://maps.googleapis.com/maps/api/js?key=", key,"&libraries=places&callback=initAutocomplete' async defer></script>"))
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
  
  # This we need to auto connect the server. 
  session$allowReconnect(TRUE)

  # This function observe the input ID - india_boundary and load the data from 
  # JSON file. 
  observeEvent(input$india_boundary == "state_boundaries", {
    leafletProxy("layer_data") %>%
      addGeoJSON(json_data, 
      fillColor = "red", 
      fillOpacity = 0.1, 
      weight = 3, 
      group = "state_boundaries") %>%  hideGroup(group = "state_boundaries")
  })
  
  # This is the main map where we render leaflet map 
  output$layer_data <- renderLeaflet({
    filtered_data <- bqdata %>%
      dplyr::filter(
        if ("All" %in% input$Category) {
          Category != ""
        } else {
          Category %in% input$Category
        }
      )
    
    leaflet(filtered_data, options = leafletOptions(zoomControl = FALSE)) %>%  
      addMapboxTiles(username = "mapbox",
       style_id = "streets-v11", 
       group = "mapbox") %>%
       setView(78.9629, 20.5937, zoom = 5) %>% 
      addFullscreenControl(pseudoFullscreen = TRUE, 
      position = "bottomright") %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)
    }") %>%
      addAwesomeMarkers(group = "Clustering", lat = ~Latitude, lng = ~Longitude,
                        icon = ~logos[Category],
                        popup = paste0(
                          "<p> <b>Heading: </b>", filtered_data$Heading, "</p>",
                          "<img src = ", filtered_data$Image,
                          ' width="100%"  height="100"', ">",
                          "<b>Description: </b>",filtered_data$Description,"<br>",
                          "<b>State Name: </b>",filtered_data$State,"<br>",
                          "<b>District Name: </b>",filtered_data$District,"<br>",
                          "<b>Village Name: </b>",filtered_data$VillageName, "<br>"
                        ),
                        clusterOptions = markerClusterOptions()) %>% 
                        hideGroup(group = "Clustering") %>%
      
      # THis function we use for the representation of the heatmap
      addHeatmap(lng = ~Longitude,
                 lat = ~Latitude,
                 intensity = 20,
                 max = 100,
                 radius = 20,
                 blur = 20, group = "HeatMap") %>% 
                 addSearchGoogle(searchOptions(autoCollapse = FALSE, minLength = 8)) %>% 
      
      addLayersControl(
        position = "bottomleft",
        baseGroups = c("light"),
        overlayGroups = 
        c("Clustering", "HeatMap", "state_boundaries", "district_boundaries", "ward_boundaries"),
        options = layersControlOptions(collapsed=TRUE)
        
      )
  })
  
  observeEvent(input$india_boundary == "district_boundaries", {
    
    leafletProxy("layer_data") %>%
      addGeoJSON(district_data, 
      fillColor = "orange", 
      fillOpacity = 0.1, 
      weight = 3, 
      color = "green", 
      group = "district_boundaries") %>% hideGroup(group = "district_boundaries")
  })
  
  observeEvent(input$india_boundary == "ward_boundaries", {
    
    leafletProxy("layer_data") %>%
      addGeoJSONChoropleth(ward_data, 
      valueProperty = "yellow", 
      group ="ward_boundaries") %>% hideGroup(group = "ward_boundaries")
  })
  
}

shinyApp(ui, server)