#install.packages('raster', repos='https://rspatial.r-universe.dev')

library(RPostgres)
library(maps)
library(sf)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(shiny)
library(googleway)
library(fontawesome)
library(leaflet.extras)
library(magrittr)
library(ggmap)
library(raster)
library(mapview)
library(mapboxapi)
library(dotenv)
library(DBI)

load_dot_env()

key <- Sys.getenv("GPS_TOKEN")
set_key(key = key)
register_google(key = key)

my_token <- Sys.getenv("MAPBOX_TOKEN")

mapboxapi::mb_access_token(my_token, install = TRUE, overwrite = TRUE)

db <- '_95263123ff933d46'
host_db <- '127.0.0.1'
db_port <- '5432'
db_user <- 'postgres'  
db_password <- '7kGP6f9qjq4jnGlf'
con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)


bqdata <- dbGetQuery(con, 'SELECT * FROM cleanscale')
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

ui_front <- fluidPage(
  fluidRow(
    fluidRow(
      column(6,
             leafletOutput("layer_data", height = 500, width = "100%")
      ),
      column(4, 
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




server <- function(input, output) {
  output$layer_data <- renderLeaflet({
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
    
    leaflet(filtered_data) %>%  addMapboxTiles(username = "mapbox", style_id = "streets-v11", group = "mapbox") %>%
      addMapboxTiles(username = "mapbox", style_id = "outdoors-v11", group = "outdoors") %>%
      addMapboxTiles(username = "mapbox", style_id = "light-v10", group = "light") %>%
      addMapboxTiles(username = "mapbox", style_id = "dark-v10", group = "dark") %>%
      addMapboxTiles(username = "mapbox", style_id = "satellite-v9", group = "satellite") %>%
      setView(78.9629, 20.5937, zoom = 4) %>%
      
      addAwesomeMarkers(group = "Clustering", lat = ~Latitude, lng = ~Longitude,
                        icon = ~ logos[Category],
 popup = paste0(
          "<p> <b>Heading: </b>", filtered_data$Heading, "</p>",
          "<img src = ", filtered_data$Image,
          ' width="100%"  height="100"', ">",
          "<p> <b>Category: </b>",
          filtered_data$Category,
          "</p>",
          "<p> <b>Description: </b>",
          filtered_data$Description,
          "</p>",
          "<p> <b>State Name: </b>",
          filtered_data$State,
          "</p>",
          "<p> <b>District Name: </b>",
          filtered_data$District,
          "</p>",
          "<p> <b>Address: </b>",
          filtered_data$Address,
          "</p>",
          "<p> <b>Pincode: </b>",
          filtered_data$Pincode,
          "</p>",
          "<p> <b>Village Name: </b>",
          filtered_data$VillageName,
          "</p>",
          "<p> <b>Village ID: </b>", filtered_data$VillageID, "</p>",
          "<p> <b>WardName: </b>", filtered_data$WardName, "</p>",
          "<p> <b>Longitude: </b>", filtered_data$Longitude, "</p>",
          "<p> <b>Latitude: </b>", filtered_data$Latitude, "</p>",
          "<p> <b>Ward Number: </b>",
          filtered_data$WardNumber,
          "</p>",
          "<p> <b>Taluk Name: </b>",
          filtered_data$TalukName,
          "</p>"
        ),
                        clusterOptions = markerClusterOptions()) %>%
      
      addAwesomeMarkers(
        group = "Markers",
        lat = ~Latitude, lng = ~Longitude,
        icon = ~ logos[Category],
        popup = paste0(
          "<p> <b>Heading: </b>", filtered_data$Heading, "</p>",
          "<img src = ", filtered_data$Image,
          ' width="100%"  height="100"', ">",
          "<p> <b>Category: </b>",
          filtered_data$Category,
          "</p>",
          "<p> <b>Description: </b>",
          filtered_data$Description,
          "</p>",
          "<p> <b>State Name: </b>",
          filtered_data$State,
          "</p>",
          "<p> <b>District Name: </b>",
          filtered_data$District,
          "</p>",
          "<p> <b>Address: </b>",
          filtered_data$Address,
          "</p>",
          "<p> <b>Pincode: </b>",
          filtered_data$Pincode,
          "</p>",
          "<p> <b>Village Name: </b>",
          filtered_data$VillageName,
          "</p>",
          "<p> <b>Village ID: </b>", filtered_data$VillageID, "</p>",
          "<p> <b>WardName: </b>", filtered_data$WardName, "</p>",
          "<p> <b>Longitude: </b>", filtered_data$Longitude, "</p>",
          "<p> <b>Latitude: </b>", filtered_data$Latitude, "</p>",
          "<p> <b>Ward Number: </b>",
          filtered_data$WardNumber,
          "</p>",
          "<p> <b>Taluk Name: </b>",
          filtered_data$TalukName,
          "</p>"
        )
      ) %>%
      
      addHeatmap(lng = ~Longitude,
                 lat = ~Latitude,
                 intensity = 20,
                 max = 100,
                 radius = 20,
                 blur = 20, group = "HeatMap") %>%  addSearchGoogle(searchOptions(autoCollapse = TRUE, minLength = 8)) %>%
      
      
      addLayersControl(
        position = "topright",
        baseGroups = c("mapbox", "outdoors", "light", "dark", "satellite"),
        overlayGroups = c("Clustering", "HeatMap", "geo_boundraies", "Markers"),
        options = layersControlOptions(collapsed=TRUE)
      )
    
  })
}

shinyApp(ui, server)