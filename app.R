
library(maps)
library(sf)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(shiny)
library(bigrquery)
library(fontawesome)
library(leaflet.extras)
library(ggmap)
library(raster)
library(mapview)
library(mapboxapi)
library(dotenv)
library(hover)
library(rjson)

load_dot_env()

json_data <- fromJSON(file="state.json")

district_data <- fromJSON(file="district.json")

ward_data <- fromJSON(file = "ward.json")

key <- Sys.getenv("GPS_TOKEN")
set_key(key = key)
register_google(key = key)

my_token <- Sys.getenv("MAPBOX_TOKEN")

mapboxapi::mb_access_token(my_token, install = TRUE, overwrite = TRUE)

data_cities = data.frame(
  city = c('Tehri' ,'Hosur', 'Hubli', 'Bangalore', 'Chennai', 'Delhi', 'Mumbai'),
  lat = c(30.3787141, 12.7378317, 15.3575775, 12.9539974, 13.0473748, 28.6921165, 19.0821978),
  lng = c(78.4286888, 77.7626863, 75.039107, 77.6309395, 79.9288083, 76.8079346, 72.741098)
)

select_boundaries = data.frame(boundary = c("state_boundaries", "district_boundaries", "ward_boundaries"))


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
  leafletOutput("layer_data", height = 700, width = "100%"),
  absolutePanel(
    style = "background: transparent; right: 0; ",
    top = 125, draggable = TRUE, width = "50%",
    checkboxInput("smooth", label = icon("list-alt", style = "color:gray;", "fa-2x")),
    conditionalPanel(
      condition = "input.smooth == true",
      selectInput(
        width = "50%",
        "District", "Select the District Name:",
        # Appending ALL to have a option to load all locations
        append("All", as.list(District$District), ),
        # selecting ALL as default option
        selected = "All",
        multiple = TRUE
      ),
      selectInput(
        width = "50%",
        "State", "Select the State Name:",
        # Appending ALL to have a option to load all locations
        append("All", as.list(State$State), ),
        # selecting ALL as default option
        selected = "All",
        multiple = TRUE
      ),
      selectInput(
        width = "50%",
        "Category", "Select the Category Name:",
        # Appending ALL to have a option to load all locations
        append("All", as.list(Category$Category), ),
        # selecting ALL as default option
        selected = "All",
        multiple = TRUE
      ),
      selectInput(
        width = "50%",
        inputId = "select_city", 
        label = "Select city",
        choices = data_cities$city
      ),
      selectInput(
        width = "50%",
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




server <- function(input, output) {
  #Zoom when select city
  observeEvent(input$select_city, {
    selected_city_data <- data_cities %>%
      filter(city == input$select_city)
    
    leafletProxy("layer_data") %>%
      setView(lng = selected_city_data$lng, lat = selected_city_data$lat, zoom=8) 
  })
  
  observeEvent(input$india_boundary, {
    select_boundary <- select_boundaries %>%
      filter(boundary == input$india_boundary)
    
    leafletProxy("layer_data") %>%
      setView(78.9629, 20.5937, zoom = 5) %>%   
      addGeoJSON(json_data, fillColor = "red", fillOpacity = 0.1, weight = 3, group = "state_boundaries") %>% hideGroup(group = "state_boundaries")
  })
  
  observeEvent(input$india_boundary, {
    select_boundary <- select_boundaries %>%
      filter("district_boundaries" == input$india_boundary)
    
    leafletProxy("layer_data") %>%
      setView(78.9629, 20.5937, zoom = 5) %>%   
      addGeoJSON(district_data, fillColor = "orange", fillOpacity = 0.1, weight = 3, color = "green", group = "district_boundaries") %>% hideGroup(group = "district_boundaries")
  })
  
  observeEvent(input$india_boundary, {
    select_boundary <- select_boundaries %>%
      filter("ward_boundaries" == input$india_boundary)
    
    leafletProxy("layer_data") %>%
      setView(78.9629, 20.5937, zoom = 5) %>%   
      addGeoJSONChoropleth(ward_data, valueProperty = "yellow", group ="ward_boundaries" ) %>% hideGroup(group = "ward_boundaries")
  })
  
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
      setView(78.9629, 20.5937, zoom = 5) %>% 
      addFullscreenControl(pseudoFullscreen = TRUE) %>%
      
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
      
      addAwesomeMarkers(
        group = "Markers",
        lat = ~Latitude, lng = ~Longitude,
        icon = ~logos[Category],
        popupOptions = (maxWidth = 200),
        popup = paste0(
          "<p> <b>Heading: </b>", filtered_data$Heading, "</p>",
          "<img src = ", filtered_data$Image,
          ' width="100%"  height="100"', ">",
          "<b>Description: </b>",filtered_data$Description,"<br>",
          "<b>State Name: </b>",filtered_data$State,"<br>",
          "<b>District Name: </b>",filtered_data$District,"<br>",
          "<b>Village Name: </b>",filtered_data$VillageName, "<br>"
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
        overlayGroups = c("Clustering", "HeatMap", "state_boundaries", "Markers", "district_boundaries", "ward_boundaries"),
        options = layersControlOptions(collapsed=TRUE)
        
      )
    
  })
}

shinyApp(ui, server)