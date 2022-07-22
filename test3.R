library(maps)
library(sf)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(shiny)
library(bigrquery)
library(googleway)
library(fontawesome)
library(leaflet.extras)
library(magrittr)
library(ggmap)
library(raster)
library(mapview)

key <- "my_key"
set_key(key = key)
register_google(key = key)

con <- dbConnect(
  bigrquery::bigquery(),
  project = "tides-saas-309509",
  dataset = "917302307943",
  billing = "tides-saas-309509"
)
sql <- "SELECT *  FROM `tides-saas-309509.917302307943.cleanscale` limit 100"
ds <- bq_dataset("tides-saas-309509", "cleanscale")
tb <- bq_dataset_query(ds,
                       query = sql,
                       billing = "tides-saas-309509"
)
bqdata <- bq_table_download(tb)
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
  ,leafletOutput('tabContentUI', height = 700, width = "100%")
)

one <- bootstrapPage(
  absolutePanel(
    style = "background: #dddddd; padding: 10px",
    leafletOutput("mymap", height = 400, width = "210%"),
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

two <- bootstrapPage(
  absolutePanel(
    style = "background: #dddddd; padding: 10px",
    leafletOutput("layer_data", height = 400, width = "210%"),
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

ui <- dashboardPage(
  dashboardHeader(title = "GeoLocation"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Maps",
        tabName = "maps",
        icon = icon("globe"),
        menuSubItem("Filter", tabName = "filter", icon = icon("map")),
        menuSubItem("layer", tabName = "Layer", icon = icon("map"))
      ),
      menuItem(
        tabName = "geoSearch",
        sidebarSearchForm(textId = "address", buttonId = "searchButton",
                          label = "Search...")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "filter",
        one
      ),
      tabItem(
        tabName = "geoSearch",
        geosearch1
      ),
      tabItem(
        tabName = "Layer",
        two
      )
    )
  )
)



server <- function(input, output) {
  
  output$tabContentUI <- renderLeaflet({
    geocode(input$address) %>% leaflet() %>% setView(78.9629, 20.5937, zoom = 5) %>% addTiles() %>% addMarkers()
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
    
    IND@data <- filtered_data
    
    leaflet(IND) %>%  addTiles(group = "OpenStreetMap") %>% setView(78.9629, 20.5937, zoom = 4) %>%
      
      addProviderTiles(providers$Esri.WorldStreetMap, options = tileOptions(minZoom=0, maxZoom=13), group = "Esri.WorldStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, options=tileOptions(minZoom=0, maxZoom=13), group = "Esri.WorldImagery") %>%
      
      addAwesomeMarkers(group = "Markers", 
                        lat = ~Latitude, lng = ~Longitude,
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
      ) %>%
      
      addHeatmap(lng = ~Longitude,
                 lat = ~Latitude,
                 intensity = 20,
                 max = 100,
                 radius = 20,
                 blur = 20, group = "HeatMap") %>%  addSearchGoogle() %>%
      
      
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Esri.WorldStreetMap", "Esri.WorldImagery"),
        overlayGroups = c("Markers", "HeatMap"),
        options = layersControlOptions(collapsed=TRUE)
      )
    
  })
  
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