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

key <- "my_key"
set_key(key = key)
register_google(key = key)

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

ui <- dashboardPage(
  dashboardHeader(title = "GeoLocation"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Maps",
        tabName = "maps",
        icon = icon("globe"),
        menuSubItem("Filter", tabName = "filter", icon = icon("map")),
        menuSubItem("Google Search", tabName = "search2", icon = icon("map"))
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
        bootstrapPage(
          absolutePanel(
            style = "background: #dddddd; padding: 10px",
            leafletOutput("mymap", height = 500, width = "210%"),
            draggable = TRUE,
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
      ),
      tabItem(
        tabName = "geoSearch",
        geosearch1
      )
    )
  )
)



server <- function(input, output) {
  
  my_address <- reactive({
    if(!is.null(input$jsValueAddressNumber)){
      if(length(grep(pattern = input$jsValueAddressNumber, x = input$jsValuePretty ))==0){
        final_address<- c(input$jsValueAddressNumber, input$jsValuePretty)
      } else{
        final_address<- input$jsValuePretty
      }
      final_address
    }
  })
  
  output$tabContentUI <- renderLeaflet({
    geocode(input$address) %>% leaflet() %>% addTiles() %>% addMarkers()
  })
  
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
      setView(78.9629, 20.5937, zoom = 4) %>%
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
        popup = paste0(
          "<p> <b>District Name: </b>",
          filtered_data$KGISDistrictName,
          "</p>",
          "<p> <b>Village Name: </b>",
          filtered_data$KGISVillageName,
          "</p>",
          "<p> <b>Anganawadi Name: </b>",
          filtered_data$AnganawadiName,
          "</p>",
          "<p> <b>Village Code: </b>",
          filtered_data$KGISVillageCode,
          "</p>",
          "<p> <b>Type: </b>", filtered_data$Type, "</p>",
          "<p> <b>Pincode: </b>", filtered_data$Pincode, "</p>",
          "<p> <b>Longitude: </b>", filtered_data$Longitude, "</p>",
          "<p> <b>Latitude: </b>", filtered_data$Latitude, "</p>",
          "<p> <b>Anganawadi Worker Name: </b>",
          filtered_data$AWWorkerName,
          "</p>",
          "<p> <b>Anganawadi Worker Phone: </b>",
          filtered_data$AWWorkerPhone,
          "</p>"
        ),
        clusterOptions = markerClusterOptions()
      )
  })
  
}

shinyApp(ui, server)