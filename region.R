library(shiny)
library(googleway)
library(ggmap)
library(shinydashboard)
library(leaflet)

key <- "mykey"

ui<- shinydashboard::dashboardPage(
  dashboardHeader(title = "GeoLocation Search"),
  sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                    label = "Search...")
)


server <- function(input, output) {
  output$mymap <- renderLeaflet({
    geo_coding <- geocode(my_address())
    leaflet(geo_coding) %>%
      addTiles() %>%
      addeMarkers()
  })
}

shinyApp(ui, server)