library(shiny)
library(googleway)
library(ggmap)
library(shinydashboard)
library(leaflet)

key <- "AIzaSyD5dFf1fbDeRK_TRP4gEsHiwVM5uiB1M7k"

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