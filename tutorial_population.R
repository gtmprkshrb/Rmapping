
library(maps)
library(leaflet)
library(dplyr)
library(shiny)


data(world.cities)
world.cities
head(world.cities)

ui <- fillPage(
  titlePanel("Glific Mapping"),
  tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
  leafletOutput("mymap", width = "100%", height = "50%"),
  fluidRow(column(
    2,
    sliderInput("slider1", "Select the Population",
      min = 0, max = 1000000, value = 100000
    ),
    radioButtons("radio", h3("Select the country"),
      choices = list(
        "Poland" = "Poland", "Norway" = "Norway",
        "Belgium" = "Belgium"
      ), selected = "Poland"
    )
  ))
)

server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    leaflet(world.cities %>%
      dplyr::filter(
        country.etc == input$radio,
        pop > input$slider1
      )) %>%
      addTiles() %>%
      addMarkers(lat = ~lat, lng = ~long)
  })
}


shinyApp(ui, server)
