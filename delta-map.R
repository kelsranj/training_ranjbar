library(shiny)
library(contentid)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(shinythemes)
library(sf)
library(leaflet)
library(snakecase)

# read in the data from EDI
sha1 <- 'hash://sha1/317d7f840e598f5f3be732ab0e04f00a8051c6d0'
delta.file <- contentid::resolve(sha1, registries=c("dataone"), store = TRUE)

# fix the sample date format, and filter for species of interest
delta_data <- read.csv(delta.file) %>% 
  mutate(SampleDate = mdy(SampleDate))  %>% 
  filter(grepl("Salmon|Striped Bass|Smelt|Sturgeon", CommonName)) %>% 
  rename(DissolvedOxygen = DO,
         Ph = pH,
         SpecificConductivity = SpCnd)

cols <- names(delta_data)

sites <- delta_data %>% 
  distinct(StationCode, Latitude, Longitude) %>% 
  drop_na() %>% 
  st_as_sf(coords = c('Longitude','Latitude'), crs = 4269,  remove = FALSE)

ui <- fluidPage(
  navbarPage(theme = shinytheme("flatly"), 
             collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Sacramento River Floodplain Data</a>'), 
             id="nav",
             windowTitle = "Sacramento River floodplain fish and water quality data",
             
             tabPanel("Data Sources",
                      verticalLayout(
                        # Application title and data  source
                        titlePanel("Sacramento River floodplain fish and water quality data"),
                        tags$hr(),
                        p("Map of sampling locations"),
                        mainPanel(leafletOutput("map"))
                      )
             ),
             
             tabPanel(
               "Explore",
               verticalLayout(
                 p("Analysis will go here...")
               )
             )
  )
)


server <- function(input, output) {
  # server will go here
  output$map <- renderLeaflet({
    leaflet(sites) %>%
      addTiles() %>%
      addCircleMarkers(data = sites,
                       lat = ~Latitude,
                       lng = ~Longitude,
                       radius = 10,
                       fillColor = "gray",
                       fillOpacity = 1,
                       weight = 0.25,
                       color = "black",
                       label = ~StationCode)
  }
    
  )
}

shinyApp(ui = ui, server = server)

