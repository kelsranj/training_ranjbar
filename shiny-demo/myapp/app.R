
# load packages -----
library(shiny)
library(palmerpenguins)
library(tidyverse)
library(DT)

# user interface
ui <- fluidPage(

    # app title
    tags$h1("My App Title"),
    
    # app subtitle -----
    p(strong("Exploring Antarctic Penguin Data")),
    
    sliderInput(inputId = "body_mass_input",
                label = "Select a range of body masses (g)", 
                value = c(3000, 4000),
                min = 2700,
                max = 6300
                ), # EO slideInput

    plotOutput(outputId = "bodyMass_scatterPlot"),
   
    
#    selectInput(inputId = "island_input",
#                label = "Choose an island",
#                choices = c("Torgersen", "Biscoe", "Dream")),

    
    checkboxGroupInput(inputId = "year_input",
                       label = "Select Years",
                       choices = c("2007", "2008", "2009"),
                       selected = c("2007", "2008")), # EO table input


    DT::dataTableOutput(outputId = "penguin_data")


) # EO fluidPage



# server instructions -----

server <- function(input, output) {
  
  # filter body masses -----
  
  body_mass_df <- reactive({
    penguins |>
      filter(body_mass_g %in% input$body_mass_input[1]:input$body_mass_input[2])
  }) # EO filter body masses
  
  # render the scatter plot -----
  output$bodyMass_scatterPlot <- renderPlot({
    
    # code to generate plot
    ggplot(na.omit(body_mass_df()), 
           aes(x = flipper_length_mm, y = bill_length_mm, 
               color = species, shape = species)) +
      geom_point() +
      scale_color_manual(values = c("Adelie" = "#FEA346", "Chinstrap" = "#B251F1", "Gentoo" = "#4BA4A4")) +
      scale_shape_manual(values = c("Adelie" = 19, "Chinstrap" = 17, "Gentoo" = 15)) +
      labs(x = "Flipper length (mm)", y = "Bill length (mm)", 
           color = "Penguin species", shape = "Penguin species") +
      theme_minimal() +
      theme(legend.position = c(0.85, 0.2),
            legend.background = element_rect(color = "white"))
    
  }) # EO renderPlot
 
  # filter for years ----
  years_df <- reactive({
    penguins |>
      filter(year %in% input$year_input)
  })
  
  
  # render the table ----
  output$penguin_data <- DT::renderDataTable({
    DT::datatable(years_df(),
                  options = list(pagelength = 10),
                  rownames = FALSE)
  }) # EO render DataTable
    
} # EO server

# Run the application 
shinyApp(ui = ui, server = server)
