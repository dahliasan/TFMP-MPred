
# Load required packages
library(shiny)
library(tidyverse)
library(janitor)
library(gganimate)
library(transformr)
library(av)
library(mapdata)

source('tfmp_functions.R')

# Define modified animate_tracks 
animate_tracks_shiny <- function(df) {
  animate_tracks(df)
  anim_save("animation.gif")
  return(list(src = 'animation.gif'))
}


# Define Shiny app
ui <- fluidPage(
  titlePanel("Track Plotter"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose data file", accept = c(".csv", ".tsv")),
      actionButton("plot", "Generate Plot")
      ),
    mainPanel(
      plotOutput("plot_output"),
      imageOutput("animate_output"),
    )
  )
)

server <- function(input, output) {
  
  # Reactive data frame
  data <- reactive({
    req(input$file)
    d <- read_csv(input$file$datapath)
    
    # Clean data
    d <- janitor::clean_names(d)
    d <- d %>% rename(id = platform_id_no, lat = latitude, lon = longitude, date = loc_date)
    d$id <- as.factor(d$id)
    
    d
  })
  
  # Plot output
  output$plot_output <- renderPlot({
    req(input$plot)
    plot_argos_tracks(data())
  })
  
  # Animation output
  output$animate_output <- renderImage({
    req(input$plot)
    animate_tracks_shiny(data())
  }, deleteFile = TRUE)
 
}

shinyApp(ui, server)
