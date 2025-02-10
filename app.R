# WonderCat Dashboard App.
library(shiny)
library(dplyr)
library(httr)
library(jsonlite)
library(bslib)
library(DT)
library(ggplot2)
library(ggraph)
library(igraph)
library(plotly)

# Set Working Directory -- Delete for Live Version
# setwd('/Users/williamquinn/Documents/DH/GitHub/WonderCat/R/')

# Import functions.
source('functions.R')
source('constants.R')

# Call API data.
data <- call_api(WP_USER, WP_KEY, URL)
data <- clean_up_dataframe(data)

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  # App title ----
  title = "Hello Shiny!",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    # Input: Slider for the number of bins ----
    sliderInput(
      inputId = "bins",
      label = "Number of bins:",
      min = 1,
      max = 50,
      value = 30
    )
  ),
  # Output: Histogram ----
  plotOutput(outputId = "distPlot")
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}

shinyApp(ui = ui, server = server)