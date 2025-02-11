library(tidyverse)
library(shiny)
library(httr2)
library(jsonlite)
library(bslib)
library(DT)
library(ggplot2)
library(ggraph)
library(igraph)
library(plotly)

source('constants.R')
source('functions.R')



response <- call_api(WP_USER, WP_KEY, URL)
data <- clean_up_dataframe(response)

# Define UI for application: using bslib library for layout.
ui <- page_navbar(
  
  # App title ----
  title = "Reading Experience Database",
  id = 'nav',
  
  # Provide optional hyperlinks in dropdown menu here.
  nav_menu(
    title = "Links",
    align = "right",
    # nav_item(...)
  ),
  
  # Build persistent sidebar ----
  sidebar = sidebar(
    
    # Inputs: ----
    # Source-Narrative Filter ----
    selectInput(
      "title", "Title Filter:",
      choices = data$title, multiple = TRUE
    ),
    # Technology Filter ----
    selectInput(
      "technology", "Technology Filter:",
      choices = data$technology, multiple = TRUE
    ),
    # Experience Filter ----
    selectInput(
      "experience", "Experience Filter:",
      choices = data$experience, multiple = TRUE
    ),
    # Benefit Filter ----
    selectInput(
      "benefit", "Experience Filter:",
      choices = data$benefit, multiple = TRUE
    ),
  ), 
    
  # Build "main panel" ----
  navset_card_underline(
    
    nav_panel("Table", DT::dataTableOutput('table')),
    
  ), # navset_card_underline() closure ----
    
fluid = TRUE) # navbarPage() closure

# Define server logic.
server <- function(input, output) {
  # Render User Inputs: ----
  reactive_df <- reactive({
    react_data <- default_data
    
    if (length(input$title) > 0) {
      react_data <- react_data %>% filter(title %in% input$title)
    }
    if (length(input$technology) > 0) {
      react_data <- react_data %>% filter(technology %in% input$technology)
    }
    if (length(input$experience) > 0) {
      react_data <- react_data %>% filter(experience %in% input$experience)
    }
    if (length(input$benefit) > 0) {
      react_data <- react_data %>% filter(benefit %in% input$benefit)
    }
    
    react_data
  })
  
  # Table Output ----
  output$table <- DT::renderDataTable({ 
    reactive_df() 
  })

#   # Output Timeline ----
#   output$timeline <- renderPlotly( 
#     plot_ly(reactive_df(), type = "scatter") %>% # , mode = ""
#       add_trace(x = ~pubDate, y = ~benefit) %>%
#       layout(showLegend = F, title = "Timeline with Rangeslider",
#              xaxis = list(rangeslider = list(visible = T)))
#     )
}

# Run the application 
shinyApp(ui = ui, server = server)

# # Create histogram of most common experience in title.
# histogram <- reactive({
#   subset() %>%
#     count(title, experience) %>%
#     ggplot(aes(x = reorder(title, -n), y = n, fill = experience)) +
#     geom_bar(stat = "identity") +
#     coord_flip() +
#     labs(x = "Source Narrative", y = "Count", fill = "Experience") +
#     theme_minimal() +
#     theme(legend.position = "bottom") +
#     ggtitle("Experiences by Source Narrative")
# })
# 
# # Output Histogram ----
# output$hist <- renderPlot( histogram() )

