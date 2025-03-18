library(tidyverse)
library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(plotly)
library(treemapify)
library(visNetwork)

source("functions.R")

data <- read_delim("wonderCat_data.tsv", delim = '\t')
wikiData <- read_delim("wikidata.tsv", delim = '\t')

# Define UI for application: using bslib library for layout.
ui <- page_navbar(
  
  # App title ----
  title = "Shiny WonderCat", id = 'nav',
  
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
      "benefit", "Benefit Filter:",
      choices = data$benefit, multiple = TRUE
    ),
  ), 
    
  # Build "main panel" ----
  navset_card_underline(
    
    nav_panel("Table", DT::dataTableOutput("table")),
    
    nav_panel("Bar Plot", 
        selectInput("barSelect", "Select Input:", list('Experiences'='experience', 'Benefits'='benefit', 'Technologies'='technology')), 
        sliderInput("barSlider", "Filter Count by Quantiles:", min = 0, max = 1, value = c(0, 1), step = 0.25), 
        plotlyOutput("barplot")
    ),

    nav_panel("Tree Map", plotOutput("treemap")),

    nav_panel("Network", 
    p("Network may take a little time to load."), 
    selectInput("netSelect1", "Select First Input:", list('Experiences'='experience', 'Benefits'='benefit', 'Technologies'='technology', "Titles"="title", "Authors"="author")), 
    selectInput("netSelect2", "Select Second Input:", list('Experiences'='experience', 'Benefits'='benefit', 'Technologies'='technology', "Titles"="title", "Authors"="author"), "title"), 
    visNetworkOutput("network"))

), fluid = TRUE
) # navbarPage() closure

# Define server logic.
server <- function(input, output) {
# Render User Inputs: ----
reactive_df <- reactive({
    react_data <- data
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
    return(react_data)
})
  
# Table Output ----
output$table <- DT::renderDataTable({reactive_df()})

# Bar Plot Output ----
barData <- reactive({
    req(input$barSelect)
    reactive_df() %>% dplyr::count(!!sym(input$barSelect)) %>% quantile(all_of(input$barSelect))
    })

output$barplot <- renderPlotly(
    barData() |>
    ggplot(aes(x = n, y = !!sym(input$barSelect), fill = !!sym(input$barSelect))) +
    geom_bar(stat = "identity"))

# Tree Map Output ---
treeData <- reactive({
    reactive_df() %>% group_by(title, experience) %>% summarize(count = n())
})

output$treemap <- renderPlot(
    ggplot(treeData(), aes(area = count, label = title, fill = experience, subgroup = experience)) + 
        geom_treemap() +
        geom_treemap_subgroup_border() +
        geom_treemap_text(fontface = "italic", colour = "black", place = "topleft", grow = FALSE) + 
        geom_treemap_subgroup_border() +
        geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.3, colour = "black", fontface = "italic", min.size = 0)
)

# Network Output ---
network <- reactive({
    net <- create_network_data(reactive_df(), input$netSelect1, input$netSelect2)
    return(net)
})

output$network <- renderVisNetwork({
    visNetwork(network()$nodes, network()$links) %>%
    visOptions(highlightNearest = TRUE, selectedBy = "label")
})

}

# Run the application 
shinyApp(ui = ui, server = server)

#   # Output Timeline ----
#   output$timeline <- renderPlotly( 
#     plot_ly(reactive_df(), type = "scatter") %>% # , mode = ""
#       add_trace(x = ~pubDate, y = ~benefit) %>%
#       layout(showLegend = F, title = "Timeline with Rangeslider",
#              xaxis = list(rangeslider = list(visible = T)))
#     )