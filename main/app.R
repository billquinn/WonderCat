library(tidyverse)
library(shiny)
library(httr2)
library(bslib)
library(DT)
library(ggplot2)
library(plotly)
library(treemapify)
library(visNetwork)

source("functions.R")

data <-  call_api_and_build_dataframe("https://env-1120817.us.reclaim.cloud/wp-json/wp/v2/user-experience")
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
    
    nav_panel("Network", 
      p("Network may take a little time to load."), 
      # Older version allowed for user to choose two columns for a network graph.
      # selectInput("netSelect1", "Select First Input:", list('Experiences'='experience', 'Benefits'='benefit', 'Technologies'='technology', "Titles"="title", "Authors"="author")), 
      # selectInput("netSelect2", "Select Second Input:", list('Experiences'='experience', 'Benefits'='benefit', 'Technologies'='technology', "Titles"="title", "Authors"="author"), "title"), 
      visNetworkOutput("network")),
    
    nav_panel("Table", textOutput("text"), DT::dataTableOutput("table")),
    
    nav_panel("Bar Plot", 
        selectInput("barSelect", "Select Input:", list('Experiences'='experience', 'Benefits'='benefit', 'Technologies'='technology')), 
        sliderInput("barSlider", "Filter Count by Deciles:", min = 1, max = 10, value = c(8, 10), step = 1), 
        plotlyOutput("barplot")
        # Data table for testing, but might be useful to see a table with the graph.
        # , 
        # DT::dataTableOutput('test')
    ),

    nav_panel("Tree Map", plotOutput("treemap")),

  ), 
fluid = TRUE) # navbarPage() closure

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
output$table <- DT::renderDataTable(
  {reactive_df() %>% select(id, author, date, title, technology, experience, benefit, QID)}, 
  selection = 'single', rownames = FALSE, options = list(dom = 't')
)

observeEvent(input$table_rows_selected, {
  selected_row <- reactive_df()[input$table_rows_selected,]
  sel_author <- selected_row[[2]]
  sel_title <- selected_row[[7]]
  sel_text <- selected_row[[8]]
  showModal(modalDialog(
    title = sel_title, 'Submitted by ', sel_author, HTML('<br><br>'), sel_text,
    easyClose = TRUE
  ))
})


# Bar Plot Output ----
barData <- reactive({
    req(input$barSelect)
    reactive_df() %>% dplyr::count(!!sym(input$barSelect), name = 'count') %>%
      mutate(decile = ntile(count, 10)) %>%
      filter(between(decile, input$barSlider[1], input$barSlider[2]))
})

output$barplot <- renderPlotly(
    barData() |>
    ggplot(aes(x = count, y = !!sym(input$barSelect), fill = !!sym(input$barSelect))) +
    geom_bar(stat = "identity"))

# output$test <- DT::renderDataTable({barData()})

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
    # net <- create_subset_network_data(reactive_df(), input$netSelect1, input$netSelect2)
    net <- create_full_network_data(reactive_df())
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