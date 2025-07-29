library(tidyverse)
library(shiny)
library(httr2)
library(bslib)
library(DT)
library(ggplot2)
library(plotly)
library(treemapify)
library(visNetwork)
library(WikidataQueryServiceR)
library(leaflet)

source("functions.R")

data <-  call_api_and_build_dataframe("https://env-1120817.us.reclaim.cloud/wp-json/wp/v2/user-experience")
wikiResp <- get_wikidata(data)

data <- inner_join(wikiResp %>% select(title, QID), data, by = "QID", multiple = "all") %>%
  unique()

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
      choices = data[order(data$title),]$title, multiple = TRUE
    ),
    # Technology Filter ----
    selectInput(
      "technology", "Technology Filter:",
      choices = data[order(data$technology),]$technology, multiple = TRUE
    ),
    # Experience Filter ----
    selectInput(
      "experience", "Experience Filter:",
      choices = data[order(data$experience),]$experience, multiple = TRUE
    ),
    # Benefit Filter ----
    selectInput(
      "benefit", "Benefit Filter:",
      choices = data[order(data$benefit),]$benefit, multiple = TRUE
    ),
  ), 
    
  # Build "main panel" ----
  navset_card_underline(
    
    nav_panel("Network", 
      p("Network may take a little time to load."), 
      visNetworkOutput("network")),
      # Older version allowed for user to choose two columns for a network graph.
      # selectInput("netSelect1", "Select First Input:", list('Experiences'='experience', 'Benefits'='benefit', 'Technologies'='technology', "Titles"="title", "Authors"="author")), 
      # selectInput("netSelect2", "Select Second Input:", list('Experiences'='experience', 'Benefits'='benefit', 'Technologies'='technology', "Titles"="title", "Authors"="author"), "title"), 
    
    nav_panel("Table", textOutput("text"), DT::dataTableOutput("table")),
    
    nav_panel("Bar Plot", 
      layout_columns(
        selectInput("barSelect", "Select Input:", list('Experiences'='experience', 'Benefits'='benefit', 'Technologies'='technology')), 
        sliderInput("barSlider", "Filter Count by Deciles:", min = 1, max = 10, value = c(8, 10), step = 1)
      ), 
        plotlyOutput("barplot")
    ),

    nav_panel("Tree Map", 
      sliderInput("treeSlider", "Filter Count by Deciles:", min = 1, max = 10, value = c(1, 2), step = 1),
      plotOutput("treemap"),
      # Data table for testing, but might be useful to see a table with the graph.
        # , 
        # DT::dataTableOutput('test')
    ),

    nav_panel("WikiData", DT::dataTableOutput("wikiTable")),

    nav_panel("World Map", leafletOutput("worldMap"))

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
  selection = 'single', rownames = FALSE, options = list(pageLength = -1, info = FALSE, lengthMenu = list(c(15, -1), c("15", "All")))
)

# Text "Alert" when clicking on data table row ----
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
    ggplot(aes(x = count, y = reorder(!!sym(input$barSelect), count), fill = !!sym(input$barSelect))) +
    geom_bar(stat = "identity") +
    xlab('count') + 
    ylab(input$barSelect) +
    theme(legend.position = 'none')
)

# Tree Map Output ---
treeData <- reactive({
    reactive_df() %>% group_by(title, experience) %>% summarize(count = n()) %>%
      mutate(decile = ntile(count, 10)) %>%
      filter(between(decile, input$treeSlider[1], input$treeSlider[2]))
})

output$treemap <- renderPlot(
    ggplot(treeData(), aes(area = count, label = title, fill = experience, subgroup = experience)) + 
        geom_treemap() +
        geom_treemap_subgroup_border() +
        geom_treemap_text(fontface = "italic", colour = "black", place = "topleft", grow = FALSE) + 
        geom_treemap_subgroup_border() +
        geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.3, colour = "black", fontface = "italic", min.size = 0) +
        scale_colour_brewer(palette = "Set1")
)

# output$test <- DT::renderDataTable({barData()})

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

# Wiki-Table Output ----
wikiData <- reactive({
  reactive_df() %>% select(QID) %>% 
  inner_join(wikiResp, by = "QID", multiple = "all") %>% 
  group_by(QID) %>% nest(data = c(genreLabel, pubDate)) %>% 
  distinct() %>% subset(select = -data)
})

output$wikiTable <- DT::renderDataTable({wikiData()}, 
  selection = 'single', rownames = FALSE, options = list(pageLength = -1, info = FALSE, lengthMenu = list(c(15, -1), c("15", "All")))
)

# Leaflet Output ----
output$worldMap <- renderLeaflet({
  leaflet(data = wikiData() %>% distinct(QID, .keep_all = TRUE)) %>%
    addTiles() %>%
    addMarkers(label = ~title, clusterOptions = markerClusterOptions())
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