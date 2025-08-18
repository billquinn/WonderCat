library(tidyverse)
library(shiny)
# library(httr2)
library(bslib)
library(DT)
library(ggplot2)
library(plotly)
library(treemapify)
library(visNetwork)
# library(WikidataQueryServiceR)
library(leaflet)
library(shinycssloaders)

source("functions.R")

# # WonderCat and WikiData API Calls
# data <-  call_api_and_build_dataframe("https://env-1120817.us.reclaim.cloud/wp-json/wp/v2/user-experience")
# wikiResp <- get_wikidata(data)

# Read-in stored files.
data <- read_csv('wonderCat.csv', col_names = TRUE)
wikiResp <- read_csv('wikiData.csv', col_names = TRUE)

# Select first item as canon for coordinates, long, and lat.
wikiResp$coordinates <- gsub('(.*),.*', '\\1', wikiResp$coordinates)
wikiResp$lon <- gsub('(.*),.*', '\\1', wikiResp$lon) %>% as.numeric()
wikiResp$lat <- gsub('(.*),.*', '\\1', wikiResp$lat) %>% as.numeric()

# Define UI for application (using bslib library for layout).
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
    # API Updates ----
    hr(),
    HTML("<p>Updated with data from <a href='https://env-1120817.us.reclaim.cloud/user-experience/'>WonderCat</a> and <a href='https://www.wikidata.org/wiki/Special:RecentChanges?hidebots=1&hidecategorization=1&limit=50&days=7&urlversion=2'>Wikidata</a> every ten minutes.</p>"),
  open = "desktop"), 
    
  # Build "main panel" ----
  navset_card_underline(
    
    nav_panel("Network", visNetworkOutput("network")),
    
    nav_panel("Table", textOutput("text"), DT::dataTableOutput("table")),

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

# Network Output ---
network_graph <- reactive({
  net <- create_full_network_data(reactive_df())
  return(net)
}) 

output$network <- renderVisNetwork({
  visNetwork(network_graph()$nodes, network_graph()$links) %>%
  visOptions(highlightNearest = TRUE, selectedBy = "label")
}) %>% bindCache(network_graph())
  
# Table Output ----
output$table <- DT::renderDataTable(
  {reactive_df() %>% select(id, author, date, title, technology, experience, benefit, QID)}, 
  selection = 'single', rownames = FALSE, options = list(pageLength = -1, info = FALSE, lengthMenu = list(c(15, -1), c("15", "All")))
)

# Text "Alert" when clicking on data table row ----
observeEvent(input$table_rows_selected, {
  selected_row <- reactive_df()[input$table_rows_selected,]
  sel_title <- selected_row[[1]]
  sel_author <- selected_row[[4]]
  sel_tech <- selected_row[[8]]
  sel_text <- selected_row[[9]]
  showModal(modalDialog(
    title = sel_title, 
      'Feature illustrating ', sel_tech, 'in ', sel_title, 'according to ', sel_author,
      HTML('<br><br>'), sel_text,
    easyClose = TRUE
  ))
})

# Wiki-Table Output ----
wikiData <- reactive({
  reactive_df() %>% select(QID) %>% 
  inner_join(wikiResp, by = "QID", multiple = "all") %>% 
  distinct()
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

# Old Functions.
# Bar Plot Nav Panel.
    # nav_panel("Bar Plot", 
    #   layout_columns(
    #     selectInput("barSelect", "Select Input:", list('Experiences'='experience', 'Benefits'='benefit', 'Technologies'='technology')), 
    #     sliderInput("barSlider", "Filter Count by Deciles:", min = 1, max = 10, value = c(8, 10), step = 1)
    #   ), 
    #     plotlyOutput("barplot")
    # ),
# Tree Map Nav Panel
    # nav_panel("Tree Map", 
    #   sliderInput("treeSlider", "Filter Count by Deciles:", min = 1, max = 10, value = c(1, 2), step = 1),
    #   plotOutput("treemap"),
    # ),

#   # Output Timeline ----
#   output$timeline <- renderPlotly( 
#     plot_ly(reactive_df(), type = "scatter") %>% # , mode = ""
#       add_trace(x = ~pubDate, y = ~benefit) %>%
#       layout(showLegend = F, title = "Timeline with Rangeslider",
#              xaxis = list(rangeslider = list(visible = T)))
#     )

# # Bar Plot Output ----
# barData <- reactive({
#     req(input$barSelect)
#     reactive_df() %>% dplyr::count(!!sym(input$barSelect), name = 'count') %>%
#       mutate(decile = ntile(count, 10)) %>%
#       filter(between(decile, input$barSlider[1], input$barSlider[2]))
# })

# output$barplot <- renderPlotly(
#     barData() |>
#     ggplot(aes(x = count, y = reorder(!!sym(input$barSelect), count), fill = !!sym(input$barSelect))) +
#     geom_bar(stat = "identity") +
#     xlab('count') + 
#     ylab(input$barSelect) +
#     theme(legend.position = 'none')
# )

# # Tree Map Output ---
# treeData <- reactive({
#     reactive_df() %>% group_by(title, experience) %>% summarize(count = n()) %>%
#       mutate(decile = ntile(count, 10)) %>%
#       filter(between(decile, input$treeSlider[1], input$treeSlider[2]))
# })

# output$treemap <- renderPlot(
#     ggplot(treeData(), aes(area = count, label = title, fill = experience, subgroup = experience)) + 
#         geom_treemap() +
#         geom_treemap_subgroup_border() +
#         geom_treemap_text(fontface = "italic", colour = "black", place = "topleft", grow = FALSE) + 
#         geom_treemap_subgroup_border() +
#         geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.3, colour = "black", fontface = "italic", min.size = 0) +
#         scale_colour_brewer(palette = "Set1")
# )