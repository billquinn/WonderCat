# App Functions

# Call Wonder Cat API
call_api_and_build_dataframe <- function(url) {
  req <- request(url)

  page_info <- req |> req_perform() |> resp_headers("x-wp-totalpages")
  total_pages <- as.numeric(page_info[[1]])

  resps <- req |>
    req_perform_iterative(
      next_req = iterate_with_offset(
        param_name = "page",
        resp_pages = \(resp) resp_body_json(resp)$info$pages
      ),
      max_reqs = total_pages
    )

  data <- vector("list", length = length(resps))

  for (i in 1:length(resps)) {
    data$i <- i
    data[[i]] <- resps[[i]] %>% resp_body_json(check_type = TRUE, simplifyVector = TRUE) %>% 
      select(id, author, date, benefit, experience, technology, acf) %>% 
      unnest(c(benefit, experience, technology, acf), names_sep = '.', keep_empty = TRUE) %>%
      rename('benefit' = contains('benefit.name')) %>% # Renames benefit.name (entry is present) to benefit (no entry).
      select(
        id, author, date, 
        benefit, experience.name, technology.name, 
        acf.feature, starts_with('acf.wikidata-qid')
        ) %>%
    unnest(benefit, keep_empty = TRUE) # Convert list into string (each "list" has only one value)
  }

  dataframe <- do.call(rbind, data)

  colnames(dataframe) <- c(
      'id', 'author', 'date', 'benefit', 'experience', 'technology', 'text', 'QID'
    )
  
  # Remove last row of dataframe.
  dataframe <- head(dataframe, -1)

  dataframe$date <- as.POSIXct(dataframe$date, format = "%Y-%m-%d")
  dataframe$date <- as.Date(dataframe$date, format = '%y%m%d')

  return(dataframe)
}


# WikiData Functions
get_wikidata <- function(dataframe){
  # Keep only properly formatted wiki ids.
  QIDS <- dataframe %>% filter(grepl('Q\\d+', QID))
  # Create 
  QIDS <- lapply('wd:', paste0, QIDS[['QID']])
  QIDS <- paste(unlist(QIDS), collapse = ' ')

  query <- paste0("
    SELECT DISTINCT
          ?item ?itemLabel ?pubDate ?genreLabel
          ?countryOriginLabel ?coordinates

      WHERE {
          VALUES ?item {",QIDS,"}

          ?item wdt:P31 ?instanceof.
          OPTIONAL {?item wdt:P136 ?genre}.
          OPTIONAL {?item wdt:P577 ?pubDate}.
          ?item wdt:P495 ?countryOrigin .
          ?countryOrigin wdt:P625 ?coordinates.
      
          SERVICE wikibase:label { bd:serviceParam wikibase:language 'en,en'. }}")
          
  wiki_resp <- query_wikidata(query)
  names(wiki_resp)[names(wiki_resp) == 'item'] <- 'QID'
  wiki_resp$QID <- sub('.*/entity/(Q\\d+)', '\\1', wiki_resp$QID)

  # Clean up dates.
  wiki_resp$pubDate <- sub('(\\d{4}-\\d{2}-\\d{2}).*', '\\1', wiki_resp$pubDate)

  # Clean up longitude and latitude.
  wiki_resp$lon <- sub('Point\\(([-]?\\d+\\.?\\d+)\\s([-]?\\d+\\.?\\d+)\\)', '\\1', wiki_resp$coordinates)
  wiki_resp$lon <- as.numeric(wiki_resp$lon)
  wiki_resp$lat <- sub('Point\\(([-]?\\d+\\.?\\d+)\\s([-]?\\d+\\.?\\d+)\\)', '\\2', wiki_resp$coordinates)
  wiki_resp$lat <- as.numeric(wiki_resp$lat)

  # Rename itemLabel to title.
  wiki_resp <- rename(wiki_resp, title = "itemLabel")

  return (wiki_resp)
}

# OLD: Dataframe Functions
# api_to_dataframe <- function(data){
#   dataframe <- data %>% 
#     select(id, author, date, benefit, experience, technology, acf) %>% 
#     unnest(c(benefit, experience, technology, acf), names_sep = '.') %>%
#     select(
#       id, author, date, 
#       benefit.name, experience.name, technology.name, 
#       acf.title_of_creative_work, starts_with('acf.wikidata-qid')
#       )
  
#   colnames(dataframe) <- c(
#     'id', 'author', 'date', 'benefit', 'experience', 'technology', 'title', 'QID'
#   )
  
#   return (tibble(dataframe))
# }

# Network Function for Building from Subset
create_subset_network_data <- function(dataframe, columnOne, columnTwo){
  # Create links from columns: source -> technology -> experience.
  links = dataframe %>% select(columnOne, columnTwo)
  colnames(links) = c('from', 'to')

  # Bind links dataframe and remove any rows with NA.
  links = links %>%
    filter(grepl('\\w+', from)) %>%
    filter(grepl('\\w+', to))

  # Create co-occurrence for link weights.
  links = links %>% group_by(from, to) %>% summarize(weight = n())

  # Create nodes from links and rename column name.
  nodeOne = dataframe[columnOne]
  colnames(nodeOne) = 'label'
  nodeOne$category = columnOne

  nodeTwo = dataframe[columnTwo]
  colnames(nodeTwo) = 'label'
  nodeTwo$category = columnTwo

  # Combine all nodes and remove whitespace entries.
  nodes = rbind(nodeOne, nodeTwo) %>% filter(grepl('\\w+', label))

  # Create node size variable
  nodes = nodes %>% group_by(label, category) %>% summarize(size = n())

  # Remove duplicate nodes.
  nodes = nodes[!duplicated(nodes),] %>% data.frame()

  # Create column ID usind nodes index as value.
  nodes$id = rownames(nodes)

  # Replace "labels" in links to nodes "id" value.
  links$from = nodes$id[ match( unlist(links$from), nodes$label)]
  links$to = nodes$id[ match( unlist(links$to), nodes$label)]

  # Set Styling.
  # We'll start by adding new node and edge attributes to our dataframes. 
  vis.nodes <- nodes
  vis.links <- links

  vis.nodes$shape  <- "dot"  
  vis.nodes$shadow <- TRUE # Nodes will drop shadow
  vis.nodes$title  <- vis.nodes$label # Text on click
  vis.nodes$label <- vis.nodes$label # Node label
  vis.nodes$size   <- 40 #vis.nodes$size # Node size
  vis.nodes$borderWidth <- 2 # Node border width

  # c("slategrey", "tomato", "gold")
  colorMap = c(technology = "slategrey", experience = "tomato", title = "gold", author = "#94797E")
  vis.nodes$color.background <- colorMap[vis.nodes$category]


  vis.nodes$color.border <- "black"
  vis.nodes$color.highlight.background <- "orange"
  vis.nodes$color.highlight.border <- "darkred"

  #vis.links$width <- links$weight # line width
  vis.links$color <- "gray"    # line color  
  vis.links$arrows <- "to" # arrows: 'from', 'to', or 'middle'
  vis.links$smooth <- TRUE    # should the edges be curved?
  vis.links$shadow <- FALSE    # edge shadow

  return(list(nodes = vis.nodes, links = vis.links))
}


# Network Function for Building from Entire Dataframe
create_full_network_data <- function(dataframe){
  # Create links from columns: source -> technology -> experience.
  title_tech = dataframe %>% select(title, technology)
  colnames(title_tech) = c('from', 'to')

  tech_exp = dataframe %>% select(technology, experience)
  colnames(tech_exp) = c('from', 'to')

  exp_user = dataframe %>% select(experience, author)
  colnames(exp_user) = c("from", "to")

  # Bind links dataframe and remove any rows with NA.
  links = rbind(title_tech, tech_exp, exp_user)  %>% # exp_user
    filter(grepl('\\w+', from)) %>%
    filter(grepl('\\w+', to))

  # Create co-occurrence for link weights.
  links = links %>% group_by(from, to) %>% summarize(weight = n())

  # Create nodes from links and rename column name.
  titles = dataframe['title']
  colnames(titles) = 'label'
  titles$category = 'source'

  technologies = dataframe['technology']
  colnames(technologies) = 'label'
  technologies$category = 'technology'

  experiences = dataframe['experience']
  colnames(experiences) = 'label'
  experiences$category = 'experience'

  users = dataframe["author"]
  colnames(users) = "label"
  users$category = "author"

  # Combine all nodes and remove whitespace entries.
  nodes = rbind(titles, technologies, experiences, users) %>% filter(grepl('\\w+', label)) # users

  # Create node size variable
  nodes = nodes %>% group_by(label, category) %>% summarize(size = n())

  # Remove duplicate nodes.
  nodes = nodes[!duplicated(nodes),] %>% data.frame()

  # Create column ID usind nodes index as value.
  nodes$id = rownames(nodes)

  # Replace "labels" in links to nodes "id" value.
  links$from = nodes$id[ match( unlist(links$from), nodes$label)]
  links$to = nodes$id[ match( unlist(links$to), nodes$label)]

  # Remove variables.
  rm(title_tech, tech_exp, titles, experiences, technologies, users) # users

  # Set Styling.
  # We'll start by adding new node and edge attributes to our dataframes. 
  vis.nodes <- nodes
  vis.links <- links

  vis.nodes$shape  <- "dot"  
  vis.nodes$shadow <- TRUE # Nodes will drop shadow
  vis.nodes$title  <- vis.nodes$label # Text on click
  vis.nodes$label <- vis.nodes$label # Node label
  vis.nodes$size   <- 40 #vis.nodes$size # Node size
  vis.nodes$borderWidth <- 2 # Node border width

  # c("slategrey", "tomato", "gold")
  colorMap = c(tecnique = "slategrey", experience = "tomato", source = "gold", author = "#94797E")
  vis.nodes$color.background <- colorMap[vis.nodes$category]


  vis.nodes$color.border <- "black"
  vis.nodes$color.highlight.background <- "orange"
  vis.nodes$color.highlight.border <- "darkred"

  #vis.links$width <- links$weight # line width
  vis.links$color <- "gray"    # line color  
  vis.links$arrows <- "to" # arrows: 'from', 'to', or 'middle'
  vis.links$smooth <- TRUE    # should the edges be curved?
  vis.links$shadow <- FALSE    # edge shadow

  return(list(nodes = vis.nodes, links = vis.links))
}