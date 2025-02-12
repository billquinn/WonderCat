# App Functions

# Call Wonder Cat API
call_api <- function(URL) {
  req <- request(URL)
  resp <- req |> req_perform() |> resp_body_json(check_type = TRUE, simplifyVector = TRUE)
  return (resp)
}


# Dataframe Functions
api_to_dataframe <- function(data){
  dataframe <- data %>% 
    select(id, author, date, benefit, experience, technology, acf) %>% 
    unnest(c(benefit, experience, technology, acf), names_sep = '.') %>%
    select(
      id, author, date, 
      benefit.name, experience.name, technology.name, 
      acf.title_of_creative_work, starts_with('acf.wikidata-qid')
      )
  
  colnames(dataframe) <- c(
    'id', 'author', 'date', 'benefit', 'experience', 'technology', 'title', 'QID'
  )
  
  return (tibble(dataframe))
}

# Network Functions
create_network_data <- function(dataframe){
  # Create links from columns: source -> technology -> experience.
  title_exp = dataframe %>% select(title, technology)
  colnames(title_exp) = c('from', 'to')

  exp_tech = dataframe %>% select(technology, experience)
  colnames(exp_tech) = c('from', 'to')

  # Bind links dataframe and remove any rows with NA.
  links = rbind(title_exp, exp_tech)  %>%
    filter(grepl('\\w+', from)) %>%
    filter(grepl('\\w+', to))

  # Create co-occurrence for link weights.
  links = links %>% group_by(from, to) %>% summarize(weight = n())

  # Create nodes from links and rename column name.
  sources = dataframe['title']
  colnames(sources) = 'label'
  sources$category = 'source'

  technologys = dataframe['technology']
  colnames(technologys) = 'label'
  technologys$category = 'technology'

  experiences = dataframe['experience']
  colnames(experiences) = 'label'
  experiences$category = 'experience'

  # Combine all nodes and remove whitespace entries.
  nodes = rbind(sources, technologys, experiences) %>% filter(grepl('\\w+', label))

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
  rm(title_exp, exp_tech, sources, experiences, technologys)

  # We'll start by adding new node and edge attributes to our dataframes. 
  vis.nodes <- nodes
  vis.links <- links

  vis.nodes$shape  <- "dot"  
  vis.nodes$shadow <- TRUE # Nodes will drop shadow
  vis.nodes$title  <- vis.nodes$label # Text on click
  vis.nodes$label <- vis.nodes$label # Node label
  vis.nodes$size   <- vis.nodes$size * 10 # Node size
  vis.nodes$borderWidth <- 2 # Node border width

  # c("slategrey", "tomato", "gold")
  colorMap = c(tecnique = "slategrey", experience = "tomato", source = "gold")
  vis.nodes$color.background <- colorMap[vis.nodes$category]


  vis.nodes$color.border <- "black"
  vis.nodes$color.highlight.background <- "orange"
  vis.nodes$color.highlight.border <- "darkred"

  vis.links$width <- links$weight # line width
  vis.links$color <- "gray"    # line color  
  vis.links$arrows <- "to" # arrows: 'from', 'to', or 'middle'
  vis.links$smooth <- TRUE    # should the edges be curved?
  vis.links$shadow <- FALSE    # edge shadow
  
  # visnet <- visNetwork(vis.nodes, vis.links)

  return(list(nodes = vis.nodes, links = vis.links))
}