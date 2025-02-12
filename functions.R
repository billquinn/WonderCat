# App Functions

# Call Wonder Cat API
call_api <- function(URL) {
  req <- request(URL)
  resp <- req |> req_perform() |> resp_body_json(check_type = TRUE, simplifyVector = TRUE)
  return (resp)
}


# Clean Up Dataframe
clean_up_dataframe <- function(data){
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