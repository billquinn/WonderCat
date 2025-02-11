# App Functions

source('constants.R')

# Call Wonder Cat API
call_api <- function(WP_USER, WP_KEY, URL) {
  req <- request(API_prefix)
  resp <- req |> req_perform() |> resp_body_json(check_type = TRUE, simplifyVector = TRUE)
  return (resp)
}


# Clean Up Dataframe
clean_up_dataframe <- function(data){
  data <- data %>% select(id, author, date, benefit, experience, technology, acf)
  # api_data = api_data[['id', 'author', 'date', 'benefit', 'experience', 'technology', 'acf']] # Select columns to work with. Add 'wikidata' when ready.
  return (data)
}
# def transform_to_dataframe(api_call):
    # api_data = pd.DataFrame(api_call)
    # api_data = api_data[['id', 'author', 'date', 'benefit', 'experience', 'technology', 'acf']] # Select columns to work with. Add 'wikidata' when ready.
#     api_data['title'] = pd.json_normalize(api_data['acf'])['title_of_creative_work']
#     api_data['QID'] = pd.json_normalize(api_data['acf'])['wikidata-qid']
#     # This should be cleaner...
#     api_data['bene_del'] = pd.json_normalize(api_data['benefit'])
#     api_data['benefit'] = pd.json_normalize(api_data['bene_del'])['name']
#     api_data['exp_del'] = pd.json_normalize(api_data['experience'])
#     api_data['experience'] = pd.json_normalize(api_data['exp_del'])['name']
#     api_data['tech_del'] = pd.json_normalize(api_data['technology'])
#     api_data['technology'] = pd.json_normalize(api_data['tech_del'])['name']
#     del api_data['acf'], api_data['bene_del'], api_data['exp_del'], api_data['tech_del']

#     return api_data


# Build Reactive Dataframe Functions
reactive_df <- reactive({
  # if (is.null(input$action) || isolate(input$action == 0)) {
  #   return(default_data)
  # }
  
  react_data <- default_data
  
  if (!is.null(input$source_narrative)) {
    react_data <- react_data %>% filter(source_narrative %in% input$source_narrative)
  }
  if (!is.null(input$technique)) {
    react_data <- react_data %>% filter(technique %in% input$technique)
  }
  if (!is.null(input$experience)) {
    react_data <- react_data %>% filter(experience %in% input$experience)
  }
  if (!is.null(input$dates)) {
    react_data <- react_data %>% filter(date_of_experience >= input$dates[1] & date_of_experience <= input$dates[2])
  }
  
  react_data
})