import requests, re, warnings
from datetime import datetime
import pandas as pd

warnings.filterwarnings('ignore')

print('Starting API calls and builing network at', datetime.today().strftime('%Y-%m-%d %H:%M:%S'))

# Declare root and easily switch from server to local directories.
root = '/root/shiny-server/apps/sample-apps/00_wondercat/'
# root = ""

# Fetch WonderCat Data through API
api_prefix = 'https://env-1120817.us.reclaim.cloud/wp-json/wp/v2/user-experience'

"""
WonderCat Functions.
"""
# Get all pages from API.
def get_total_pagecount():
    api_url = f'{api_prefix}?page=1&per_page=100'
    response = requests.get(api_url)
    pages_count = response.headers['X-WP-TotalPages']
    return int(pages_count)

# Get API response.
def read_wordpress_post_with_pagination():
    total_pages = get_total_pagecount()
    current_page = 1
    all_page_items_json = []
    while current_page <= total_pages:
        api_url = f"{api_prefix}?page={current_page}&per_page=100"
        page_items = requests.get(api_url)
        page_items_json = page_items.json()
        all_page_items_json.extend(page_items_json)
        current_page = current_page + 1
    return all_page_items_json

# Transforms API response to dataframe.
def transform_to_dataframe(api_call):
    api_data = pd.DataFrame(api_call)
    api_data = api_data[['id', 'author', 'date', 'benefit', 'experience', 'technology', 'acf']]
    # This should be cleaner...
    api_data['bene_del'] = pd.json_normalize(api_data['benefit'])
    api_data['benefit'] = pd.json_normalize(api_data['bene_del'])['name']
    api_data['exp_del'] = pd.json_normalize(api_data['experience'])
    api_data['experience'] = pd.json_normalize(api_data['exp_del'])['name']
    api_data['tech_del'] = pd.json_normalize(api_data['technology'])
    api_data['technology'] = pd.json_normalize(api_data['tech_del'])['name']
    api_data['text'] = pd.json_normalize(api_data['acf'])['feature']
    api_data['QID'] = pd.json_normalize(api_data['acf'])['wikidata-qid']
    del api_data['acf'], api_data['bene_del'], api_data['exp_del'], api_data['tech_del']

    # Convert date of experience to Y-m-d
    api_data['date'] = api_data['date'].str.replace(r'(\d{4}-\d{2}-\d{2}).*', '\\1', regex = True)
    api_data['date'] = pd.to_datetime(api_data['date'])

    return api_data

wp_call = read_wordpress_post_with_pagination()
wonderCat = transform_to_dataframe(wp_call)

"""
WikiData Functions.
"""
# wonderCat = pd.read_csv('/root/shiny-server/apps/sample-apps/00_wondercat/wonderCat.csv')

# Gather all QID's from dataframe.
def get_QIDS(df):
    # Gather QIDS and validate with regular expression.
    QIDS = df['QID'].unique()
    regex = re.compile(r'Q\d+')
    QIDS = [s for s in QIDS if regex.match(s)]

    # Append 'wd:' prefix for sparql query.
    QIDS = ' '.join(['wd:' + x for x in QIDS if isinstance(x, str)])

    return QIDS

# Build SPARQL query.
def build_query_call_api(QIDS):
    QIDS = QIDS

    # Build SPARQL Query.
    sparql_query = """
    SELECT DISTINCT
        ?item ?itemLabel
        (group_concat(DISTINCT(?dateLabel); separator=',') as ?pubDates)
        (group_concat(DISTINCT(?genreLabel); separator=',') as ?genres)
        (group_concat(DISTINCT(?countryOriginLabel); separator=',') as ?origin)
        (group_concat(DISTINCT(?coordinatesLabel); separator=',') as ?coordinates)

        WHERE {
            VALUES ?item { %s }
            ?item wdt:P31 ?instanceof.
            OPTIONAL{?item wdt:P577 ?pubDate}.
            OPTIONAL{?item wdt:P136 ?genre}.
            ?item wdt:P495 ?origin.
            ?origin wdt:P625 ?coordinates.

            SERVICE wikibase:label {
            bd:serviceParam wikibase:language 'en,en'.
            ?item rdfs:label ?itemLabel.
            ?pubDate rdfs:label ?dateLabel.
            ?genre rdfs:label ?genreLabel.
            ?origin rdfs:label ?countryOriginLabel.
            ?coordinates rdfs:label ?coordinatesLabel.
            }
        }
        GROUP BY ?item ?itemLabel
    """ % (QIDS)

    # Call API
    url = 'https://query.wikidata.org/bigdata/namespace/wdq/sparql'
    res = requests.get(url, params={'query': sparql_query, 'format': 'json'}).json()

    return res

# Create dataframe from API results.
def api_to_dataframe(res):
    wiki_df =[]

    # Loop through WikiQuery Results.
    for i in res['results']['bindings']:
        # Build empty dictionary.
        wiki_item = {}
        # Loop through each item's keys.
        for k in i.keys():
            # Append values to wiki_item
            wiki_item[k] = i[k]['value']

        # Once item's keys looped, append new dictionary to list for dataframe.
        wiki_df.append(wiki_item)

    wiki_df = pd.DataFrame(wiki_df)

    # Clean up item/QID field.
    wiki_df['item'] = wiki_df['item'].str.replace(r'.*/(Q\d+)', r'\1', regex = True)
    wiki_df = wiki_df.rename(columns = {'item':'QID'})

    # Clean up date field. Currently returning only year due to some dates being "out of bounds" (too old).
    wiki_df['pubDates'] = wiki_df['pubDates'].str.replace(r'(\d{4}-\d{2}-\d{2}).*', r'\1', regex = True)
    wiki_df['pubDates'] = pd.to_datetime(wiki_df['pubDates'], errors = 'coerce')

    # Create Longitude and Latitude columns.
    reg_pattern = r'Point\(([-]?\d+\.?\d+)\s([-]?\d+\.?\d+)\)'
    wiki_df['lon'] = wiki_df['coordinates'].str.replace(reg_pattern, r'\1', regex = True)
    wiki_df['lat'] = wiki_df['coordinates'].str.replace(reg_pattern, r'\2', regex = True)

    return wiki_df

# Call WikiData API and create dataframe.
qids = get_QIDS(wonderCat.drop_duplicates()) # Remove rows that have any null values and drop duplicates. This might be something to talk through.
api_results = build_query_call_api(qids)
wikiData = api_to_dataframe(api_results)

# Rename wikiData itemLabel to title.
wikiData.rename(columns={'itemLabel':'title'}, inplace=True)

# Use WikiData title in place of WonderCat title.
wonderCat = pd.merge(wonderCat, wikiData[['title', 'QID']], on = "QID")


"""
Save dataframes as .csv
"""
wonderCat.to_csv(root + "wonderCat.csv", index = False)
print ('WonderCat fetched.')
wikiData.to_csv(root + "wikiData.csv", sep = ",", index = False)
print('WikiData fetched.')

"""
Network functions.
"""
# def create_nodes_and_links(dataframe):
#     # Create link/edge pairs.
#     title_tech = dataframe[['title', 'technology']]
#     title_tech.rename(columns = {'title': 'from', 'technology': 'to'}, inplace = True)

#     tech_exp = dataframe[['technology', 'experience']]
#     tech_exp.rename(columns = {'technology': 'from', 'experience': 'to'}, inplace = True)

#     exp_user = dataframe[['experience', 'author']]
#     exp_user.rename(columns = {'experience': 'from', 'author': 'to'}, inplace = True)

#     # Join pairs.
#     links = pd.concat([title_tech, tech_exp, exp_user]) 

#     # Clean pairs of whitespace.
#     links['from'] = links['from'].str.replace(r'\\w', '')
#     links['to'] = links['to'].str.replace(r'\\w', '')

#     # Create link/edge weights.
#     links = links.groupby(['from', 'to']).size().to_frame(name = 'weight').reset_index()

#     # Create nodes from links and rename column name.
#     titles = dataframe[['title']]
#     titles.rename(columns = {'title': 'label'}, inplace = True)
#     titles['category'] = 'title'

#     technologies = dataframe[['technology']]
#     technologies.rename(columns = {'technology': 'label'}, inplace = True)
#     technologies['category'] = 'technology'

#     experiences = dataframe[['experience']]
#     experiences.rename(columns = {'experience': 'label'}, inplace = True)
#     experiences['category'] = 'experience'

#     users = dataframe[["author"]]
#     users.rename(columns = {'author': 'label'}, inplace = True)
#     users['category'] = 'user'

#     # Concatenate nodes.
#     nodes = pd.concat([titles, technologies, experiences, users]) # users

#     # Create node "size" from frequency.
#     nodes = nodes.groupby(['label', 'category']).size().to_frame(name = 'size').reset_index()

#     # Remove duplicates from nodes.
#     nodes.drop_duplicates(inplace = True)

#     # Create node "id's."
#     nodes['id'] = nodes.index

#     # Replace link's 'labels' with node id's.
#     label_id_map = pd.Series(nodes['id'].values, index = nodes['label']).to_dict()
#     links = links.replace({'from': label_id_map})
#     links = links.replace({'to': label_id_map})

#     return (links, nodes)

# # Create links and nodes.
# links, nodes = create_nodes_and_links(wonderCat)

# # Save data.
# links.to_csv(root + "links.csv", index = False)
# nodes.to_csv(root + "nodes.csv", index = False)
# print ('Network components built.')