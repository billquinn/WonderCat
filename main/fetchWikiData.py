import requests, re
from datetime import datetime
import pandas as pd

# Read-in WonderCat data.
wonderCat = pd.read_csv('/root/shiny-server/apps/sample-apps/00_wondercat/wonderCat.csv')

# Remove rows that have any null values and drop duplicates. This might be something to talk through.
wonderCat = wonderCat.dropna(how='any',axis=0).drop_duplicates()

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

# Get QIDS.
qids = get_QIDS(wonderCat)

# Call Wikidata API.
api_results = build_query_call_api(qids)

# Convert API data to dataframe.
wikidata = api_to_dataframe(api_results)

# Save dataframe as .csv
wikidata.to_csv("/root/shiny-server/apps/sample-apps/00_wondercat/wikiData.csv", sep = ",", index = False)
print('WikiData fetched: ', datetime.today().strftime('%Y-%m-%d %H:%M:%S'))