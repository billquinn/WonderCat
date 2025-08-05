import requests, re
import pandas as pd
import numpy as np

# Read-in WonderCat data.
wonderCat = pd.read_csv('wonderCat.csv')
print (wonderCat.shape)

# Remove rows that have any null values and drop duplicates. This might be something to talk through.
wonderCat = wonderCat.dropna(how='any',axis=0).drop_duplicates()
print (wonderCat.shape)

# Gather all QID's from dataframe.
def get_QIDS(df):
    # Gather QIDS and validate with regular expression.
    QIDS = df['QID'].unique()
    regex = re.compile('Q\d+')
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
    # Old Wiki-Query (one genre per row)"""
    # SELECT DISTINCT
    #       ?item ?itemLabel ?pubDate ?genreLabel
    #       ?countryOriginLabel ?coordinates

    #   WHERE {
    #       VALUES ?item { %s }

    #       ?item wdt:P31 ?instanceof.
    #       OPTIONAL {?item wdt:P136 ?genre}.
    #       OPTIONAL {?item wdt:P577 ?pubDate}.
    #       ?item wdt:P495 ?countryOrigin .
    #       ?countryOrigin wdt:P625 ?coordinates.
      
    #       SERVICE wikibase:label { bd:serviceParam wikibase:language 'en,en'. }}
    # """ % (QIDS)

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
    wiki_df['item'] = wiki_df['item'].str.replace('.*/(Q\d+)', '\\1', regex = True)
    wiki_df = wiki_df.rename(columns = {'item':'QID'})

    # Clean up date field. Currently returning only year due to some dates being "out of bounds" (too old).
    wiki_df['pubDates'] = wiki_df['pubDates'].str.replace('(\d{4}-\d{2}-\d{2}).*', '\\1', regex = True)
    wiki_df['pubDates'] = pd.to_datetime(wiki_df['pubDates'], errors = 'coerce')

    # Create Longitude and Latitude columns.
    reg_pattern = 'Point\(([-]?\d+\.?\d+)\s([-]?\d+\.?\d+)\)'
    wiki_df['long'] = wiki_df['coordinates'].str.replace(reg_pattern, '\\1', regex = True)
    wiki_df['lat'] = wiki_df['coordinates'].str.replace(reg_pattern, '\\2', regex = True)

    # # (Old method for concatenating genres) Concatenate genres.
    # genres = wikidata[['QID', 'genreLabel']]
    # genres['genreLabel'].replace('', np.nan, inplace = True)
    # genres.dropna(subset=['genreLabel'], inplace=True)
    # genres = genres.groupby('QID')['genreLabel'].apply(lambda x: ','.join(x)).reset_index()

    # # Re-merge concatenated rows with rest of data.
    # wiki_df = wikidata.drop(['genreLabel'], axis=1).merge(genres, how = 'inner', on = 'QID')

    return wiki_df

# Get QIDS.
qids = get_QIDS(wonderCat)

# Call Wikidata API.
api_results = build_query_call_api(qids)

# Convert API data to dataframe.
wikidata = api_to_dataframe(api_results)

# Save dataframe as .csv
wikidata.to_csv("wikidata.csv", sep = ",", index = False)