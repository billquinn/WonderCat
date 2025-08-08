import requests
from datetime import datetime
import pandas as pd

# Fetch WonderCat Data through API
api_prefix = 'https://env-1120817.us.reclaim.cloud/wp-json/wp/v2/user-experience'

def get_total_pagecount():
    api_url = f'{api_prefix}?page=1&per_page=100'
    response = requests.get(api_url)
    pages_count = response.headers['X-WP-TotalPages']
    return int(pages_count)

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

# Transform API JSON to Dataframe
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

# Call Data from WordPress API
wp_call = read_wordpress_post_with_pagination()

# Reshape wp_call (json) as dataframe.
data = transform_to_dataframe(wp_call)

# Write to file.
data.to_csv("/root/shiny-server/apps/sample-apps/00_wondercat/wonderCat.csv", index = False)
print('WonderCat fetched: ', datetime.today().strftime('%Y-%m-%d %H:%M:%S'))