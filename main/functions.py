
import requests, base64
import pandas as pd
import networkx as nx
import plotly.express as px

import constants

"""
WordPress API Credentials and Functions
"""
api_prefix = 'https://env-1120817.us.reclaim.cloud/wp-json/wp/v2/user-experience'

# Import credentials
WP_USER = constants.WP_USER
WP_KEY = constants.WP_KEY
wp_credentials = WP_USER + WP_KEY
wp_token = base64.b64encode(wp_credentials.encode())
wp_header = {'Authorization': 'Basic ' + wp_token.decode('utf-8')}

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

"""
Transform API JSON to Dataframe
"""
def transform_to_dataframe(api_call):
    api_data = pd.DataFrame(api_call)
    api_data = api_data[['id', 'author', 'date', 'benefit', 'experience', 'technology', 'acf']] # Select columns to work with. Add 'wikidata' when ready.
    api_data['title'] = pd.json_normalize(api_data['acf'])['title_of_creative_work']
    api_data['QID'] = pd.json_normalize(api_data['acf'])['wikidata-qid']
    # This should be cleaner...
    api_data['bene_del'] = pd.json_normalize(api_data['benefit'])
    api_data['benefit'] = pd.json_normalize(api_data['bene_del'])['name']
    api_data['exp_del'] = pd.json_normalize(api_data['experience'])
    api_data['experience'] = pd.json_normalize(api_data['exp_del'])['name']
    api_data['tech_del'] = pd.json_normalize(api_data['technology'])
    api_data['technology'] = pd.json_normalize(api_data['tech_del'])['name']
    del api_data['acf'], api_data['bene_del'], api_data['exp_del'], api_data['tech_del']

    return api_data


"""
Network Functions
"""
# Create network graph from dataframe (df) and three columns (c1, c2, c3),
# where c1 is the source of c2; c2 is the source of c3.
def create_network_graph(df, c1, c2, c3, c4):
    c1_c2 = df[[c1, c2]].rename(columns = {c1:'source', c2:'target'})
    c2_c3 = df[[c2, c3]].rename(columns = {c2:'source', c3:'target'})
    c3_c4 = df[[c3, c4]].rename(columns = {c3:'source', c4:'target'})

    df_graph = pd.concat([c1_c2, c2_c3, c3_c4], ignore_index = True)
    df_graph = df_graph.query('source != "source"') # Ensure a "source" not isn't present.

    # Create weight column based on occurences of source-target pairings.
    df_graph = pd.DataFrame(df_graph.value_counts()).reset_index()
    df_graph = df_graph.rename(columns={'count':'weight'})

    # Initialize graph object.
    G = nx.from_pandas_edgelist(df_graph, 'source', 'target', 'weight')

    # Remove self-loops.
    G.remove_edges_from(nx.selfloop_edges(G))

    # Add nodes.
    nodes = list(dict.fromkeys(df_graph['source'].values.tolist() + df_graph['target'].values.tolist()))
    nodes = pd.DataFrame(nodes, columns = ['source'])
    G.add_nodes_from(nodes)

    # Set degree attributes.
    nx.set_node_attributes(G, dict(G.degree(G.nodes())), 'degree')

    # Set node type attributes.
    node_attrs = df.to_dict('list')
    node_attrs = {k: node_attrs[k] for k in (c1, c2, c3, c4)}
    node_attrs = {v: k for k, values in node_attrs.items() for v in values}
    nx.set_node_attributes(G, node_attrs, 'type')

    # These measures are causing an error (nxPowerIterationFailedConvergence)
    # # Find centrality measures.
    # betweenness_dict = nx.betweenness_centrality(G)
    # eigenvector_dict = nx.eigenvector_centrality(G)
    # degree_cent_dict = nx.degree_centrality(G)

    # # Assign each centrality measure to an attribute.
    # nx.set_node_attributes(G, betweenness_dict, 'betweenness')
    # nx.set_node_attributes(G, eigenvector_dict, 'eigenvector')
    # nx.set_node_attributes(G, degree_cent_dict, 'degree_cent')

    # Declare node and edge positions with seed for reproducibility.
    pos = nx.spring_layout(G, seed=7)
    nx.set_node_attributes(G, pos, 'pos')

    # # Find communities.
    # communities = nx.community.naive_greedy_modularity_communities(G)

    # # Create a dictionary that maps nodes to their community.
    # modularity_dict = {}
    # for i, c in enumerate(communities):
    #     for name in c:
    #         modularity_dict[name] = i
    
    # nx.set_node_attributes(G, modularity_dict, 'group')

    # Double check "source" node is removed.
    G.remove_node('source')

    return (G)

# Create dataframes for nodes and edges.
# Create visualization "traces" for edges.
def create_edge_traces(graph_object):
    # Declare Edge & Node Locations on x, y axis.
    edge_x = []
    edge_y = []
    for edge in graph_object.edges():
        x0, y0 = graph_object.nodes[edge[0]]['pos']
        x1, y1 = graph_object.nodes[edge[1]]['pos']
        edge_x.append(x0)
        edge_x.append(x1)
        edge_x.append(None)
        edge_y.append(y0)
        edge_y.append(y1)
        edge_y.append(None)

    edges_df = pd.DataFrame({
        'x':edge_x,
        'y':edge_y
    })

    return edges_df

# Create visualization "traces" for nodes.
def create_node_traces(graph_object):
    node_x = []
    node_y = []
    for node in graph_object.nodes():
        x, y = graph_object.nodes[node]['pos']
        node_x.append(x)
        node_y.append(y)

    # Declare Node Attributes
    node_text = []
    node_color = []
    for node, attributes in graph_object.nodes(data=True):
        node_text.append(node)
        node_color.append(attributes['type'])

    node_size = []
    for node, adjacencies in enumerate(graph_object.adjacency()):
        node_size.append(len(adjacencies[1]) * 2)

    nodes_df = pd.DataFrame({
        'x':node_x,
        'y':node_y,
        'text':node_text,
        'type':node_color,
        'size':node_size
    })

    return nodes_df

# Create network visualization with Plotly.
def create_network_visualization(graph_object):
    node_traces = create_node_traces(graph_object)
    edge_traces = create_edge_traces(graph_object)

    fig = px.line(
        edge_traces, x = 'x', y = 'y'
    ).update_traces(line_color='#888')

    node_trace = px.scatter(
        node_traces, x="x", y="y", 
        color="type", size='size', opacity = 1,
        hover_data=['text']
    ).data

    fig.add_traces(node_trace)

    return fig