# WonderCat App
# https://shiny.posit.co/py/layouts/
# https://shiny.posit.co/py/components/inputs/select-multiple/

# Import necessary libraries.
import re, warnings, sys, os
import pandas as pd
import numpy as np
import seaborn as sns
import plotly.express as px
import networkx as nx
from pathlib import Path
from shiny import App, reactive, render, ui, req
from shinywidgets import output_widget, render_widget

from functions import *

# # Declare directory.
# abs_dir = Path(__file__).parent
# data = pd.read_csv(abs_dir / 'Data/reading-experiences.csv', sep = ',')

# Call Data from WordPress API
wp_call = read_wordpress_post_with_pagination()

# Reshape wp_call (json) as dataframe.
data = transform_to_dataframe(wp_call)

# Add page title and sidebar.
app_ui = ui.page_fluid(

    # Title ----
    ui.panel_title("WonderCat App", "Window title"),

    # Page Layout (sidebar) ----
    ui.layout_sidebar(

        # Sidebar Options ----
        ui.sidebar(
            # 'Sidebar', bg="#f8f8f8",
            ui.input_selectize("title", "Titles", data['title'].unique().tolist(), multiple=True),
            ui.input_selectize("experience", "Experiences", data['experience'].unique().tolist(), multiple=True),
            ui.input_selectize("benefit", "Benefits", data['benefit'].unique().tolist(), multiple=True),
            ui.input_selectize("technology", "Technologies", data['technology'].unique().tolist(), multiple=True)

            # ui.input_slider("date", "Date", data['pubDate'].min(), data['pubDate'.max()])
            # ui.input_date_range("date", "Date Range", start=data['pubDate'].min(), end=data['pubDate'].max())
            ),
        
        # Tabs ----
        ui.navset_card_pill(
            # Dataframe Tab ----
            ui.nav_panel(
                "Data Table",
                ui.output_data_frame('dataframe')
                ),

            # Barchart Tab ----
            ui.nav_panel(
                "Bar Chart",
                ui.input_selectize('bar_select', 'Select an option below.', 
                                   {'experience':'Experiences', 'benefit':'Benefits', 'technology': 'Technologies'}),
                ui.output_plot('bar_chart')
                ),

            # Network Tab ----
            ui.nav_panel(
                "Network Graph",
                output_widget('network')
                ),

            # Treemap Tab ----
            ui.nav_panel(
                "Treemap",
                output_widget('treemap')
                ),

            # Timeline Tab ----
            ui.nav_panel(
                "Timeline",
                output_widget('timeline')
                ),
            
            id = 'tabs'
            )
    ),
)

# Define server.
def server(input, output, session):

    # Create reactive dataframe.
    @reactive.calc
    def filter_data():
        data_selected = data
        
        if input.title():
            data_selected = data_selected[data_selected['title'].isin(input.title())]
        elif input.experience():
            data_selected = data_selected[(data_selected['experience'].isin(input.experience()))]
        elif input.benefit():
            data_selected = data_selected[(data_selected['benefit'].isin(input.benefit()))]
        elif input.technology():
            data_selected = data_selected[(data_selected['technology'].isin(input.technology()))]
        # elif input.date():
        #     data_selected = data_selected[(data_selected['pubDate'].min() <= input.date() <= data_selected['pubDate'].max())]

        return data_selected

    # Render DataFrame ----
    @render.data_frame
    def dataframe():
        return render.DataGrid(filter_data())
    
    # Render Bar Chart
    @render.plot(alt="Bar Chart")  
    def bar_chart():
        bar_data = filter_data().groupby(input.bar_select()).size().to_frame('count').reset_index()
        ax = sns.barplot(bar_data, x = 'count', y=input.bar_select(), hue = input.bar_select())
        ax.set_title("Title")
        ax.set_xlabel('count')
        ax.set_ylabel(input.bar_select())
        return ax

    # Render Network Graph ----
    @render_widget
    def network():
        net_data = filter_data()

        graph = create_network_graph(net_data, 'title', 'experience', 'benefit') # add reader eventually.
        net_fig = create_network_visualization(graph)

        return net_fig

        

    # Render Treemap ----
    @render_widget
    def treemap():
        tree_data = filter_data().groupby(['title', 'experience']).size().to_frame('count').reset_index()

        tree_fig = px.treemap(
            tree_data, path=[px.Constant("All"), 'experience', 'title'], 
            values='count', color='experience'
        ).update_traces(
            marker=dict(cornerradius=5)
        )
        
        return tree_fig

    # Render Timeline ----
    @render_widget
    def timeline():
        timeline_data = filter_data()[pd.notnull(filter_data()['pubDate'])]

        time_fig = px.scatter(
            timeline_data, x = 'pubDate', y = 'benefit', color='benefit'
            ).update_layout(
            xaxis=dict(
                rangeslider=dict(
                    visible=True
                ),
                type="date"
            )
        )

        return time_fig


app = App(app_ui, server)