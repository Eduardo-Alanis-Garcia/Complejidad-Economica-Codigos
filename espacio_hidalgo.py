import pandas as pd
import networkx as nx
import plotly.graph_objects as go
from sklearn.preprocessing import MinMaxScaler
import numpy as np


def espacio_hidalgo_red(datos, anio):

    G = nx.from_pandas_edgelist(
        datos, 
        source='nodo1_nombre', 
        target='nodo2_nombre', 
        edge_attr='peso_arista', 
        create_using=nx.Graph()
    )

    # DataFrame de nodos con sus pesos
    nodos1 = datos[['nodo1_nombre', 'peso_nodo1']].rename(columns={'nodo1_nombre': 'nodo', 'peso_nodo1': 'peso'})
    nodos2 = datos[['nodo2_nombre', 'peso_nodo2']].rename(columns={'nodo2_nombre': 'nodo', 'peso_nodo2': 'peso'})
    nodos = pd.concat([nodos1, nodos2])
    nodos.drop_duplicates(inplace=True)
    nodos.sort_values(by="nodo", ascending=True, inplace=True)

    # DataFrame de etiquetas
    datos['nodo1_abr'] = datos['nodo1'].apply(lambda x: f"{x:03d}")
    datos['nodo2_abr'] = datos['nodo2'].apply(lambda x: f"{x:03d}")
    etiquetas1 = datos[['nodo1_nombre', 'nodo1_abr']].rename(columns={'nodo1_nombre': 'nodo', 'nodo1_abr': 'etiquetas'})
    etiquetas2 = datos[['nodo2_nombre', 'nodo2_abr']].rename(columns={'nodo2_nombre': 'nodo', 'nodo2_abr': 'etiquetas'})
    etiqueta = pd.concat([etiquetas1, etiquetas2])
    etiqueta.drop_duplicates(inplace=True)
    etiqueta.sort_values(by="etiquetas", ascending=True, inplace=True)

    # DataFrame de color
    color1 = datos[['nodo1_nombre', 'nodo1_region']].rename(columns={'nodo1_nombre': 'nodo', 'nodo1_region': 'region'})
    color2 = datos[['nodo2_nombre', 'nodo2_region']].rename(columns={'nodo2_nombre': 'nodo', 'nodo2_region': 'region'})
    color = pd.concat([color1, color2])
    color.drop_duplicates(inplace=True)
    color.sort_values(by="nodo", ascending=True, inplace=True)

    # Asignar atributos a los nodos en el grafo
    peso_dict = nodos.set_index('nodo')['peso'].to_dict()
    nx.set_node_attributes(G, peso_dict, 'peso')

    etiqueta_dict = etiqueta.set_index('nodo')['etiquetas'].to_dict()
    nx.set_node_attributes(G, etiqueta_dict, 'etiquetas')

    color_dict = color.set_index('nodo')['region'].to_dict()
    nx.set_node_attributes(G, color_dict, 'region')

    # Posición de los nodos
    pos = nx.spring_layout(G, seed=1)

    # Crear trazado de aristas
    edge_x, edge_y = [], []
    for edge in G.edges():
        x0, y0 = pos[edge[0]]
        x1, y1 = pos[edge[1]]
        edge_x.extend([x0, x1, None])
        edge_y.extend([y0, y1, None])

    edge_trace = go.Scatter(
        x=edge_x, y=edge_y, 
        line=dict(width=0.5, color='#888'), 
        hoverinfo='none', 
        mode='lines', 
        showlegend=False
    )

    # Extraer coordenadas y propiedades de los nodos
    node_x, node_y = [], []
    pesos_nodos, etiquetas, colores = [], [], []

    for node in G.nodes():
        x, y = pos[node]
        node_x.append(x)
        node_y.append(y)
        pesos_nodos.append(G.nodes[node].get('peso', 0))
        etiquetas.append(G.nodes[node].get('etiquetas', ''))
        colores.append(G.nodes[node].get('region', ''))

    # Escalar los pesos para definir tamaños visibles
    scaler = MinMaxScaler(feature_range=(10, 30))
    node_sizes = scaler.fit_transform(np.array(pesos_nodos).reshape(-1, 1)).flatten()

    # Diccionario para mapear valores a colores
    color_map = {
        "Región Actopan": "blue",
        "Región Apan": "orange",
        "Región Huejutla": "green",
        "Región Huichapan": "red",
        "Región Ixmiquilpan": "purple",
        "Región Jacala": "brown",
        "Región Mineral de la Reforma": "pink",
        "Región Pachuca": "gray",
        "Región Tizayuca": "olive",
        "Región Tula": "cyan",
        "Región Tulancingo": "crimson",
        "Región Zacualtipán": "black"
    }

    # Crear trazas de nodos separadas por categoría
    node_traces = []
    categorias = sorted({valor for valor in colores if valor in color_map})
    nombres = list(G.nodes)

    for categoria in categorias:
        indices = [i for i, valor in enumerate(colores) if valor == categoria]
        custom_data_trace = [[nombres[i], pesos_nodos[i], colores[i]] for i in indices]
        
        trace = go.Scatter(
            x=[node_x[i] for i in indices],
            y=[node_y[i] for i in indices],
            mode='markers+text',  # Incluye los labels y los markers
            customdata=custom_data_trace,
            text=[etiquetas[i] for i in indices],
            textposition='bottom center',
            marker=dict(
                size=[node_sizes[i] for i in indices],
                color=color_map.get(categoria, 'black'),
                line_width=2
            ),
            legendgroup=str(categoria),
            name=str(categoria)
        )
        node_traces.append(trace)

    # Crear la figura
    fig = go.Figure(
        data=[edge_trace] + node_traces,
        layout=go.Layout(
            title={
                "text": "Red de Municipios en Hidalgo <br><span style='font-size:14px;'> (Semestre " + anio + " ) </span>",
                "x": 0.5,
                "xanchor": "center"
            },
            hovermode='closest',
            margin=dict(b=20, l=5, r=5, t=40),
            plot_bgcolor='rgba(0,0,0,0)',
            xaxis=dict(showgrid=False, zeroline=False, showticklabels=False),
            yaxis=dict(showgrid=False, zeroline=False, showticklabels=False)
        )
    )

    # Actualizar todas las trazas (sin selector) para aplicar el hovertemplate
    fig.update_traces(
        hovertemplate=
            "<b>Municipio:</b> %{customdata[0]}<br>" +
            "<b>Número de empleados:</b> %{customdata[1]}<br>" +
            "<extra>%{customdata[2]}</extra>",
        marker=dict(showscale=False)
    )

    fig.update_layout(
        legend_title=dict(text="Región:")
    )

    fig.show()