import plotly.express as px
import plotly.graph_objects as go

def tabla(df):
    # Crear la tabla con desplazamiento
    table = go.Figure(data=[go.Table(
        header=dict(values=["CVE MUN", "Municipio"],
                    fill_color='lightgray',
                    align='left'),
        cells=dict(values=[df["CVE_MUN"], df["Municipio"]],
                fill_color='white',
                align='left')),
    ])

    # Ajustar tamaño y agregar desplazamiento
    table.update_layout(
        title="Diccionario de municipios",
        height=400,  # Ajustar tamaño de la tabla
        width=350
    )

    return(table)
