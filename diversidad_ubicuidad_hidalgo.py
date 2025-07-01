import pandas as pd
import plotly.express as px
import plotly.graph_objects as go

def diversidad_municipal(df, anio):

    df = df.copy()
    diversidad_anio = f"diversidad_{anio}"
    especializado_anio = f"especializado_{anio}"

    df['CVE_MUN'] = df['CVE_MUN'].apply(lambda x: f"{x:03d}")


    # Crear la figura de dispersión e incluir 'estado' como texto en cada punto
    fig = px.scatter(
        df,
        x= diversidad_anio,
        y= especializado_anio,
        text='CVE_MUN',
        color="Región",
        symbol="Región",
        custom_data=['Municipio'],
        category_orders={
        "Región": [
            "Región Actopan",
            "Región Apan",
            "Región Huejutla",
            "Región Huichapan",
            "Región Ixmiquilpan",
            "Región Jacala",
            "Región Mineral de la Reforma",
            "Región Pachuca",
            "Región Tizayuca",
            "Región Tula",
            "Región Tulancingo",
            "Región Zacualtipán"
        ]
        },
        color_discrete_map={
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

    )

    # Agregar líneas de referencia en la mediana
    fig.add_vline(x=df[diversidad_anio].mean(), line_color='red', line_dash='dash')
    fig.add_hline(y=df[especializado_anio].mean(), line_color='red', line_dash='dash')

    # Actualizar la posición del texto para que se muestre debajo de cada punto
    fig.update_traces(textposition='bottom center')



    fig.update_layout(
        title={
            "text": "Diversidad y ubicuidad promedio de las clases de actividad economica <br><span style='font-size:14px;'> (Semestre " + anio + ") </span>",
            "x": 0.5,  # Centrar el título
            "xanchor": "center"
        },
        xaxis_title="Diversidad por municipio",
        yaxis_title="Ubicuidad promedio",
        coloraxis_showscale=False,       # Ocultar escala de colores
        #paper_bgcolor='rgba(0,0,0,0)',  # Fondo general transparente
        plot_bgcolor='rgba(0,0,0,0)',    # Fondo del área de trazado transparente
        xaxis=dict(
            showgrid=True,              # Mostrar líneas de la cuadrícula en X
            gridcolor='lightgray',      # Color de las líneas de la cuadrícula
            gridwidth=0.3,              # Grosor de las líneas de la cuadrícula
            zeroline=True,              # Mostrar línea del eje en X=0
            zerolinecolor='lightgray',  # Color de la línea del eje
            zerolinewidth=1             # Grosor de la línea del eje
        ),
        yaxis=dict(
            showgrid=True,              # Mostrar líneas de la cuadrícula en Y
            gridcolor='lightgray',
            gridwidth=0.3,
            zeroline=True,              # Mostrar línea del eje en Y=0
            zerolinecolor='lightgray',
            zerolinewidth=1
        )
    )

    # Personalizar el hover si es necesario
    fig.update_traces(
        hovertemplate=
        "<b>Municipio:</b> %{customdata[0]}<br>" +
        "<b>Ubicuidad Promedio:</b> %{y:.2f}<br>" +
        "<b>Diversidad:</b> %{x:.0f}<br>"
    )
    # Figura
    return(fig)