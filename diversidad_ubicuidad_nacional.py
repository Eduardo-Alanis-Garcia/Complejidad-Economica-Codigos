import plotly.express as px

def diversidad_nacional(df, anio):


    df = df.copy()
    diversidad_anio = f"diversidad_{anio}"
    especializado_anio = f"especializado_{anio}"

    
    # Crear la figura de dispersión e incluir 'estado' como texto en cada punto
    fig = px.scatter(
        df,
        x= diversidad_anio,
        y= especializado_anio,
        text='NOM_ABR',
        color="region",
        symbol="region",
        custom_data=['NOM_ENT'],
        category_orders={
            "region": [
                "Noroeste",
                "Noreste",
                "Occidente",
                "Oriente",
                "Centronorte",
                "Centrosur",
                "Sureste",
                "Suroeste"
            ]
        },
        color_discrete_map={
            "Noroeste": "pink",
            "Noreste": "orange",
            "Occidente": "yellow",
            "Oriente": "purple",
            "Centronorte": "blue",
            "Centrosur": "black",
            "Sureste": "green",
            "Suroeste": "red"
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
        xaxis_title="Diversidad por estado",
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
        "<b>Estado:</b> %{customdata[0]}<br>" +
        "<b>Diversidad:</b> %{x:.0f}<br>" +
        "<b>Ubicuidad Promedio:</b> %{y:.2f}<br>"
    )

    fig.update_layout(
        legend_title_text='Región'
    )

    return(fig)
