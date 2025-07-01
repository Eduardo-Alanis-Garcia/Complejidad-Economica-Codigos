import plotly.express as px
import pandas as pd

def afinidad(df, anio):


    d = df.copy()  # Para evitar modificar el DataFrame original

    afinidad_anio = f"afinidad_{anio}"
    print(afinidad_anio)
    complejidad_producto_anio = f"complejidad_producto_{anio}"
    print(complejidad_producto_anio)

    personal_ocupado_anio = f"personal_ocupado_{anio}"

  
    d = d.dropna(subset=[afinidad_anio, complejidad_producto_anio, personal_ocupado_anio])

    # Colores
    d['Titulo_corto'] = d['Título_dos_digitos'].astype(str).apply(lambda x: x[:25] + '...' if len(x) > 25 else x)
    
    def espacios(cadena):
        opciones = {
            "Agricultura, cría y explotación de animales, aprovechamiento forestal, pesca y caza": "Agricultura, cría <br> y explotación de animales, <br> aprovechamiento forestal, <br> pesca y caza",
            "Generación, transmisión, distribución y comercialización de energía eléctrica, suministro de agua y de gas natural por ductos al consumidor final": "Generación, transmisión, <br> distribución y comercialización de <br> energía eléctrica, suministro de agua <br> y de gas natural por ductos <br> al consumidor final",
            "Transportes, correos y almacenamiento": "Transportes, correos <br> y almacenamiento",
            "Información en medios masivos": "Información en <br> medios masivos",
            "Servicios financieros y de seguros": "Servicios financieros <br> y de seguros",
            "Servicios inmobiliarios y de alquiler de bienes muebles e intangibles   ": "Servicios inmobiliarios y <br> de alquiler de bienes <br> muebles e intangibles   ",
            "Servicios profesionales, científicos y técnicos":  "Servicios profesionales, <br> científicos y técnicos",
            "Dirección y administración de grupos empresariales o corporativos": "Dirección y administración <br> de grupos empresariales <br> o corporativos",
            "Servicios de apoyo a los negocios y manejo de residuos, y servicios de remediación": "Servicios de apoyo a los negocios <br> y manejo de residuos, <br> y servicios de remediación",
            "Servicios de salud y de asistencia social": "Servicios de salud y <br> de asistencia social",
            "Servicios de esparcimiento culturales y deportivos, y otros servicios recreativos": "Servicios de esparcimiento <br> culturales y deportivos, <br> y otros servicios recreativos",
            "Servicios de alojamiento temporal y de preparación de alimentos y bebidas": "Servicios de alojamiento temporal <br> y de preparación de alimentos <br> y bebidas",
            "Otros servicios excepto actividades gubernamentales": "Otros servicios excepto <br> actividades gubernamentales",
            "Actividades legislativas, gubernamentales, de impartición de justicia y de organismos internacionales y extraterritoriales": "Actividades legislativas, <br> gubernamentales, de impartición de justicia <br> y de organismos internacionales <br> y extraterritoriales"
        }
        return opciones.get(cadena.strip(), cadena)
    d['Título_dos_digitos'] = d['Título_dos_digitos'].apply(lambda x: espacios(str(x)) if pd.notna(x) else x)

    fig = px.scatter(
        d,
        x= afinidad_anio,
        y= complejidad_producto_anio,
        size= personal_ocupado_anio,
        color='Titulo_corto',
        custom_data=['Título', 'codigo_act', 'Título_dos_digitos'],
        color_continuous_scale=px.colors.sequential.Viridis,  # Paleta de colores
    )

    fig.update_layout(
        title={
            "text": "Afinidad y complejidad por producto <br><span style='font-size:14px;'> (Semestre " + anio + ") </span>",
            "x": 0.5,  # Centrar el título
            "xanchor": "center"
        },
        xaxis_title="Afinidad",
        yaxis_title="Complejidad del Producto",
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

    # Actualizar los trazos (traces) para personalizar el hover y los marcadores
    fig.update_traces(
        hovertemplate=
        "<b>Actividad:</b> %{customdata[0]}<br>" +
        "<b>Código:</b> %{customdata[1]}<br>" +
        "<b>Afinidad:</b> %{x:.2f}<br>" +
        "<b>Complejidad:</b> %{y:.2f}<br>" +
        "<b>Personal:</b> %{marker.size:.0f}<br>" +
        "<extra>%{customdata[2]}</extra>",
        marker=dict(showscale=False)  # Desactiva la escala de colores
    )

    fig.update_layout(
        legend_title_text='Sector'
    )

    return(fig)