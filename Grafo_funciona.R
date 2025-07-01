library(igraph)             
library(ggraph)
library(plotly)
library(economiccomplexity)

resumen = function(directorio = direccion, nombre_guardado = guardado){
  library(dplyr)
  library(stringr)
  library(economiccomplexity)
  media_trabajadores=function(str){
    return(switch(str,
                  "0 a 5 personas"= 2.5,
                  "6 a 10 personas" = 8,
                  "11 a 30 personas" = 20.5,
                  "31 a 50 personas" = 40.5,
                  "51 a 100 personas" = 75.5,
                  "101 a 250 personas" = 175.5,
                  "251 y más personas" = 251
    ))
  }
  
  ### Prepara base
  denue = read.csv(directorio) |>
    dplyr::mutate(per_ocu = ifelse(substr(per_ocu, 1, 3) == "251", 
                                   "251 y más personas", per_ocu)) |>
    dplyr::mutate(per_ocu = sapply(per_ocu, media_trabajadores, 
                                   simplify = TRUE, USE.NAMES = FALSE)*conteo) |>
    dplyr::group_by(cve_ent, codigo_act) |>
    dplyr::summarise(suma_per_ocu = sum(per_ocu, na.rm = TRUE))
  
  return(denue)
}
balassa = function(directorio = direccion, nombre_guardado = guardado){
  library(dplyr)
  library(stringr)
  library(economiccomplexity)
  media_trabajadores=function(str){
    return(switch(str,
                  "0 a 5 personas"= 2.5,
                  "6 a 10 personas" = 8,
                  "11 a 30 personas" = 20.5,
                  "31 a 50 personas" = 40.5,
                  "51 a 100 personas" = 75.5,
                  "101 a 250 personas" = 175.5,
                  "251 y más personas" = 251
    ))
  }
  
  ### Prepara base
  denue = read.csv(directorio) |>
    dplyr::mutate(per_ocu = ifelse(substr(per_ocu, 1, 3) == "251", 
                                   "251 y más personas", per_ocu)) |>
    dplyr::mutate(per_ocu = sapply(per_ocu, media_trabajadores, 
                                   simplify = TRUE, USE.NAMES = FALSE)*conteo) |>
    dplyr::group_by(cve_ent, codigo_act) |>
    dplyr::summarise(suma_per_ocu = sum(per_ocu, na.rm = TRUE))
  
  ### Balassa
  b = balassa_index(data = denue, country = "cve_ent", product = "codigo_act", value = "suma_per_ocu")
  
  return(b)
}
nacional_entidad_ice_personal = function(directorio = direccion, nombre_guardado = guardado){
  ### Definir funcion
  media_trabajadores=function(str){
    return(switch(str,
                  "0 a 5 personas"= 2.5,
                  "6 a 10 personas" = 8,
                  "11 a 30 personas" = 20.5,
                  "31 a 50 personas" = 40.5,
                  "51 a 100 personas" = 75.5,
                  "101 a 250 personas" = 175.5,
                  "251 y más personas" = 251
    ))
  }
  
  ### Prepara base
  denue = read.csv(directorio) |>
    dplyr::mutate(per_ocu = ifelse(substr(per_ocu, 1, 3) == "251", 
                                   "251 y más personas", per_ocu)) |>
    dplyr::mutate(per_ocu = sapply(per_ocu, media_trabajadores, 
                                   simplify = TRUE, USE.NAMES = FALSE)*conteo) |>
    dplyr::group_by(cve_ent, codigo_act) |>
    dplyr::summarise(suma_per_ocu = sum(per_ocu, na.rm = TRUE))
  
  ### Complejidad Economica
  complejidad =  complexity_measures(balassa_index = balassa_index(data = denue, country = "cve_ent", product = "codigo_act", value = "suma_per_ocu"), 
                                     method = "eigenvalues" ) 
  ICE_NE = data.frame(names(complejidad$complexity_index_country), complejidad$complexity_index_country)
  colnames(ICE_NE) = c("CVEGEO", paste0("ICE_Personal_", unlist(lapply(nombre_guardado, function(x) substr(x, start = 1, stop = 5)))))
  #todos = data.frame(Estado = names(complejidad$complexity_index_country))
  #todos = merge(x = todos, y = ICE_NE, by = "Estado")
  return(ICE_NE)
}


setwd("C:/Users/eagel/OneDrive/Escritorio/Lalo/Escuela/Practicas/Chamba/")
archivos = list.files(path = "Febrero/Cosas Realizadas/Resumen/Nacional/", recursive = TRUE, full.names = TRUE)
archivos_csv = grep("\\.csv$", archivos, value = TRUE)
archivos_csv = subset(archivos_csv, !grepl("diccionario", archivos_csv))
g = basename(archivos_csv)

base = resumen(directorio = archivos_csv[18], nombre_guardado = g[18])
bi = balassa(directorio = archivos_csv[18], nombre_guardado = g[18])
ice = nacional_entidad_ice_personal(directorio = archivos_csv[18], nombre_guardado = g[18])

base = base |> dplyr::group_by(cve_ent) |>
  dplyr::summarise(suma = sum(suma_per_ocu, na.rm = TRUE))


pro = proximity(bi)
net = projections(pro$proximity_country, pro$proximity_product)
aggregated_countries = setNames(base$suma, base$cve_ent)

V(net$network_country)$size = aggregated_countries[match(V(net$network_country)$name, names(aggregated_countries))]






ggraph(net$network_country, layout = "kk") +
  # geom_edge_link(aes(edge_width = weight), edge_colour = "#a8a8a8") +
  geom_edge_link(edge_colour = "#a8a8a8") +
  geom_node_point(aes(size = size), color = "#002948") +
  geom_node_text(aes(label = name), size = 2, vjust = 2.2) +
  ggtitle("Proximity Based Network Projection for Countries") +
  theme_void()


a= net$network_country
edge_attr(a)
vertex_attr(a)

a
E(a)$weight
vertex_attr(a)

library(igraph)
library(plotly)

# Obtener el layout con Kamada-Kawai se utiliza la función layout_with_kk() de igraph para calcular las coordenadas de los nodos usando el algoritmo de Kamada-Kawai. Este algoritmo posiciona los nodos de manera que se minimicen las tensiones entre ellos, ofreciendo una visualización estéticamente agradable.
coords = layout_with_kk(net$network_country) # Es una matriz en la que cada fila representa un nodo y las columnas corresponden a las coordenadas en los ejes x e y.

# Extraer nodos coordenadas
node_x = coords[, 1]
node_y = coords[, 2]

# Extraer aristas
edge_list = as_edgelist(net$network_country) # Convierte la estructura del grafo en una matriz donde cada fila representa una conexión (arista) entre dos nodos.
edge_x = c()
edge_y = c()

for (i in 1:nrow(edge_list)) {
  x0 = node_x[as.numeric(edge_list[i, 1])]
  y0 = node_y[as.numeric(edge_list[i, 1])]
  x1 = node_x[as.numeric(edge_list[i, 2])]
  y1 = node_y[as.numeric(edge_list[i, 2])]
  
  # Se añade un NA entre cada par de aristas para que Plotly sepa que debe romper la línea y no unir segmentos que no pertenecen a la misma arista.
  edge_x = c(edge_x, x0, x1, NA)
  edge_y = c(edge_y, y0, y1, NA)
}

# Tamaño de los nodos basado en aggregated_countries
node_size = V(net$network_country)$size
node_text = paste0("Country: ", V(net$network_country)$name, "<br>Size: ", node_size)   # Se crea un texto que combina el nombre del país (atributo name) y su tamaño (size).

# Crear la traza de aristas
edge_trace = list(
  x = edge_x,
  y = edge_y,
  mode = 'lines',
  line = list(width = 0.5, color = '#a8a8a8'),
  hoverinfo = 'none',      # Se establece en 'none' para que no se muestre información al pasar el cursor sobre las líneas.
  type = 'scatter'         # Se define como 'scatter' para trazar líneas en Plotly.
)

# Crear la traza de nodos
node_trace = list(
  x = node_x,           # Posiciones de los nodos.
  y = node_y,           # Posiciones de los nodos.
  mode = 'markers',     # 'markers' indica que se mostrarán como puntos.
  text = node_text,     # para mostrar información (nombre del país y tamaño) cuando se pasa el cursor.
  hoverinfo = 'text',   # para mostrar información (nombre del país y tamaño) cuando se pasa el cursor.
  marker = list(
    showscale = TRUE,       # Activa la barra de colores.
    colorscale = 'YlGnBu',  # Se utiliza una escala de colores
    reversescale = TRUE,    # Invierte la escala de colores.
    color = node_size,      # 
    size = node_size,       # Escalar tamaño
    colorbar = list(        # Configura la barra de color, definiendo grosor, título y alineación.
      thickness = 15,
      title = "Node Size",
      xanchor = 'left'
    ),  
    line = list(width = 2)  # Define el ancho del borde alrededor de cada nodo.
  ),
  type = 'scatter'          # Se establece como 'scatter' para graficar los nodos en Plotly.
)

# Combinar trazas en plotly
fig = plot_ly() |>        
  add_trace(
    x = edge_trace$x,
    y = edge_trace$y,
    mode = edge_trace$mode,
    line = edge_trace$line,
    hoverinfo = edge_trace$hoverinfo,
    type = edge_trace$type
  ) |>
  add_trace(
    x = node_trace$x,
    y = node_trace$y,
    mode = node_trace$mode,
    text = node_trace$text,
    hoverinfo = node_trace$hoverinfo,
    marker = node_trace$marker,
    type = node_trace$type
  ) |>
  layout(
    title = list(
      text = "<br>Proximity Network Graph",
      font = list(size = 16)
    ),
    showlegend = FALSE,
    hovermode = 'closest',
    margin = list(b = 20, l = 5, r = 5, t = 40),
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
  )

# Mostrar gráfico
fig
