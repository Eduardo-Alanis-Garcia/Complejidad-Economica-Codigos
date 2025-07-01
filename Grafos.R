library(visNetwork)

nodes <- data.frame(id = 1:5, 
                    label = paste("Nodo", 1:5),
                    title = c("Descripción del nodo 1", 
                              "Descripción del nodo 2", 
                              "Descripción del nodo 3", 
                              "Descripción del nodo 4", 
                              "Descripción del nodo 5"))

# Definir datos de enlaces
edges <- data.frame(from = c(1, 2, 3, 4), 
                    to   = c(2, 3, 4, 5))

# Crear el grafo interactivo
visNetwork(nodes, edges) %>%
  # Permite seleccionar un nodo mediante un menú desplegable
  visOptions(nodesIdSelection = TRUE,
             # Resalta al nodo y sus vecinos al hacer hover o click
             highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)) %>%
  # Habilita la interacción de zoom y desplazamiento
  visInteraction(zoomView = TRUE, dragView = TRUE) %>%
  # Evento para hacer zoom al hacer click en un nodo
  visEvents(select = "function(event) {
              if(event.nodes.length > 0) {
                var nodeId = event.nodes[0];
                this.focus(nodeId, {scale:1.5});
              }
            }")









###################


nodes <- data.frame(
  id = 1:5,
  label = paste("Nodo", 1:5),
  title = c("Descripción del nodo 1", 
            "Descripción del nodo 2", 
            "Descripción del nodo 3", 
            "Descripción del nodo 4", 
            "Descripción del nodo 5")
)

# Datos de enlaces
edges <- data.frame(
  from = c(1, 2, 3, 4),
  to   = c(2, 3, 4, 5)
)

# Crear el grafo interactivo con botones de navegación
visNetwork(nodes, edges) %>%
  visOptions(
    nodesIdSelection = TRUE,
    highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)
  ) %>%
  # Habilita los controles de zoom, desplazamiento y navegación
  visInteraction(
    navigationButtons = TRUE, 
    zoomView = TRUE, 
    dragView = TRUE
  ) %>%
  # Evento para hacer zoom sobre un nodo al seleccionarlo
  visEvents(
    select = "function(event) {
                if (event.nodes.length > 0) {
                  var nodeId = event.nodes[0];
                  this.focus(nodeId, {scale:1.5});
                }
              }"
  )


library(plotly)


















##############################

library(shiny)
library(plotly)
library(igraph)

# Crear un grafo simple (por ejemplo, un anillo de 10 nodos)
g <- make_ring(10)
layout_coords <- layout_with_fr(g)
nodes <- data.frame(
  id = 1:10,
  x = layout_coords[, 1],
  y = layout_coords[, 2],
  label = paste("Nodo", 1:10),
  desc = paste("Descripción del nodo", 1:10)
)

# Obtener los datos de enlaces usando la función de igraph
edges_df <- igraph::as_data_frame(g, what = "edges")
# Si en tu versión de igraph no funciona, prueba con:
# edges_df <- get.data.frame(g, what = "edges")

# Convertir los identificadores a numérico en caso de que sean caracteres/factores
edges_df$from <- as.numeric(as.character(edges_df$from))
edges_df$to   <- as.numeric(as.character(edges_df$to))

# Generar los segmentos para trazar los enlaces con plotly
edge_segments <- data.frame()
for(i in 1:nrow(edges_df)) {
  x0 <- nodes$x[edges_df$from[i]]
  y0 <- nodes$y[edges_df$from[i]]
  x1 <- nodes$x[edges_df$to[i]]
  y1 <- nodes$y[edges_df$to[i]]
  
  edge_segments <- rbind(edge_segments,
                         data.frame(x = x0, y = y0, xend = x1, yend = y1))
}


ui <- fluidPage(
  plotlyOutput("networkPlot"),
  verbatimTextOutput("nodeInfo"),
  actionButton("reset", "Resetear Zoom")
)

server <- function(input, output, session) {
  
  # Renderizar la gráfica inicial con plotly
  output$networkPlot <- renderPlotly({
    plot_ly() %>%
      # Agregar los enlaces como segmentos
      add_segments(data = edge_segments,
                   x = ~x, y = ~y, xend = ~xend, yend = ~yend,
                   line = list(color = 'gray'),
                   hoverinfo = 'none',
                   showlegend = FALSE) %>%
      # Agregar los nodos como puntos
      add_markers(data = nodes,
                  x = ~x, y = ~y,
                  text = ~paste(label, "<br>", desc),
                  hoverinfo = 'text',
                  marker = list(size = 10, color = 'blue')) %>%
      layout(
        xaxis = list(title = "", zeroline = FALSE),
        yaxis = list(title = "", zeroline = FALSE)
      )
  })
  
  # Mostrar información del nodo seleccionado y sus vecinos
  output$nodeInfo <- renderPrint({
    d <- event_data("plotly_click")
    if(is.null(d)) {
      "Haz clic en un nodo para ver su información."
    } else {
      # Encontrar el nodo más cercano al punto clickeado
      clicked_node <- nodes[which.min((nodes$x - d$x)^2 + (nodes$y - d$y)^2), ]
      # Obtener los vecinos usando igraph
      vecinos <- neighbors(g, clicked_node$id)
      vecinos_info <- nodes[nodes$id %in% as.numeric(names(vecinos)), ]
      
      cat("Nodo seleccionado:\n")
      print(clicked_node)
      cat("\nNodos vecinos:\n")
      print(vecinos_info)
    }
  })
  
  # Capturar el evento de clic y hacer zoom sobre el nodo
  observeEvent(input$networkPlot_click, {
    d <- event_data("plotly_click")
    if(!is.null(d)){
      clicked_node <- nodes[which.min((nodes$x - d$x)^2 + (nodes$y - d$y)^2), ]
      
      # Definir un rango alrededor del nodo para simular el zoom
      delta <- 0.2  # Ajustar este valor según convenga
      xrange <- c(clicked_node$x - delta, clicked_node$x + delta)
      yrange <- c(clicked_node$y - delta, clicked_node$y + delta)
      
      # Actualizar la vista de la gráfica con plotlyProxy
      plotlyProxy("networkPlot", session) %>%
        plotlyProxyInvoke("relayout", list(
          xaxis = list(range = xrange),
          yaxis = list(range = yrange)
        ))
    }
  })
  
  # Botón para resetear la vista a la gráfica original
  observeEvent(input$reset, {
    xrange <- range(nodes$x)
    yrange <- range(nodes$y)
    plotlyProxy("networkPlot", session) %>%
      plotlyProxyInvoke("relayout", list(
        xaxis = list(range = xrange),
        yaxis = list(range = yrange)
      ))
  })
}

shinyApp(ui, server)





####
library(shiny)
library(plotly)
library(igraph)

# Crear un grafo simple (por ejemplo, un anillo de 10 nodos)
g <- make_ring(10)
layout_coords <- layout_with_fr(g)
nodes <- data.frame(
  id = 1:10,
  x = layout_coords[, 1],
  y = layout_coords[, 2],
  label = paste("Nodo", 1:10),
  desc = paste("Descripción del nodo", 1:10)
)

# Obtener los datos de enlaces usando igraph
edges_df <- igraph::as_data_frame(g, what = "edges")
edges_df$from <- as.numeric(as.character(edges_df$from))
edges_df$to   <- as.numeric(as.character(edges_df$to))

# Generar los segmentos para trazar los enlaces con plotly
edge_segments <- data.frame()
for(i in 1:nrow(edges_df)) {
  x0 <- nodes$x[edges_df$from[i]]
  y0 <- nodes$y[edges_df$from[i]]
  x1 <- nodes$x[edges_df$to[i]]
  y1 <- nodes$y[edges_df$to[i]]
  
  edge_segments <- rbind(edge_segments,
                         data.frame(x = x0, y = y0, xend = x1, yend = y1))
}

ui <- fluidPage(
  plotlyOutput("networkPlot"),
  verbatimTextOutput("nodeInfo"),
  actionButton("reset", "Resetear Zoom")
)

server <- function(input, output, session) {
  
  # Renderizar la gráfica inicial con plotly asignando 'source'
  output$networkPlot <- renderPlotly({
    plot_ly(source = "network") %>%
      # Agregar los enlaces como segmentos
      add_segments(data = edge_segments,
                   x = ~x, y = ~y, xend = ~xend, yend = ~yend,
                   line = list(color = 'gray'),
                   hoverinfo = 'none',
                   showlegend = FALSE) %>%
      # Agregar los nodos como puntos
      add_markers(data = nodes,
                  x = ~x, y = ~y,
                  text = ~paste(label, "<br>", desc),
                  hoverinfo = 'text',
                  marker = list(size = 10, color = 'blue')) %>%
      layout(
        xaxis = list(title = "", zeroline = FALSE),
        yaxis = list(title = "", zeroline = FALSE)
      )
  })
  
  # Mostrar información del nodo seleccionado y sus vecinos
  output$nodeInfo <- renderPrint({
    d <- event_data("plotly_click", source = "network")
    if(is.null(d)) {
      "Haz clic en un nodo para ver su información."
    } else {
      clicked_node <- nodes[which.min((nodes$x - d$x)^2 + (nodes$y - d$y)^2), ]
      vecinos <- neighbors(g, clicked_node$id)
      vecinos_info <- nodes[nodes$id %in% as.numeric(names(vecinos)), ]
      
      cat("Nodo seleccionado:\n")
      print(clicked_node)
      cat("\nNodos vecinos:\n")
      print(vecinos_info)
    }
  })
  
  # Evento de clic para hacer zoom sobre el nodo
  observeEvent(event_data("plotly_click", source = "network"), {
    d <- event_data("plotly_click", source = "network")
    if(!is.null(d)){
      clicked_node <- nodes[which.min((nodes$x - d$x)^2 + (nodes$y - d$y)^2), ]
      
      # Aumentamos delta para notar mejor el efecto de zoom
      delta <- 0.5  
      xrange <- c(clicked_node$x - delta, clicked_node$x + delta)
      yrange <- c(clicked_node$y - delta, clicked_node$y + delta)
      
      plotlyProxy("networkPlot", session) %>%
        plotlyProxyInvoke("relayout", list(
          xaxis = list(range = xrange),
          yaxis = list(range = yrange)
        ))
    }
  })
  
  # Botón para resetear la vista a la gráfica original
  observeEvent(input$reset, {
    xrange <- range(nodes$x)
    yrange <- range(nodes$y)
    plotlyProxy("networkPlot", session) %>%
      plotlyProxyInvoke("relayout", list(
        xaxis = list(range = xrange),
        yaxis = list(range = yrange)
      ))
  })
}

shinyApp(ui, server)


































##############
library(shiny)
library(plotly)
library(igraph)

# Crear un grafo simple (por ejemplo, un anillo de 10 nodos)
g <- make_ring(10)
layout_coords <- layout_with_fr(g)
nodes <- data.frame(
  id = 1:10,
  x = layout_coords[, 1],
  y = layout_coords[, 2],
  label = paste("Nodo", 1:10),
  desc = paste("Descripción del nodo", 1:10)
)

# Obtener los datos de enlaces usando igraph
edges_df <- igraph::as_data_frame(g, what = "edges")
edges_df$from <- as.numeric(as.character(edges_df$from))
edges_df$to   <- as.numeric(as.character(edges_df$to))

# Generar los segmentos para trazar los enlaces con plotly
edge_segments <- data.frame()
for(i in 1:nrow(edges_df)) {
  x0 <- nodes$x[edges_df$from[i]]
  y0 <- nodes$y[edges_df$from[i]]
  x1 <- nodes$x[edges_df$to[i]]
  y1 <- nodes$y[edges_df$to[i]]
  
  edge_segments <- rbind(edge_segments,
                         data.frame(x = x0, y = y0, xend = x1, yend = y1))
}

ui <- fluidPage(
  plotlyOutput("networkPlot"),
  verbatimTextOutput("nodeInfo"),
  actionButton("reset", "Resetear Zoom")
)

server <- function(input, output, session) {
  
  # Renderizar la gráfica inicial con plotly asignando 'source'
  output$networkPlot <- renderPlotly({
    plot_ly(source = "network") %>%
      # Agregar los enlaces como segmentos
      add_segments(data = edge_segments,
                   x = ~x, y = ~y, xend = ~xend, yend = ~yend,
                   line = list(color = 'gray'),
                   hoverinfo = 'none',
                   showlegend = FALSE) %>%
      # Agregar los nodos como puntos
      add_markers(data = nodes,
                  x = ~x, y = ~y,
                  text = ~paste(label, "<br>", desc),
                  hoverinfo = 'text',
                  marker = list(size = 10, color = 'blue')) %>%
      layout(
        xaxis = list(title = "", zeroline = FALSE),
        yaxis = list(title = "", zeroline = FALSE)
      )
  })
  
  # Mostrar información del nodo seleccionado y sus vecinos
  output$nodeInfo <- renderPrint({
    d <- event_data("plotly_click", source = "network")
    if(is.null(d)) {
      "Haz clic en un nodo para ver su información."
    } else {
      clicked_node <- nodes[which.min((nodes$x - d$x)^2 + (nodes$y - d$y)^2), ]
      vecinos <- neighbors(g, clicked_node$id)
      vecinos_info <- nodes[nodes$id %in% as.numeric(names(vecinos)), ]
      
      cat("Nodo seleccionado:\n")
      print(clicked_node)
      cat("\nNodos vecinos:\n")
      print(vecinos_info)
    }
  })
  
  # Evento de clic para hacer zoom sobre el nodo
  observeEvent(event_data("plotly_click", source = "network"), {
    d <- event_data("plotly_click", source = "network")
    if(!is.null(d)){
      clicked_node <- nodes[which.min((nodes$x - d$x)^2 + (nodes$y - d$y)^2), ]
      
      # Aumentamos delta para notar mejor el efecto de zoom
      delta <- 0.5  
      xrange <- c(clicked_node$x - delta, clicked_node$x + delta)
      yrange <- c(clicked_node$y - delta, clicked_node$y + delta)
      
      plotlyProxy("networkPlot", session) %>%
        plotlyProxyInvoke("relayout", list(
          xaxis = list(range = xrange),
          yaxis = list(range = yrange)
        ))
    }
  })
  
  # Botón para resetear la vista a la gráfica original
  observeEvent(input$reset, {
    xrange <- range(nodes$x)
    yrange <- range(nodes$y)
    plotlyProxy("networkPlot", session) %>%
      plotlyProxyInvoke("relayout", list(
        xaxis = list(range = xrange),
        yaxis = list(range = yrange)
      ))
  })
}

shinyApp(ui, server)



















#############
library(shiny)
library(plotly)
library(igraph)

# -------------------------------------------------------
# 2) Creación de un grafo de ejemplo (10 nodos)
#    y asignación de un layout
# -------------------------------------------------------
# En tu caso, puedes construir o leer tu propio grafo
# desde datos, y asignar el layout que desees.
g <- make_ring(10)  # grafo "anillo" de 10 nodos
coords <- layout_with_fr(g)  # layout estilo "force-directed"

# Definimos un data frame de nodos con sus coordenadas x, y
nodes <- data.frame(
  id    = 1:vcount(g),
  x     = coords[, 1],
  y     = coords[, 2],
  # Puedes usar 'category' o 'sector' si quieres colorearlos
  # por alguna variable. Aquí solo ponemos un nombre genérico.
  label = paste("Nodo", 1:vcount(g))
)

# Para las aristas, extraemos los pares (from, to)
edges_df <- as.data.frame(g, what = "edges")

# Generamos un data frame con los segmentos (x0,y0) -> (x1,y1)
# para poder dibujarlos con plotly
edge_segments <- do.call(
  rbind,
  lapply(seq_len(nrow(edges_df)), function(i) {
    from_i <- edges_df$from[i]
    to_i   <- edges_df$to[i]
    data.frame(
      x    = nodes$x[from_i],
      y    = nodes$y[from_i],
      xend = nodes$x[to_i],
      yend = nodes$y[to_i]
    )
  })
)

# -------------------------------------------------------
# 3) Definimos la app Shiny (UI y Server)
# -------------------------------------------------------

ui <- fluidPage(
  # Título opcional
  h3("Ejemplo de Grafo con Plotly y Resaltado Dinámico"),
  
  # Salida del grafo
  plotlyOutput("networkPlot", height = "600px"),
  
  # Información del nodo clicado
  verbatimTextOutput("infoNodo"),
  
  # Botón para resetear vista (zoom)
  actionButton("resetZoom", "Resetear Zoom")
)

server <- function(input, output, session) {
  
  # Variable reactiva que guarda el "id" del nodo seleccionado
  selectedNode <- reactiveVal(NULL)
  
  # -----------------------------------------------------
  # Función auxiliar para construir la figura Plotly
  # en base al nodo seleccionado
  # -----------------------------------------------------
  buildPlot <- function(selected_id) {
    # Copiamos el data frame de nodos para manipular color/opacidad
    df_nodes <- nodes
    
    # Caso 1: Si no hay nodo seleccionado, todos en gris
    if (is.null(selected_id)) {
      df_nodes$color  <- "gray"
      df_nodes$opacity <- 1
    } else {
      # Obtenemos los vecinos del nodo seleccionado en el grafo igraph
      nbrs <- as_ids(neighbors(g, selected_id))
      
      # Asignamos color base gris a todos
      df_nodes$color  <- "lightgray"
      df_nodes$opacity <- 0.3  # algo más tenue para no seleccionados
      
      # El nodo seleccionado en rosa
      df_nodes$color[df_nodes$id == selected_id] <- "hotpink"
      df_nodes$opacity[df_nodes$id == selected_id] <- 1
      
      # Sus vecinos en verde
      df_nodes$color[df_nodes$id %in% nbrs] <- "limegreen"
      df_nodes$opacity[df_nodes$id %in% nbrs] <- 1
    }
    
    # Construimos la figura Plotly
    plot_ly() %>%
      # 1) Agregamos segmentos (aristas)
      add_segments(
        data       = edge_segments,
        x          = ~x, 
        y          = ~y, 
        xend       = ~xend, 
        yend       = ~yend,
        line       = list(color = 'lightgray'),
        hoverinfo  = 'none',
        showlegend = FALSE
      ) %>%
      # 2) Agregamos nodos como puntos
      add_markers(
        data       = df_nodes,
        x          = ~x,
        y          = ~y,
        text       = ~paste0("ID: ", id, "<br>", label),
        hoverinfo  = 'text',
        marker     = list(
          size       = 12,
          color      = ~color,
          opacity    = ~opacity,
          line       = list(width = 1, color = 'black')
        ),
        # Asignamos un 'key' para cada punto, que será el ID
        # Esto nos ayuda a identificar el nodo clicado
        key        = ~id,
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(title = "", zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(title = "", zeroline = FALSE, showticklabels = FALSE),
        dragmode = "pan"
      )
  }
  
  # -----------------------------------------------------
  # 4) Renderizamos el grafo inicialmente sin nodo seleccionado
  # -----------------------------------------------------
  output$networkPlot <- renderPlotly({
    buildPlot(selectedNode())
  })
  
  # -----------------------------------------------------
  # 5) Observamos el evento de clic en la gráfica
  #    y actualizamos la variable 'selectedNode'
  # -----------------------------------------------------
  observeEvent(event_data("plotly_click"), {
    d <- event_data("plotly_click")
    # 'd$key' es el ID del nodo que se clicó (definido en add_markers(key=~id))
    if (!is.null(d$key)) {
      # Actualizamos la variable reactiva
      selectedNode(as.numeric(d$key))
      
      # Opcional: Hacemos un pequeño zoom centrado en ese nodo
      # Definimos un "delta" para el zoom
      delta <- 0.3
      node_clicked <- nodes[nodes$id == d$key, ]
      x_range <- c(node_clicked$x - delta, node_clicked$x + delta)
      y_range <- c(node_clicked$y - delta, node_clicked$y + delta)
      
      plotlyProxy("networkPlot", session) %>%
        plotlyProxyInvoke("relayout", list(
          xaxis = list(range = x_range),
          yaxis = list(range = y_range)
        ))
    }
  })
  
  # -----------------------------------------------------
  # 6) Cada vez que cambia 'selectedNode', re-renderizamos
  #    la gráfica para actualizar colores
  # -----------------------------------------------------
  observeEvent(selectedNode(), {
    output$networkPlot <- renderPlotly({
      buildPlot(selectedNode())
    })
  })
  
  # -----------------------------------------------------
  # 7) Mostramos en texto info del nodo seleccionado
  # -----------------------------------------------------
  output$infoNodo <- renderPrint({
    req(selectedNode())  # aseguramos que no sea NULL
    sel_id <- selectedNode()
    cat("Nodo seleccionado:\n")
    print(nodes[nodes$id == sel_id, ])
    
    # Vecinos
    nbrs <- as_ids(neighbors(g, sel_id))
    cat("\nVecinos:\n")
    print(nodes[nodes$id %in% nbrs, ])
  })
  
  # -----------------------------------------------------
  # 8) Botón para resetear el zoom a la vista original
  # -----------------------------------------------------
  observeEvent(input$resetZoom, {
    x_range <- range(nodes$x)
    y_range <- range(nodes$y)
    plotlyProxy("networkPlot", session) %>%
      plotlyProxyInvoke("relayout", list(
        xaxis = list(range = x_range),
        yaxis = list(range = y_range)
      ))
  })
}
shinyApp(ui, server)































































































###########################

library(igraph)

# 1. Crear un ejemplo de red

# Creamos un grafo de ejemplo
set.seed(123)
g <- sample_gnp(n = 10, p = 0.3)  # 10 nodos, prob. 0.3 de arista

# Si deseas que sea no dirigido, asegurarse de que sea "undirected"
g <- as.undirected(g, mode = "collapse")

# Calculamos un layout (posiciones 2D de los nodos)
lay <- layout_with_fr(g)  # layout estilo Fruchterman-Reingold




# 2.1. Data frame de nodos

# Nodos
nodes <- data.frame(
  id = V(g)$name %||% as.character(1:vcount(g)),  # nombres de nodos
  x = lay[, 1],
  y = lay[, 2]
)

# 2.2. Data frame de aristas

# Obtenemos aristas como pares (fuente, destino)
edge_list <- as_edgelist(g)

# Para cada arista, generamos las coordenadas (x,y) de su nodo origen y destino
edges <- data.frame(
  x = lay[edge_list[, 1], 1],
  y = lay[edge_list[, 1], 2],
  xend = lay[edge_list[, 2], 1],
  yend = lay[edge_list[, 2], 2]
)

# 3. Construir el gráfico base con plotly

library(plotly)

p <- plot_ly() %>%
  # 1) Aristas
  add_segments(
    x = ~edges$x,
    y = ~edges$y,
    xend = ~edges$xend,
    yend = ~edges$yend,
    line = list(color = 'gray', width = 1),
    hoverinfo = 'none',   # Para que no aparezca tooltip sobre las líneas
    showlegend = FALSE
  ) %>%
  # 2) Nodos
  add_markers(
    x = ~nodes$x,
    y = ~nodes$y,
    text = ~nodes$id,         # Texto a mostrar en hover
    hoverinfo = 'text',
    marker = list(
      size = 10,
      color = 'tomato'
    ),
    # Guardamos en 'customdata' el ID para recuperarlo en clic
    customdata = ~nodes$id,
    key = ~nodes$id,
    showlegend = FALSE
  ) %>%
  layout(
    xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
    yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE)
  )

p


# 4. Manejar el clic en un nodo con Shiny

library(shiny)
library(igraph)
library(plotly)

ui <- fluidPage(
  plotlyOutput("networkPlot")
)

server <- function(input, output, session) {
  
  # 1) Creamos el grafo y data frames de nodos/aristas (igual que antes)
  set.seed(123)
  g <- sample_gnp(n = 10, p = 0.3)
  g <- as.undirected(g, mode = "collapse")
  lay <- layout_with_fr(g)
  
  nodes <- data.frame(
    id = 1:vcount(g),
    x = lay[,1],
    y = lay[,2]
  )
  edges <- as_edgelist(g)
  edges <- data.frame(
    x = lay[edges[,1], 1],
    y = lay[edges[,1], 2],
    xend = lay[edges[,2], 1],
    yend = lay[edges[,2], 2]
  )
  
  # 2) Generamos un reactiveVal para guardar el nodo seleccionado
  selectedNode <- reactiveVal(NULL)
  
  # 3) Renderizamos el plotly inicial
  output$networkPlot <- renderPlotly({
    
    # Si no hay nodo seleccionado, todos los nodos van con color 'tomato'
    # Si hay un nodo seleccionado, lo marcamos en otro color y sus vecinos en otro
    sel <- selectedNode()
    colorVector <- rep('tomato', nrow(nodes))
    sizeVector  <- rep(10, nrow(nodes))
    
    if (!is.null(sel)) {
      # Vecinos del nodo seleccionado
      nb <- neighbors(g, sel) 
      nb_ids <- as.numeric(nb)  # vectores de IDs
      
      # Cambiamos color del nodo seleccionado
      colorVector[sel] <- 'blue'
      sizeVector[sel] <- 15
      
      # Cambiamos color de sus vecinos
      colorVector[nb_ids] <- 'green'
      sizeVector[nb_ids] <- 12
    }
    
    plot_ly() %>%
      # Aristas
      add_segments(
        x = ~edges$x,
        y = ~edges$y,
        xend = ~edges$xend,
        yend = ~edges$yend,
        line = list(color = 'gray', width = 1),
        hoverinfo = 'none',
        showlegend = FALSE
      ) %>%
      # Nodos
      add_markers(
        x = ~nodes$x,
        y = ~nodes$y,
        text = ~paste("Nodo:", nodes$id),
        hoverinfo = 'text',
        marker = list(size = sizeVector, color = colorVector),
        customdata = ~nodes$id,  # Importante para capturar evento
        key = ~nodes$id,
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
        yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE)
      ) %>%
      # Si hay un nodo seleccionado, ajustamos el rango de ejes (zoom)
      layout(
        xaxis = if(!is.null(sel)) list(range = c(nodes$x[sel]-0.2, nodes$x[sel]+0.2)) else list(),
        yaxis = if(!is.null(sel)) list(range = c(nodes$y[sel]-0.2, nodes$y[sel]+0.2)) else list()
      )
  })
  
  # 4) Observamos el evento de clic en un nodo
  observeEvent(event_data("plotly_click"), {
    d <- event_data("plotly_click")
    # 'customdata' contendrá el id del nodo clicado
    clickedNode <- d$customdata
    if(!is.null(clickedNode)) {
      selectedNode(as.numeric(clickedNode))
    }
  })
}

shinyApp(ui, server)























































































































# Tu grafo de países
g <- net$network_country


library(igraph)
coords <- layout_with_kk(g)  # matriz con 2 columnas (x, y) y tantas filas como nodos


nodes <- data.frame(
  id = V(g)$name,
  x  = coords[, 1],
  y  = coords[, 2],
  size = V(g)$size  # la misma que usaste en ggraph
)


# Obtenemos las aristas como pares (fuente, destino)
rownames(coords) <- V(g)$name

edge_list <- as_edgelist(g)  # por defecto, devuelve nombres si existen
edges <- data.frame(
  x    = coords[ edge_list[,1], 1 ],
  y    = coords[ edge_list[,1], 2 ],
  xend = coords[ edge_list[,2], 1 ],
  yend = coords[ edge_list[,2], 2 ]
)





library(plotly)

p <- plot_ly() %>%
  # 1) Agregar segmentos para las aristas
  add_segments(
    x = ~edges$x,
    y = ~edges$y,
    xend = ~edges$xend,
    yend = ~edges$yend,
    line = list(color = '#a8a8a8', width = 1),
    hoverinfo = 'none',
    showlegend = FALSE
  ) %>%
  # 2) Agregar los nodos como marcadores
  add_markers(
    x = ~nodes$x,
    y = ~nodes$y,
    text = ~paste("País:", nodes$id, "<br>Size:", nodes$size),
    hoverinfo = 'text',
    marker = list(
      color = '#002948',
      size  = ~nodes$size  # <- utilizamos la variable size de los nodos
    ),
    customdata = ~nodes$id,  # útil si luego quieres usar click events
    key        = ~nodes$id,
    showlegend = FALSE
  ) %>%
  layout(
    title = "Proximity Based Network Projection for Countries",
    xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
    yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE)
  )

p



library(shiny)
library(igraph)
library(plotly)

ui <- fluidPage(
  plotlyOutput("networkPlot")
)

server <- function(input, output, session) {
  
  # 1) Tu grafo igraph
  g <- net$network_country
  
  # 2) Layout
  coords <- layout_with_kk(g)
  
  # 3) Creamos data frames de nodos y aristas
  nodes <- data.frame(
    id   = V(g)$name,
    x    = coords[,1],
    y    = coords[,2],
    size = V(g)$size
  )
  edge_list <- as_edgelist(g)
  edges <- data.frame(
    x    = coords[edge_list[,1], 1],
    y    = coords[edge_list[,1], 2],
    xend = coords[edge_list[,2], 1],
    yend = coords[edge_list[,2], 2]
  )
  
  # Variable reactiva para guardar nodo seleccionado
  selectedNode <- reactiveVal(NULL)
  
  # 4) Renderizamos el plotly
  output$networkPlot <- renderPlotly({
    sel <- selectedNode()
    
    # Por defecto, color azul oscuro y size según V(g)$size
    colorVector <- rep('#002948', nrow(nodes))
    sizeVector  <- nodes$size
    
    # Si hay un nodo seleccionado, resaltamos
    if(!is.null(sel)) {
      # El ID clicado es un texto; convertimos para ubicar índice en data.frame
      sel_index <- which(nodes$id == sel)
      
      # Buscamos vecinos en el grafo (retorna un vector de nodos igraph)
      nb <- neighbors(g, sel)
      nb_names <- V(g)[nb]$name
      
      # Resaltar el nodo seleccionado (por ejemplo, color = 'red')
      colorVector[sel_index] <- 'red'
      sizeVector[sel_index]  <- sizeVector[sel_index] * 1.5  # agrandarlo
      
      # Resaltar vecinos (por ejemplo, color = 'green')
      for(nm in nb_names) {
        i <- which(nodes$id == nm)
        colorVector[i] <- 'green'
        sizeVector[i]  <- sizeVector[i] * 1.3
      }
    }
    
    # Construimos el gráfico
    plot_ly() %>%
      add_segments(
        x = ~edges$x,
        y = ~edges$y,
        xend = ~edges$xend,
        yend = ~edges$yend,
        line = list(color = '#a8a8a8', width = 1),
        hoverinfo = 'none',
        showlegend = FALSE
      ) %>%
      add_markers(
        x = ~nodes$x,
        y = ~nodes$y,
        text = ~paste("País:", nodes$id, "<br>Size:", nodes$size),
        hoverinfo = 'text',
        marker = list(
          color = colorVector,
          size  = sizeVector
        ),
        customdata = ~nodes$id,
        key        = ~nodes$id,
        showlegend = FALSE
      ) %>%
      layout(
        title = "Proximity Based Network Projection for Countries",
        xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
        yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE)
      ) %>%
      # Si hay un nodo seleccionado, ajustamos rango (zoom) a sus coordenadas
      layout(
        xaxis = if(!is.null(sel_index)) list(range = c(nodes$x[sel_index]-0.2, nodes$x[sel_index]+0.2)) else list(),
        yaxis = if(!is.null(sel_index)) list(range = c(nodes$y[sel_index]-0.2, nodes$y[sel_index]+0.2)) else list()
      )
  })
  
  # 5) Observamos el evento de clic
  observeEvent(event_data("plotly_click"), {
    d <- event_data("plotly_click")
    # 'customdata' es el ID del nodo (aquí, el nombre del país)
    if(!is.null(d$customdata)) {
      selectedNode(d$customdata)
    }
  })
}

shinyApp(ui, server)


























































































































# Cargar las librerías necesarias
library(igraph)
library(plotly)

# Fijar la semilla para reproducibilidad
set.seed(42)

# Crear un grafo geométrico aleatorio con 50 nodos y un radio de 0.125.
# La opción coords = TRUE asigna a cada nodo coordenadas (x, y) generadas aleatoriamente.
g <- sample_grg(50, 0.125, coords = TRUE)

# Extraer las coordenadas de los nodos
node_x <- V(g)$x
node_y <- V(g)$y

# Construir las coordenadas para las aristas
edge_list <- as_edgelist(g)
edge_x <- c()
edge_y <- c()

for (i in 1:nrow(edge_list)) {
  # Coordenadas del nodo de origen
  x0 <- node_x[as.numeric(edge_list[i, 1])]
  y0 <- node_y[as.numeric(edge_list[i, 1])]
  # Coordenadas del nodo destino
  x1 <- node_x[as.numeric(edge_list[i, 2])]
  y1 <- node_y[as.numeric(edge_list[i, 2])]
  
  # Se agregan los puntos y NA para separar cada segmento (equivalente al None de Python)
  edge_x <- c(edge_x, x0, x1, NA)
  edge_y <- c(edge_y, y0, y1, NA)
}

# Crear la traza de las aristas usando plotly
edge_trace <- list(
  x = edge_x,
  y = edge_y,
  mode = 'lines',
  line = list(width = 0.5, color = '#888'),
  hoverinfo = 'none',
  type = 'scatter'
)

# Calcular el grado (número de conexiones) para cada nodo
node_degree <- degree(g)
node_text <- paste0("# of connections: ", node_degree)

# Crear la traza de los nodos
node_trace <- list(
  x = node_x,
  y = node_y,
  mode = 'markers',
  text = node_text,
  hoverinfo = 'text',
  marker = list(
    showscale = TRUE,
    colorscale = 'YlGnBu',
    reversescale = TRUE,
    color = node_degree,
    size = 10,
    colorbar = list(
      thickness = 15,
      title = "Node Connections",
      xanchor = 'left'
    ),
    line = list(width = 2)
  ),
  type = 'scatter'
)

# Combinar las trazas en una figura interactiva con un layout similar
fig <- plot_ly() %>%
  add_trace(
    x = edge_trace$x,
    y = edge_trace$y,
    mode = edge_trace$mode,
    line = edge_trace$line,
    hoverinfo = edge_trace$hoverinfo,
    type = edge_trace$type
  ) %>%
  add_trace(
    x = node_trace$x,
    y = node_trace$y,
    mode = node_trace$mode,
    text = node_trace$text,
    hoverinfo = node_trace$hoverinfo,
    marker = node_trace$marker,
    type = node_trace$type
  ) %>%
  layout(
    title = list(
      text = "<br>Network graph made with R",
      font = list(size = 16)
    ),
    showlegend = FALSE,
    hovermode = 'closest',
    margin = list(b = 20, l = 5, r = 5, t = 40),
    annotations = list(
      list(
        text = "R code: <a href='https://plotly.com/r/network-graphs/'>https://plotly.com/r/network-graphs/</a>",
        showarrow = FALSE,
        xref = "paper", yref = "paper",
        x = 0.005, y = -0.002
      )
    ),
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
  )

# Mostrar el gráfico interactivo
fig
























































































#
library(plotly)
library(igraph)
library(igraphdata)

# Read Graph File
data(karate, package="igraphdata")
G <- upgrade_graph(karate)
L <- layout.circle(G)


#
vs <- V(G)
es <- as.data.frame(get.edgelist(G))

Nv <- length(vs)
Ne <- length(es[1]$V1)


#
library(plotly)

Xn <- L[,1]
Yn <- L[,2]

network <- plot_ly(x = ~Xn, y = ~Yn, mode = "markers", text = vs$label, hoverinfo = "text")



#
edge_shapes <- list()
for(i in 1:Ne) {
  v0 <- es[i,]$V1
  v1 <- es[i,]$V2
  
  edge_shape = list(
    type = "line",
    line = list(color = "#030303", width = 0.3),
    x0 = Xn[v0],
    y0 = Yn[v0],
    x1 = Xn[v1],
    y1 = Yn[v1]
  )
  
  edge_shapes[[i]] <- edge_shape
}



#
axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)

fig <- layout(
  network,
  title = 'Karate Network',
  shapes = edge_shapes,
  xaxis = axis,
  yaxis = axis
)

fig








































library(igraph)
library(plotly)

# Obtener el layout con Kamada-Kawai
coords <- layout_with_kk(net$network_country)

# Extraer nodos
node_x <- coords[, 1]
node_y <- coords[, 2]

# Extraer aristas
edge_list <- as_edgelist(net$network_country)
edge_x <- c()
edge_y <- c()

for (i in 1:nrow(edge_list)) {
  x0 <- node_x[as.numeric(edge_list[i, 1])]
  y0 <- node_y[as.numeric(edge_list[i, 1])]
  x1 <- node_x[as.numeric(edge_list[i, 2])]
  y1 <- node_y[as.numeric(edge_list[i, 2])]
  
  edge_x <- c(edge_x, x0, x1, NA)
  edge_y <- c(edge_y, y0, y1, NA)
}

# Tamaño de los nodos basado en aggregated_countries
node_size <- V(net$network_country)$size
node_text <- paste0("Country: ", V(net$network_country)$name, "<br>Size: ", node_size)

# Crear la traza de aristas
edge_trace <- list(
  x = edge_x,
  y = edge_y,
  mode = 'lines',
  line = list(width = 0.5, color = '#a8a8a8'),
  hoverinfo = 'none',
  type = 'scatter'
)

# Crear la traza de nodos
node_trace <- list(
  x = node_x,
  y = node_y,
  mode = 'markers',
  text = node_text,
  hoverinfo = 'text',
  marker = list(
    showscale = TRUE,
    colorscale = 'YlGnBu',
    reversescale = TRUE,
    color = node_size,
    size = 10 + node_size / max(node_size) * 15,  # Escalar tamaño
    colorbar = list(
      thickness = 15,
      title = "Node Size",
      xanchor = 'left'
    ),
    line = list(width = 2)
  ),
  type = 'scatter'
)

# Combinar trazas en plotly
fig <- plot_ly() %>%
  add_trace(
    x = edge_trace$x,
    y = edge_trace$y,
    mode = edge_trace$mode,
    line = edge_trace$line,
    hoverinfo = edge_trace$hoverinfo,
    type = edge_trace$type
  ) %>%
  add_trace(
    x = node_trace$x,
    y = node_trace$y,
    mode = node_trace$mode,
    text = node_trace$text,
    hoverinfo = node_trace$hoverinfo,
    marker = node_trace$marker,
    type = node_trace$type
  ) %>%
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



