funcion_grafos =  function(directorio = direccion, nombre_guardado = guardado){
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
  base = resumen(directorio = directorio, nombre_guardado = nombre_guardado)
  balassa_matriz = balassa_index(data = base, country = "cve_ent", product = "codigo_act", value = "suma_per_ocu")
  pro = proximity(balassa_matriz)
  net = projections(pro$proximity_country, pro$proximity_product)
  
  pesos_grafo = base |> dplyr::group_by(cve_ent) |>
    dplyr::summarise(peso_nodo1 = sum(suma_per_ocu, na.rm = TRUE))
  
  pais = net$network_country
  peso_aristas = edge_attr(pais, "weight")
  conexiones = as_edgelist(pais)
  interes = as.data.frame(cbind(conexiones,peso_aristas ))
  colnames(interes) = c("nodo1", "nodo2", "peso_arista")
  
  p = merge(x = interes, y = pesos_grafo, by.x = "nodo1", by.y = "cve_ent")
  colnames(pesos_grafo)[2] = "peso_nodo2"
  
  p = merge(x = p, y = pesos_grafo, by.x = "nodo2", by.y = "cve_ent")
  p = p[, c(2,1,3:5)]
  
  p = p |>
    dplyr::mutate(nodo1 = as.numeric(nodo1), nodo2 = as.numeric(nodo2)) |>
    dplyr::arrange(nodo1,nodo2)
  return(p)
}

setwd("C:/Users/eagel/OneDrive/Escritorio/Lalo/Escuela/Practicas/Chamba/")
archivos = list.files(path = "Febrero/Cosas Realizadas/Resumen/Nacional/", recursive = TRUE, full.names = TRUE)
archivos_csv = grep("\\.csv$", archivos, value = TRUE)
archivos_csv = subset(archivos_csv, !grepl("diccionario", archivos_csv))
g = basename(archivos_csv)

for (i in seq_along(g)) {
  cat("Vamos en ", g[i], " faltan ", length(g) - i, "\n")
  p = funcion_grafos(directorio = archivos_csv[i], nombre_guardado = g[i])
  write.csv(p, paste0("Febrero/Cosas Realizadas/28 febrero/Grafica Espacio Producto Entidades/", "grafo_", g[i]), row.names = F, fileEncoding = "UTF-8")
}
