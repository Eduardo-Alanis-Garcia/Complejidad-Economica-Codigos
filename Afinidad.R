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

M = balassa(directorio = archivos_csv[18], nombre_guardado = g[18])
phi = proximity(M)
phi_i = phi$proximity_product
cat("La dimension de M es ", nrow(M), " x ", ncol(M))
cat("La dimension de Phi es ", nrow(phi_i), " x ", ncol(phi_i))


w11 = (M[1,] %*% phi_i[1,])/sum(phi_i[1,])


matriz_vacia = matrix(NA, nrow = nrow(M), ncol = ncol(phi_i))


for (i in 1:nrow(matriz_vacia)) {
  for (j in 1:ncol(matriz_vacia)) {
    matriz_vacia[i,j] = (M[i,] %*% phi_i[j,])/sum(phi_i[j,])
  }
}

I = diag(length(rowSums(as.matrix(phi_i))))*(1/rowSums(as.matrix(phi_i)))
matriz_vacia_2 = (as.matrix(M) %*% t(as.matrix(phi_i))) %*% I

















### Prueba de grafica de afinidad


base_fun = function(directorio = direccion, nombre_guardado = guardado, digitos = n){
  library(economiccomplexity)
  library(dplyr)
  library(stringr)
  
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
  denue =  read.csv(directorio) |>
    dplyr::mutate(per_ocu = ifelse(substr(per_ocu, 1, 3) == "251", 
                                   "251 y más personas", per_ocu)) |>
    dplyr::mutate(per_ocu = sapply(per_ocu, media_trabajadores, 
                                   simplify = TRUE, USE.NAMES = FALSE)*conteo) |>
    dplyr::mutate(codigo_act = substr(codigo_act, 1, digitos)) |>
    dplyr::group_by(cve_ent, codigo_act) |>
    dplyr::summarise(suma_per_ocu = sum(per_ocu, na.rm = TRUE))
  return(denue)
}
balassa_afinidad = function(directorio = direccion, nombre_guardado = guardado, digitos = n){
  library(economiccomplexity)
  library(dplyr)
  library(stringr)
  
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
  denue =  read.csv(directorio) |>
    dplyr::mutate(per_ocu = ifelse(substr(per_ocu, 1, 3) == "251", 
                                   "251 y más personas", per_ocu)) |>
    dplyr::mutate(per_ocu = sapply(per_ocu, media_trabajadores, 
                                   simplify = TRUE, USE.NAMES = FALSE)*conteo) |>
    dplyr::mutate(codigo_act = substr(codigo_act, 1, digitos)) |>
    dplyr::group_by(cve_ent, codigo_act) |>
    dplyr::summarise(suma_per_ocu = sum(per_ocu, na.rm = TRUE))
  
  ### 
  b = balassa_index(data = denue, country = "cve_ent", product = "codigo_act", value = "suma_per_ocu")
  
  return(b)
}


base = base_fun(directorio = archivos_csv[18], nombre_guardado = g[18] , digitos = 2)
base = base |> 
  dplyr::group_by(codigo_act) |>
  dplyr::summarise(personal = sum(suma_per_ocu))

# Balassa
M = balassa_afinidad(directorio = archivos_csv[18], nombre_guardado = g[18] , digitos = 2)

# Indice de complejidad producto
complejidad =  complexity_measures(balassa_index = M, method = "eigenvalues" ) 
complejidad_producto = data.frame(names(complejidad$complexity_index_product), complejidad$complexity_index_product)
colnames(complejidad_producto) = c("Codigo", "Complejidad Producto")


# Afinidad
phi = proximity(M)
phi_i = as.matrix(phi$proximity_product)
I = diag(length(rowSums(as.matrix(phi_i))))*(1/rowSums(as.matrix(phi_i)))
matriz_afinidad = (as.matrix(M) %*% t(as.matrix(phi_i))) %*% I
colnames(matriz_afinidad) = colnames(phi_i)
afinidad = matriz_afinidad[13,]


plot(afinidad, complejidad_producto$`Complejidad Producto`, xlab = "Afinidad", ylab = "Complejidad de Producto", main = "Afinidad y complejidad del producto en Hidalgo(Semestre 2 2024)")










