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

a = net$network_country

p = data.frame(estado = 1, vecinos = as.character(unlist(a[[1]], use.names = F)))
for (i in 2:length(a)) {
  w = data.frame(estado = i, vecinos = as.character(unlist(a[[i]], use.names = F)))
  p = rbind(p, w)
}


p = p |>
  dplyr::group_by(estado) |>
  summarise(v = paste(vecinos, collapse = ", "))


write.csv(p, "Febrero/Cosas Realizadas/20 febrero/prueba.csv", row.names = F)

































































######### Muchos Grafos
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
    dplyr::summarise(suma_per_ocu = sum(per_ocu, na.rm = TRUE), conteo = sum(conteo, na.rm = TRUE))
  
  return(denue)
}
resumen_unidades = function(directorio = direccion, nombre_guardado = guardado){
  library(dplyr)
  library(stringr)
  library(economiccomplexity)
  
  ### Prepara base
  denue = read.csv(directorio) |>
    dplyr::group_by(cve_ent) |>
    dplyr::summarise(!!paste0("unidades_", sub(pattern = ".csv$", replacement = "", x = nombre_guardado)) := sum(conteo, na.rm = TRUE))
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


setwd("C:/Users/eagel/OneDrive/Escritorio/Lalo/Escuela/Practicas/Chamba/")
archivos = list.files(path = "Febrero/Cosas Realizadas/Resumen/Nacional/", recursive = TRUE, full.names = TRUE)
archivos_csv = grep("\\.csv$", archivos, value = TRUE)
archivos_csv = subset(archivos_csv, !grepl("diccionario", archivos_csv))
g = basename(archivos_csv)

for (i in seq_along(g)) {
  base = resumen(directorio = archivos_csv[i], nombre_guardado = g[i])
  bi = balassa(directorio = archivos_csv[i], nombre_guardado = g[i])
  pro = proximity(bi)
  net = projections(pro$proximity_country, pro$proximity_product)
  a = net$network_country
  
  library(economiccomplexity)
  base2 = resumen_unidades(directorio = archivos_csv[i], nombre_guardado = g[i])
  bi_base2 = balassa_index(data = base, country = "cve_ent", product = "codigo_act", value = "conteo")
  pro_base2 = proximity(bi_base2)
  net_base2 = projections(pro_base2$proximity_country, pro_base2$proximity_product)
  a_base2 = net_base2$network_country
  
  
  base = base |> dplyr::group_by(cve_ent) |>
    dplyr::summarise(!!paste0("personal_ocupado_", sub(pattern = ".csv$", replacement = "", x = g[i])) := sum(suma_per_ocu, na.rm = TRUE))
  
  p = data.frame(estado = 1, vecinos = as.character(unlist(a[[1]], use.names = F)))
  p_base2 = data.frame(estado = 1, vecinos = as.character(unlist(a_base2[[1]], use.names = F)))
  for (j in 2:length(a)) {
    w = data.frame(estado = j, vecinos = as.character(unlist(a[[j]], use.names = F)))
    p = rbind(p, w)
    
    w_base2 = data.frame(estado = j, vecinos = as.character(unlist(a_base2[[j]], use.names = F)))
    p_base2 = rbind(p_base2, w_base2)
  }
  
  
  p = p |>
    dplyr::group_by(estado) |>
    summarise(!!paste0("vecinos_personal_", sub(pattern = ".csv$", replacement = "", x = g[i])) := paste(vecinos, collapse = ", "))
  
  p_base2 = p_base2 |>
    dplyr::group_by(estado) |>
    summarise(!!paste0("vecinos_unidades_", sub(pattern = ".csv$", replacement = "", x = g[i])) := paste(vecinos, collapse = ", "))
  
  p = merge(x = p, y = base, by.x = "estado", by.y = "cve_ent")
  p_base2 = merge(x = p_base2, y = base2, by.x = "estado", by.y = "cve_ent")
  
  p = merge(x = p, y = p_base2, by = "estado")
  
  
  write.csv(p, paste0("Febrero/Cosas Realizadas/20 febrero/Personal Ocupado/", g[i]), row.names = F, fileEncoding = "UTF-8")
}




























###############
### Juntar ####
###############


setwd("C:/Users/eagel/OneDrive/Escritorio/Lalo/Escuela/Practicas/Chamba/")
archivos = list.files(path = "Febrero/Cosas Realizadas/20 febrero/Personal Ocupado/", recursive = TRUE, full.names = TRUE)
archivos_csv = grep("\\.csv$", archivos, value = TRUE)



lista = vector("list", length(archivos_csv))

for (i in seq_along(archivos_csv)) {
  lista[[i]] = read.csv(file = archivos_csv[i])
}


product_grafo = Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "estado", all = TRUE),
                          lista)

write.csv(product_grafo, "Febrero/Cosas Realizadas/20 febrero/Personal Ocupado/personal_unidades.csv", fileEncoding = "UTF-8", row.names = F)




  