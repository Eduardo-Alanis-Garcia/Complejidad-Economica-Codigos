#######################
### Nacional Entidad ##
#######################
library(stringr)
library(economiccomplexity)

directorio = archivos_csv[2]

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
nacional_entidad_ice_unidades = function(directorio = direccion, nombre_guardado = guardado){
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
    dplyr::group_by(cve_ent, codigo_act) |>
    dplyr::summarise(suma_unidades = sum(conteo, na.rm = TRUE))
  
  ### Complejidad Economica
  complejidad =  complexity_measures(balassa_index = balassa_index(data = denue, country = "cve_ent", product = "codigo_act", value = "suma_unidades"), 
                                     method = "eigenvalues" ) 
  ICE_NE = data.frame(names(complejidad$complexity_index_country), complejidad$complexity_index_country)
  colnames(ICE_NE) = c("CVEGEO", paste0("ICE_Unidades_", unlist(lapply(nombre_guardado, function(x) substr(x, start = 1, stop = 5)))))
  return(ICE_NE)
}

### Uso de la funcion



archivos = list.files(path = "Febrero/Cosas Realizadas/Resumen/Nacional/", recursive = TRUE, full.names = TRUE)
archivos_csv = grep("\\.csv$", archivos, value = TRUE)
archivos_csv = subset(archivos_csv, !grepl("diccionario", archivos_csv))
g = basename(archivos_csv)

ICE_NE_personal = nacional_entidad_ice_personal(directorio = archivos_csv[1], nombre_guardado = g[1])
todos_personal = ICE_NE_personal

ICE_NE_unidades = nacional_entidad_ice_unidades(directorio = archivos_csv[1], nombre_guardado = g[1])
todos_unidades = ICE_NE_unidades

for (i in 2:length(archivos_csv)) {
  cat("Se esta uniendo el archivo ", i, "con nombre", g[i])
  ICE_NE_personal = nacional_entidad_ice_personal(directorio = archivos_csv[i], nombre_guardado = g[i])
  todos_personal = merge(x = todos_personal, y = ICE_NE_personal, by = "CVEGEO")
  
  ICE_NE_unidades = nacional_entidad_ice_unidades(directorio = archivos_csv[i], nombre_guardado = g[i])
  todos_unidades = merge(x = todos_unidades, y = ICE_NE_unidades, by = "CVEGEO")
}

nacional = merge(x = todos_personal, y = todos_unidades, by = "CVEGEO")
nacional = nacional[order(as.numeric(nacional$CVEGEO), decreasing = F),]

write.csv(nacional, file = "../Cosas Realizadas/7 febrero/nacional.csv", fileEncoding = "UTF-8")









#########################
### Estatal Municipal ###
#########################
setwd("C:/Users/eagel/OneDrive/Escritorio/Lalo/Escuela/Practicas/Chamba/")

library(dplyr)
library(stringr)
library(economiccomplexity)



estatal_ice_personal = function(directorio = direccion, nombre_guardado = guardado){
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
    dplyr::filter(cve_ent == 13) |>
    dplyr::mutate(per_ocu = ifelse(substr(per_ocu, 1, 3) == "251", 
                                   "251 y más personas", per_ocu)) |>
    dplyr::mutate(per_ocu = sapply(per_ocu, media_trabajadores, 
                                   simplify = TRUE, USE.NAMES = FALSE)*conteo) |>
    dplyr::mutate(cvegeo = paste0("13", sprintf("%03d", cve_mun))) |>
    dplyr::group_by(cvegeo, codigo_act) |>
    dplyr::summarise(suma_per_ocu = sum(per_ocu, na.rm = TRUE))
  
  ### Complejidad Economica
  complejidad =  complexity_measures(balassa_index = balassa_index(data = denue, country = "cvegeo", product = "codigo_act", value = "suma_per_ocu"), 
                                     method = "eigenvalues" ) 
  ICE_NE = data.frame(names(complejidad$complexity_index_country), complejidad$complexity_index_country)
  colnames(ICE_NE) = c("CVEGEO", paste0("ICE_Personal_", unlist(lapply(nombre_guardado, function(x) substr(x, start = 1, stop = 5)))))
  #todos = data.frame(Estado = names(complejidad$complexity_index_country))
  #todos = merge(x = todos, y = ICE_NE, by = "Estado")
  return(ICE_NE)
}
estatal_ice_unidades = function(directorio = direccion, nombre_guardado = guardado){
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
    dplyr::filter(cve_ent == 13) |>
    dplyr::mutate(cvegeo = paste0("13", sprintf("%03d", cve_mun))) |>
    dplyr::group_by(cvegeo, codigo_act) |>
    dplyr::summarise(suma_unidades = sum(conteo, na.rm = TRUE))
  
  ### Complejidad Economica
  complejidad =  complexity_measures(balassa_index = balassa_index(data = denue, country = "cvegeo", product = "codigo_act", value = "suma_unidades"), 
                                     method = "eigenvalues" ) 
  ICE_NE = data.frame(names(complejidad$complexity_index_country), complejidad$complexity_index_country)
  colnames(ICE_NE) = c("CVEGEO", paste0("ICE_Unidades_", unlist(lapply(nombre_guardado, function(x) substr(x, start = 1, stop = 5)))))
  return(ICE_NE)
}

### Uso de la funcion
archivos = list.files(path = "Febrero/Cosas Realizadas/Resumen/Nacional/", recursive = TRUE, full.names = TRUE)
archivos_csv = grep("\\.csv$", archivos, value = TRUE)
archivos_csv = subset(archivos_csv, !grepl("diccionario", archivos_csv))
g = basename(archivos_csv)

ICE_NE_personal = estatal_ice_personal(directorio = archivos_csv[1], nombre_guardado = g[1])
todos_personal = ICE_NE_personal

ICE_NE_unidades = estatal_ice_unidades(directorio = archivos_csv[1], nombre_guardado = g[1])
todos_unidades = ICE_NE_unidades

for (i in 2:length(archivos_csv)) {
  cat("Se esta uniendo el archivo ", i, "con nombre", g[i])
  ICE_NE_personal = estatal_ice_personal(directorio = archivos_csv[i], nombre_guardado = g[i])
  todos_personal = merge(x = todos_personal, y = ICE_NE_personal, by = "CVEGEO")
  
  ICE_NE_unidades = estatal_ice_unidades(directorio = archivos_csv[i], nombre_guardado = g[i])
  todos_unidades = merge(x = todos_unidades, y = ICE_NE_unidades, by = "CVEGEO")
}

estatal = merge(x = todos_personal, y = todos_unidades, by = "CVEGEO")
estatal = estatal[order(as.numeric(estatal$CVEGEO), decreasing = F),]

write.csv(estatal ,"Febrero/Cosas Realizadas/7 febrero/estatal.csv", fileEncoding = "UTF-8", row.names = F )

































































































### Prueba

library(readxl)
setwd("C:/Users/eagel/OneDrive/Escritorio/Lalo/Escuela/Practicas/Chamba/")
archivos = list.files(path = "Febrero/Cosas Realizadas/Resumen/Nacional/", recursive = TRUE, full.names = TRUE)
archivos_csv = grep("\\.csv$", archivos, value = TRUE)
archivos_csv = subset(archivos_csv, !grepl("diccionario", archivos_csv))
denue = read.csv(archivos_csv[18]) 

unique(substr(denue$codigo_act, start = 1, stop = 6))
unique(substr(denue$codigo_act, start = 1, stop = 5))
unique(substr(denue$codigo_act, start = 1, stop = 4))
unique(substr(denue$codigo_act, start = 1, stop = 3))


scian = read_excel("Febrero/Archivos Utilizados/17 febrero/Complexity/scian_2023_categorias_y_productos.xlsx", sheet = 2)

tres = read_excel("Febrero/Archivos Utilizados/17 febrero/Complexity/scian_2023_categorias_y_productos.xlsx", sheet = 3, col_names = T) 
colnames(tres) = tres[1,]
tres = tres[-1,]
tres = tres |> filter(!is.na(Código)) |>
  select(1, 2) 


cuatro = read_excel("Febrero/Archivos Utilizados/17 febrero/Complexity/scian_2023_categorias_y_productos.xlsx", sheet = 4, col_names = T)
colnames(cuatro) = cuatro[1,]
cuatro = cuatro[-1,]
cuatro = cuatro |> filter(!is.na(Código)) |>
  select(1, 2) 


cinco = read_excel("Febrero/Archivos Utilizados/17 febrero/Complexity/scian_2023_categorias_y_productos.xlsx", sheet = 5, col_names = T)
colnames(cinco) = cinco[1,]
cinco = cinco[-1,]
cinco = cinco |> filter(!is.na(Código)) |>
  select(1, 2) 


seis = read_excel("Febrero/Archivos Utilizados/17 febrero/Complexity/scian_2023_categorias_y_productos.xlsx", sheet = 6, col_names = T)
colnames(seis) = seis[1,]
seis = seis[-1,]
seis = seis |> filter(!is.na(Código)) |>
  select(1, 2) 



setwd("C:/Users/eagel/OneDrive/Escritorio/Lalo/Escuela/Practicas/Chamba/")

preparativo_afinidad = function(directorio = direccion, nombre_guardado = guardado){
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
    dplyr::mutate(codigo_act = substr(codigo_act, 1, 6)) |>
    dplyr::group_by(cve_ent, codigo_act) |>
    dplyr::summarise(suma_per_ocu = sum(per_ocu, na.rm = TRUE))
  
  ### Complejidad Economica
  complejidad =  complexity_measures(balassa_index = balassa_index(data = denue, country = "cve_ent", product = "codigo_act", value = "suma_per_ocu"), 
                                     method = "eigenvalues" ) 
  ICE_NE = data.frame(names(complejidad$complexity_index_product), complejidad$complexity_index_product)
  colnames(ICE_NE) = c("Codigo", paste0("product_personal_", unlist(lapply(nombre_guardado, function(x) substr(x, start = 1, stop = 5)))))
  return(ICE_NE)
}

archivos = list.files(path = "Febrero/Cosas Realizadas/Resumen/Nacional/", recursive = TRUE, full.names = TRUE)
archivos_csv = grep("\\.csv$", archivos, value = TRUE)
archivos_csv = subset(archivos_csv, !grepl("diccionario", archivos_csv))
g = basename(archivos_csv)


lista = vector("list", length(archivos_csv))

for (i in seq_along(archivos_csv)) {
  lista[[i]] = preparativo_afinidad(directorio = archivos_csv[i], nombre_guardado = g[i])
}


product_personal = Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Codigo", all = TRUE),
       lista)







preparativo_afinidad_unidades = function(directorio = direccion, nombre_guardado = guardado){
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
    dplyr::mutate(codigo_act = substr(codigo_act, 1, 6)) |>
    dplyr::group_by(cve_ent, codigo_act) |>
    dplyr::summarise(suma_unidades = sum(conteo, na.rm = TRUE))
  
  ### Complejidad Economica
  complejidad =  complexity_measures(balassa_index = balassa_index(data = denue, country = "cve_ent", product = "codigo_act", value = "suma_unidades"), 
                                     method = "eigenvalues" ) 
  ICE_NE = data.frame(names(complejidad$complexity_index_product), complejidad$complexity_index_product)
  colnames(ICE_NE) = c("Codigo", paste0("product_unidades_", unlist(lapply(nombre_guardado, function(x) substr(x, start = 1, stop = 5)))))
  return(ICE_NE)
}
lista2 = vector("list", length(archivos_csv))
for (i in seq_along(archivos_csv)) {
  lista2[[i]] = preparativo_afinidad_unidades(directorio = archivos_csv[i], nombre_guardado = g[i])
}

product_unidades = Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Codigo", all = TRUE),
                          lista2)


productos = merge(x = product_personal , y = product_unidades , by = "Codigo")
productos = merge(x = productos, y = seis, by.x = "Codigo", by.y = "Código", all.x = T )
colnames(productos)
productos = productos[, c(1, 38, 2:37)]


write.csv(productos, "Febrero/Cosas Realizadas/18 febrero/productos_nacional_seis.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(productos, "Febrero/Cosas Realizadas/18 febrero/productos_nacional_seisl1.csv", fileEncoding = "latin1", row.names = F)






































#######################
##### 5 digitos #######
#######################

preparativo_afinidad_personal = function(directorio = direccion, nombre_guardado = guardado, digitos = n){
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
  
  ### Complejidad Economica
  complejidad =  complexity_measures(balassa_index = balassa_index(data = denue, country = "cve_ent", product = "codigo_act", value = "suma_per_ocu"), 
                                     method = "eigenvalues" ) 
  ICE_NE = data.frame(names(complejidad$complexity_index_product), complejidad$complexity_index_product)
  colnames(ICE_NE) = c("Codigo", paste0("product_personal_", unlist(lapply(nombre_guardado, function(x) substr(x, start = 1, stop = 5)))))
  return(ICE_NE)
}
preparativo_afinidad_unidades = function(directorio = direccion, nombre_guardado = guardado, digitos = n){
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
    dplyr::mutate(codigo_act = substr(codigo_act, 1, digitos)) |>
    dplyr::group_by(cve_ent, codigo_act) |>
    dplyr::summarise(suma_unidades = sum(conteo, na.rm = TRUE))
  
  ### Complejidad Economica
  complejidad =  complexity_measures(balassa_index = balassa_index(data = denue, country = "cve_ent", product = "codigo_act", value = "suma_unidades"), 
                                     method = "eigenvalues" ) 
  ICE_NE = data.frame(names(complejidad$complexity_index_product), complejidad$complexity_index_product)
  colnames(ICE_NE) = c("Codigo", paste0("product_unidades_", unlist(lapply(nombre_guardado, function(x) substr(x, start = 1, stop = 5)))))
  return(ICE_NE)
}

archivos = list.files(path = "Febrero/Cosas Realizadas/Resumen/Nacional/", recursive = TRUE, full.names = TRUE)
archivos_csv = grep("\\.csv$", archivos, value = TRUE)
archivos_csv = subset(archivos_csv, !grepl("diccionario", archivos_csv))
g = basename(archivos_csv)

lista1 = vector("list", length(archivos_csv))
lista2 = vector("list", length(archivos_csv))

for (i in seq_along(archivos_csv)) {
  cat("Vamos en", i, "faltan", length(archivos_csv) - i, "\n")
  lista1[[i]] = preparativo_afinidad_personal(directorio = archivos_csv[i], nombre_guardado = g[i], digitos = 5)
  lista2[[i]] = preparativo_afinidad_unidades(directorio = archivos_csv[i], nombre_guardado = g[i], digitos = 5) 
}

product_personal = Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Codigo", all = TRUE),
                          lista1)

product_unidades = Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Codigo", all = TRUE),
                          lista2)


productos = merge(x = product_personal , y = product_unidades , by = "Codigo")

cinco = read_excel("Febrero/Archivos Utilizados/17 febrero/Complexity/scian_2023_categorias_y_productos.xlsx", sheet = 5, col_names = T)
colnames(cinco) = cinco[1,]
cinco = cinco[-1,]
cinco = cinco |> filter(!is.na(Código)) |>
  select(1, 2)

productos = merge(x = productos, y = cinco, by.x = "Codigo", by.y = "Código", all.x = T )
colnames(productos)
productos = productos[ , c(1, 38, 2:37) ]


write.csv(productos, "Febrero/Cosas Realizadas/18 febrero/productos_nacional_cinco.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(productos, "Febrero/Cosas Realizadas/18 febrero/productos_nacional_cincol1.csv", fileEncoding = "latin1", row.names = F)





##############
### Cuatro ###
##############

lista1 = vector("list", length(archivos_csv))
lista2 = vector("list", length(archivos_csv))

for (i in seq_along(archivos_csv)) {
  cat("Vamos en", i, "faltan", length(archivos_csv) - i, "\n")
  lista1[[i]] = preparativo_afinidad_personal(directorio = archivos_csv[i], nombre_guardado = g[i], digitos = 4)
  lista2[[i]] = preparativo_afinidad_unidades(directorio = archivos_csv[i], nombre_guardado = g[i], digitos = 4) 
}

product_personal = Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Codigo", all = TRUE),
                          lista1)

product_unidades = Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Codigo", all = TRUE),
                          lista2)


productos = merge(x = product_personal , y = product_unidades , by = "Codigo")


cuatro = read_excel("Febrero/Archivos Utilizados/17 febrero/Complexity/scian_2023_categorias_y_productos.xlsx", sheet = 4, col_names = T)
colnames(cuatro) = cuatro[1,]
cuatro = cuatro[-1,]
cuatro = cuatro |> filter(!is.na(Código)) |>
  select(1, 2) 

productos = merge(x = productos, y = cuatro, by.x = "Codigo", by.y = "Código", all.x = T )
colnames(productos)
productos = productos[ , c(1, 38, 2:37) ]

write.csv(productos, "Febrero/Cosas Realizadas/18 febrero/productos_nacional_cuatro.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(productos, "Febrero/Cosas Realizadas/18 febrero/productos_nacional_cuatrol1.csv", fileEncoding = "latin1", row.names = F)



############
### Tres ###
############

lista1 = vector("list", length(archivos_csv))
lista2 = vector("list", length(archivos_csv))

for (i in seq_along(archivos_csv)) {
  cat("Vamos en", i, "faltan", length(archivos_csv) - i, "\n")
  lista1[[i]] = preparativo_afinidad_personal(directorio = archivos_csv[i], nombre_guardado = g[i], digitos = 3)
  lista2[[i]] = preparativo_afinidad_unidades(directorio = archivos_csv[i], nombre_guardado = g[i], digitos = 3) 
}

product_personal = Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Codigo", all = TRUE),
                          lista1)

product_unidades = Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Codigo", all = TRUE),
                          lista2)

productos = merge(x = product_personal , y = product_unidades , by = "Codigo")

tres = read_excel("Febrero/Archivos Utilizados/17 febrero/Complexity/scian_2023_categorias_y_productos.xlsx", sheet = 3, col_names = T) 
colnames(tres) = tres[1,]
tres = tres[-1,]
tres = tres |> filter(!is.na(Código)) |>
  select(1, 2) 

productos = merge(x = productos, y = tres, by.x = "Codigo", by.y = "Código", all.x = T )
colnames(productos)
productos = productos[ , c(1, 38, 2:37) ]

write.csv(productos, "Febrero/Cosas Realizadas/18 febrero/productos_nacional_tres.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(productos, "Febrero/Cosas Realizadas/18 febrero/productos_nacional_tresl1.csv", fileEncoding = "latin1", row.names = F)



###########
### Dos ###
###########

lista1 = vector("list", length(archivos_csv))
lista2 = vector("list", length(archivos_csv))

for (i in seq_along(archivos_csv)) {
  cat("Vamos en", i, "faltan", length(archivos_csv) - i, "\n")
  lista1[[i]] = preparativo_afinidad_personal(directorio = archivos_csv[i], nombre_guardado = g[i], digitos = 2)
  lista2[[i]] = preparativo_afinidad_unidades(directorio = archivos_csv[i], nombre_guardado = g[i], digitos = 2) 
}

product_personal = Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Codigo", all = TRUE),
                          lista1)

product_unidades = Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Codigo", all = TRUE),
                          lista2)

productos = merge(x = product_personal , y = product_unidades , by = "Codigo")

dos = read_excel("Febrero/Archivos Utilizados/17 febrero/Complexity/scian_2023_categorias_y_productos.xlsx", sheet = 2, col_names = T) 
colnames(dos) = dos[1,]
dos = dos[-1,]
dos = dos |> filter(!is.na(Código)) |>
  select(1, 2) 

productos = merge(x = productos, y = dos, by.x = "Codigo", by.y = "Código", all.x = T )
colnames(productos)
productos = productos[ , c(1, 38, 2:37) ]

a = which(as.numeric(productos$Codigo) %in% c(48:49))
productos$Título[a] = "Transportes, correos y almacenamiento"

write.csv(productos, "Febrero/Cosas Realizadas/18 febrero/productos_nacional_dos.csv", fileEncoding = "UTF-8", row.names = F)
write.csv(productos, "Febrero/Cosas Realizadas/18 febrero/productos_nacional_dosl1.csv", fileEncoding = "latin1", row.names = F)



















