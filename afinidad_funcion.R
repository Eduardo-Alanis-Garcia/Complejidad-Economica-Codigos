setwd("C:/Users/eagel/OneDrive/Escritorio/Lalo/Escuela/Practicas/Chamba/")

library(economiccomplexity)

archivos = list.files(path = "Febrero/Cosas Realizadas/Resumen/Nacional/", recursive = TRUE, full.names = TRUE)
archivos_csv = grep("\\.csv$", archivos, value = TRUE)
archivos_csv = subset(archivos_csv, !grepl("diccionario", archivos_csv))
g = basename(archivos_csv)

### Dos digitos
afinidad = function(directorio, nombre_guardado, digitos){
  # Librerias ha utilizar
  library(economiccomplexity)
  library(dplyr)
  library(stringr)
  
  # Funcion para filtrar csv
  base_filtracion = function(directorio = direccion, nombre_guardado = guardado, digitos = digitos){
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
  
  # Abrir base
  datos = base_filtracion(directorio = directorio, nombre_guardado = nombre_guardado , digitos = digitos)
  
  # Calculo de balassa
  M = economiccomplexity::balassa_index(data = datos, country = "cve_ent", product = "codigo_act", value = "suma_per_ocu" )
  
  # Indice de complejidad producto
  complejidad =  complexity_measures(balassa_index = M, method = "eigenvalues" ) 
  complejidad_producto = data.frame(names(complejidad$complexity_index_product), complejidad$complexity_index_product)
  colnames(complejidad_producto) = c("codigo_act", "complejidad_producto")
  
  
  # Afinidad
  phi = proximity(M)
  phi_product = as.matrix(phi$proximity_product)
  I = diag(length(rowSums(as.matrix(phi_product))))*(1/rowSums(as.matrix(phi_product)))
  matriz_afinidad = (as.matrix(M) %*% t(as.matrix(phi_product))) %*% I
  colnames(matriz_afinidad) = colnames(phi_product)
  afinidad = cbind(colnames(matriz_afinidad) ,matriz_afinidad[13,])  # Afinidad de hidalgo
  colnames(afinidad) = c("codigo_act", "afinidad")
  
  
  # Personal por producto
  personal = datos |> dplyr::filter(cve_ent == 13) |>
    dplyr::group_by(codigo_act) |>
    dplyr::summarise(personal_ocupado = sum(suma_per_ocu))
  
  preparacion_afinidad = merge(x = complejidad_producto, y = afinidad, by.x = "codigo_act", by.y = "codigo_act" )
  preparacion_afinidad = merge(x = preparacion_afinidad, y = personal, by.x = "codigo_act", by.y = "codigo_act" )
  
  
  nombres = sub(x = nombre_guardado, pattern = ".csv", replacement = "")
  colnames(preparacion_afinidad)[c(2:ncol(preparacion_afinidad))] = paste0(colnames(preparacion_afinidad)[c(2:ncol(preparacion_afinidad))],"_",nombres)
  return(preparacion_afinidad)
}
affinity = afinidad(directorio = archivos_csv[1], nombre_guardado = g[1], digitos = 2)
for (i in 2:length(g)) {
  a = afinidad(directorio = archivos_csv[i], nombre_guardado = g[i], digitos = 2)
  cat("Va en el archivo ", nombre_guardado = g[i], " faltan ", length(g) - i, "donde las filas son", nrow(a) ,"\n" )
  affinity = merge(x = affinity, y = a, by = "codigo_act", all.x = T, all.y = T )
}

write.csv(affinity, "Febrero/Cosas Realizadas/25 febrero/afinidad/afinidad_dos.csv", row.names = F, fileEncoding = "UTF-8")


### Tres digitos
affinity = afinidad(directorio = archivos_csv[1], nombre_guardado = g[1], digitos = 3)
for (i in 2:length(g)) {
  a = afinidad(directorio = archivos_csv[i], nombre_guardado = g[i], digitos = 3)
  cat("Va en el archivo ", nombre_guardado = g[i], " faltan ", length(g) - i, "donde las filas son", nrow(a) ,"\n" )
  affinity = merge(x = affinity, y = a, by = "codigo_act", all.x = T, all.y = T )
}

write.csv(affinity, "Febrero/Cosas Realizadas/25 febrero/afinidad/afinidad_tres.csv", row.names = F, fileEncoding = "UTF-8")



### Cuatro digitos
affinity = afinidad(directorio = archivos_csv[1], nombre_guardado = g[1], digitos = 4)
for (i in 2:length(g)) {
  a = afinidad(directorio = archivos_csv[i], nombre_guardado = g[i], digitos = 4)
  cat("Va en el archivo ", nombre_guardado = g[i], " faltan ", length(g) - i, "donde las filas son", nrow(a) ,"\n" )
  affinity = merge(x = affinity, y = a, by = "codigo_act", all.x = T, all.y = T )
}

write.csv(affinity, "Febrero/Cosas Realizadas/25 febrero/afinidad/afinidad_cuatro.csv", row.names = F, fileEncoding = "UTF-8")



### Cinco digitos
affinity = afinidad(directorio = archivos_csv[1], nombre_guardado = g[1], digitos = 5)
for (i in 2:length(g)) {
  a = afinidad(directorio = archivos_csv[i], nombre_guardado = g[i], digitos = 5)
  cat("Va en el archivo ", nombre_guardado = g[i], " faltan ", length(g) - i, "donde las filas son", nrow(a) ,"\n" )
  affinity = merge(x = affinity, y = a, by = "codigo_act", all.x = T, all.y = T )
}

write.csv(affinity, "Febrero/Cosas Realizadas/25 febrero/afinidad/afinidad_cinco.csv", row.names = F, fileEncoding = "UTF-8")


### Seis digitos
affinity = afinidad(directorio = archivos_csv[1], nombre_guardado = g[1], digitos = 6)
for (i in 2:length(g)) {
  a = afinidad(directorio = archivos_csv[i], nombre_guardado = g[i], digitos = 6)
  cat("Va en el archivo ", nombre_guardado = g[i], " faltan ", length(g) - i, "donde las filas son", nrow(a) ,"\n" )
  affinity = merge(x = affinity, y = a, by = "codigo_act", all.x = T, all.y = T )
}

write.csv(affinity, "Febrero/Cosas Realizadas/25 febrero/afinidad/afinidad_seis.csv", row.names = F, fileEncoding = "UTF-8")





setwd("C:/Users/eagel/OneDrive/Escritorio/Lalo/Escuela/Practicas/Chamba/")

afinidad = read.csv("Febrero/Cosas Realizadas/25 febrero/afinidad/afinidad_dos.csv")
plot(afinidad$afinidad_2024B, afinidad$complejidad_producto_2024B)




