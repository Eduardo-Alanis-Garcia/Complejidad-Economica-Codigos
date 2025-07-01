# Funcion de diversidad vs ubicuidad
dvsu = function(directorio = direccion, nombre_guardado = guardado, digitos = digitos){
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
  
  M = economiccomplexity::balassa_index(data = denue, country = "cve_ent", product = "codigo_act", value = "suma_per_ocu" )
  
  M = as.matrix(M)
  
  diversidad = rowSums(M)
  ubicuidad = colSums(M)
  
  especializado = (1/diversidad)*(M%*%ubicuidad)
  
  datos_grafica = cbind(c(1:nrow(M)), diversidad, especializado)
  colnames(datos_grafica) = c("estado", "diversidad", "especializado" )
  return(datos_grafica)
}

setwd("C:/Users/eagel/OneDrive/Escritorio/Lalo/Escuela/Practicas/Chamba/")
archivos = list.files(path = "Febrero/Cosas Realizadas/Resumen/Nacional/", recursive = TRUE, full.names = TRUE)
archivos_csv = grep("\\.csv$", archivos, value = TRUE)
archivos_csv = subset(archivos_csv, !grepl("diccionario", archivos_csv))
g = basename(archivos_csv)

### seis

guardar = dvsu(directorio = archivos_csv[1], nombre_guardado = g[1], digitos = 6)
colnames(guardar)[c(2,3)] = paste0(colnames(guardar)[c(2,3)],"_",sub(pattern = ".csv", replacement = "", x = g[1]))

for (i in 2:length(g)) {
  cat("Vamos en ", g[i], " faltan ", length(g) - i)
  diver = dvsu(directorio = archivos_csv[i], nombre_guardado = g[i], digitos = 6)
  colnames(diver)[c(2,3)] = paste0(colnames(diver)[c(2,3)],"_",sub(pattern = ".csv", replacement = "", x = g[i]))
  guardar = merge(x = guardar, y = diver, by = "estado")
}

write.csv(guardar, "Febrero/Cosas Realizadas/27 febrero/diversidad_vs_ubicuidad/dive_vs_ubi_6.csv", row.names = F, fileEncoding = "UTF-8")



### cinco

guardar = dvsu(directorio = archivos_csv[1], nombre_guardado = g[1], digitos = 5)
colnames(guardar)[c(2,3)] = paste0(colnames(guardar)[c(2,3)],"_",sub(pattern = ".csv", replacement = "", x = g[1]))

for (i in 2:length(g)) {
  cat("Vamos en ", g[i], " faltan ", length(g) - i)
  diver = dvsu(directorio = archivos_csv[i], nombre_guardado = g[i], digitos = 5)
  colnames(diver)[c(2,3)] = paste0(colnames(diver)[c(2,3)],"_",sub(pattern = ".csv", replacement = "", x = g[i]))
  guardar = merge(x = guardar, y = diver, by = "estado")
}

write.csv(guardar, "Febrero/Cosas Realizadas/27 febrero/diversidad_vs_ubicuidad/dive_vs_ubi_5.csv", row.names = F, fileEncoding = "UTF-8")


### cuatro

guardar = dvsu(directorio = archivos_csv[1], nombre_guardado = g[1], digitos = 4)
colnames(guardar)[c(2,3)] = paste0(colnames(guardar)[c(2,3)],"_",sub(pattern = ".csv", replacement = "", x = g[1]))

for (i in 2:length(g)) {
  cat("Vamos en ", g[i], " faltan ", length(g) - i)
  diver = dvsu(directorio = archivos_csv[i], nombre_guardado = g[i], digitos = 4)
  colnames(diver)[c(2,3)] = paste0(colnames(diver)[c(2,3)],"_",sub(pattern = ".csv", replacement = "", x = g[i]))
  guardar = merge(x = guardar, y = diver, by = "estado")
}

write.csv(guardar, "Febrero/Cosas Realizadas/27 febrero/diversidad_vs_ubicuidad/dive_vs_ubi_4.csv", row.names = F, fileEncoding = "UTF-8")

### Tres

guardar = dvsu(directorio = archivos_csv[1], nombre_guardado = g[1], digitos = 3)
colnames(guardar)[c(2,3)] = paste0(colnames(guardar)[c(2,3)],"_",sub(pattern = ".csv", replacement = "", x = g[1]))

for (i in 2:length(g)) {
  cat("Vamos en ", g[i], " faltan ", length(g) - i)
  diver = dvsu(directorio = archivos_csv[i], nombre_guardado = g[i], digitos = 3)
  colnames(diver)[c(2,3)] = paste0(colnames(diver)[c(2,3)],"_",sub(pattern = ".csv", replacement = "", x = g[i]))
  guardar = merge(x = guardar, y = diver, by = "estado")
}

write.csv(guardar, "Febrero/Cosas Realizadas/27 febrero/diversidad_vs_ubicuidad/dive_vs_ubi_3.csv", row.names = F, fileEncoding = "UTF-8")




### Dos

guardar = dvsu(directorio = archivos_csv[1], nombre_guardado = g[1], digitos = 2)
colnames(guardar)[c(2,3)] = paste0(colnames(guardar)[c(2,3)],"_",sub(pattern = ".csv", replacement = "", x = g[1]))

for (i in 2:length(g)) {
  cat("Vamos en ", g[i], " faltan ", length(g) - i)
  diver = dvsu(directorio = archivos_csv[i], nombre_guardado = g[i], digitos = 2)
  colnames(diver)[c(2,3)] = paste0(colnames(diver)[c(2,3)],"_",sub(pattern = ".csv", replacement = "", x = g[i]))
  guardar = merge(x = guardar, y = diver, by = "estado")
}

write.csv(guardar, "Febrero/Cosas Realizadas/27 febrero/diversidad_vs_ubicuidad/dive_vs_ubi_2.csv", row.names = F, fileEncoding = "UTF-8")
