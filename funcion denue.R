library(archive)
setwd("C:/Users/eagel/OneDrive/Escritorio/Lalo/Escuela/Practicas/Chamba/Febrero/Archivos Utilizados/4 febrero/")

DNUE = function(direccion = "2024A/", descomprimido = "2024A/Descomprimidos/", guardar = "2024A.csv"){
  
archivos = list.files(path = direccion, recursive = TRUE, full.names = TRUE)
archivos_zip = grep("\\.zip$", archivos, value = TRUE)

library(archive)

### Funcion
extraer_archivos = function(archivo_rar, destino_base) {
  # Obtener el nombre del archivo sin extensión
  nombre_carpeta = tools::file_path_sans_ext(basename(archivo_rar))
  
  # Crear la ruta destino con el nombre del archivo original
  destino_final = file.path(destino_base, nombre_carpeta)
  
  # Crear la carpeta destino si no existe
  if (!dir.exists(destino_final)) {
    dir.create(destino_final, recursive = TRUE)
  }
  
  # Extraer el archivo en la carpeta correspondiente
  archive_extract(archivo_rar, dir = destino_final)
}

### Descomprimir 
for (i in archivos_zip) {
  extraer_archivos(archivo_rar = i, destino_base = descomprimido)
}

### Filtrar archivos .csv
archivos_descomprimidos = list.files(path = descomprimido, recursive = TRUE, full.names = TRUE)
archivos_csv = grep("\\.csv$", archivos_descomprimidos, value = TRUE)
archivos_csv = subset(archivos_csv, !grepl("denue_diccionario_de_datos", archivos_csv))

### Leer archivos en una lista
lista_csv = list()   # Lista vacia
for (i in seq_along(archivos_csv)) {
  lista_csv[[i]] = read.csv(archivos_csv[i], fileEncoding = "latin1")
  print(i)
}

#Filtrar a Hidalgo
lista_csv = lapply(lista_csv, function(df) df[df$cve_ent == 13, ]) # Filtras a Hidalgo

# Agregar 0's a la izquierda
for (i in seq_along(lista_csv)) {
  lista_csv[[i]]$cve_mun = formatC(lista_csv[[i]]$cve_mun, width = 3, flag = "0")
  lista_csv[[i]]$cve_loc = formatC(lista_csv[[i]]$cve_loc, width = 4, flag = "0")
}

# Columna de clave
for (i in seq_along(lista_csv)) {
  CLAVE = paste0(lista_csv[[i]]$cve_mun,lista_csv[[i]]$cve_loc)
  lista_csv[[i]] = cbind(lista_csv[[i]], CLAVE)
  colnames(lista_csv[[i]]) = colnames(lista_csv[[1]])
}

# Juntar bases y guardar 
union = do.call(rbind, lista_csv)
write.csv(union, file = paste0(descomprimido, "Todos.csv"), fileEncoding = "latin1")


### Funcion de media
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
union$per_ocu_media = sapply(union$per_ocu,media_trabajadores,simplify = T,USE.NAMES = F)

### Summary
library(dplyr)
resumen = union |>
  group_by(cve_mun, cve_loc, codigo_act) |>
  summarise(suma_final = sum(as.numeric(unlist(per_ocu_media)), na.rm = TRUE))

# Guardar Summary
write.csv(x = resumen, file = paste0("../../Cosas Realizadas/Resumen/", guardar), fileEncoding = "latin1")

}

prueba = DNUE(direccion = "2015/", descomprimido = "2015/Descomprimidos/" , guardar = "2015.csv")




d = c("2017B/", "2017A/", "2016B/", "2016A/"  )
des = c("2017B/Descomprimidos/", "2017A/Descomprimidos/", "2016B/Descomprimidos/", "2016A/Descomprimidos/")
g = c("2017B.csv", "2017A.csv", "2016B.csv", "2016A.csv")

for (i in 1:4) {
  prueba = DNUE(direccion = d[i], descomprimido = des[i] , guardar = g[i])
}















































#### Funcion para todos

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

library(archive)
library(dplyr)
setwd("C:/Users/eagel/OneDrive/Escritorio/Lalo/Escuela/Practicas/Chamba/Febrero/Archivos Utilizados/")

archivos = list.files(path = "4 febrero/", recursive = TRUE, full.names = TRUE)
archivos_csv = grep("\\.csv$", archivos, value = TRUE)
archivos_csv = subset(archivos_csv, grepl("Todos.csv", archivos_csv))
archivos_csv = archivos_csv[-1]

nombres = unlist(lapply(archivos_csv, function(x) substr(x, start =11, stop = 15)))

lista_csv = list()   # Lista vacia
for (i in seq_along(archivos_csv)) {
  print(archivos_csv[i])
  lista_csv[[i]] = read.csv(archivos_csv[i], fileEncoding = "latin1")
  lista_csv[[i]]$per_ocu_media = sapply(lista_csv[[i]]$per_ocu,media_trabajadores,simplify = T,USE.NAMES = F)
  
  resumen = lista_csv[[i]] |>
    group_by(cve_mun, cve_loc, codigo_act) |>
    summarise(
      suma_final = sum(as.numeric(unlist(per_ocu_media)), na.rm = TRUE),
      conteo = n()
    )
  write.csv(x = resumen, file = paste0("../Cosas Realizadas/Resumen/", nombres[i], ".csv"), fileEncoding = "latin1",row.names = F)
}

