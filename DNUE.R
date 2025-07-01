library(archive)
setwd("C:/Users/eagel/OneDrive/Escritorio/Lalo/Escuela/Practicas/Chamba/Febrero/Archivos Utilizados/4 febrero/")

archivos = list.files(path = "2024B/", recursive = TRUE, full.names = TRUE)
archivos_zip = grep("\\.zip$", archivos, value = TRUE)

library(archive)

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

# Descomprimir 
for (i in archivos_zip) {
  extraer_archivos(archivo_rar = i, destino_base = "2024B/Descomprimidos/")
}

archivos_descomprimidos = list.files(path = "2024B/Descomprimidos/", recursive = TRUE, full.names = TRUE)
archivos_csv = grep("\\.csv$", archivos_descomprimidos, value = TRUE)
archivos_csv = subset(archivos_csv, !grepl("denue_diccionario_de_datos", archivos_csv))

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
}


union = do.call(rbind, lista_csv)
write.csv(union, file = "2024B/Descomprimidos/Todos.csv", fileEncoding = "latin1")


media_trabajadores=function(str){
  return(switch(str,
                "0 a 5 personas"=3,
                "6 a 10 personas" = 8,
                "11 a 30 personas" = 20,
                "51 a 100 personas" = 75,
                "31 a 50 personas" = 40,
                "101 a 250 personas" = 175,
                "251 y más personas" = 250
  ))
}

union$per_ocu_media = sapply(union$per_ocu,media_trabajadores,simplify = T,USE.NAMES = F)

library(dplyr)

resumen = union |>
  group_by(cve_mun, cve_loc, codigo_act) |>
  summarise(suma_final = sum(as.numeric(per_ocu_media), na.rm = TRUE))


write.csv(x = resumen, file = "../../Cosas Realizadas/Resumen/2024B.csv", fileEncoding = "latin1")


            