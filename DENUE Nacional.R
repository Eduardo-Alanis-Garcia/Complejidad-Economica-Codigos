library(archive)
setwd("C:/Users/eagel/OneDrive/Escritorio/Lalo/Escuela/Practicas/Chamba/Febrero/Archivos Utilizados/4 febrero/")


DNUE_Nacional = function(direccion = "2024A/", descomprimido = "2024A/Descomprimidos/", guardar = "2024A.csv"){
  
  archivos = list.files(path = direccion, recursive = TRUE, full.names = TRUE)
  archivos_zip = grep("\\.zip$", archivos, value = TRUE)
  
  library(archive)
  library(stringr)
  
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
  archivos_csv = subset(archivos_csv, !grepl("resumen", archivos_csv))
  archivos_csv = subset(archivos_csv, !grepl("Todos", archivos_csv))
  
  ### Leer archivos en una lista
  lista_csv = list()   # Lista vacia
  colm = c("cve_ent", "cve_mun", "codigo_act", "nombre_act", "per_ocu")
  for (i in seq_along(archivos_csv)) {
    cat("Abriendo csv", i, "Direccion:", archivos_csv[i], "\n")
    lista_csv[[i]] = read.csv(archivos_csv[i], fileEncoding = "latin1")
    colnames(lista_csv[[i]]) = str_to_lower(colnames(lista_csv[[i]]))
    colnames(lista_csv[[i]]) = str_squish(colnames(lista_csv[[i]]))
    lista_csv[[i]] = lista_csv[[i]][,colm] |>
    dplyr::mutate(per_ocu = ifelse(substr(per_ocu, 1, 3) == "251", "251 y más personas", per_ocu))
  }
  
  # Juntar bases
  union = do.call(rbind, lista_csv)
  diccionario = union[, c(3,4)]
  diccionario = unique(diccionario)
  
  ### Summary
  library(dplyr)
  resumen = union |>
    group_by(cve_ent, cve_mun, codigo_act, per_ocu) |>
    summarise(conteo = n())
  
  # Guardar Summary
  write.csv(x = resumen, file = paste0("../../Cosas Realizadas/Resumen/Nacional/", guardar), fileEncoding = "latin1", row.names = F)
  write.csv(x = diccionario, file = paste0("../../Cosas Realizadas/Resumen/Nacional/diccionario_", guardar), fileEncoding = "latin1", row.names = F)
}

d = c("2024B/","2024A/", "2023B/", "2022B/", "2022A/","2021B/", "2021A/", "2020B/", "2020A/"   ,"2019B/", "2019A/", "2018B/", "2018A/","2017B/", "2017A/", "2016B/", "2016A/")
des = c("2024B/Descomprimidos/","2024A/Descomprimidos/", "2023B/Descomprimidos/", "2022B/Descomprimidos/", "2022A/Descomprimidos/","2021B/Descomprimidos/", "2021A/Descomprimidos/", "2020B/Descomprimidos/", "2020A/Descomprimidos/"   ,"2019B/Descomprimidos/", "2019A/Descomprimidos/", "2018B/Descomprimidos/", "2018A/Descomprimidos/","2017B/Descomprimidos/", "2017A/Descomprimidos/", "2016B/Descomprimidos/", "2016A/Descomprimidos/")
g = c("2024B.csv","2024A.csv", "2023B.csv", "2022B.csv", "2022A.csv","2021B.csv", "2021A.csv", "2020B.csv", "2020A.csv"   ,"2019B.csv", "2019A.csv", "2018B.csv", "2018A.csv","2017B.csv", "2017A.csv", "2016B.csv", "2016A.csv")

for (i in 1:length(d)) {
  prueba = DNUE_Nacional(direccion = d[i], descomprimido = des[i] , guardar = g[i])
}




#########################################
###### Correcion de diccionarios ########
#########################################


DNUE_Nacional = function(direccion = "2024A/", descomprimido = "2024A/Descomprimidos/", guardar = "2024A.csv"){
  
  archivos = list.files(path = direccion, recursive = TRUE, full.names = TRUE)
  archivos_zip = grep("\\.zip$", archivos, value = TRUE)
  
  library(archive)
  library(stringr)
  
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
  archivos_csv = subset(archivos_csv, !grepl("resumen", archivos_csv))
  archivos_csv = subset(archivos_csv, !grepl("Todos", archivos_csv))
  
  ### Leer archivos en una lista
  lista_csv = list()   # Lista vacia
  colm = c("cve_ent", "cve_mun", "codigo_act", "nombre_act", "per_ocu")
  for (i in seq_along(archivos_csv)) {
    cat("Abriendo csv", i, "Direccion:", archivos_csv[i], "\n")
    lista_csv[[i]] = read.csv(archivos_csv[i], fileEncoding = "UTF-8", check.names = F )
    colnames(lista_csv[[i]]) = str_to_lower(colnames(lista_csv[[i]]))
    colnames(lista_csv[[i]]) = str_squish(colnames(lista_csv[[i]]))
    lista_csv[[i]] = lista_csv[[i]][,colm] |>
      dplyr::mutate(per_ocu = ifelse(substr(per_ocu, 1, 3) == "251", "251 y más personas", per_ocu))
  }
  
  # Juntar bases
  union = do.call(rbind, lista_csv)
  diccionario = union[, c("codigo_act", "nombre_act")]
  diccionario = unique(diccionario)
  
  ### Summary
  library(dplyr)
  resumen = union |>
    group_by(cve_ent, cve_mun, codigo_act, per_ocu) |>
    summarise(conteo = n())
  
  # Guardar Summary
  write.csv(x = resumen, file = paste0("../../Cosas Realizadas/Resumen/Nacional/", guardar), fileEncoding = "UTF-8", row.names = F)
  write.csv(x = diccionario, file = paste0("../../Cosas Realizadas/Resumen/Nacional/diccionario_", guardar), fileEncoding = "latin1", row.names = F)
}

d = c("2024B/","2024A/", "2023B/", "2022B/", "2022A/","2021B/", "2021A/", "2020B/", "2020A/"   ,"2019B/", "2019A/", "2018B/", "2018A/","2017B/", "2017A/", "2016B/", "2016A/")
des = c("2024B/Descomprimidos/","2024A/Descomprimidos/", "2023B/Descomprimidos/", "2022B/Descomprimidos/", "2022A/Descomprimidos/","2021B/Descomprimidos/", "2021A/Descomprimidos/", "2020B/Descomprimidos/", "2020A/Descomprimidos/"   ,"2019B/Descomprimidos/", "2019A/Descomprimidos/", "2018B/Descomprimidos/", "2018A/Descomprimidos/","2017B/Descomprimidos/", "2017A/Descomprimidos/", "2016B/Descomprimidos/", "2016A/Descomprimidos/")
g = c("2024B.csv","2024A.csv", "2023B.csv", "2022B.csv", "2022A.csv","2021B.csv", "2021A.csv", "2020B.csv", "2020A.csv"   ,"2019B.csv", "2019A.csv", "2018B.csv", "2018A.csv","2017B.csv", "2017A.csv", "2016B.csv", "2016A.csv")



for (i in 12:length(d)) {
  prueba = DNUE_Nacional(direccion = d[i], descomprimido = des[i] , guardar = g[i])
}










































#########################
###Caso Especial 2015 ###
#########################


DNUE_Nacional = function(direccion = "2024A/", descomprimido = "2024A/Descomprimidos/", guardar = "2024A.csv"){
  
  direccion = "2015/"
  descomprimido = "2015/Descomprimidos/"
  guardar = "2015.csv"
  
  
  archivos = list.files(path = direccion, recursive = TRUE, full.names = TRUE)
  archivos_zip = grep("\\.zip$", archivos, value = TRUE)
  
  library(archive)
  library(stringr)
  
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
  archivos_csv = subset(archivos_csv, !grepl("resumen", archivos_csv))
  archivos_csv = subset(archivos_csv, !grepl("Todos", archivos_csv))
  
  ### Leer archivos en una lista
  lista_csv = list()   # Lista vacia
  colm = c("clave.entidad", "clave.municipio", "clave.localidad","cã.digo.de.la.clase.de.actividad.scian", "nombre.de.clase.de.la.actividad", "descripcion.estrato.personal.ocupado")
  colm1 = c("cve_ent", "cve_mun", "cve_loc" ,"codigo_act", "nombre_act", "per_ocu")
  for (i in seq_along(archivos_csv)) {
    cat("Abriendo csv", i, "Direccion:", archivos_csv[i], "\n")
    lista_csv[[i]] = read.csv(archivos_csv[i], fileEncoding = "latin1")
    colnames(lista_csv[[i]]) = str_to_lower(colnames(lista_csv[[i]]))
    colnames(lista_csv[[i]]) = str_squish(colnames(lista_csv[[i]]))
    lista_csv[[i]] = lista_csv[[i]][,colm] |> 
      dplyr::mutate(per_ocu = ifelse(substr(descripcion.estrato.personal.ocupado, 1, 3) == "251", "251 y más personas", descripcion.estrato.personal.ocupado)) 
    colnames(lista_csv[[i]]) = colm1  
  }
  
  # Juntar bases
  union = do.call(rbind, lista_csv)
  union = union[, -6]
  colnames(union) = colm1
  diccionario = union[, c(4,5)]
  diccionario = unique(diccionario)
  
  ### Summary
  library(dplyr)
  resumen = union |>
    group_by(cve_ent, cve_mun, codigo_act, per_ocu) |>
    summarise(conteo = n())
  
  # Guardar Summary
  write.csv(x = resumen, file = paste0("../../Cosas Realizadas/Resumen/Nacional/", guardar), fileEncoding = "latin1", row.names = F)
  write.csv(x = diccionario, file = paste0("../../Cosas Realizadas/Resumen/Nacional/diccionario_", guardar), fileEncoding = "latin1", row.names = F)
}


prueba = DNUE_Nacional(direccion = "2015/" , descomprimido = "2015/Descomprimidos/" , guardar = "2015.csv")




################################
library(readr)



setwd("C:/Users/eagel/OneDrive/Escritorio/Lalo/Escuela/Practicas/Chamba/Febrero/Archivos Utilizados/4 febrero/")

DENUE_2016 = read.csv("2015/Descomprimidos/denue_00_11_25022015_csv/DENUE_INEGI_11_.csv", fileEncoding = "UTF-8", check.names = F)
nombres_columnas = colnames(DENUE_2016)





























