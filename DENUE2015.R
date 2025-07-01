################
### Nacional ###
################

setwd("C:/Users/eagel/OneDrive/Escritorio/Lalo/Escuela/Practicas/Chamba/Febrero/Archivos Utilizados/4 febrero/")

DENUE_2015 = read.csv("2015/Descomprimidos/denue_02_csv/conjunto_de_datos/denue_inegi_02_.csv", fileEncoding = "latin1", check.names = F)

  
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
  archivos_csv = subset(archivos_csv, grepl("DENUE_INEGI", archivos_csv))
  
  
  ### Leer archivos en una lista
  lista_csv = list()   # Lista vacia
  colm = c("clave entidad", "clave municipio", "clave localidad","codigo de la clase de actividad scian", "nombre de clase de la actividad", "descripcion estrato personal ocupado")
  colm1 = c("cve_ent", "cve_mun", "cve_loc" ,"codigo_act", "nombre_act", "per_ocu")
  for (i in seq_along(archivos_csv)) {
    cat("Abriendo csv", i, "Direccion:", archivos_csv[i], "\n")
    lista_csv[[i]] = read.csv(archivos_csv[i], fileEncoding = "UTF-8", check.names = F)
    colnames(lista_csv[[i]]) = str_to_lower(colnames(lista_csv[[i]]))
    colnames(lista_csv[[i]]) = str_squish(colnames(lista_csv[[i]]))
    colnames(lista_csv[[i]]) = iconv(colnames(lista_csv[[i]]), from = "UTF-8", to = "ASCII//TRANSLIT")
    lista_csv[[i]] = lista_csv[[i]][, colm] |> 
      dplyr::mutate(per_ocu = ifelse(substr(`descripcion estrato personal ocupado`, 1, 3) == "251", 
                                     "251 y más personas", `descripcion estrato personal ocupado`))
    lista_csv[[i]] = lista_csv[[i]][, -6]
    colnames(lista_csv[[i]]) = colm1
  }
  
  
  # Juntar bases
  union = do.call(rbind, lista_csv)
  colnames(union) = colm1
  diccionario = union[, c("codigo_act", "nombre_act")]
  diccionario = unique(diccionario)
  
  ### Summary
  library(dplyr)
  resumen = union |>
    group_by(cve_ent, cve_mun, codigo_act, per_ocu) |>
    summarise(conteo = n())
  
  # Guardar Summary
  write.csv(x = resumen, file = paste0("../../Cosas Realizadas/Resumen/Nacional/", guardar), fileEncoding = "latin1", row.names = F)
  write.csv(x = diccionario, file = paste0("../../Cosas Realizadas/Resumen/Nacional/diccionario_", guardar), fileEncoding = "latin1", row.names = F)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #################
  ### Localidad ###
  #################
  
  setwd("C:/Users/eagel/OneDrive/Escritorio/Lalo/Escuela/Practicas/Chamba/Febrero/Archivos Utilizados/4 febrero/")
  
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
  archivos_csv = subset(archivos_csv, grepl("DENUE_INEGI", archivos_csv))
  
  
  ### Leer archivos en una lista
  lista_csv = list()   # Lista vacia
  colm = c("clave entidad", "clave municipio", "clave localidad","codigo de la clase de actividad scian", "nombre de clase de la actividad", "descripcion estrato personal ocupado")
  colm1 = c("cve_ent", "cve_mun", "cve_loc" ,"codigo_act", "nombre_act", "per_ocu")
  for (i in seq_along(archivos_csv)) {
    cat("Abriendo csv", i, "Direccion:", archivos_csv[i], "\n")
    lista_csv[[i]] = read.csv(archivos_csv[i], fileEncoding = "UTF-8", check.names = F)
    colnames(lista_csv[[i]]) = str_to_lower(colnames(lista_csv[[i]]))
    colnames(lista_csv[[i]]) = str_squish(colnames(lista_csv[[i]]))
    colnames(lista_csv[[i]]) = iconv(colnames(lista_csv[[i]]), from = "UTF-8", to = "ASCII//TRANSLIT")
    lista_csv[[i]] = lista_csv[[i]][, colm] 
    colnames(lista_csv[[i]]) = colm1
    lista_csv[[i]] = lista_csv[[i]][lista_csv[[i]]$cve_ent == 13,]|> 
      dplyr::mutate(per_ocu = ifelse(substr(per_ocu, 1, 3) == "251", 
                                     "251 y más personas", per_ocu))
  }
  
  
  # Juntar bases
  union = do.call(rbind, lista_csv)
  diccionario = union[, c("codigo_act", "nombre_act")]
  diccionario = unique(diccionario)
  
  ### Summary
  library(dplyr)
  resumen = union |>
    group_by(cve_mun, cve_loc, codigo_act, per_ocu) |>
    summarise(conteo = n())
  
  # Guardar Summary
  write.csv(x = resumen, file = paste0("../../Cosas Realizadas/Resumen/Localidad/", guardar), fileEncoding = "latin1", row.names = F)
  write.csv(x = diccionario, file = paste0("../../Cosas Realizadas/Resumen/Localidad/diccionario_", guardar), fileEncoding = "latin1", row.names = F)
  
