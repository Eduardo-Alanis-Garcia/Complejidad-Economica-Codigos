# Funcion Balassa
balassa = function(directorio = direccion, nombre_guardado = guardado, digitos = digitos){
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
    
    return(M)
  }


setwd("C:/Users/eagel/OneDrive/Escritorio/Lalo/Escuela/Practicas/Chamba/")

archivos = list.files(path = "Febrero/Cosas Realizadas/Resumen/Nacional/", recursive = TRUE, full.names = TRUE)
archivos_csv = grep("\\.csv$", archivos, value = TRUE)
archivos_csv = subset(archivos_csv, !grepl("diccionario", archivos_csv))
g = basename(archivos_csv)

M = balassa(directorio = archivos_csv[18], nombre_guardado = g[18], digitos = 6)
M = as.matrix(M)

diversidad = rowSums(M)
ubicuidad = colSums(M)

especializado = (1/diversidad)*(M%*%ubicuidad)

datos = cbind(c(1:nrow(M)), diversidad, especializado)
colnames(datos) = c("estado", "diversidad", "especializado" )



plot(diversidad, especializado)
abline(h= mean(especializado), col="red", lwd=2)  # Línea sobre el eje X en rojo
abline(v=mean(diversidad), col="red", lwd=2) 


plot(diversidad, especializado)
abline(h= median(especializado), col="red", lwd=2)  # Línea sobre el eje X en rojo
abline(v=median(diversidad), col="red", lwd=2) 
