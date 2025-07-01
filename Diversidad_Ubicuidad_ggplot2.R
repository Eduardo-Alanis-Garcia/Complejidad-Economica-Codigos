setwd("C:/Users/eagel/OneDrive/Escritorio/Lalo/Escuela/Gob/Proyectos/")

datos = sf::read_sf("../Importantes_documentos_usar/Municipios/municipiosjair.shp")

datos = datos |>  sf::st_drop_geometry() |> 
  dplyr::select(CVE_MUN,NOM_MUN) |> 
  dplyr::mutate(CVE_MUN = as.numeric(CVE_MUN),
                Ubicuidad = sample(x = 1:50, size = 84, replace = T),
                Diversidad = sample(x = 1:50, size = 84, replace = T)
                )


plot(x = datos$Diversidad, y = datos$Ubicuidad)

library(ggplot2)

grafica = ggplot(data = datos, aes(x = Diversidad, y = Ubicuidad)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Diversidad", y = "Ubicuidad") +
  geom_vline(xintercept = mean(datos$Diversidad), linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = mean(datos$Ubicuidad), linetype = "dashed", color = "red", size = 1)

plot(grafica)
