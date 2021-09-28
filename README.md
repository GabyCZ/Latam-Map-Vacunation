# Latam-Map-Vacunation
#Importando librerías
library(sf)
library(purrr)
library(tidyverse)
library(ggplot2)
library(ggrepel)

#Estableciendo estructura
wd            <- list()
wd$root       <- paste0("C:/Users/gcairampoma/Desktop/Mapa")
wd$inputs     <- paste0(wd$root, "01_inputs/")
wd$shapef     <- paste0(wd$inputs, "shapefiles/")
wd$datasets   <- paste0(wd$inputs, "datasets/")
wd$outputs    <- paste0(wd$root, "02_outputs/")

#Importar archivos
latam_sf <- st_read(paste0(wd$shapef, "america del sur geogpsperu.shp"))

#Mapa Base
ggplot(data = latam_sf) +   geom_sf()

#Mapa con leyenda
latam_sf <- latam_sf %>% mutate(centroid = map(geometry, st_centroid), 
                              coords = map(centroid, st_coordinates), 
                              coords_x = map_dbl(coords, 1), coords_y = map_dbl(coords, 2))
#Y color
ggplot(data = latam_sf) +
  geom_sf(fill="skyblue3", color="black", alpha = 0.7)+ 
  geom_text_repel(mapping = aes(coords_x, coords_y, label = PAIS), size = 2)

#Importar base de datos
primera_dosis <- read_csv(paste0(wd$datasets,"Primera_dosis.csv"))
segunda_dosis <- read_csv(paste0(wd$datasets,"segunda_dosis.csv"))

#Los juntamos
latam_datos <- latam_sf %>%
  left_join(primera_dosis) %>%
  left_join(segunda_dosis)

####Gráficos finales
#Gráfico 1 dosis
ggplot(latam_datos) +
  geom_sf(aes(fill = 1_DOSIS))+
  labs(title = "Porcentaje de población con primera dosis 24/09/21",
       caption = "Fuente: Americas Society Council of the Americas\nElaboración propia",
       x="Longitud",
       y="Latitud",
       fill = "Tasa de vacunados con primera dosis")+
  scale_fill_gradient(low = "steelblue1", high = "steelblue4")+
  theme_bw()
ggsave(paste0(wd$outputs, "1dosismap1.png"))}

#Gráfico 2 dosis
ggplot(latam_datos) +
  geom_sf(aes(fill = 2_DOSIS))+
  labs(title = "Porcentaje de población con segunda dosis 24/09/21",
       caption = "Fuente: Americas Society Council of the Americas\nElaboración propia",
       x="Longitud",
       y="Latitud",
       fill = "Años de educación")+
  scale_fill_gradient(low = "darkseagreen1", high = "darkseagreen4")+
  theme_bw()
ggsave(paste0(wd$outputs, "2dosismap1.png"))

#Gráfico 1 y 2 dosis
ggplot(latam_datos) +
  geom_sf(aes(fill = 1_DOSIS))+
  scale_fill_gradient(low = "steelblue1", high = "steelblue4")+
  geom_point(aes(coords_x, coords_y, size = educ), color = "darkseagreen3")+
  labs(title = "Población completa y parcialmente vacunada en Latam",
       caption = "Fuente: Americas Society Council of the Americas\nElaboración propia",
       x="Longitud",
       y="Latitud",
       fill = "Parcialmente vacunada",
       size = "Completamente vacunada")+
  theme_bw()
ggsave(paste0(wd$outputs, "vacunationlatam.png"))




