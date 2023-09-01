library(readxl)
library(tidyverse)
library(magrittr)
library(dplyr)
library(haven)
library(janitor)
library(lubridate)
library(glue)
library(data.table)
library(comunicacion)
library(ggplot2)
library(ggrepel)
library(ggtext)
library(gt)
library(sf)



###Gráfico con el mapa de los puertos

arg <- read_sf("/srv/DataDNMYE/capas_sig/argentina.geojson")


aero <- read_sf("/srv/DataDNMYE/capas_sig/aeropuertos_anac.gpkg") %>% 
  filter(nombre %in% c("Aeropuerto Int. Ministro Pistarini",
                       "Aeroparque Jorge Newbery",
                       "Aeropuerto Int. El Plumerillo/Gob. Francisco Gabrielli",
                       "Aeropuerto Int. Ingeniero Ambrosio Taravella")) %>% 
  rename(nombre_paso=nombre) %>% 
  select(nombre_paso,geom)


puerto <- read_sf("/srv/DataDNMYE/capas_sig/puertos_seleccionados.gpkg") %>% 
  filter(nombre_paso %in% c("Puerto Buenos Aires"))%>% 
  select(nombre_paso,geom)

cristo <- read_sf("/srv/DataDNMYE/capas_sig/pasos_seleccionados.gpkg") %>% 
  filter(nombre_paso %in% c("Paso Cristo Redentor"))%>% 
  select(nombre_paso,geom)

pasos <- rbind(aero,puerto,cristo) %>% 
  mutate(nombre_paso=case_when(nombre_paso=="Aeropuerto Int. Ingeniero Ambrosio Taravella"~"Aep. Córdoba",
                               nombre_paso=="Aeropuerto Int. El Plumerillo/Gob. Francisco Gabrielli"~"Aep. Mendoza",
                               nombre_paso=="Aeroparque Jorge Newbery"~"J Newbery",
                               nombre_paso=="Aeropuerto Int. Ministro Pistarini"~"Aep. Ezeiza",
                               nombre_paso=="Puerto Buenos Aires"~"Puerto CABA",
                               nombre_paso=="Paso Cristo Redentor"~"Cto. Redentor")) %>% 
  mutate(peso=case_when(nombre_paso=="Aep. Córdoba"~0.01,
                        nombre_paso=="Aep. Mendoza"~0.02,
                        nombre_paso=="J Newbery"~0.01,
                        nombre_paso=="Aep. Ezeiza"~0.397,
                        nombre_paso=="Puerto CABA"~0.115,
                        nombre_paso=="Cto. Redentor"~0.039)) %>% 
  mutate(peso=round(peso*100,1))%>%
  mutate(etiqueta=glue("{nombre_paso}; \n{peso}%"))



source("inset_antartida.R")

mapa <- ggplot()+
  geom_sf(data=arg,fill=dnmye_colores("gris medio"))+
  theme_light()+
  theme_void()+
  labs(title = "Pasos relevados por la ETI, según peso sobre total del turismo receptivo",
       subtitle = glue("Año 2022."),
       color="")+
  theme(legend.position = "left",
        plot.title = element_text(hjust = 2.2,size = 10),
        plot.subtitle = element_text(colour = dnmye_colores("gris oscuro"),
                                     hjust = 0.3,size=8))+
  geom_point(data = pasos,aes(geometry = geom,color=etiqueta,size=peso),
             stat = "sf_coordinates")+
  scale_color_dnmye()+
  scale_size(range = c(3,5),guide = "none")
inset_antartida(mapa, x = 0.3, y = 0, scale = 0.4, map_fill = "grey")

rm(arg,puertos)

mapa