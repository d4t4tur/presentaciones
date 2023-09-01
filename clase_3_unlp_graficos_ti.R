library(herramientas)
library(comunicacion)
library(tidyverse)
library(data.table)
library(lubridate)

#preparacion graficos turismo internacional

anio_ref = 2023

# Base turistas desde 2016 ####

base_turistas_ori <- read_file_srv("/srv/DataDNMYE/turismo_internacional/bases_proceso/turismo_internacional_visitantes.rds")

base_turistas <- base_turistas_ori %>% 
  filter(tipo_visitante == "Turistas") %>% 
  select(anio,mes,turismo_internac,casos_ponderados,
         pais_agrupado, destino_agrup, via) %>% 
  group_by(anio,mes,turismo_internac,
           pais_agrupado, destino_agrup, via) %>% 
  summarise (casos_ponderados = sum(casos_ponderados))  %>% 
  ungroup()

#completo meses faltantes: 

base_turistas <- complete (base_turistas, 
                           expand(base_turistas, anio, mes,
                                  nesting(destino_agrup, 
                                          pais_agrupado, 
                                          turismo_internac, 
                                          via)),
                           fill = list(casos_ponderados = 0))


#paso mes a factor (con Total)
base_turistas <- base_turistas %>% 
  mutate (mes = fcase(mes == 1 ,"Enero", mes == 2 ,"Febrero", 
                      mes == 3 ,"Marzo", mes == 4 ,"Abril", 
                      mes == 5 ,"Mayo",    mes == 6 ,"Junio",
                      mes == 7 ,"Julio", mes == 8 ,"Agosto",  
                      mes == 9 ,"Septiembre", mes == 10 ,"Octubre",
                      mes == 11 ,"Noviembre", mes == 12 ,"Diciembre"))


base_turistas$mes<- factor(base_turistas$mes, 
                           levels = c("Total", "Enero",	"Febrero",	"Marzo", "Abril",	
                                      "Mayo",	"Junio",	"Julio",	"Agosto",	
                                      "Septiembre", "Octubre",	"Noviembre",	
                                      "Diciembre"), 
                           ordered = TRUE)

# Serie historica ####

#levanto datos historicos: 90-2015 anuales (por pais_agrupado/destino_agrup)

#Receptivo:  

receptivo_90_2015 <- read_file_srv("/srv/DataDNMYE/turismo_internacional/bases_proceso/series/serie_anual_pais_1990.xlsx", 
                                   sheet = "receptivo") %>% 
  filter(anio <= 2015) %>% 
  rename(turistas_r = viajes_turistas)

receptivo_completo <- base_turistas %>% 
  filter(turismo_internac == "Receptivo") %>% 
  group_by(anio, pais_agrupado) %>% 
  summarise(turistas_r = sum(casos_ponderados))%>% 
  rbind(receptivo_90_2015)

#Emisivo: 

emisivo_90_2015 <- read_file_srv("/srv/DataDNMYE/turismo_internacional/bases_proceso/series/serie_emisivo_1990_ancho.xlsx", sheet = "anual_pais_90") %>% 
  filter(anio <= 2015) %>% 
  pivot_longer(cols = -anio, 
               names_to = c("destino_agrup"),
               values_to = "turistas_e") 

emisivo_completo <- base_turistas %>% 
  filter(turismo_internac == "Emisivo") %>% 
  group_by(anio, destino_agrup) %>% 
  summarise(turistas_e = sum(casos_ponderados))%>% 
  rbind(emisivo_90_2015)

#serie anual sin pais: 

emisivo_completo_sin_pais <- emisivo_completo %>% 
  group_by(anio) %>% 
  summarise(casos_ponderados = sum(turistas_e)) %>% 
  mutate(turismo_internac= "Emisivo")

receptivo_completo_sin_pais <- receptivo_completo %>% 
  group_by(anio) %>% 
  summarise(casos_ponderados = sum(turistas_r)) %>%
  mutate(turismo_internac= "Receptivo")

total_anual_sin_pais <- receptivo_completo_sin_pais %>% 
  rbind(emisivo_completo_sin_pais) %>% 
  filter(anio <= 2022) %>%  #dejo hasta ultimo año completo
  mutate(anio = ymd(paste(anio, 1, 1, sep = "-")))
  

#Grafico serie historica ####

serie_anual <- ggplot(total_anual_sin_pais, aes(anio,casos_ponderados/1000000, colour = turismo_internac)) +   
  geom_hline(yintercept = 0, color = "grey", alpha =0.7) + 
  geom_line(linewidth = 1.2 , alpha = 0.8) + 
  geom_point(size = 2.0, alpha = 0.8)+ 
  #geom_text(aes(label = round(casos_ponderados/1000000,1), vjust = 1))+
  scale_color_dnmye() + 
  scale_x_date(date_breaks = "1 years", date_labels = "%Y", expand = c(.01,.01))+ 
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = "," )) + 
  theme_minimal()+
  theme(legend.position = "bottom", 
        axis.text.x = element_text (angle=90),
        plot.caption = ggtext::element_markdown() ) +
  labs(title = "Viajes de turistas internacionales por año. 1990-2022",
       y = "en millones de viajes de turistas", 
       x = "", 
       color= "",
       caption =  "**Fuente**: DNMyE en base a datos de DNM y ETI." )

serie_anual

ggsave("imgs/clases_unlp/ti_serie_anual.png", width = 12, height = 6)

#Grafico por pais NR 2023 ####

base_turistas_actual <- base_turistas %>% 
  filter(anio == 2023, turismo_internac == "Receptivo") %>% 
  group_by(anio,turismo_internac,
           pais_agrupado, via) %>% 
  summarise (casos_ponderados = sum(casos_ponderados))%>% 
  ungroup() %>% 
  mutate (pais_agrupado = as_factor(pais_agrupado),
          pais_agrupado = fct_relevel(pais_agrupado, c("Bolivia","Brasil",
                                                       "Chile", "Paraguay", 
                                                       "Uruguay",
                                                       "EE.UU. y Canadá",
                                                       "Resto de América",
                                                       "Europa", 
                                                       "Resto del mundo")) 
  )


graf_pais_ti <- ggplot(base_turistas_actual) +   
  geom_bar(aes(x= pais_agrupado, weight= casos_ponderados, fill =via))+
  scale_fill_dnmye() +
  geom_hline(yintercept = 0, color = "grey", alpha =0.7, size = 0.5) + 
  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = "," )) + 
  theme_minimal()+
  theme(legend.position = "bottom", 
        axis.text.x =element_text (size =11, angle=45, vjust = 0.6),
        axis.text.y = element_text(size = 12),
        legend.text = element_text (size =12),
        plot.caption =  element_text (size =12, hjust = 0.0)) +
  labs(title = "Viajes de turistas no residentes por país de residencia, según medio de transporte",
       subtitle = "Total país. Enero a Junio 2023",
       y = "", 
       x = "", 
       color= "",
       fill="",
       caption =  "Fuente: Dirección Nacional de Mercados y Estadstica, Ministerio de Turismo y Deportes" )

graf_pais_ti

ggsave("imgs/clases_unlp/ti_paisvia.png", width = 12, height = 6)

#graf_paisvia_ti <- ggplot(base_turistas_actual) +   
#  geom_bar(aes(x= via, weight= casos_ponderados, fill =pais_agrupado))+
#  scale_fill_dnmye() +
#  geom_hline(yintercept = 0, color = "grey", alpha =0.7, size = 0.5) + 
#  scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = "," )) + 
#  theme_minimal()+
#  theme(legend.position = "bottom", 
#        axis.text.x =element_text (size =11, angle=45, vjust = 0.6),
#        axis.text.y = element_text(size = 12),
#        legend.text = element_text (size =12),
#        plot.caption =  element_text (size =12, hjust = 0.0)) +
#  labs(title = "Viajes de turistas no residentes por vía, según país de residencia",
#       subtitle = "Total país. Enero a Junio 2023",
#       y = "", 
#       x = "", 
#       color= "",
#       fill="",
#       caption =  "Fuente: Dirección Nacional de Mercados y Estadstica, Ministerio de Turismo y Deportes" )
#
#graf_paisvia_ti

#ggsave("imgs/clases_unlp/ti_viapais.png", width = 12)


paso_anio_ref <- base_turistas_ori %>% 
  filter(tipo_visitante == "Turistas", anio == anio_ref, turismo_internac == "Receptivo") %>% 
  group_by(via, paso_publ) %>% 
  summarise (casos_ponderados = sum(casos_ponderados))  %>% 
  ungroup()

pasos_agrup <-paso_anio_ref %>% 
  mutate(paso2 = fct_lump(paso_publ, n = 22, w = casos_ponderados, other_level = "Otros")) %>% 
  group_by(paso2) %>% 
  summarise (casos_ponderados = sum(casos_ponderados)) %>% 
  ungroup() %>% 
  mutate (porcentaje = round(casos_ponderados/sum(casos_ponderados), 3), 
          eti = if_else(paso2 %in% 
                          c("Aeropuerto Ezeiza-Aeroparque", "Puerto de Buenos Aires", "Paso Cristo Redentor", "Aeropuerto Mendoza", "Aeropuerto Córdoba"), 
                        "Paso ETI", "Paso No ETI")) 

pasos_agrup %>% count(eti, wt = porcentaje)

ggplot(pasos_agrup, aes(x = fct_reorder(paso2, porcentaje), y = porcentaje*100, fill = eti)) +
  geom_col() +
  coord_flip()+
  geom_text(aes(label = scales::label_percent(decimal.mark= "," )(porcentaje)),
            position = position_stack(vjust = .5),
            size = 3) +
  labs(title =  "Porcentaje de viajes de turistas por paso. Enero -Junio 2023",
       x="", 
       y = "", 
       fill = "") +
  theme_minimal() 

ggsave("imgs/clases_unlp/ti_pasos.png", width = 12, height = 5)
