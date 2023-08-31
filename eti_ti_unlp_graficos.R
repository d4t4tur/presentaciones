library(readxl)
library(tidyverse)
library(magrittr)
library(writexl)
#library(dplyr)
library(haven)
library(lubridate)
#library(janitor)
library(glue)
library(comunicacion)

options(scipen = 9999)


#Cargamos las bases de la ETI

eti_e <- readRDS("/srv/DataDNMYE/eti/bases/eti_nr_2009_2023.rds")%>%
  filter(p3_3<=2022 & p3_3>=2010 & vis==2 & wpf>0) %>% 
  filter((paso_final=="Ezeiza y Aeroparque" & (p3_3<2020 | p3_3>2021 | (anio_trim=="2020-01-01" | anio_trim=="2021-01-01" | anio_trim=="2021-10-01"))) |
           (paso_final %in% c("Aep. Córdoba","Puerto de Buenos Aires","Aep. Mendoza","Cristo Redentor") & (p3_3<2020 | p3_3>2021 | (anio_trim=="2020-01-01" | anio_trim=="2021-10-01"))))



eti_a <- readRDS("/srv/DataDNMYE/eti/bases/eti_a_2014_2023.rds")%>%
  filter(p3_3<=2022 & p3_3>=2010 & vis==2 & wpf>0) %>% 
  filter((paso_final=="Ezeiza y Aeroparque" & (p3_3<2020 | p3_3>2021 | (anio_trim=="2020-01-01" | anio_trim=="2021-01-01" | anio_trim=="2021-10-01"))) |
           (paso_final %in% c("Aep. Córdoba","Puerto de Buenos Aires","Aep. Mendoza","Cristo Redentor") & (p3_3<2020 | p3_3>2021 | (anio_trim=="2020-01-01" | anio_trim=="2021-10-01"))))



#######################MOTIVO########################

#Turistas por motivo

eti_motivo <- read.csv("/srv/DataDNMYE/eti/recursos/receptivo/turistas_no_residentes_por_motivo_de_viaje_segun_paso_trimestral.csv") %>% 
  mutate(anio=year(indice_tiempo),
         trim=case_when(month(indice_tiempo)==1~"trim I",
                        month(indice_tiempo)==4~"trim II",
                        month(indice_tiempo)==7~"trim III",
                        month(indice_tiempo)==10~"trim IV")) %>% 
  filter(anio>=2018)


graf <- eti_motivo %>%
  filter(anio<=2022) %>% 
  group_by(anio,paso,motivo) %>% 
  summarise(turistas=sum(turistas_no_residentes,na.rm = TRUE)) %>% 
  mutate(dist=round(prop.table(turistas)*100,1)) %>% 
  ungroup()

ggplot(data = graf %>% mutate(motivo=factor(motivo,
                                              levels = c("Visita flia/amigos",
                                                         "Vacaciones/ocio/recreación",
                                                         "Negocios/congreso/conferencia",
                                                         "Otros")),
                                paso=factor(paso,
                                            levels = c("Ezeiza y Aeroparque",
                                                       "Aep. Córdoba",
                                                       "Aep. Mendoza",
                                                       "Puerto de Buenos Aires",
                                                       "Cristo Redentor")))
       ,aes(x=anio,y=dist,fill=motivo,label=glue("{dist} %")))+
  geom_col()+
  geom_text(position = position_stack(vjust = .5))+
  facet_wrap(~paso)+
  scale_fill_dnmye()+
  scale_x_continuous(breaks = seq(from=min(graf$anio),to=max(graf$anio)))+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(title = "Distribución de los turistas no residentes segun motivo de viaje",
       subtitle = "Por paso de ingreso y año. Años 2018-2022. ",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: ETI")+
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = "dark grey"),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
        panel.grid.major.y =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth = 1),
        panel.grid.major.x =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth  = 1),
        panel.background = element_blank())


ggsave("imgs/clases_unlp/c3_eti_motivo.png",width =12 ,height =8 )


rm(graf)

#Turistas RESIDENTES por motivo

eti_arg_motivo <- read.csv("/srv/DataDNMYE/eti/recursos/emisivo/turistas_residentes_por_motivo_de_viaje_segun_paso_trimestral.csv") %>% 
  mutate(anio=year(indice_tiempo),
         trim=case_when(month(indice_tiempo)==1~"trim I",
                        month(indice_tiempo)==4~"trim II",
                        month(indice_tiempo)==7~"trim III",
                        month(indice_tiempo)==10~"trim IV")) %>% 
  filter(anio>=2018)


graf_b <- graf %>% 
  filter(paso=="Ezeiza y Aeroparque") %>% 
  rename(no_resi=dist) %>% 
  select(1,3,5) %>% 
  filter(anio<=2022)

graf_c <- eti_arg_motivo %>% 
  group_by(anio,paso,motivo) %>% 
  summarise(turistas=sum(turistas_residentes,na.rm = TRUE)) %>% 
  mutate(dist=round(prop.table(turistas)*100,1)) %>% 
  ungroup() %>% 
  filter(paso=="Ezeiza y Aeroparque") %>% 
  rename(residentes=dist) %>%
  filter(anio<=2022) %>% 
  select(1,3,5) %>% 
  left_join(graf_b) %>% 
  pivot_longer(cols=c(residentes,no_resi),
               names_to = "origen_viajeros",
               values_to="viajeros") %>% 
  arrange(anio,origen_viajeros,motivo)

ggplot(data = graf_c %>% mutate(motivo=factor(motivo,
                                               levels = c("Visita flia/amigos",
                                                          "Vacaciones/ocio/recreación",
                                                          "Negocios/congreso/conferencia",
                                                          "Otros"))),
       aes(x=origen_viajeros,y=viajeros,fill=motivo,label=glue("{viajeros} %")))+
  geom_col()+
  geom_text(position = position_stack(vjust = .5))+
  facet_wrap(~anio)+
  scale_fill_dnmye()+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(title = "Distribución de los turistas según residencia, por motivo de viaje",
       subtitle = "Ezeiza y Aeroparque. Años 2018-2022. ",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: ETI")+
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = "dark grey"),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
        panel.grid.major.y =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth = 1),
        panel.grid.major.x =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth  = 1),
        panel.background = element_blank())


ggsave("imgs/clases_unlp/c3_eti_motivo_residencia.png",width =12 ,height =8 )


rm(graf_b,graf_c,graf)

rm

###Motivo por trimestre

# Apertura por trimestre

graf <- eti_motivo %>%
  filter(anio==2019) %>% 
  group_by(trim,paso,motivo) %>% 
  summarise(turistas=sum(turistas_no_residentes,na.rm = TRUE)) %>% 
  mutate(dist=round(prop.table(turistas)*100,1)) %>% 
  ungroup()


ggplot(data = graf %>% mutate(motivo=factor(motivo,
                                               levels = c("Visita flia/amigos",
                                                          "Vacaciones/ocio/recreación",
                                                          "Negocios/congreso/conferencia",
                                                          "Otros")),
                                 paso=factor(paso,
                                             levels = c("Ezeiza y Aeroparque",
                                                        "Aep. Córdoba",
                                                        "Aep. Mendoza",
                                                        "Puerto de Buenos Aires",
                                                        "Cristo Redentor")))
       ,aes(x=trim,y=dist,fill=motivo,label=glue("{dist} %")))+
  geom_col()+
  geom_text(position = position_stack(vjust = .5))+
  facet_wrap(~paso)+
  scale_fill_dnmye()+
  #scale_x_continuous(breaks = seq(from=min(graf_1b$anio),to=max(graf_1b$anio)))+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(title = "Distribución de los turistas no residentes segun motivo de viaje",
       subtitle = "Por paso de ingreso y trimestre. Año 2019.",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: ETI")+
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = "dark grey"),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
        panel.grid.major.y =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth = 1),
        panel.grid.major.x =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth  = 1),
        panel.background = element_blank())


ggsave("imgs/clases_unlp/c3_eti_motivo_trim.png",width =12 ,height =8 )


rm(eti_motivo,graf,eti_arg_motivo)


###############################TIPO DE ALOJAMIENTO##########################3

#Turistas por tipo de alojamiento

eti_aloja <- read.csv("/srv/DataDNMYE/eti/recursos/receptivo/turistas_no_residentes_por_tipo_de_alojamiento_segun_paso_trimestral.csv") %>% 
  mutate(anio=year(indice_tiempo),
         trim=case_when(month(indice_tiempo)==1~"trim I",
                        month(indice_tiempo)==4~"trim II",
                        month(indice_tiempo)==7~"trim III",
                        month(indice_tiempo)==10~"trim IV")) %>% 
  filter(anio>=2018 & anio<=2022)



graf <- eti_aloja %>% 
  group_by(anio,paso,alojamiento) %>% 
  summarise(turistas=sum(turistas_no_residentes,na.rm = TRUE)) %>% 
  mutate(dist=round(prop.table(turistas)*100,1)) %>% 
  ungroup()



ggplot(data = graf %>% mutate(alojamiento=factor(alojamiento,
                                                       levels = c("Hotel 4 y 5 estrellas",
                                                                  "Hotel 1,2, y 3 estrellas",
                                                                  "Casa flia./amigos",
                                                                  "Otros")),
                                    paso=factor(paso,
                                                levels = c("Ezeiza y Aeroparque",
                                                           "Aep. Córdoba",
                                                           "Aep. Mendoza",
                                                           "Puerto de Buenos Aires",
                                                           "Cristo Redentor")))
       ,aes(x=anio,y=dist,fill=alojamiento,label=glue("{dist} %")))+
  geom_col()+
  geom_text(position = position_stack(vjust = .5))+
  facet_wrap(~paso)+
  scale_fill_dnmye()+
  scale_x_continuous(breaks = seq(from=min(graf$anio),to=max(graf$anio)))+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(title = "Distribución de los turistas no residentes segun tipo de alojamiento utilizado",
       subtitle = "Por paso de ingreso y año. Años 2018-2022",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: ETI")+
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
        panel.grid.major.y =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth = 1),
        panel.grid.major.x =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth  = 1),
        panel.background = element_blank())


ggsave("imgs/clases_unlp/c3_eti_aloja.png",width =12 ,height =8 )



###SUMAMOS DATOS DE RESIDENTES

eti_arg_aloja <- read.csv("/srv/DataDNMYE/eti/recursos/emisivo/turistas_residentes_por_tipo_de_alojamiento_segun_paso_trimestral.csv") %>% 
  mutate(anio=year(indice_tiempo),
         trim=case_when(month(indice_tiempo)==1~"trim I",
                        month(indice_tiempo)==4~"trim II",
                        month(indice_tiempo)==7~"trim III",
                        month(indice_tiempo)==10~"trim IV")) %>% 
  filter(anio>=2018 & anio<=2022)


graf_b <- graf %>% 
  filter(paso=="Ezeiza y Aeroparque") %>% 
  rename(no_resi=dist) %>% 
  select(1,3,5) %>% 
  filter(anio<=2022)

graf_c <- eti_arg_aloja %>% 
  group_by(anio,paso,alojamiento) %>% 
  summarise(turistas=sum(turistas_residentes,na.rm = TRUE)) %>% 
  mutate(dist=round(prop.table(turistas)*100,1)) %>% 
  ungroup() %>% 
  filter(paso=="Ezeiza y Aeroparque") %>% 
  rename(residentes=dist) %>%
  filter(anio<=2022) %>% 
  select(1,3,5) %>% 
  left_join(graf_b) %>% 
  pivot_longer(cols=c(residentes,no_resi),
               names_to = "origen_viajeros",
               values_to="viajeros") %>% 
  arrange(anio,origen_viajeros,alojamiento) %>% 
  mutate(origen_viajeros=case_when(origen_viajeros=="no_resi"~"No Residentes",
                                   origen_viajeros=="residentes"~"Residentes"))


# Gráfico:

ggplot(data = graf_c,
       aes(x=origen_viajeros,y=viajeros,fill=alojamiento,label=glue("{viajeros} %")))+
  geom_col()+
  geom_text(position = position_stack(vjust = .5))+
  facet_wrap(~anio)+
  scale_fill_dnmye()+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(title = "Distribución de los turistas según residencia, por motivo de alojamiento",
       subtitle = "Ezeiza y Aeroparque. Años 2018-2022. ",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: ETI")+
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour =dnmye_colores("gris oscuro")),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
        panel.grid.major.y =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth = 1),
        panel.grid.major.x =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth  = 1),
        panel.background = element_blank())
ggsave("imgs/clases_unlp/c3_eti_aloja_resi.png",width =12 ,height =8 )


rm(graf,graf_b,graf_c)



# Apertura por trimestre

graf <- eti_aloja %>%
  filter(anio==2019) %>% 
  group_by(trim,paso,alojamiento) %>% 
  summarise(turistas=sum(turistas_no_residentes,na.rm = TRUE)) %>% 
  mutate(dist=round(prop.table(turistas)*100,1)) %>% 
  ungroup()

ggplot(data = graf %>% mutate(alojamiento=factor(alojamiento,
                                                    levels = c("Hotel 4 y 5 estrellas",
                                                               "Hotel 1,2, y 3 estrellas",
                                                               "Casa flia./amigos",
                                                               "Otros")),
                                 paso=factor(paso,
                                             levels = c("Ezeiza y Aeroparque",
                                                        "Aep. Córdoba",
                                                        "Aep. Mendoza",
                                                        "Puerto de Buenos Aires",
                                                        "Cristo Redentor")))
       ,aes(x=trim,y=dist,fill=alojamiento,label=glue("{dist} %")))+
  geom_col()+
  geom_text(position = position_stack(vjust = .5))+
  facet_wrap(~paso)+
  scale_fill_dnmye()+
  #scale_x_continuous(breaks = seq(from=min(graf_2$anio),to=max(graf_2$anio)))+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(title = "Distribución de los turistas no residentes segun tipo de alojamiento utilizado",
       subtitle = "Por paso de ingreso. Año 2022.",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: ETI")+
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = "dark grey"),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
        panel.grid.major.y =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth = 1),
        panel.grid.major.x =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth  = 1),
        panel.background = element_blank())
ggsave("imgs/clases_unlp/c3_eti_aloja_trim.png",width =12 ,height =8 )

rm(graf,eti_aloja,eti_arg_aloja)


###########################GASTO############################

#Insumo

gasto <- read.csv("/srv//DataDNMYE/eti/recursos/receptivo/gasto_total_promedio_diario_por_turista_en_usd_turistas_no_residentes_trimestral_segun_paso.csv") %>% 
  mutate(anio=year(indice_tiempo),
         trim=case_when(month(indice_tiempo)==1~"trim I",
                        month(indice_tiempo)==4~"trim II",
                        month(indice_tiempo)==7~"trim III",
                        month(indice_tiempo)==10~"trim IV")) %>% 
  filter(anio>=2018)

gasto_total <- gasto %>% 
  group_by(anio,paso) %>% 
  summarise(gasto=sum(gasto_total_en_usd_no_residentes,na.rm = TRUE)) %>% 
  mutate(gasto=round(gasto/100000,1)) %>% 
  filter(anio !=2020 & anio !=2021)


# Grafico

ggplot(data = gasto_total %>% mutate(paso=factor(paso,
                                                 levels = c("Ezeiza y Aeroparque",
                                                            "Aep. Córdoba",
                                                            "Aep. Mendoza",
                                                            "Puerto de Buenos Aires",
                                                            "Cristo Redentor"))), aes(y=gasto,x=anio,label=gasto))+
  geom_bar(stat='identity',position="dodge",color=dnmye_colores("cian"), fill=dnmye_colores("cian"))+
  geom_label(fill="white",size=2.5, position=position_dodge(width=0.9),vjust=0.25)+
  facet_wrap(~paso)+
  labs(title = "Gasto total (en millones de dólares) de turistas no residentes",
       subtitle = "Por paso de ingreso. Años 2018,2019,2022.",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: ETI")+
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = "dark grey"),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
        panel.grid.major.y =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth = 1),
        panel.grid.major.x =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth  = 1),
        panel.background = element_blank())+
  scale_x_continuous(breaks = seq(from=min(gasto_total$anio),to=max(gasto_total$anio)))
ggsave("imgs/clases_unlp/c3_eti_gasto.png",width =12 ,height =8 )


rm(gasto,gasto_total)

################Gastos con insumos de las bases

# Gasto promedio diario 


gasto_prom <- eti_e %>%
  group_by(p3_3,paso_final) %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE),
            pernoctes=sum(totaln*wpf,na.rm = TRUE),
            gasto=sum(gastoest2*wpf,na.rm = TRUE))%>% 
  mutate(estadia=round(pernoctes/viajeros,1),
         gasto_tur=round(gasto/viajeros,1),
         gasto_diario=round(gasto/pernoctes,1)) %>% 
  filter(p3_3>2015) %>% 
  rename(anio=p3_3)

# Gráfico de gasto promedio diario:

ggplot(data = gasto_prom %>% mutate(paso_final=factor(paso_final,
                                                      levels = c("Ezeiza y Aeroparque",
                                                                 "Aep. Córdoba",
                                                                 "Aep. Mendoza",
                                                                 "Puerto de Buenos Aires",
                                                                 "Cristo Redentor"))), aes(y=gasto_diario,x=anio,label=gasto_diario))+
  geom_bar(stat='identity',position="dodge",color=dnmye_colores("rosa"), fill=dnmye_colores("rosa"))+
  geom_label(fill="white",size=3, position=position_dodge(width=0.9),vjust=0.40)+
  facet_wrap(~paso_final)+
  labs(title = "Gasto promedio diario (en dólares) de turistas no residentes",
       subtitle = "Por paso de ingreso. Años 2016-2022.",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: ETI")+
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
        panel.grid.major.y =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth = 1),
        panel.grid.major.x =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth  = 1),
        panel.background = element_blank())+
  scale_x_continuous(breaks = seq(from=min(gasto_prom$anio),to=max(gasto_prom$anio)))
ggsave("imgs/clases_unlp/c3_eti_gpd.png",width =12 ,height =8 )




# Gráfico de gasto promedio por turista:

ggplot(data = gasto_prom %>% mutate(paso_final=factor(paso_final,
                                                      levels = c("Ezeiza y Aeroparque",
                                                                 "Aep. Córdoba",
                                                                 "Aep. Mendoza",
                                                                 "Puerto de Buenos Aires",
                                                                 "Cristo Redentor"))), aes(y=gasto_tur,x=anio,label=gasto_tur))+
  geom_bar(stat='identity',position="dodge",color=dnmye_colores("rosa"), fill=dnmye_colores("rosa"))+
  geom_label(fill="white",size=3, position=position_dodge(width=0.9),vjust=0.40)+
  facet_wrap(~paso_final)+
  labs(title = "Gasto promedio por turista (en dólares) de turistas no residentes",
       subtitle = "Por paso de ingreso. Años 2016-2022.",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: ETI")+
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
        panel.grid.major.y =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth = 1),
        panel.grid.major.x =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth  = 1),
        panel.background = element_blank())+
  scale_x_continuous(breaks = seq(from=min(gasto_prom$anio),to=max(gasto_prom$anio)))
ggsave("imgs/clases_unlp/c3_eti_gpt.png",width =12 ,height =8 )


# Gasto por turista en Ezeiza y Aeroparque por mercado


gasto_prom_eya <- eti_e %>%
  filter(paso_final=="Ezeiza y Aeroparque") %>% 
  group_by(p3_3,orig_eya) %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE),
            pernoctes=sum(totaln*wpf,na.rm = TRUE),
            gasto=sum(gastoest2*wpf,na.rm = TRUE))%>% 
  mutate(estadia=round(pernoctes/viajeros,1),
         gasto_tur=round(gasto/viajeros,1),
         gasto_diario=round(gasto/pernoctes,1)) %>% 
  filter(p3_3>=2019) %>% 
  rename(anio=p3_3)

# Gráfico de gasto promedio por turista en Ezeiza y Aeroparque:

ggplot(data = gasto_prom_eya, aes(y=gasto_tur,x=orig_eya,label=gasto_tur))+
  geom_bar(stat='identity',position="dodge",color="sky blue", fill="blue")+
  geom_label(fill="white",size=3, position=position_dodge(width=0.9),vjust=0.40)+
  facet_wrap(~anio)+
  labs(title = "Gasto promedio por turista (en dólares) de turistas no residentes por origen",
       subtitle = "Ezeiza y Aeroparque. Años 2019-2022.",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: ETI")+
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = "dark grey"),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
        panel.grid.major.y =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth = 1),
        panel.grid.major.x =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth  = 1),
        panel.background = element_blank(),
        axis.text.x = element_text (angle=90))


# COMPARACIÓN DE GASTO EMISIVO-RECEPTIVO


gasto_e <- eti_e %>%
  filter(paso_final=="Ezeiza y Aeroparque") %>% 
  group_by(p3_3) %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE),
            pernoctes=sum(totaln*wpf,na.rm = TRUE),
            gasto=sum(gastoest2*wpf,na.rm = TRUE))%>% 
  mutate(estadia=round(pernoctes/viajeros,1),
         gasto_tur=round(gasto/viajeros,1),
         gasto_diario=round(gasto/pernoctes,1)) %>% 
  filter(p3_3>=2019) %>% 
  rename(anio=p3_3) %>% 
  mutate(origen="receptivo") %>% 
  select(anio,origen,gasto_tur,gasto_diario)


gasto_a <- eti_a %>%
  filter(paso_final=="Ezeiza y Aeroparque") %>% 
  group_by(p3_3) %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE),
            pernoctes=sum(totaln*wpf,na.rm = TRUE),
            gasto=sum(gastoest2*wpf,na.rm = TRUE))%>% 
  mutate(estadia=round(pernoctes/viajeros,1),
         gasto_tur=round(gasto/viajeros,1),
         gasto_diario=round(gasto/pernoctes,1)) %>% 
  filter(p3_3>=2019) %>% 
  rename(anio=p3_3)%>% 
  mutate(origen="emisivo") %>% 
  select(anio,origen,gasto_tur,gasto_diario)

gasto_comparado <- rbind(gasto_e,gasto_a)


# Gráfico GPT:


ggplot(data = gasto_comparado %>% mutate(origen=factor(origen,
                                                       levels = c("receptivo",
                                                                  "emisivo"))),
       aes(y=gasto_tur,x=origen,label=gasto_tur,fill=origen))+
  geom_bar(stat='identity',position="dodge",color="sky blue")+
  geom_label(fill="white",size=3, position=position_dodge(width=0.9),vjust=0.40)+
  facet_wrap(~anio)+
  coord_flip()+
  labs(title = "Gasto promedio por turista (en dólares) por residencia",
       subtitle = "Ezeiza y Aeroparque. Años 2019-2022.",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: ETI")+
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = "dark grey"),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "",
        panel.grid.major.y =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth = 1),
        panel.grid.major.x =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth  = 1),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        #axis.text.y = element_blank(),
        #axis.ticks.y = element_blank()
  )



# Gráfico GPT:


ggplot(data = gasto_comparado %>% mutate(origen=factor(origen,
                                                       levels = c("receptivo",
                                                                  "emisivo"))),
       aes(y=gasto_diario,x=origen,label=gasto_diario,fill=origen))+
  geom_bar(stat='identity',position="dodge",color="sky blue")+
  geom_label(fill="white",size=3, position=position_dodge(width=0.9),vjust=0.40)+
  facet_wrap(~anio)+
  coord_flip()+
  labs(title = "Gasto diario por turista (en dólares) por residencia",
       subtitle = "Ezeiza y Aeroparque. Años 2019-2022.",
       x = "",
       y = "",
       colour="",
       caption = "Fuente: ETI")+
  theme(text = element_text(size = 12),
        plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = "dark grey"),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        legend.position = "",
        panel.grid.major.y =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth = 1),
        panel.grid.major.x =  element_line (colour = "grey",
                                            size = 0.1,
                                            linewidth  = 1),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        #axis.text.y = element_blank(),
        #axis.ticks.y = element_blank()
  )



########DESTINOS VISITADOS

#Insumo:


destinos <- read.csv("recursos/receptivo/turistas_pernoctes_por_provincia_segun_destinos_visitados_y_alojamiento_ezeiza_aeroparque_trimestral.csv") %>% 
  mutate(anio=year(indice_tiempo),
         trim=case_when(month(indice_tiempo)==1~"trim I",
                        month(indice_tiempo)==4~"trim II",
                        month(indice_tiempo)==7~"trim III",
                        month(indice_tiempo)==10~"trim IV")) %>% 
  filter(anio==2022)

dest <- destinos %>% 
  group_by(provincia_de_destino) %>%
  filter(destinos_visitados !="Solo resto") %>% 
  summarise(turistas=round(sum(turistas_no_residentes)/1000,1)) %>% 
  arrange(desc(turistas))

#gráfico


orden <- (dest$turistas)

ggplot(data=dest,
       aes(x=turistas,y=provincia_de_destino,fill=provincia_de_destino,
           label=turistas))+
  geom_col()+ 
  geom_text(hjust = "inward") +
  #scale_fill_brewer()+
  theme(legend.position='none')+
  labs(title = "Turistas no residentes (en miles) según provincia visitada.",
       subtitle ="Año 2022.",
       x = "",
       y = "",
       colour="",
       caption = "ETI.")+
  theme_minimal()+
  theme(plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(size=8),
        plot.title=element_text(hjust = 0,face="bold",size=10),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major.y = element_line(color = "grey",
                                          size = 0.3,
                                          linetype = 4),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank())


rm(orden_cruceros)

