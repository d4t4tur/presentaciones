library(tidyverse)
library(herramientas)
library(sf)
library(scales)

anio_corte <- 2022

# puna --------------------------------------------------------------------


puna <- read_file_srv("/srv/DataDNMYE/capas_sig/puna_localidades_bahra_2022.gpkg") %>%
  mutate(anio = anio_corte)

puna_geo_dist <- puna %>% 
  distinct(provincia, departamento_partido, localidad,
           cod_pcia, cod_depto, geom)


# aero --------------------------------------------------------------------


aeropuertos <- read_file_srv("/srv/DataDNMYE/prueba/puna_aeropuertos_osrm.rds")

anti_join(puna_geo_dist, aeropuertos, by = c("provincia" = "provincia.x", "departamento_partido" = "departamento_partido",
                                             "localidad" = "localidad")) %>% 
  st_drop_geometry() %>% 
  filter(!is.na(cod_pcia)) 

aeropuertos <- aeropuertos %>% 
  select(-c(anio.y, anio.x, provincia)) %>% 
  rename(provincia = provincia.x)

aeropuertos <- aeropuertos %>% 
  unnest_wider(col = osrm) 

aeropuertos <- aeropuertos %>% 
  mutate(durations = map_dbl(durations, ~.x[1]),
         distances = map_dbl(distances, ~.x[1])) %>% 
  unnest_wider(col = c(sources, destinations), names_sep = "_") 
  
aeropuertos <- aeropuertos %>% 
  group_by(provincia, departamento_partido, localidad) %>% 
  slice_min(order_by = durations, n = 2) %>%
  filter(distances <= 300*1000)

aeropuertos_wider <- aeropuertos %>% 
  select(provincia, departamento_partido, localidad,
         plazas, establecimientos, orden_aeropuerto,
         media_pax_cabotaje, media_pax_internacional, rutas_int,
         rutas_cabotaje, indice_cabotaje, indice_int, durations, aeropuerto_etiqueta_anac, 
          ) %>% 
  group_by(provincia, departamento_partido, localidad) %>% 
  arrange(durations) %>% 
  mutate(orden_aeropuerto = 1:n()) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(provincia, departamento_partido,
                          localidad, plazas, establecimientos),
               names_from = orden_aeropuerto,
              values_from = c( media_pax_cabotaje, media_pax_internacional,
                               rutas_int, rutas_cabotaje,  indice_cabotaje, indice_int, 
                               durations, aeropuerto_etiqueta_anac)) %>% 
  st_drop_geometry()


puna_aero <- aeropuertos_wider %>% 
  ungroup() %>% 
  mutate(across(matches("durations"), ~ replace_na(.x, 900)),
         across(matches("indice"), ~ replace_na(.x, 0))) %>% 
  # promedio de ponderador de distancia * media de pax * cantidad de rutas para los 2 aeropuertos mas cercanos a menos de 300km
  mutate(cabotaje = rescale(1/2*((1-rescale(log(durations_1)))*rescale(log(indice_cabotaje_1+1))+(1-rescale(log(durations_2)))*rescale(log(indice_cabotaje_2+1)))),
         internacional = rescale(1/2*((1-rescale(log(durations_1)))*rescale(log(indice_int_1+1))+(1-rescale(log(durations_2)))*rescale(log(indice_int_2+1))))
  ) %>% 
  mutate(rank_aerocabotaje = rank(cabotaje),
         rank_aerointernacional = rank(internacional)) 


# cnrt --------------------------------------------------------------------

puna_cnrt <- read_file_srv("/srv/DataDNMYE/prueba/puna_cnrt.rds")


# empleo ------------------------------------------------------------------

puna_empleo <- read_file_srv("/srv/DataDNMYE/empleo/puna_deptos_empleo.rds")



# puna union --------------------------------------------------------------

puna_union <-  left_join(puna, puna_cnrt) %>%
  left_join(., puna_aero ) %>% 
  left_join(puna_empleo %>% st_drop_geometry())

puna_union <- puna_union %>% 
  select(c(provincia, departamento_partido, localidad,
         geom, anio, plazas, establecimientos, pax_cnrt,
         cabotaje, internacional,puestos_rct_prop, puestos_rct))

puna_union <- puna_union %>% 
  mutate(across(.cols = c( plazas, establecimientos, pax_cnrt,
                           cabotaje, internacional,puestos_rct_prop, puestos_rct),
                ~ replace_na(.x, 0)))

puna_union <- puna_union %>% 
  mutate(establecimientos = ifelse(establecimientos == 0  & plazas >0, 1, establecimientos)) %>% 
  mutate(establecimientos = rescale(log(establecimientos))) %>% 
  mutate(across(.cols = c( plazas, pax_cnrt, puestos_rct),
                ~ rescale(log(.x+1))), 
         puestos_rct_prop = rescale(puestos_rct_prop))

puna_union <- puna_union %>% 
  rowwise() %>% 
  mutate(indice_turismo = (plazas/2 + establecimientos/2 + pax_cnrt +
                               cabotaje/2 + internacional/2 + puestos_rct_prop/2+puestos_rct/2)/4)
