
# cargar paquetes ---------------------------------------------------------

# install.packages("remotes")
# install.packages("tidyverse")
# install.packages("sf")
# install.packages("mapview")
# install.packages("GADMTools")
# remotes::install_github("paleolimbot/ggspatial")
# install.packages("leaflet")
# install.packages("leaflet.extras2")
# install.packages("spdep")
# install.packages("spatstat")
# install.packages("raster")
# install.packages("smacpod")
# install.packages("ggspatial")

library(tidyverse)
library(sf)
library(mapview)
library(GADMTools)
library(ggspatial)
library(leaflet)
library(leaflet.extras2)
library(spdep)
library(spatstat)  
library(raster)
library(smacpod)
library(ggspatial)
library(here)      # find data/script files

# limpiar memoria ---------------------------------------------------------

rm(list=ls())


# lectura de datos --------------------------------------------------------

covid_p<- read_csv(url("https://zenodo.org/record/4915889/files/covid19data.csv?download=1"))

# definimos ruta de los datos 
#path_to_data <- here("covid19data.csv")
#path_to_data

#covid<- read.csv(path_to_data, header = TRUE, sep = ",", dec = ".")

# Manejo de datos espaciales ----------------------------------------------


# Patrones de puntos  -----------------------------------------------------

# especificar latitud y  longitud  y sistema de coordenadas ---------------


covid_p <- covid_p %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# grafico simple con geometria sf ------------------------------------------


covid_p %>%
  filter(FECHA_RESULTADO == "2020-12-11") %>%
  ggplot() +
  geom_sf() 


# mapa interactivo --------------------------------------------------------


m_p <- covid_p %>% 
  filter(FECHA_RESULTADO == "2020-12-10") %>%
  mapview(layer.name = "puntos")

m_p



# datos en poligonos ------------------------------------------------------
## datos espaciales de las divisiones políticas de todos los países-------------
peru <- gadm_sf_loadCountries("PER", level=3)

## extraemos el objeto sf--------------------
lima_sf <- peru %>%
  pluck("sf") %>%
## Filtramos los datos espaciales solo de Lima metropolitana--------------------
  filter(NAME_2 == "Lima") %>%

## Editamos algunos errores en nuestros datos espaciales------------------------
  mutate(NAME_3 = ifelse(NAME_3 == "Magdalena Vieja",
                         "Pueblo Libre", NAME_3))


# covid$FECHA_RESULTADO<-as.Date(covid$FECHA_RESULTADO)


covid_count <- covid %>%
  group_by(DISTRITO, FECHA_RESULTADO) %>%
  summarise(casos = n()) %>%
  ungroup() %>%
  
  complete(FECHA_RESULTADO = seq.Date(min(FECHA_RESULTADO, na.rm =T),
                                      max(FECHA_RESULTADO, na.rm = T),
                                      by="day"),
           nesting(DISTRITO), fill = list(n = 0))

covid_sf <- lima_sf %>%
  mutate(DISTRITO = toupper(NAME_3)) %>%
  full_join(covid_count, by = "DISTRITO", "FECHA_RESULTADO")


# grafico simple para verificar si los datos estan proyectados cor --------

covid_sf %>%
  filter(FECHA_RESULTADO == "2020-12-11") %>%
  ggplot() +
  geom_sf()


m_sf <- covid_sf %>% 
  filter(FECHA_RESULTADO == "2020-12-11") %>%
  mapview(layer.name = "distritos")

m_sf


# multiples capas ---------------------------------------------------------

ggplot() +
  geom_sf(data = covid_sf %>% 
            filter(FECHA_RESULTADO == "2020-12-11")) + 
  geom_sf(data = covid_p %>% 
            filter(FECHA_RESULTADO == "2020-12-11"))

m_p + m_sf


# Visualización de datos espaciales ---------------------------------------


## Patrones de puntos ------------------------------------------------------

covid_p %>%
  filter(FECHA_RESULTADO == "2020-12-11") %>%
  ggplot() +
  geom_sf(aes(col = SEXO), alpha = .2) +
  facet_wrap(.~SEXO)

## Una variable--------------------------------------
covid_p %>%
  filter(FECHA_RESULTADO == "2020-12-11") %>%
  ggplot() +
  geom_sf(aes(col = SEXO), alpha = .2) +
  facet_wrap(.~SEXO)


covid_p %>% 
  filter(FECHA_RESULTADO == "2020-12-11") %>%
  mapview(layer.name = "points", zcol = "SEXO", burst = T)

# dos o más variables--------------------------------------

covid_p %>%
  filter(FECHA_RESULTADO == "2020-04-11" |
           FECHA_RESULTADO == "2020-12-11") %>%
  ggplot() +
  geom_sf(aes(col = SEXO), alpha = .2) +
  facet_grid(SEXO~FECHA_RESULTADO) +
  guides(col = F)


m1 <- covid_p %>%
  filter(FECHA_RESULTADO == "2020-04-11") %>%
  mapview(zcol = "SEXO", layer.name = "2020-04-11 - SEXO")

m2 <- covid_p %>%
  filter(FECHA_RESULTADO == "2020-12-11") %>%
  mapview(zcol = "SEXO", layer.name = "2020-12-11 - SEXO")
m1 + m2


## composicionxxxxxxxxxxxxxxxxxxxx
covid_p %>%
  filter(FECHA_RESULTADO == "2020-12-11") %>%
  ggplot() +
  geom_sf(data = covid_sf) +
  geom_sf(aes(col = EDAD), alpha = .2) +
  scale_color_viridis_c(option = "B") +
  annotation_scale() +
  annotation_north_arrow(location = "tr",
                         style = north_arrow_nautical)+
  theme_bw()


# definiremos una ventana espacial de análisis ----------------------------

covid_subset <- covid %>%
  filter(FECHA_RESULTADO == "2020-05-05")


covid_win <- owin(xrange = range(covid_subset$lon),
                  yrange = range(covid_subset$lat))


#  definiremos el objeto patrón de puntos  a partir de los registros --------

covid_ppp  <-  ppp(covid_subset$lon, 
                   covid_subset$lat, 
                   window = covid_win)

# el objeto de la clase densidad lo convertiremos a uno de clase r --------

densidad_raster_cov <- raster(density(covid_ppp, bw.ppl), 
                              crs = 4326) %>%

# Eliminaremos las áreas fuera de nuestra zona de estudio -----------------

  
  mask(lima_sf)

# representacion de la densidad -------------------------------------------


densidad_raster_cov %>% 
  mapview()


# deteccion de clusters ---------------------------------------------------


## Datos de patrones puntuales ---------------------------------------------


### Estadísticas de escaneo espacial (Spatial Scan Statistics-SSS) ----------
covid_subset_posi <- covid %>%
  filter(FECHA_RESULTADO == "2020-05-05") %>%
  mutate(positividad = ifelse(METODODX == "PCR", 1, 0))

covid_scan_ppp <- ppp(covid_subset_posi$lon, 
                      covid_subset_posi$lat,
                      range(covid_subset_posi$lon),
                      range(covid_subset_posi$lat),
                      marks = as.factor(covid_subset_posi$positividad))

# Aplicaremos la prueba de escaneo espacial propuesto por M. Kulld --------

covid_scan_test <- spscan.test(covid_scan_ppp,
                               nsim = 49, case = 2, 
                               maxd=.15, alpha = 0.05)
covid_scan_test
# subconjunto de análisis es convertido a una clase adecuada para graficar -----
# Construimos el centroide del clúster
cent <- tibble(lon = covid_scan_test$clusters[[1]]$coords[,1],
               lat = covid_scan_test$clusters[[1]]$coords[,2]) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F)  

# Construimos el área del clúster en base al radio
clust <- cent %>%
  st_buffer(dist = covid_scan_test$clusters[[1]]$r)




# Graficaremos el clúster detectado empleando el paquete mapview ----------

cluster <- mapview(clust, alpha.regions = 0, color = "red") 

points <- covid_subset_posi %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  mapview(zcol = "positividad", alpha.regions = .4, alpha = 0) 

cluster + points 



# Datos agregados (en polígonos) ------------------------------------------



# Autocorrelación espacial (global): Moran I ------------------------------

covid_sf_subset <- covid_sf %>%
  filter(FECHA_RESULTADO == "2020-05-05") %>%
  mutate(casos = replace_na(casos, 0))

# a partir de la distribución de los polígonos (distritos) en el área de estudio definiremos la matriz de vecindad. --------
#------------------------------ERROR---------Empty geometries found------------
covid.nb <- poly2nb(covid_sf_subset, queen=TRUE,snap = 0.13)

covid.lw <- nb2listw(covid.nb, style="W", zero.policy=TRUE)
?poly2nb
