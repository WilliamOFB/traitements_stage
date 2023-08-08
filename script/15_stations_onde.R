### Import et traitements ONDE ###
# Packages ----
library(hubeau)
library(tidyverse)
library(mapview)
library(sf)

# Imports des données ----
stations_onde <- get_ecoulement_stations()

bv_onde <- read_sf("chemin/vers/ma/couche/bv_onde.gpkg") %>% # ou .shp ou en .RData
  st_transform(crs=2154)
  
pe_bv_onde <- read_sf("chemin/vers/ma/couche/plando_bv_onde.gpkg") %>% # ou .shp ou en .RData
  st_transform(crs=2154)

## Tri ----
stations_onde <- stations_onde %>% 
  filter(code_departement %in% c("22","35","29","56","44","53","85","49","72"))

stations_onde_geo <- stations_onde %>% 
  sf::st_as_sf(coords = c("coordonnee_x_station","coordonnee_y_station"),
               crs=2154)

mapview::mapview(stations_onde_geo)

## Sauvegarde ----
sf::st_write(stations_geo,
             dsn = "chemin/vers/mon/fichier/stations_onde.gpkg")

save(stations_onde_geo,
     file = "chemin/vers/mon/fichier/stations_onde.RData")

# Décompte des PE par BV ----
## Attribution ----
intersections <- st_intersection(pe_bv_onde, bv_onde)

## Décompte ----
counts <- intersections %>% 
  group_by(gid_plando) %>%
  st_drop_geometry() %>% 
  summarise(n = n())

onde_plando <- bv_onde %>% 
  left_join(y = counts)

## Sauvegarde ----
save(onde_plando,
     file = "chemin/vers/mon/fichier/onde_plando.RData")

st_write(onde_plando,
         dsn = "chemin/vers/mon/fichier/onde_plando.gpkg")
