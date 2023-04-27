# Package ----
library(hubeau)
library(tidyverse)
library(mapview)
library(sf)

#Import des données ----
stations_onde <- get_ecoulement_stations(
#  code_departement = my_dept,
 # fields = param_stations
)

## Tri ----
stations_onde <- stations_onde %>% 
  filter(code_departement %in% c("22","35","29","56","44","53","85","49","72"))

stations_onde_geo <- stations_onde %>% 
  sf::st_as_sf(coords = c("coordonnee_x_station","coordonnee_y_station"),
               crs=2154)

mapview::mapview(stations_onde_geo)

## Sauvegarde ----
sf::st_write(stations_geo,
             dsn="processed_data/stations_onde.gpkg")

save(stations_onde_geo,
     file = "processed_data/stations_onde.RData")

# Décompte des PE par BV ----
## Import des données (Qgis) ----
bv_onde <- read_sf("../../SIG/2-Exploitation/BV_ONDE_total.gpkg")

pe_bv_onde <- read_sf("../../SIG/2-Exploitation/BV/PE_BV_ONDE.gpkg")

#pe_bv_onde <- pe_bv_onde %>%
#  sf :: st_make_valid()

#pe_bv_onde <- pe_bv_onde %>%
#  fortify()

# Attribution ----
intersections <- st_intersection(pe_bv_onde, bv_onde)

## Comptage ----
counts <- intersections %>% 
  group_by(id) %>%
  st_drop_geometry() %>% 
  summarise(n = n())

onde_plando <- bv_onde %>% 
  left_join(y = counts)

## Sauvegarde ----
save(onde_plando,
     file = "processed_data/onde_plando.RData")

st_write(onde_plando,
         dsn = "../../SIG/2-Exploitation/BV/onde_plando.gpkg")
