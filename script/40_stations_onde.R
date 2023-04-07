library(hubeau)
library(tidyverse)
library(mapview)
stations <- get_ecoulement_stations(
#  code_departement = my_dept,
 # fields = param_stations
)

stations <- stations %>% 
  filter(code_departement %in% c("22","35","29","56","44","53","85","49","72"))

stations_geo <- stations %>% 
  sf::st_as_sf(coords = c("coordonnee_x_station","coordonnee_y_station"),
               crs=2154)

mapview::mapview(stations_geo)

sf::st_write(stations_geo,
             dsn="processed_data/stations_onde.gpkg")
