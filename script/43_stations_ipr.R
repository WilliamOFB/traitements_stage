# Package ----
library(hubeau)
library(tidyverse)
library(mapview)
library(sf)

# Décompte des PE par BV ----
## Import des données (Qgis) ----
bv_ipr <- read_sf("../../SIG/2-Exploitation/BV/BV_stations_IPR.gpkg")

pe_bv_ipr <- read_sf("../../SIG/2-Exploitation/BV/PE_BV_IPR.gpkg")

#pe_bv_ipr <- pe_bv_ipr %>%
#  sf :: st_make_valid()

#pe_bv_ipr <- pe_bv_ipr %>%
#  fortify()

# Attribution ----
intersections <- st_intersection(pe_bv_ipr, bv_ipr)

## Comptage ----
counts <- intersections %>% 
  group_by(id) %>%
  st_drop_geometry() %>% 
  summarise(n = n())

ipr_plando <- bv_ipr %>% 
  left_join(y = counts)

## Sauvegarde ----
save(ipr_plando,
     file = "processed_data/ipr_plando.RData")

st_write(ipr_plando,
         dsn = "../../SIG/2-Exploitation/BV/ipr_plando.gpkg")


### Sélection des PE et CE à 1,2km en amont de la station ###
# Import des données ----
courdo_bv_ipr <- read_sf("../../SIG/2-Exploitation/BV/CE_BV_IPR.gpkg")

exutoire_ipr <- read_sf("../../SIG/2-Exploitation/BV/Sites_IPR_perimetre_XY.gpkg")

bv_ipr <- read_sf("../../SIG/2-Exploitation/BV/ipr_plando.gpkg")
# Autres données au-dessus (plando bv)

