# Package ----
library(hubeau)
library(tidyverse)
library(mapview)
library(sf)
library(aspe)

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

exutoire_ipr <- read_sf("../../SIG/2-Exploitation/BV/Sites_IPR_perim_ME.gpkg")

bv_ipr <- read_sf("../../SIG/2-Exploitation/BV/ipr_plando.gpkg")
# Autres données au-dessus (plando bv)

# Récupération des métriques IPR ----
load(file = "../../3-Statistiques/traitements_stage/raw_data/tables_sauf_mei_2023_03_08_15_08_27.RData")

passerelle <- mef_creer_passerelle()

metriques_ipr <- passerelle %>% 
  mef_ajouter_metriques() %>% 
  mef_ajouter_ope_date() %>% 
  mef_ajouter_dept() %>% 
  mef_ajouter_ipr() %>% 
  mef_ajouter_libelle_site()

points_geo <- geo_attribuer(points_geo = exutoire_ipr,
                            poly_sf = massdo)

## Sélection des départements et des dates ----
metriques_ipr_date <- metriques_ipr %>% 
  filter(annee == c(2018, 2019, 2020, 2021, 2022),
         dept == c(22, 29, 35, 44, 49, 53, 56, 72, 85))

save(metriques_ipr_date,
     file = "processed_data/ipr_metriques_perim.RData")
