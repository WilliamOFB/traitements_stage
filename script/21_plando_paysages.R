# Packages ----
library(tidyverse)
library(mapview)
library(sf)
library(rvest)
library(httr)
library(xml2)
library(dplyr)

# Import ----
load("processed_data/plando_comm_her_me.RData")

#paysages_pdl <- read_sf("raw_data/r_unite_paysagere_r52.shp")

#paysages_22 <- read_sf("raw_data/unites_paysages_22.shp")

#paysages_bret <- read_sf("../../SIG/2-Exploitation/Paysages_293556.gpkg")

paysages_full <- read_sf("../../SIG/2-Exploitation/Paysages/paysages_full.gpkg")

## Vérification ----
mapview(paysages_full)

#paysages_full <- paysages_full %>%
#  sf :: st_make_valid()

#paysages_full <- paysages_full %>%
#  fortify()

# Nommage ----
## Don d'un nom à chaque entité ----
paysages_full <- paysages_full %>% 
  mutate(NOM_UNITE = if_else(is.na(NOM_UNITE), nom_up, NOM_UNITE)) %>% 
  mutate(ID_UNITE = as.character(ID_UNITE))

## Don d'un ID à chaque entité ----
paysages_full <- paysages_full %>% 
  mutate(ID_UNITE = if_else(is.na(ID_UNITE), id_up, ID_UNITE)) %>% 
  mutate(ID_UNITE = if_else(is.na(ID_UNITE), ID, ID_UNITE)) %>% 
  select(ID_UNITE:area, famille_up, url_atlas, geom) #suppression des colonnes inutiles

# Attribution ----
intersections <- st_intersection(plando_comm_her_me, paysages_full)

# Compter le nombre de polygones de "plando" présents dans chaque polygone de "paysages_full"
counts <- intersections %>% 
  group_by(ID_UNITE) %>%
  st_drop_geometry() %>% 
  summarise(n = n())

paysages_plando <- paysages_full %>% 
  left_join(y = counts)

# Sauvegarde ----
save(paysages_plando,
     file = "processed_data/paysages_plando.RData")

st_write(paysages_plando,
         dsn = "../../SIG/2-Exploitation/Paysages/paysages_plando.gpkg")
