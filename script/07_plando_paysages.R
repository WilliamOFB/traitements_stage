# Packages ----
library(tidyverse)
library(mapview)
library(sf)

# Imports ----
plando <- read_sf("chemin/vers/ma/couche/plando.gpkg") %>% # ou .shp ou en .RData 
  filter(is.na(ERU),
         is.na(Orage),
         is.na(Ecoul_nat),
         is.na(Transition))

paysages <- read_sf("chemin/vers/ma/couche/paysages.gpkg") # ou .shp ou en .RData 

## Vérification ----
mapview(paysages)

# SI besoin de rendre la couche valide
paysages <- paysages %>%
  sf :: st_make_valid()

paysages <- paysages %>%
  fortify()

# Nommage ----
## Don d'un nom à chaque entité ----
paysages <- paysages %>% 
  mutate(NOM_UNITE = if_else(is.na(NOM_UNITE), nom_up, NOM_UNITE)) %>% 
  mutate(ID_UNITE = as.character(ID_UNITE))

## Don d'un ID à chaque entité ----
paysages <- paysages %>% 
  mutate(ID_UNITE = if_else(is.na(ID_UNITE), id_up, ID_UNITE)) %>% 
  mutate(ID_UNITE = if_else(is.na(ID_UNITE), ID, ID_UNITE)) %>% 
  select(ID_UNITE:area, famille_up, url_atlas, geom) #suppression des colonnes inutiles

# Attribution ----
intersections <- st_intersection(plando_comm_her_me, paysages)

# Compter le nombre de polygones de "plando" présents dans chaque polygone de "paysages"
counts <- intersections %>% 
  group_by(ID_UNITE) %>%
  st_drop_geometry() %>% 
  summarise(n = n())

paysages_plando <- paysages %>% 
  left_join(y = counts)

# Sauvegarde ----
save(paysages_plando,
     file = "processed_data/plando_paysages.RData")

st_write(paysages_plando,
         dsn = "chemin/vers/on/fichier/plando_paysages.gpkg")