### Vérification des géométries des plans d'eau ###
# Packages ----
library(tidyverse)
library(mapview)
library(sf)
library(dplyr)

# Imports ----
plando <- read_sf("chemin/vers/ma/couche/plando.gpkg") %>% # ou .shp
  st_transform (crs = 2154)

plando_uniques <- plando %>% 
  distinct(gid_plando,
           surface_plando,
           .keep_all = TRUE)

# Vérification ----
mapview(plando)

# Sauvegarde ----
save(plando,
     file = "processed_data/plando.RData")