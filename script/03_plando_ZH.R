### Attribution aux plans d'eau de zone humide probable ###
# Packages ----
library(sf)
library(tidyverse)
library(mapview)

# Imports ----
zh <- read_sf('chemin/vers/ma/couche/zh_vecteur.gpkg') %>% # ou .shp ou en .RData 
  st_transform(crs = 2154)

plando <- read_sf("chemin/vers/ma/couche/plando.gpkg") # ou .shp ou en .RData 

# Traitements ----
plando_index <- plando %>% 
  st_join(zh,
          join = st_intersects,
          largest = FALSE)

plando_select <- plando_index %>% 
  mutate(ZH = if_else(!is.na(zh), 1, 0)) %>% 
  st_drop_geometry()

## Sauvegarde ----
save(plando_select,
     file = "chemin/vers/mon/fichier/plando_zh.RData")

st_write(plando_select,
         dsn = "chemin/vers/mon/fichier/plando_zh.gpkg")