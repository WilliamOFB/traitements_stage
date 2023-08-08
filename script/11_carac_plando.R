### Valeurs détaillées des plando aux masses d'eau ###
# Packages ----
library(tidyverse)
library(dplyr)
library(mapview)
library(sf)

# Imports ----
massdo <- read_sf("chemin/vers/ma/couche/massdo.gpkg") %>% # ou .shp ou en .RData
  st_transform(2154)

plando <- read_sf("chemin/vers/ma/couche/plando.gpkg") %>% # ou .shp ou en .RData
  filter(is.na(Orage),
         is.na(Ecoul_nat),
         is.na(Transition),
         is.na(ERU))

plando_ssMarais <- plando_select %>% 
  filter(is.na(Marais))

source <- read_sf("chemin/vers/ma/couche/source_topage_courdo.shp")

# Attribution à la ME de la distance moyenne du plando au courdo ----
plando_dist_CE <- plando_select %>% 
  group_by(cdbvspemdo) %>% 
  summarise(mean_dist_CE = mean(distance)) %>% 
  st_drop_geometry()

massdo <- massdo %>% 
  left_join(plando_dist_CE)

# Attribution à la ME de la distance moyenne du plando sur source à la source ----
## Traitements ----
plando_source <- plando_select %>% 
  filter(R0_Topage == 1)

plando_source_dist <- plando_source %>% 
  group_by(cdbvspemdo) %>% 
  summarise(mean_dist_source = mean(dist_R0_Top)) %>% 
  st_drop_geometry()

massdo <- massdo %>% 
  left_join(plando_source_dist)

## Sauvegarde ----
save(massdo,
     file = "processed_data/massdo_source.RData")

write_sf(massdo,
         dsn = "chemin/vers/mon/fichier/massdo_source.gpkg")

# Taille moyenne / médiane des PE par ME ----
## En général ----
plando_taille_mean <- plando_select %>% 
  group_by(cdbvspemdo) %>% 
  summarise(mean_surf = mean(surface_plando)) %>% 
  st_drop_geometry()

massdo <- massdo %>% 
  left_join(plando_taille_mean)

### Sauvegarde intermédiaire ----
write_sf(massdo,
         dsn = "chemin/vers/mon/fichier/massdo_mean_surf.gpkg")