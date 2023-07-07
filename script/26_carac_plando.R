# Préparation ----
## Packages ----
library(tidyverse)
library(dplyr)
library(mapview)
library(sf)

## Imports ----
massdo <- read_sf("../../SIG/2-Exploitation/Masses_eau/ME_toutes/massdo_nb_mares_full.gpkg")

plando <- read_sf("../../SIG/2-Exploitation/Plando/plando_prelev_uniques.gpkg")

plando_select <- plando %>% 
  filter(is.na(Orage),
         is.na(Ecoul_nat),
         is.na(Transition),
         is.na(ERU))

plando_ssMarais <- plando_select %>% 
  filter(is.na(Marais))

source <- read_sf("../../SIG/2-Exploitation/Sources/R0_TOPAGE_BREPDL_HER.shp")

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
# OU
plando_source <- read_sf("../../SIG/2-Exploitation/Plando/plando_source.gpkg")

plando_source_dist <- plando_source %>% 
  group_by(cdbvspemdo) %>% 
  summarise(mean_dist_source = mean(dist_R0_Top)) %>% 
  st_drop_geometry()

massdo <- massdo %>% 
  left_join(plando_source_dist)

## Sauvegarde ----
save(massdo,
     file = "processed_data/massdo_230707.RData")

write_sf(massdo,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_toutes/massdo_dist_source.gpkg")

# Taille moyenne / médiane des PE par ME ----
## En général ----
plando_taille_mean <- plando_select %>% 
  group_by(cdbvspemdo) %>% 
  summarise(mean_surf = mean(surface_plando)) %>% 
  st_drop_geometry()

massdo <- massdo %>% 
  left_join(plando_taille_mean)

## Selon le rang de Strahler ----


## Selon la distance au CE ----

