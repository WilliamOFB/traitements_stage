### Calcul des densités numériques et surfaciques des PE par communes ###
### WARNING : un PE est attribué à 1 seule commune et n'est pas coupé selon les limites
### Pour voir ce traitement, ouvrir "..._densites"

# Packages ----
library(tidyverse)
library(dplyr)
library(mapview)
library(sf)

# Imports ----
communes <- read_sf("chemin/vers/ma/couche/communes.gpkg") # ou .shp ou en .RData

plando <- read_sf("chemin/vers/ma/couche/plando.RData") # ou .shp ou en .RData

# Traitements ----
communes <- communes %>% 
  mutate(surface_comm = st_area(.))

surf_plando_comm <- plando %>% 
  select(surface_plando,
         INSEE_COM,
         NOM) %>% 
  st_drop_geometry() %>% 
  group_by(INSEE_COM, NOM) %>% 
  dplyr::summarise(surf_plando_comm = sum(surface_plando)) %>% 
  view()

communes <- communes %>% 
  left_join(y = surf_plando_comm) %>% 
  mutate(densite_surf = (surf_plando_comm / surface_comm)*100)

## Certaines communes n'ont pas de PE ----
test <- communes %>% 
  select(INSEE_COM,
         NOM,
         surf_plando_comm) %>% 
  st_drop_geometry()

setdiff(test, surf_plando_comm)

## On attribut 0 aux valeurs NA ----
communes <- communes %>% 
  mutate_if(is.numeric, ~replace(.,is.na(.),0))

mapview(communes)
