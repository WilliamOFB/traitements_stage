### Quelle longueur de courdo le plando intercepte-t-il ? ###

# Packages ----
library(sf)
library(tidyverse)
library(dplyr)
library(mapview)

rm(list = setdiff(ls(), "plando")) # suppression des objets sauf "plando"

# Import ----
plando <- read_sf("chemin/vers/ma/couche/plando.gpkg") %>% # ou .shp ou en .RData
  filter(is.na(Orage),
         is.na(Ecoul_nat),
         is.na(Transition),
         is.na(ERU))

courdo <- read_sf("chemin/vers/ma/couche/courdo.gpkg") %>% # ou .shp ou en .RData
  st_transform(crs=2154)

# Sélection ----
plando_courdo <- plando %>% 
  dplyr::filter(dist_courdo==0)

## Initialisation de la colonne "longueur_interceptee" à 0 ----
plando_courdo$longueur_interceptee <- 0

## Attribution ----
intersect <- st_intersection(plando_sur_ce, troncon) %>% 
  select(gid_plando,
         gid_ce) %>% 
  mutate(long_intercept = st_length(intersect))

intersect_test <- intersect %>% 
  group_by(gid_plando) %>% 
  summarize(long_intercept = sum(long_intercept,
                                       na.rm = TRUE)) %>% 
  st_drop_geometry()

plando_intercept <- plando %>% 
  left_join(y = intersect_test)

## Sauvegarde ----
save(plando_intercept,
     file = "chemin/vers/ma/couche/plando.RData")

st_write(plando_intercept,
         dsn = "chemin/vers/ma/couche/plando_intersect.gpkg")

# Linéaire intercepté des courdo par PE par ME ----
## Imports supplémentaires ----
massdo <- st_read("chemin/vers/ma/couche/massdo.gpkg") # ou .shp ou en .RData

## Attribution ----
plando_me_intercept <- plando_intercept %>% 
  group_by(cdbvspemdo) %>% 
  summarize(long_intercept = sum(long_intercept)) %>% 
  st_drop_geometry()

massdo_long_intercept <- plando_me_intercept %>% 
  left_join(y = massdo)

## Sauvegarde ----
st_write(massdo_long_intercept,
         dsn = "chemin/vers/mon/fichier/massdo_courdo.gpkg")