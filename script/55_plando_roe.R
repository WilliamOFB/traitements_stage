# Packages ----
library(tidyverse)
library(dplyr)
library(mapview)
library(sf)
library(sp)

# Import ----
roe <- read_sf("../../SIG/2-Exploitation/ROE/ROE_perimetre_etude.shp") %>% 
  st_transform(crs = 2154)

plando <- read_sf("processed_data/plando_comm_her_me.gpkg")# %>% 
#  st_transform(crs = 2154)

# Traitements ----
plus_proche_plando <- sf::st_nearest_feature(x = roe,
                                             y = plando)

dist <- st_distance(roe, plando[plus_proche_plando,], by_element = TRUE)

test_roe <- roe %>% 
  cbind(dist) %>% 
  cbind(plando[plus_proche_plando,]) %>% 
  select(identifian,
         statut_cod,
         nom_princi,
         fpi_nom1,
         emo_nom1,
         fnt_nom1,
         usage_nom1,
         usage_nom2,
         usage_nom3,
         usage_nom4:hauteur__1,
         nom_topo,
         distance_m = dist,
         gid_plando) %>% 
  sf::st_drop_geometry() %>% 
  mutate(distance_m = round(distance_m))

test_plando <- plando %>% 
  left_join(y = test_roe)

test_plando <- test_plando %>% 
  sf::st_make_valid()

test_plando <- test_plando %>% 
  fortify()

## Sauvegarde ----
st_write(test_plando,
         dsn = "../../SIG/2-Exploitation/ROE/plando_delim_roe.gpkg")

## Test sur Rennes ----
plando_rennes <- plando %>% 
  filter(NOM == 'Rennes')

roe_rennes <- roe %>% 
  filter(commune_no == 'RENNES')

proche_plando_rennes <- sf::st_nearest_feature(x = roe_rennes,
                                             y = plando_rennes)

dist_rennes <- st_distance(roe_rennes, plando_rennes[proche_plando_rennes,], by_element = TRUE)

test_roe_rennes <- roe_rennes %>% 
  cbind(dist_rennes) %>% 
  cbind(plando_rennes[proche_plando_rennes,]) %>% 
  select(identifian,
         statut_cod,
         nom_princi,
         fpi_nom1,
         emo_nom1,
         fnt_nom1,
         usage_nom1,
         usage_nom2,
         usage_nom3,
         usage_nom4:hauteur__1,
         nom_topo,
         distance_m = dist_rennes,
         gid_plando) %>% 
  sf::st_drop_geometry() %>% 
  mutate(distance_m = round(distance_m))

test_plando_rennes <- plando_rennes %>% 
  left_join(y = test_roe_rennes)

test_plando_rennes <- test_plando_rennes %>% 
  sf::st_make_valid()

test_plando_rennes <- test_plando_rennes %>% 
  fortify()

mapview(test_plando_rennes) +
  mapview(roe_rennes)