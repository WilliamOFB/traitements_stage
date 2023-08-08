### Création de la couche communes ###
# Packages ----
library(tidyverse)
library(mapview)
library(sf)
library(dplyr)
library(stringr)

# Imports ----
plando <- read_sf("chemin/vers/ma/couche/plando.gpkg") %>% # ou .shp ou en .RData 
  filter(is.na(ERU),
         is.na(Orage),
         is.na(Ecoul_nat),
         is.na(Transition))

plando_ssMarais <- plando %>% 
  filter(is.na(Marais))

commune <- read_sf("chemin/vers/ma/couche/communes.gpkg") # ou .shp ou en .RData 

courdo <- read_sf("chemin/vers/ma/couche/courdo.shp") %>% # ou .shp ou en .RData
  rename(gid_ce = gid) %>% 
  st_transform(crs = 2154) %>% 
  mutate(long = st_length(geometry))

# Pré-traitement ----
inter_plando_comm <- st_intersection(plando_ssMarais, commune)

inter_plando_comm <- inter_plando_comm %>% 
  select(gid_plando,
         distance:join_QAHAUN,
         join_Q5BASN:join_Q5HAUN,
         surface_plando,
         ZH:X2016_1,
         PERIM:R0_Topage,
         ID:INSEE_REG,
         join_StreamOrde) %>% 
  mutate(surface_plando = st_area(geom)) %>% 
  rename(INSEE_COM = INSEE_COM.1)

inter_plando_comm <- inter_plando_comm %>% 
  mutate(surf_intersect = st_area(.))

# Attribution des plando ----
## Sélection des plans d'eau ----
plando_cours <- inter_plando_comm %>% 
  filter(distance == 0)

plando_source <- inter_plando_comm %>% 
  filter(R0_Topage == 1)

plando_nappe <- inter_plando_comm %>% 
  filter(NAPPE == 1)

plando_zh <- inter_plando_comm %>% 
  filter(ZH == 1)

plando_conn <- inter_plando_comm %>% 
  filter(distance == 0 |
           NAPPE == 1 |
           R0_Topage == 1)

plando_tbv <- inter_plando_comm %>% 
  filter(join_StreamOrde == c(1,2),
         distance <= 500)

## Décompte des plans d'eau ----
plando_comm <- inter_plando_comm %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_ssMarais = n()) # Possibilité de supprimer '_ssMarais' dans la suite du code

plando_cours_comm <- plando_cours %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_cours_ssMarais = n())

plando_source_comm <- plando_source %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_source_ssMarais = n())

plando_nappe_comm <- plando_nappe %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_nappe_ssMarais = n())

plando_zh_comm <- plando_zh %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_zh_ssMarais = n())

plando_conn_comm <- plando_conn %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_conn_ssMarais = n())

plando_tbv_comm <- plando_tbv %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_tbv_ssMarais = n())

liste_ncomm <- list(commune,
                    plando_comm,
                    plando_cours_comm,
                    plando_source_comm,
                    plando_nappe_comm,
                    plando_zh_comm,
                    plando_conn_comm,
                    plando_tbv_comm)

commune <- liste_ncomm %>% 
  reduce(left_join,
         by = "INSEE_COM")

## Densités numériques ----
commune <- commune %>%
  mutate(dens_num_ssMarais = n_PE_ssMarais/(Superficie/1000000),
         dens_num_cours_ssMarais = n_PE_cours_ssMarais/(Superficie/1000000),
         dens_num_source_ssMarais = n_PE_source_ssMarais/(Superficie/1000000),
         dens_num_nappe_ssMarais = n_PE_nappe_ssMarais/(Superficie/1000000),
         dens_num_zh_ssMarais = n_PE_zh_ssMarais/(Superficie/1000000),
         dens_num_conn_ssMarais = n_PE_conn_ssMarais/(Superficie/1000000),
         dens_num_tbv_ssMarais = n_PE_tbv_ssMarais/(Superficie/1000000))

## Surface(s) et densité surfacique ----
surf_plando_comm <- inter_plando_comm %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(surf_PE_ssMarais = sum(surface_plando))

surf_plando_cours_comm <- plando_cours %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(surf_PE_cours_ssMarais = sum(surface_plando))

surf_plando_source_comm <- plando_source %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(surf_PE_source_ssMarais = sum(surface_plando))

surf_plando_nappe_comm <- plando_nappe %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(surf_PE_nappe_ssMarais = sum(surface_plando))

surf_plando_zh_comm <- plando_zh %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(surf_PE_zh_ssMarais = sum(surface_plando))

surf_plando_conn_comm <- plando_conn %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(surf_PE_conn_ssMarais = sum(surface_plando))

surf_plando_tbv_comm <- plando_tbv %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(surf_PE_tbv_ssMarais = sum(surface_plando))

list_surf_comm <- list(commune,
                       surf_plando_comm,
                       surf_plando_cours_comm,
                       surf_plando_source_comm,
                       surf_plando_nappe_comm,
                       surf_plando_zh_comm,
                       surf_plando_conn_comm,
                       surf_plando_tbv_comm)

commune <- list_surf_comm %>% 
  reduce(left_join,
         by = "INSEE_COM") %>% 
  mutate(dens_surf_ssMarais = (surf_PE_ssMarais/(Superficie))*100)

# Si besoin : possibilité de calculer les densités surfaciques par type de plando

# Attribution des cours d'eau
## Totalité des courdo ----
inter_courdo_comm <- st_intersection(courdo, commune) 

inter_courdo_comm <- inter_courdo_comm %>% 
  mutate(long_intersect = st_length(geometry)) %>%
  st_drop_geometry() %>% 
  select(gid_ce,
         StreamOrde,
         INSEE_COM,
         long_intersect)

compte_courdo <- inter_courdo_comm %>% 
  group_by(INSEE_COM) %>%
  summarise(lineaire_CE = sum(long_intersect))

commune <- commune %>% 
  left_join(compte_courdo)

## Courdo de TBV ----
courdo_tbv_comm <- inter_courdo_comm %>% 
  filter(StreamOrde == c(1,2))

compte_ce_tbv <- courdo_tbv_comm %>% 
  group_by(INSEE_COM) %>% 
  summarise(lineaire_tbv = sum(long_intersect))

commune <- commune %>% 
  left_join(compte_ce_tbv)

# TBV ----
commune <- commune %>% 
  mutate(Ptage_PE_tbv = (n_PE_tbv / n_PE)*100)

# Sauvegarde finale ----
save(commune,
     file = "processed_data/communes.RData")

st_write(commune,
         dsn = "chemin/vers/mon/fichier/communes.gpkg")
