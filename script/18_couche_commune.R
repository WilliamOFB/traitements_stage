# Packages ----
library(tidyverse)
library(mapview)
library(sf)
library(dplyr)
library(stringr)

# Import ----
plando <- read_sf("../../SIG/2-Exploitation/Plando/plando_full_source_230712.gpkg") %>% 
  filter(is.na(ERU),
         is.na(Orage),
         is.na(Ecoul_nat),
         is.na(Transition))

plando_uniques <- plando %>% 
  distinct(gid_plando,
           surface_plando,
           .keep_all = TRUE)

commune <- read_sf("../../SIG/2-Exploitation/Admin/communes_perimetre.gpkg")

courdo <- read_sf("../../SIG/Cours_eau/TronconHydrographique_Bretagne_Pays_de_la_Loire_non_aqueduc_strahler.shp") %>% 
  rename(gid_ce = gid) %>% 
  st_transform(crs = 2154) %>% 
  mutate(long = st_length(geometry))

# Pré-traitement ----
inter_plando_comm <- st_intersection(plando, commune)

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
## Sélection des plans d'eau
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
  summarise(n_PE = n())

plando_cours_comm <- plando_cours %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_cours = n())

plando_source_comm <- plando_source %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_source = n())

plando_nappe_comm <- plando_nappe %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_nappe = n())

plando_zh_comm <- plando_zh %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_zh = n())

plando_conn_comm <- plando_conn %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_conn = n())

plando_tbv_comm <- plando_tbv %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_tbv = n())

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
  mutate(dens_num = n_PE/(Superficie/1000000),
         dens_num_cours = n_PE_cours/(Superficie/1000000),
         dens_num_source = n_PE_source/(Superficie/1000000),
         dens_num_nappe = n_PE_nappe/(Superficie/1000000),
         dens_num_zh = n_PE_zh/(Superficie/1000000),
         dens_num_conn = n_PE_conn/(Superficie/1000000),
         dens_num_tbv = n_PE_tbv/(Superficie/1000000))

## Surface(s) et densité surfacique ----
surf_plando_comm <- inter_plando_comm %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(surf_PE = sum(surface_plando))

surf_plando_cours_comm <- plando_cours %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(surf_PE_cours = sum(surface_plando))

surf_plando_source_comm <- plando_source %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(surf_PE_source = sum(surface_plando))

surf_plando_nappe_comm <- plando_nappe %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(surf_PE_nappe = sum(surface_plando))

surf_plando_zh_comm <- plando_zh %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(surf_PE_zh = sum(surface_plando))

surf_plando_conn_comm <- plando_conn %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(surf_PE_conn = sum(surface_plando))

surf_plando_tbv_comm <- plando_tbv %>% 
  group_by(INSEE_COM) %>% 
  st_drop_geometry() %>% 
  summarise(surf_PE_tbv = sum(surface_plando))

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
  mutate(dens_surf = (surf_PE/(Superficie))*100)

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
     file = "processed_data/comm_perimetre_230718.RData")

st_write(commune,
         dsn = "../../SIG/2-Exploitation/Admin/commune_full_230718.gpkg")
