### Attribution des HER2 et communes aux plans d'eau ###
# Packages ----
library(mapview)
library(sf)
library(devtools)
library(tidyverse)
library(stringi)

# Imports ----
her2 <- read_sf("chemin/vers/ma/couche/her2.gpkg") %>% # ou .shp
  st_transform(crs = 2154)

plando <- read_sf(".chemin/vers/ma/couche/plando.gpkg") %>% # ou .shp ou .RData
  st_transform(crs = 2154) %>% 
  mutate(surface_plando = st_area(.)) %>% 
  rename(gid_plando = gid)

communes <- read_sf("chemin/vers/ma/couche/communes.gpkg") %>% # ou .shp
  st_transform(2154)

## Vérification ----
mapviewOptions(fgb = FALSE)
mapview (hec2)
mapview(plando)
mapview(communes)

names(plando)

# HER2 ----
## Renommage de certains attributs ----
nouveaux_noms <- stringr::str_replace(string = names(plando),
                                      pattern = ",C,254",
                                      replacement = "_BR")

nouveaux_noms <- stringr::str_replace(string = nouveaux_noms,
                                      pattern = ",N,10,0",
                                      replacement = "_BR")

nouveaux_noms <- stringr::str_replace(string = nouveaux_noms,
                                      pattern = "50s",
                                      replacement = "cinquante")
nouveaux_noms

names(plando) <- nouveaux_noms

names(hec2)

## Intersection entre plando et her2 ----
plando_her2 <- plando %>% 
  st_intersection(her2) %>% # découpage des PE selon les her2
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry() %>% 
  select(gid_plando,
         CdHER2,
         NomHER2,
         surface_plando,
         surface_intersect) %>% 
  mutate(pc_plando_sur_her2 = surface_intersect / surface_plando)

## Affectation d'une seule her2 à un plan d'eau ----
affectation_her2 <- plando_her2 %>% 
  group_by(gid_plando) %>%  # groupement pour chaque plan d'eau selon leur gid
  filter(pc_plando_sur_her2 == max(pc_plando_sur_her2)) %>% # pourcentage maxi
  ungroup()

plando_her2 <- plando %>% 
  left_join(y = affectation_her2) %>% 
  select(gid_plando,
         CdOH,
         CdPlanEau_,
         NatureSE,
         Persistanc,
         SaliniteSE,
         join_gid,
         join_CdOH,
         join_Persistanc,
         join_SensEcoule,
         join_CdCoursEau,
         join_StreamOrde,
         distance,
         Marais,
         ERU,
         Transition,
         R0,
         NAPPE,
         join_QABASN,
         join_QAMOY_MN,
         join_QAHAUN,
         join_ROBUSTES_1,
         join_QA_LOCAL,
         distance_2,
         prel_VOLUME,
         prel_USAGE,
         prel_USAGE_BR,
         cinquante,
         join_join_ID_BDCARTH,
         join_Q5BASN,
         join_Q5HAUN,
         join_Q5MOY_MN,
         join_ROBUSTES_1_2,
         ROE_identifian,
         ROE_usage_nom1,
         ROE_usage_nom2,
         ROE_usage_nom3,
         ROE_usage_nom4,
         distance_3,
         surface_plando,
         CdHER2,
         NomHER2)

## Sauvegarde ----
save(plando_her2,
     file = "chemin/vers/ma/sauvegarde/plando_her2.RData")

sf::st_write(plando_her2,
             dsn="processed_data/plando_her2.gpkg")

# Communes ----
communes29 <- communes %>% 
  filter(INSEE_DEP == "29")

mapview(communes29)

communes22 <- communes %>% 
  filter(INSEE_DEP == "22")

communes56 <- communes %>% 
  filter(INSEE_DEP == "56")

communes35 <- communes %>% 
  filter(INSEE_DEP == "35")

communes44 <- communes %>% 
  filter(INSEE_DEP == "44")

communes85 <- communes %>% 
  filter(INSEE_DEP == "85")

communes49 <- communes %>% 
  filter(INSEE_DEP == "49")

communes72 <- communes %>% 
  filter(INSEE_DEP == "72")

communes53 <- communes %>% 
  filter(INSEE_DEP == "53")

## Intersection entre plando et communes ----
plando_comm <- plando %>%
  st_intersection(communes) %>% # découpage des plans d'eau selon les communes
  mutate(surface_intersect = st_area(.)) %>%   # superficie des intersects
  select(gid_plando,  # sélection des variables à conserver
         INSEE_COM,
         NOM,
         INSEE_DEP,
         surface_plando,
         surface_intersect) %>%   
  st_drop_geometry() %>% # suppression de la géométrie
  mutate(pc_du_plando_sur_com = surface_intersect / surface_plando) # calcul du % de chaque plando par communes

## Affectation d'une seule commune à un plan d'eau ----
affectation_commune <- plando_comm %>% 
  group_by(gid_plando) %>%  # groupement pour chaque plan d'eau selon leur gid
  filter(pc_du_plando_sur_com == max(pc_du_plando_sur_com)) %>% # pourcentage maxi
  ungroup() %>% 
  select(gid_plando, # sélection des variables à conserver
         INSEE_COM,
         NOM,
         INSEE_DEP,
         surface_plando)

plando_comm <- plando %>% 
  left_join(y = affectation_commune) %>% 
  select(NatureSE,
         OrigineSE,
         Persistanc,
         surface_plando,
         INSEE_COM,
         INSEE_DEP,
         NOM)
## Vérification ----
mapview(plando_comm,
        col.region = "red")+
  mapview(communes,
          alpha = 0.5,
          alpha.region = 0.2)

save(plando_comm,
     file = "chemin/vers/ma/sauvegarde/plando_comm.RData")

sf::st_write(plando_comm,
             dsn="processed_data/plando_comm.gpkg")
