### Attribution des valeurs de plans d'eau aux masses d'eau ###
# Packages ----
library(tidyverse)
library(mapview)
library(sf)
library(dplyr)

# Import ----
plando <- read_sf("chemin/vers/ma/couche/plando.gpkg") %>% # ou .shp ou en .RData
  st_transform(crs = 2154) %>% 
  filter(is.na(Orage),
         is.na(Ecoul_nat),
         is.na(Transition),
         is.na(ERU))

massdo <- read_sf("chemin/vers/ma/couche/massdo.gpkg") %>% # ou .shp ou en .RData
  st_transform(2154)

## Vérification ----
mapview(massdo_total)

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

plando_mares <- plando %>% 
       filter(Mares == 1)

# Compte PE par ME ----
## Attribution ----
counts_total <- plando %>% # compte du nombre de polygones de "plando" présents dans chaque polygone de "me"
  group_by(cdbvspemdo) %>%
  summarise(n_PE = n()) %>% 
  st_drop_geometry()

nb_plando_me <- massdo %>% 
  left_join(y = counts_total)

### Sauvegarde ----
save(nb_plando_me,
     file = "processed_data/massdo_plando.RData")

st_write(nb_plando_me,
         dsn = "chemin/vers/mon/fichier/massdo_plando.gpkg")

## Plans d'eau sur cours ----
### Attribution ----
counts_cours <- plando_cours %>% # possibilité de changer le code ci-dessous pour source, nappe, zh, connectés...
  group_by(cdbvspemdo) %>%
  summarise(n_PE_cours = n()) %>% 
  st_drop_geometry

nb_plando_cours_me <- nb_plando_me %>% 
  left_join(y = counts_cours)

nb_plando_cours_me <- nb_plando_cours_me %>% 
  mutate(dens_num_PE_cours = (n_PE_cours / (surfacebvs/1000000)))

## Plans d'eau TBV ----
###Attribution ----
counts_tbv <- plando_tbv %>% 
  group_by(cdbvspemdo) %>%
  summarise(n_PE_tbv = n()) %>% 
  st_drop_geometry

nb_plando_tbv_me <- nb_plando_me %>% 
  left_join(y = counts_tbv)

nb_plando_tbv_me <- nb_plando_tbv_me %>% 
  mutate(n_PE_tbv = n_PE_tbv) %>% 
  mutate(Ptage_PE_tbv = (n_PE_tbv / n_PE)*100)

## Sauvegarde ----
save(nb_plando_tbv_me,
     file = "processed_data/massdo_plando.RData")

st_write(nb_plando_tbv_me,
         dsn = "chemin/vers/mon/fichier/massdo_plando.gpkg")
