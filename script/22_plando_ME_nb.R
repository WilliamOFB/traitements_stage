# Packages ----
library(tidyverse)
library(mapview)
library(sf)
library(dplyr)

# Import ----
plando <- read_sf("../../SIG/2-Exploitation/Plando/plando_prelev_uniques.gpkg")

massdo_total <- read_sf("../../SIG/2-Exploitation/Masses_eau/ME_toutes/massdo_toutes_full_tbv.gpkg")

massdo_perim <- read_sf("../../SIG/2-Exploitation/Masses_eau/ME_toutes/massdo_perim_full_surf.gpkg")

massdo_courdo <- read_sf("../../SIG/2-Exploitation/Masses_eau/massdo_full_ripi.gpkg")

plando_courdo <- read_sf("../../SIG/2-Exploitation/Plando/plando_cours_inME.gpkg")

## Vérification ----
mapview(massdo_total)

plando_uniques <- plando %>% 
  distinct(gid_plando,
           surface_plando,
           .keep_all = TRUE)

## Sélection des PE à attribuer
plando_select_zh <- plando %>% 
  filter(is.na(Orage),
         is.na(Ecoul_nat),
         is.na(Marais),
         is.na(ERU),
         ZH == 1)

plando_mares <- plando_uniques %>% 
       filter(Mares == 1,
              is.na(Orage),
              is.na(Ecoul_nat),
              is.na(Marais),
              is.na(ERU))

# Compte PE par ME ----
## Attribution ----
#intersect <- st_intersection(plando_uniques, massdo) %>% 
#  mutate(surface_intersect = st_area(.)) %>%
#  st_drop_geometry() %>% 
#  mutate(pc_plando_sur_me = surface_intersect / surface_plando)

# Compter le nombre de polygones de "plando" présents dans chaque polygone de "me"
counts_total <- plando_select %>% 
  group_by(cdbvspemdo) %>%
  summarise(n_PE_full = n()) %>% 
  st_drop_geometry()
#  select(gid_me,
#         nombvspemd,
#         n)

nb_me_plando <- massdo_total %>% 
  left_join(y = counts_total)

nb_me_plando <- nb_me_plando %>% 
  left_join((y=counts))

nb_me_plando <- nb_me_plando %>% 
  mutate(n_PE = n_PE_good) %>% 
  select(fid:geom)

nb_me_plando <- nb_me_plando %>% 
  mutate(Ptage_PEzh_full = (n_PE_zh_ful / n_PE_full)*100)

## Plans d'eau connectes ----
### Tri des entités ----
plando_conn <- plando_select %>% 
  filter(NAPPE == 1 |
           R0 == 1 |
           distance == 0 |
           sur_cours_Police == 0)

### Attribution ----
counts_conn <- plando_conn %>% 
  group_by(cdbvspemdo) %>%
  summarise(n_PE_conn = n()) %>% 
  st_drop_geometry

nb_me_plando_conn <- nb_me_plando %>% 
  left_join(y = counts_conn)

nb_me_plando_conn <- nb_me_plando_conn %>% 
  mutate(dens_num_PE_conn = (n_PE_conn / (surfacebvs/1000000)))

## Sauvegarde ----
save(nb_me_plando,
     file = "processed_data/nb_plando_me.RData")

#st_write(nb_me_plando,
#         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_toutes/massdo_perim_full_nb3.gpkg")

st_write(nb_me_plando_conn,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_toutes/massdo_toutes_full_nb_conn.gpkg")

# Compte nombre PE inf et sup 500m² par ME ----
## Séletion des entités ----
plando_inf500 <- plando_select %>% 
  filter(surface_plando <= 500)

plando_sup500 <- plando_select %>% 
  filter(surface_plando >= 500)

### Vérification ----
doublons <- plando_sup500 %>% 
  group_by(gid_plando,
           surface_plando) %>% 
  filter(n() >  1)

plando500_uniques <- plando_sup500 %>% 
  distinct(gid_plando,
           surface_plando,
           .keep_all = TRUE)

## Plans d'eau en général ----
###Attribution ----
#### Inférieur à 500 ----
counts_500 <- plando_inf500 %>% 
  group_by(cdbvspemdo) %>%
  summarise(n_PE_inf500 = n()) %>% 
  st_drop_geometry

nb_me_plando_500 <- nb_me_plando_conn %>% 
  left_join(y = counts_500)

#### Supérieur à 500 ----
counts_sup500 <- plando_sup500 %>% 
  group_by(cdbvspemdo) %>%
  summarise(n_PE_sup500_good = n()) %>% 
  st_drop_geometry

nb_me_plando_sup500 <- nb_me_plando_500 %>% 
  left_join(y = counts_sup500)

nb_me_plando_sup500 <- nb_me_plando_sup500 %>% 
  mutate(n_PE_sup500 = n_PE_sup500_good) %>% 
  select(fid:n_PE_inf500)

## Sauvegarde ----
#st_write(nb_me_plando_500,
#         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_toutes/massdo_perim_full_nb.gpkg")

# Compte nombre PE sup 1000m² par ME ----
## Séletion des entités ----
plando_sup1000 <- plando_select %>% 
  filter(surface_plando >= 1000)

## Plans d'eau en général ----
###Attribution ----
counts_1000 <- plando_sup1000 %>% 
  group_by(cdbvspemdo) %>%
  summarise(n_PE_sup1000_good = n()) %>% 
  st_drop_geometry

nb_me_plando_1000 <- nb_me_plando_sup500 %>% 
  left_join(y = counts_1000)

## Sauvegarde ----
st_write(nb_me_plando1000_full,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_toutes/massdo_toutes_nb1000.gpkg")

## Plans d'eau sur cours ----
### Tri des entités ----
plando_500_cours <- plando_inf500 %>% 
  filter(distance == 0,
         sur_cours_Police == 0)

###Attribution ----
counts_500_cours <- plando_500_cours %>% 
  group_by(cdbvspemdo) %>%
  summarise(n_PE_cours_inf500 = n()) %>% 
  st_drop_geometry

nb_me_plando500_cours <- nb_me_plando_1000 %>% 
  left_join(y = counts_500_cours)

nb_me_plando500_cours <- nb_me_plando500_cours %>% 
  select(fid:n_PE_sup500,
         n_PE_tbv:n_PE_cours_inf500)

## Sauvegarde ----
#st_write(nb_me_plando_500,
#         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_toutes/massdo_perim_full_nb.gpkg")

## Plans d'eau sur source ----
### Tri des entités ----
plando_500_source <- plando_inf500 %>% 
  filter(R0 == 1)

###Attribution ----
counts_500_source <- plando_500_source %>% 
  group_by(cdbvspemdo) %>%
  summarise(n_PE_source_inf500 = n()) %>% 
  st_drop_geometry

nb_me_plando500_source <- nb_me_plando500_cours %>% 
  left_join(y = counts_500_source)

## Sauvegarde ----
#st_write(nb_me_plando_500,
#         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_toutes/massdo_perim_full_nb.gpkg")

## Plans d'eau sur nappe ----
### Tri des entités ----
plando_500_nappe <- plando_inf500 %>% 
  filter(NAPPE == 1)

###Attribution ----
counts_500_nappe <- plando_500_nappe %>% 
  group_by(cdbvspemdo) %>%
  summarise(n_PE_nappe_inf500 = n()) %>% 
  st_drop_geometry

nb_me_plando500_nappe <- nb_me_plando500_source %>% 
  left_join(y = counts_500_nappe)

## Sauvegarde ----
#st_write(nb_me_plando_500,
#         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_toutes/massdo_perim_full_nb.gpkg")

## Plans d'eau en sur ZH ----
### Tri des entités ----
plando_500_zh <- plando_inf500 %>% 
  filter(ZH == 1)

###Attribution ----
counts_500_zh <- plando_500_zh %>% 
  group_by(cdbvspemdo) %>%
  summarise(n_PE_zh_inf500 = n()) %>% 
  st_drop_geometry

nb_me_plando500_zh <- nb_me_plando500_nappe %>% 
  left_join(y = counts_500_zh)

## Sauvegarde ----
#st_write(nb_me_plando_zh,
#         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_toutes/massdo_perim_full_nb.gpkg")

## Plans d'eau TBV ----
### Tri des entités ----
plando_tbv <- plando_select %>% 
  filter(join_StreamOrde == 1 |
         join_StreamOrde == 2)

###Attribution ----
counts_tbv <- plando_tbv %>% 
  group_by(cdbvspemdo) %>%
  summarise(n_PE_tbv_good = n()) %>% 
  st_drop_geometry

nb_me_plando_tbv <- massdo %>% 
  left_join(y = counts_tbv)

nb_me_plando_tbv <- nb_me_plando_tbv %>% 
  mutate(n_PE_tbv = n_PE_tbv_good) %>% 
  mutate(Ptage_PE_tbv = (n_PE_tbv_good / n_PE)*100) %>%
  select(gid_me:n_PE_zh_inf500)

## Sauvegarde ----
save(nb_me_plando_tbv,
     file = "processed_data/massdo_toutes_full_nb.RData")

st_write(nb_me_plando_tbv,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_toutes/massdo_toutes_full_tbv.gpkg")
