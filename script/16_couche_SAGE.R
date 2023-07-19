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

sage <- read_sf("../../SIG/2-Exploitation/SAGE/sage_perimetre.gpkg")

courdo <- read_sf("../../SIG/Cours_eau/TronconHydrographique_Bretagne_Pays_de_la_Loire_non_aqueduc_strahler.shp") %>% 
  rename(gid_ce = gid) %>% 
  st_transform(crs = 2154) %>% 
  mutate(long = st_length(geometry))

prel_totaux <- read_sf("../../SIG/2-Exploitation/Prelevements/Prel_DIR2_tout_confondu_2008_2016.gpkg") %>% 
  select(Nat_capt,
         "2008_1":"2016_1",
         geom)

# Attribution des plando ----
## Sélection des plans d'eau
plando_cours <- plando %>% 
  filter(distance == 0)

plando_source <- plando %>% 
  filter(R0_Topage == 1)

plando_nappe <- plando %>% 
  filter(NAPPE == 1)

plando_zh <- plando %>% 
  filter(ZH == 1)

plando_conn <- plando %>% 
  filter(distance == 0 |
           NAPPE == 1 |
           R0_Topage == 1)

plando_tbv <- plando %>% 
  filter(join_StreamOrde == c(1,2),
         distance <= 500)

## Décompte des plans d'eau ----
plando_sage <- plando %>% 
  group_by(code) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE = n())

plando_cours_sage <- plando_cours %>% 
  group_by(code) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_cours = n())

plando_source_sage <- plando_source %>% 
  group_by(code) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_source = n())

plando_nappe_sage <- plando_nappe %>% 
  group_by(code) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_nappe = n())

plando_zh_sage <- plando_zh %>% 
  group_by(code) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_zh = n())

plando_conn_sage <- plando_conn %>% 
  group_by(code) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_conn = n())

plando_tbv_sage <- plando_tbv %>% 
  group_by(code) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_tbv = n())

liste_nSAGE <- list(sage,
                    plando_sage,
                    plando_cours_sage,
                    plando_source_sage,
                    plando_nappe_sage,
                    plando_zh_sage,
                    plando_conn_sage,
                    plando_tbv_sage)

sage <- liste_nSAGE %>% 
  reduce(left_join,
         by = "code")

## Densités numériques ----
sage <- sage %>%
  mutate(dens_num = n_PE/superficie,
         dens_num_cours = n_PE_cours/superficie,
         dens_num_source = n_PE_source/superficie,
         dens_num_nappe = n_PE_nappe/superficie,
         dens_num_zh = n_PE_zh/superficie,
         dens_num_conn = n_PE_conn/superficie,
         dens_num_tbv = n_PE_tbv/superficie)

## Surface(s) et densité surfacique ----
surf_plando_sage <- plando %>% 
  group_by(code) %>% 
  st_drop_geometry() %>% 
  summarise(surf_PE = sum(surface_plando))

surf_plando_cours_sage <- plando_cours %>% 
  group_by(code) %>% 
  st_drop_geometry() %>% 
  summarise(surf_PE_cours = sum(surface_plando))

surf_plando_source_sage <- plando_source %>% 
  group_by(code) %>% 
  st_drop_geometry() %>% 
  summarise(surf_PE_source = sum(surface_plando))

surf_plando_nappe_sage <- plando_nappe %>% 
  group_by(code) %>% 
  st_drop_geometry() %>% 
  summarise(surf_PE_nappe = sum(surface_plando))

surf_plando_zh_sage <- plando_zh %>% 
  group_by(code) %>% 
  st_drop_geometry() %>% 
  summarise(surf_PE_zh = sum(surface_plando))

surf_plando_conn_sage <- plando_conn %>% 
  group_by(code) %>% 
  st_drop_geometry() %>% 
  summarise(surf_PE_conn = sum(surface_plando))

surf_plando_tbv_sage <- plando_tbv %>% 
  group_by(code) %>% 
  st_drop_geometry() %>% 
  summarise(surf_PE_tbv = sum(surface_plando))

list_surf_sage <- list(sage,
#                       surf_plando_sage,
                       surf_plando_cours_sage,
                       surf_plando_source_sage,
                       surf_plando_nappe_sage,
                       surf_plando_zh_sage,
                       surf_plando_conn_sage,
                       surf_plando_tbv_sage)

sage <- list_surf_sage %>% 
  reduce(left_join,
         by = "code") %>% 
  mutate(dens_surf = (surf_PE/(superficie*1000000))*100)

# Si besoin : possibilité de calculer les densités surfaciques par type de plando

# Attribution des cours d'eau
## Totalité des courdo ----
inter_courdo_sage <- st_intersection(courdo, sage) 

inter_courdo_sage <- inter_courdo_sage %>% 
  mutate(long_intersect = st_length(geom)) %>%
  st_drop_geometry() %>% 
  select(gid_ce,
         StreamOrde,
         code,
         long_intersect)

compte_courdo <- inter_courdo_sage %>% 
  group_by(code) %>%
  summarise(lineaire_CE = sum(long_intersect))

sage <- sage %>% 
  left_join(compte_courdo)

## Courdo de TBV ----
courdo_tbv_sage <- inter_courdo_sage %>% 
  filter(StreamOrde == c(1,2))

compte_ce_tbv <- courdo_tbv_sage %>% 
  group_by(code) %>% 
  summarise(lineaire_tbv = sum(long_intersect))

sage <- sage %>% 
  left_join(compte_ce_tbv)

# Prélèvements ----
## En plan d'eau ----
prel_plando <- prel_totaux %>% 
  filter(Nat_capt %in% c("RA", "RC", "RN", "RO", "RP"))

inter_prel_pe <- st_intersection(prel_plando, sage)

compte_prel_sage <- inter_prel_pe %>% 
  group_by(code) %>% 
  st_drop_geometry() %>%  
  summarise(X2008_PE = sum(X2008_1, na.rm = TRUE),
            X2009_PE = sum(X2009_1, na.rm = TRUE),
            X2010_PE = sum(X2010_1, na.rm = TRUE),
            X2011_PE = sum(X2011_1, na.rm = TRUE),
            X2012_PE = sum(X2012_1, na.rm = TRUE),
            X2013_PE = sum(X2013_1, na.rm = TRUE),
            X2014_PE = sum(X2014_1, na.rm = TRUE),
            X2015_PE = sum(X2015_1, na.rm = TRUE),
            X2016_PE = sum(X2016_1, na.rm = TRUE))

## Totaux ----
inter_prel_tot <- st_intersection(prel_totaux, sage)

compte_prel_tot <- inter_prel_tot %>% 
  group_by(code) %>% 
  summarise(X2008_tot = sum(X2008_1, na.rm = TRUE),
            X2009_tot = sum(X2009_1, na.rm = TRUE),
            X2010_tot = sum(X2010_1, na.rm = TRUE),
            X2011_tot = sum(X2011_1, na.rm = TRUE),
            X2012_tot = sum(X2012_1, na.rm = TRUE),
            X2013_tot = sum(X2013_1, na.rm = TRUE),
            X2014_tot = sum(X2014_1, na.rm = TRUE),
            X2015_tot = sum(X2015_1, na.rm = TRUE),
            X2016_tot = sum(X2016_1, na.rm = TRUE)) %>% 
  st_drop_geometry()

liste_prel <- list(sage,
                   compte_prel_tot,
                   compte_prel_sage)

sage <- liste_prel %>% 
  reduce(left_join,
         by = "code") %>% 
  mutate(Ptage_prel_PE_2013 = (X2013_PE / X2013_tot)*100)

# TBV ----
sage <- sage %>% 
  mutate(Ptage_PE_tbv = (n_PE_tbv / n_PE)*100)

# Sauvegarde finale ----
save(sage,
     file = "processed_data/sage_perimetre_230718.RData")

st_write(sage,
         dsn = "../../SIG/2-Exploitation/SAGE/sage_full_230718.gpkg")
