### Le plan d'eau est-il sur un cours d'eau ? ###

# Packages ----
library(sf)
library(tidyverse)
library(dplyr)
library(mapview)

rm(list = setdiff(ls(), "plando")) #suppression des objets sauf "plando"

# Import ----
plando <- read_sf("../../SIG/2-Exploitation/Plando/plando_prelev.gpkg") %>% 
  st_transform(crs=2154)# %>% 
#  rename(gid_pe = gid)

## Vérification ----
plando_uniques <- plando %>% 
  distinct(gid_plando,
           surface_plando,
           .keep_all = TRUE)

# Imports : suite ----
courdo <- read_sf("raw_data/TronconHydrographique_Bretagne_Pays_de_la_Loire_non_aqueduc_strahler.shp") %>% 
  st_transform(crs=2154)# %>% 
#  rename(gid_ce = gid) %>% 
#  select(-path)

# Renommage de certains attributs (si besoin) ----
nouveaux_noms <- stringr::str_replace(string = names(plando),
                                      pattern = ",C,254",
                                      replacement = "_BR")

nouveaux_noms <- stringr::str_replace(string = nouveaux_noms,
                                      pattern = ",N,10,0",
                                      replacement = "_BR")

nouveaux_noms <- stringr::str_replace(string = nouveaux_noms,
                                      pattern = "50s",
                                      replacement = "cinquante")

names(plando) <- nouveaux_noms

# Délétion de colonnes ----
plando <- plando %>% 
  select(-(SystemeAlt:ProjCoordO),
         -(join_SensEcoule:join_C_SS_SECT),
         -(join_layer:join_Pente_pm),
         -(CdPlanEa_1:CdPlanEa_2),
         -(join_LARGEUR))

# Intersection ----
## Sélection des intersects ----
plando_sur_ce <- plando_uniques %>% 
  dplyr::filter(distance==0)# %>% 
#  st_drop_geometry()

## Sauvergarde ----
save(plando_sur_ce,
     file = "processed_data/plando_sur_ce.RData")

# Quelle longueur de courdo le plando intercepte-t-il ? ----
load("processed_data/plando_sur_ce.RData")

# intersection <- st_intersection(plando_sur_ce, troncon)

## Initialisation de la colonne "longueur_interceptee" à 0 ----
plando_sur_ce$longueur_interceptee <- 0

## TESTS ----
### TEST 1 Calcul de la longueur interceptée par chaque entité de "plando" ----
troncon <- st_transform(troncon,
                        crs = st_crs(plando_sur_ce))

### TEST 2 ----
plando_sur_ce <- plando_sur_ce %>%
  mutate(longueur_interceptee = map_dbl(geom, function(g) {
    lengths <- st_length(st_intersection(g, troncon))
    sum(lengths, na.rm = TRUE)
  }))

### TEST 3 ----
plando_sur_ce <- plando_sur_ce %>%
  mutate(longueur_interceptee = mapply(function(g) {
        lengths <- st_length(st_intersection(g, troncon))
        sum(lengths, na.rm = TRUE)},
      geom))

### TEST 4 ----
plando_sur_ce$longueur_interceptee <- 0

for (i in 1:length(plando_sur_ce)) {
  intersect <- st_intersection(plando_sur_ce[i, ], troncon)
  lengths <- st_length(intersect)
  plando_sur_ce$longueur_interceptee[i] <- sum(lengths, na.rm = TRUE)
}

### TEST 5 Calcul de la longueur interceptée par chaque entité de "plando" ----
for (i in seq_along(plando_sur_ce)) {
  intersection <- st_intersection(plando[i, ], troncon)
  plando_sur_ce[i, "longueur_interceptee"] <- sum(st_length(intersection))
}

plando_sur_ce <- plando_sur_ce %>% 
  select(-longueur_interceptee)

plando_test <- plando_sur_ce %>% 
  group_by(gid_plando) %>%
  summarize(long_intercept = sum(long_intercept, na.rm = TRUE))

### TEST 6 ----
intersect <- st_intersection(plando_sur_ce, troncon) %>% 
#  mutate(longueur_interceptee = st_length(intersect)) %>% 
  select(gid_plando,
         gid_ce,
#         longueur_interceptee
         ) #%>% 
#  st_drop_geometry()

intersect <- intersect %>% 
  mutate(longueur_interceptee = st_length(intersect))

intersect_test <- intersect %>% 
  group_by(gid_plando) %>% 
  summarize(longueur_interceptee = sum(longueur_interceptee,
                                       na.rm = TRUE)) %>% 
  st_drop_geometry()

plando_intercept <- plando %>% 
  left_join(y = intersect_test)

### Sauvegarde ----
save(plando_intercept,
     file = "processed_data/intersect_plando.RData")

st_write(plando_intercept,
         dsn = "../../SIG/2-Exploitation/plando_unique_intersect.gpkg")

# NON RÉUSSI - Linéaire intercepté des courdo par PE par ME ----
## Imports supplémentaires ----
massdo <- st_read("../../SIG/2-Exploitation/Masses_eau/ME_toutes/massdo_perim_full_surf.gpkg")

plando_me_intercept <- plando_in_me %>% 
  group_by(cdbvspemdo) %>% 
  summarize(long_interc = sum(longueur_interceptee)) %>% 
  st_drop_geometry()

massdo_long_intercept <- plando_me_intercept %>% 
  left_join(y = massdo)

st_write(massdo_long_intercept,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_toutes/massdo_perim_full_long.gpkg")

# Linéaire moyen entre chaque plans d'eau ----
