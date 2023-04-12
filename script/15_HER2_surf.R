library(mapview)
library(sf)
library(devtools)
library(tidyverse)
library(stringi)

#HEC niveau 2
hec2 <- read_sf("../traitements_stage/raw_data/HER_niv2_perimetre_etude.shp") %>% 
  st_transform(crs = 2154)

mapview (hec2)

#surfaces en eau
plando <- read_sf("../traitements_stage/raw_data/SE_tronc_prel_toutdebit_ROE.gpkg") %>% 
  st_transform(crs = 2154) %>% 
  mutate(surface_plando = st_area(.)) %>% 
  rename(gid_plando = gid)

mapviewOptions(fgb = FALSE)
mapview(plando)

names(plando)

# renommage de certains attributs
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

plando_her2 <- plando %>% 
  st_intersection(hec2) %>% # découpage des PE selon les HER2
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry() %>% 
  select(gid_plando,
         CdHER2,
         NomHER2,
         surface_plando,
         surface_intersect) %>% 
  mutate(pc_plando_sur_her2 = surface_intersect / surface_plando)


## vérification
# names(plando_her2)

# affectation d'une seule her2 à un plan d'eau
affectation_her2 <- plando_her2 %>% 
  group_by(gid_plando) %>%  # groupement pour chaque plan d'eau selon leur gid
  filter(pc_plando_sur_her2 == max(pc_plando_sur_her2)) %>% # pourcentage maxi
  ungroup()

plando_her2_2 <- plando %>% 
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
         AREA_SE,
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

save(plando_her2_2,
     file = "../traitements_stage/processed_data/plando_her2.RData")

sf::st_write(plando_her2_2,
             dsn="processed_data/plando_her2.gpkg")

#même chose avec communes
communes <- read_sf("../traitements_stage/raw_data/Communes_perimetre_etude.shp") %>%
  st_transform(2154)

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

plando_22 <- plando %>%
  st_intersection(communes22) %>% # découpage des plans d'eau selon les communes
  mutate(surface_intersect = st_area(.)) %>%   # superficie des intersects
  select(gid_plando,  # sélection des variables à conserver
         INSEE_COM,
         NOM,
         INSEE_DEP,
         surface_plando,
         surface_intersect) %>%   
  st_drop_geometry() %>% # suppression de la géométrie
  mutate(pc_du_plando_sur_com = surface_intersect / surface_plando) # calcul du % de chaque plando par communes

# affectation d'une seule commune à un plan d'eau
affectation_commune22 <- plando_22 %>% 
  group_by(gid_plando) %>%  # groupement pour chaque plan d'eau selon leur gid
  filter(pc_du_plando_sur_com == max(pc_du_plando_sur_com)) %>% # pourcentage maxi
  ungroup() %>% 
  select(gid_plando, # sélection des variables à conserver
         INSEE_COM,
         NOM,
         INSEE_DEP,
         surface_plando)

plando22 <- plando %>% 
  left_join(y = affectation_commune22) %>% 
  select(NatureSE,
         OrigineSE,
         Persistanc,
         surface_plando,
         INSEE_COM,
         INSEE_DEP,
         NOM) %>% 
  filter(INSEE_DEP == "22")
  
mapview(plando22,
        col.region = "red")+
  mapview(communes22,
          alpha = 0.5,
          alpha.region = 0.2)

save(plando22,
     file = "../traitements_stage/processed_data/plando22.RData")

save(communes29,
     file = "../traitements_stage/processed_data/communes29.RData")
