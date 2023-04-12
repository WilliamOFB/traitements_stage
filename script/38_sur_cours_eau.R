### Le plan d'eau est-il sur un cours d'eau ? ###

rm(list = setdiff(ls(), "plando")) #suppression des objets sauf "plando"

# Import ----
plando <- read_sf("raw_data/SE_tronc_prel_toutdebit_ROE.gpkg") %>% 
  st_transform(crs=2154) %>% 
  rename(gid_pe = gid)

troncon <- read_sf("raw_data/TronconHydrographique_Bretagne_Pays_de_la_Loire_non_aqueduc_strahler.shp") %>% 
  st_transform(crs=2154) %>% 
  rename(gid_ce = gid) %>% 
  select(-path)

# Renommage de certains attributs ----
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
plando_sur_ce <- plando %>% 
  dplyr::filter(distance==0) %>% 
  st_drop_geometry()

## Sauvergarde ----
save(plando_sur_ce,
     file = "processed_data/plando_sur_ce.RData")
