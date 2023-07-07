# Pourcentage cumulé de géologie par ME ----
## Packages ----
library(tidyverse)
library(mapview)
library(sf)
library(dplyr)

## Import ----
geol <- read_sf("../../SIG/carte_lithoplogique_simplifiee_vectorisee_perimetre_etude.gpkg")

massdo <- read_sf("raw_data/BVMasseEauSansCoteTransition_edl2019_perimetre-etude.gpkg") %>% 
  rename(gid_me = gid) %>% 
  mutate(surfacebvs = st_area(.))

ptage_geol_me <- readxl::read_xlsx("../../SIG/3-Resultats/Pourcentage_geol_me.xlsx")

## Traitements ----
# Réalisés sur Qgis et importés ensuite

geol_intersect <- read_sf("../../SIG/2-Exploitation/Geologie/Intersect_geol_me_surf.gpkg")

geol_intersect <- geol_intersect %>% 
  st_drop_geometry()

### Tri des données ----
geol_tri <- geol_intersect %>% 
  arrange(cdbvspemdo,
          surf_cut)

# Calcul de la somme cumulé des surfaces pour chaque ME
id_me <- ptage_geol_me %>% 
  pull(gid_me) %>% 
  unique() %>% 
  sample(15)

test <- ptage_geol_me %>% 
  filter(gid_me %in% id_me)

modal_geol <- geol_tri %>% 
  pull(descr) %>% 
  unique()

geol_tri <- geol_tri %>% 
  mutate(descr = as.factor(descr),
         descr = fct_relevel(descr,
                             "Argiles",
                             "Calcaires, marne et gypse",
                             "Craies",
                             "Sables",
                             "Basaltes et rhyolites",
                             "Granites",
                             "Ophiolites",
                             "Gneiss",
                             "Micaschistes",
                             "Schistes et grès"))

ptage_geol_me <- ptage_geol_me %>% 
  mutate(descr = as.factor(descr),
         descr = fct_relevel(descr,
                             "Argiles",
                             "Calcaires, marne et gypse",
                             "Craies",
                             "Sables",
                             "Basaltes et rhyolites",
                             "Granites",
                             "Ophiolites",
                             "Gneiss",
                             "Micaschistes",
                             "Schistes et grès"))

ggplot2::ggplot(data = geol_tri,
                aes(x = cdbvspemdo,
                    y = ptage,
                    fill = descr)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#D4FFBF",
                               "#73b3ff",
                               "#BFD1FF",
                               "#ffff73",
                               "#004ca8",
                               "#ff0000",
                               "#008700",
                               "#ffbfe8",
                               "#ffebbf",
                               "#8a8a45"))
                                      
# Pourcentages majoritaires de géologie par ME ----
## Packages supplémentaires ----
library(stringr)

## Imports supplémentaires ----
massdo <- read_sf("../../SIG/2-Exploitation/Masses_eau/massdo_dens_prelev.gpkg")

massdo_lacustre <- read_sf("../../SIG/2-Exploitation/Masses_eau/ME_lac/massdo_lac_dens_prel.gpkg")

massdo_cote <- read_sf("../../SIG/2-Exploitation/Masses_eau/ME_cotiere/massdo_cote_dens_PE.gpkg")

massdo_trans <- read_sf("../../SIG/2-Exploitation/Masses_eau/ME_transition/massdo_trans_dens_PE.gpkg")

massdo_geol <- read_sf("../../SIG/2-Exploitation/Geologie/Ptage_geol_me.gpkg")

## Application ----
### Renommage ----
new_noms <- stringr::str_replace(string = names(massdo_geol),
                                 pattern = "ptage_",
                                 replacement = "")

new_noms <- stringr::str_replace(string = new_noms,
                                 pattern = "clay",
                                 replacement = "argile")

new_noms <- stringr::str_replace(string = new_noms,
                                 pattern = "basalt",
                                 replacement = "basalte")

names(massdo_geol) <- new_noms

### Attribution ----
geol_majo_me <- massdo_geol %>% 
  rowwise() %>% 
  mutate(top_geol = names(.)[18:27][which.max(c_across(argile:schist_gres))],
         deux_geol = names(.)[18:27][order(-c_across(argile:schist_gres))][2],
         val_top_geol = across(argile:schist_gres)[[which.max(c_across(argile:schist_gres))]],
         val_deux_geol = map_dbl(names(.)[18:27][order(-c_across(argile:schist_gres))][2], ~as.numeric(across(.x)))) %>% 
  select(cdbvspemdo,
         top_geol,
         val_top_geol,
         deux_geol,
         val_deux_geol) %>% 
  ungroup() %>% 
  st_drop_geometry()

geol_majo_me <- geol_majo_me %>% 
  mutate(top_geol = case_when(top_geol == "micasch" ~ "micaschistes",
                              top_geol == "calc_marn" ~ "calcaires et marnes",
                              top_geol == "schist_gres" ~ "schistes et grès",
                              TRUE ~ top_geol),
         deux_geol = case_when(deux_geol == "micasch" ~ "micaschistes",
                               deux_geol == "calc_marn" ~ "calcaires et marnes",
                               deux_geol == "schist_gres" ~ "schistes et grès",
                               TRUE ~ deux_geol))

massdo_full_geol <- massdo %>% 
  left_join(y = geol_majo_me)

massdo_lac_geol <- massdo_lacustre %>% 
  left_join(y = geol_majo_me)

massdo_cote_geol <- massdo_cote %>% 
  left_join(y = geol_majo_me)

massdo_trans_geol <- massdo_trans %>% 
  left_join(y = geol_majo_me)

## Sauvegarde ----
save(massdo_full_geol,
     file = "processed_data/massdo_full_geol.RData")

save(massdo_lac_geol,
     file = "processed_data/massdo_lac_geol.RData")

st_write(massdo_full_geol,
         dsn = "../../SIG/2-Exploitation/Masses_eau/massdo_full_geol.gpkg")

st_write(massdo_lac_geol,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_lac/massdo_lac_geol.gpkg")

st_write(massdo_trans_geol,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_transition/massdo_trans_geol.gpkg")

st_write(massdo_cote_geol,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_cotiere/massdo_cote_geol.gpkg")
