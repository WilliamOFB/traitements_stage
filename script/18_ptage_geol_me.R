### Géologie par masses d'eau ###
# Packages ----
library(tidyverse)
library(mapview)
library(sf)
library(dplyr)
library(readxl)
library(stringr)

# Import ----
geologie <- read_sf("chemin/vers/ma/couche/geologie.gpkg") %>% # ou .shp ou en .RData
  st_transform(crs = 2154)

massdo <- read_sf("chemin/vers/ma/couche/massdo.gpkg") # ou .shp ou en .RData

ptage_geol_me <- readxl::read_xlsx("chemin/vers/mon/fichier/pourcentage_geol_me.xlsx")

# Réalisés sur Qgis et importés ensuite
geol_intersect <- read_sf("chemin/vers/ma/couche/geol_intersect_massdo.gpkg") %>% # ou .shp ou en .RData
  st_transform(crs = 2154) %>% 
  st_drop_geometry()

# Pourcentage cumulé de géologie par ME ----
## Tri des données ----
geol_tri <- geol_intersect %>% 
  arrange(cdbvspemdo,
          surf_cut)

## Calcul de la somme cumulé des surfaces pour chaque ME ----
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
# Imports supplémentaires ----
massdo_geol <- read_sf("chemin/vers/ma/couche/ptage_geol_par_me.gpkg") # ou .shp ou en .RData

# Application ----
## Renommage ----
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

## Attribution ----
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

massdo <- massdo %>% 
  left_join(y = geol_majo_me)

## Sauvegarde ----
save(massdo,
     file = "chemin/vers/mon/fichier/massdo_geol.RData")

st_write(massdo,
         dsn = "chemin/vers/mon/fichier/massdo_geol.gpkg")
