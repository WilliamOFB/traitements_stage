# Packages ----
library(tidyverse)
library(mapview)
library(sf)
library(dplyr)

# Import ----
geol <- read_sf("../../SIG/carte_lithoplogique_simplifiee_vectorisee_perimetre_etude.gpkg")

me <- read_sf("raw_data/BVMasseEauSansCoteTransition_edl2019_perimetre-etude.gpkg") %>% 
  rename(gid_me = gid) %>% 
  mutate(surfacebvs = st_area(.))

ptage_geol_me <- readxl::read_xlsx("../../SIG/3-Resultats/Pourcentage_geol_me.xlsx")

# Traitements ----
# Réalisés sur Qgis et importés ensuite

geol_intersect <- read_sf("../../SIG/2-Exploitation/Geologie/Intersect_geol_me_surf.gpkg")

geol_intersect <- geol_intersect %>% 
  st_drop_geometry()

## Tri des données ----
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
                                        
                    