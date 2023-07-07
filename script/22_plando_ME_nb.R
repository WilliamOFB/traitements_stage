# Packages ----
library(tidyverse)
library(mapview)
library(sf)
library(dplyr)

# Import ----
load("processed_data/plando_comm_her_me.RData")

me <- read_sf("raw_data/BVMasseEauSansCoteTransition_edl2019_perimetre-etude.gpkg") %>% 
  rename(gid_me = gid)

massdo_courdo <- read_sf("../../SIG/2-Exploitation/Masses_eau/massdo_full_ripi.gpkg")

plando_courdo <- read_sf("../../SIG/2-Exploitation/Plando/plando_cours_inME.gpkg")

## Vérification ----
mapview(massdo_courdo)

# Attribution ----
intersections <- st_intersection(plando_in_me, massdo_courdo) %>% 
  mutate(surface_intersect = st_area(.)) %>%
  st_drop_geometry() %>% 
  mutate(pc_plando_sur_me = surface_intersect / surface_plando)

# Compter le nombre de polygones de "plando" présents dans chaque polygone de "me"
counts <- intersections %>% 
  group_by(cdbvspemdo) %>%
  summarise(n_PE_good = n())# %>% 
#  select(gid_me,
#         nombvspemd,
#         n)
#  st_drop_geometry() %>% 


nb_me_plando <- massdo_courdo %>% 
  left_join(y = counts)

# Sauvegarde ----
save(nb_me_plando,
     file = "processed_data/nb_plando_me.RData")

st_write(nb_me_plando,
         dsn = "../../SIG/2-Exploitation/Paysages/ME_nb_plando.gpkg")

st_write(nb_me_plando,
         dsn = "../../SIG/2-Exploitation/Masses_eau/massdo_courdo_full_nb.gpkg")
