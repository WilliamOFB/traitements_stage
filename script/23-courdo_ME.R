# Packages ----
library(tidyverse)
library(mapview)
library(sf)
library(dplyr)

# Import ----
me <- read_sf("../../SIG/2-Exploitation/ME_nb_plando.gpkg")

courdo <- read_sf("../../SIG/Cours_eau/TronconHydrographique_Bretagne_Pays_de_la_Loire_non_aqueduc_strahler.shp") %>% 
  rename(gid_ce = gid)

# Attribution ----
intersections <- st_intersection(courdo, me) %>% 
  mutate(length_intersect = st_length(.)) %>%
  st_drop_geometry() %>% 
  select(gid_ce,
         CdOH,
         NatureTH,
         Persistanc,
         OrigineTH,
         ClasseLarg,
         StreamOrde,
         gid_me,
         cdbvspemdo,
         nombvspemd,
         cdmassedea,
         n,
         length_intersect)#%>% 
#  mutate(pc_courdo_in_me = length_intersect / surface_plando)

counts <- intersections %>% 
  group_by(gid_me) %>%
  summarise(long_tot = sum(length_intersect))

long_ce_me <- me %>% 
  left_join(y = counts) %>% 
  mutate(lineaire_mean = long_tot / n)

# Sauvegarde ----
save(long_ce_me,
     file = "processed_data/lineaire_mean_me.RData")

st_write(long_ce_me,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_plando_courdo.gpkg")
