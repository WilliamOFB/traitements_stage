# Attribution des volumes prélevés au PE le plus proche ----
## Packages nécessaires ----
library(mapview)
library(sf)
library(dplyr)
library(tidyverse)

## Imports ----
points_prelev <- st_read("../../SIG/2-Exploitation/Prelevements/Prel_DIR2_total_2008_2016.gpkg") %>% 
  st_transform(crs = 2154)

prelev_totaux <- st_read("../../SIG/2-Exploitation/Prelevements/Prel_DIR2_tout_confondu_2008_2016.gpkg") %>% 
  st_transform(crs = 2154)

plando <- st_read("../../SIG/2-Exploitation/plando_unique_intersect.gpkg")

plando_pb <- st_read("../../SIG/2-Exploitation/Plando/plando_inME_prel.gpkg")

massdo <- st_read("../../SIG/2-Exploitation/Masses_eau/massdo_dens_PE_categ.gpkg")

massdo_lacustre <- st_read("../../SIG/2-Exploitation/Masses_eau/ME_plando_courdo_lacustre.gpkg")

massdo_cote <- st_read("../../SIG/2-Exploitation/Masses_eau/ME_cotiere/massdo_cote_geol.gpkg")

massdo_trans <- st_read("../../SIG/2-Exploitation/Masses_eau/ME_transition/massdo_trans_geol.gpkg")

massdo_total <- st_read("../../SIG/2-Exploitation/Masses_eau/ME_toutes/massdo_perim_full_densnum.gpkg")

## Application ----
points_prelev_ssdoubles <- prelev_totaux %>% 
  group_by(geom) %>% 
  summarise(X2008_1 = sum(X2008_1),
            X2009_1 = sum(X2009_1),
            X2010_1 = sum(X2010_1),
            X2011_1 = sum(X2011_1),
            X2012_1 = sum(X2012_1),
            X2013_1 = sum(X2013_1),
            X2014_1 = sum(X2014_1),
            X2015_1 = sum(X2015_1),
            X2016_1 = sum(X2016_1),
            rais_soc = first(Rais_soc),
            Dprt = first(Dprt),
            INSEE = first(INSEE_com),
            num_prelev = first(num_prelev),
            num_SIREN = first(Num_SIREN),
            code_NIC = first(Code_Nic),
            code_Nace = first(Code_Nace),
            lib_nace = first(Lib_Nace),
            num_compt = first(num_compt),
            lib_compt = first(lib_compt),
            typ_coord = first(typ_coord),
            Nat_capt = first(Nat_capt))

plus_proche_pe <- sf::st_nearest_feature(x = points_prelev_ssdoubles,
                                         y = plando)

### Distance ----
dist <- st_distance(points_prelev_ssdoubles,
                    plando[plus_proche_pe,],
                    by_element = TRUE)

# Test
test <- points_prelev_ssdoubles %>% 
  cbind(dist) %>% 
  cbind(plando[plus_proche_pe,]) %>% 
  select(X2008_1:X2016_1,
         num_prelev,
         dist:distance_2,
         cinquante:geom.1,
         dist_prel = dist) %>% 
  st_drop_geometry() %>% 
  mutate(dist_prel = round(dist_prel))

plando_prelev <- points_prelev_ssdoubles %>% 
  cbind(dist) %>% 
  cbind(plando[plus_proche_pe,]) %>% 
  select(X2008_1:X2016_1,
         num_prelev,
         dist:gid_plando,
         geom.1,
         dist_prel = dist) %>% 
  st_drop_geometry() %>% 
  mutate(dist_prel = round(dist_prel))

plando_test <- plando %>% 
  left_join(y = plando_prelev)

### Sauvegarde intermédiaire ----
st_write(plando_test,
         dsn = "../../SIG/2-Exploitation/Plando/plando_prelev.gpkg")

save(plando_test,
     file = "processed_data/plando_in_ME_prel.RData")

### Suite application ----
plando_prelev_me <- plando_test %>% 
  group_by(cdbvspemdo) %>% 
  summarise(X2008_1 = sum(X2008_1, na.rm = TRUE),
            X2009_1 = sum(X2009_1, na.rm = TRUE),
            X2010_1 = sum(X2010_1, na.rm = TRUE),
            X2011_1 = sum(X2011_1, na.rm = TRUE),
            X2012_1 = sum(X2012_1, na.rm = TRUE),
            X2013_1 = sum(X2013_1, na.rm = TRUE),
            X2014_1 = sum(X2014_1, na.rm = TRUE),
            X2015_1 = sum(X2015_1, na.rm = TRUE),
            X2016_1 = sum(X2016_1, na.rm = TRUE)) %>% 
  st_drop_geometry()

names_plando <- stringr::str_replace(string = names(plando_prelev_me),
                                     pattern = "_1",
                                     replacement = "_total")

names(plando_prelev_me) <- names_plando

massdo_prelev <- massdo_total %>% 
  left_join(y = plando_prelev_me)

#### Autre application ----
intersect <- st_intersection(massdo_total, prelev_plando_good)

sum_values <- intersect %>% 
       group_by(cdbvspemdo) %>% 
       summarise(X2008_1_plando = sum(X2008_1.1, na.rm = TRUE),
                 X2009_1_plando = sum(X2009_1.1, na.rm = TRUE),
                 X2010_1_plando = sum(X2010_1.1, na.rm = TRUE),
                 X2011_1_plando = sum(X2011_1.1, na.rm = TRUE),
                 X2012_1_plando = sum(X2012_1.1, na.rm = TRUE),
                 X2013_1_plando = sum(X2013_1.1, na.rm = TRUE),
                 X2014_1_plando = sum(X2014_1.1, na.rm = TRUE),
                 X2015_1_plando = sum(X2015_1.1, na.rm = TRUE),
                 X2016_1_plando = sum(X2016_1.1, na.rm = TRUE)) %>% 
  st_drop_geometry()

massdo_prelev_plando <- massdo_total %>% 
  left_join(sum_values)

massdo_prelev_plando <- massdo_prelev_plando %>% 
  select(fid2:val_deux_geol,
         dens_surf_PEcours_PEtotal:geom)

new_names <- stringr::str_replace(string = names(massdo_prelev),
                                  pattern = "_1.1",
                                  replacement = "_total")

names(massdo_prelev) <- new_names

massdo_prelev <- massdo_prelev %>% 
  select(cdbvspemdo,
         X2008_total:X2016_total,
         geom)

massdo_lac_prelev <- massdo_lacustre %>% 
  left_join(y = plando_prelev_me)

massdo_trans_prelev <- massdo_trans %>% 
  left_join(y = plando_prelev_me)

massdo_cote_prelev <- massdo_cote %>% 
  left_join(y = plando_prelev_me)

### Sauvegarde finale ----
st_write(massdo_prelev,
         dsn = "../../SIG/2-Exploitation/Masses_eau/massdo_dens_prelev.gpkg")

st_write(massdo_lac_prelev,
         dsn = "../../SIG/2-Exploitation/Masses_eau/massdo_lac_dens_prelev.gpkg")

st_write(massdo_cote_prelev,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_cotiere/massdo_cote_full_prelev.gpkg")

st_write(massdo_trans_prelev,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_transition/massdo_trans_full_prelev.gpkg")

st_write(massdo_prelev_plando,
         dsn = "../../SIG/2-Exploitation/Masses_eau/massdo_perim_full_prelev.gpkg")

save(massdo_prelev,
     file = "processed_data/massdo_dens_prelev.RData")

save(massdo_lac_prelev,
     file = "processed_data/massdo_lac_dens_prelev.RData")

save(massdo_prelev,
     file = 'processed_data/massdo_totale_full_prelev.RData')

load('processed_data/massdo_totale_full_prelev.RData')
