### Attribution des volumes prélevés au PE le plus proche ###
# Packages ----
library(mapview)
library(sf)
library(dplyr)
library(tidyverse)

# Imports ----
prelevements <- st_read("chemin/vers/ma/couche/points_prelevements.gpkg") %>% # ou .shp ou en .RData 
  st_transform(crs = 2154)

plando <- read_sf("chemin/vers/ma/couche/plando.gpkg") %>% # ou .shp ou en .RData 
  filter(is.na(ERU),
         is.na(Orage),
         is.na(Ecoul_nat),
         is.na(Transition))

massdo <- read_sf("chemin/vers/ma/couche/massdo.gpkg") %>% # ou .shp ou en .RData
  st_transform(2154)

# Application ----
points_prelev_ssdoubles <- prelevements %>% 
  group_by(geom) %>% 
  summarise(prel_2008_tot = sum(X2008_1),
            prel_2009_tot = sum(X2009_1),
            prel_2010_tot = sum(X2010_1),
            prel_2011_tot = sum(X2011_1),
            prel_2012_tot = sum(X2012_1),
            prel_2013_tot = sum(X2013_1),
            prel_2014_tot = sum(X2014_1),
            prel_2015_tot = sum(X2015_1),
            prel_2016_tot = sum(X2016_1),
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

## Distance ----
dist <- st_distance(points_prelev_ssdoubles,
                    plando[plus_proche_pe,],
                    by_element = TRUE)

### Test ----
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

## Sauvegarde intermédiaire ----
st_write(plando_test,
         dsn = "chemin/vers/mon/fichier/plando_prelev.gpkg")

save(plando_test,
     file = "chemin/vers/mon/fichier/plando_prelev.RData")

## Suite application ----
plando_prelev_me <- plando_test %>% 
  group_by(cdbvspemdo) %>% 
  summarise(prel_2008_tot = sum(X2008_1, na.rm = TRUE),
            prel_2009_tot = sum(X2009_1, na.rm = TRUE),
            prel_2010_tot = sum(X2010_1, na.rm = TRUE),
            prel_2011_tot = sum(X2011_1, na.rm = TRUE),
            prel_2012_tot = sum(X2012_1, na.rm = TRUE),
            prel_2013_tot = sum(X2013_1, na.rm = TRUE),
            prel_2014_tot = sum(X2014_1, na.rm = TRUE),
            prel_2015_tot = sum(X2015_1, na.rm = TRUE),
            prel_2016_tot = sum(X2016_1, na.rm = TRUE)) %>% 
  st_drop_geometry()

names_plando <- stringr::str_replace(string = names(plando_prelev_me),
                                     pattern = "_1",
                                     replacement = "_tot")

names(plando_prelev_me) <- names_plando

massdo_prelev <- massdo_total %>% 
  left_join(y = plando_prelev_me)

### Autre application ----
intersect <- st_intersection(massdo_total, prelev_plando_good)

sum_values <- intersect %>% 
       group_by(cdbvspemdo) %>% 
       summarise(prel_2008_pe = sum(X2008_1.1, na.rm = TRUE),
                 prel_2009_pe = sum(X2009_1.1, na.rm = TRUE),
                 prel_2010_pe = sum(X2010_1.1, na.rm = TRUE),
                 prel_2011_pe = sum(X2011_1.1, na.rm = TRUE),
                 prel_2012_pe = sum(X2012_1.1, na.rm = TRUE),
                 prel_2013_pe = sum(X2013_1.1, na.rm = TRUE),
                 prel_2014_pe = sum(X2014_1.1, na.rm = TRUE),
                 prel_2015_pe = sum(X2015_1.1, na.rm = TRUE),
                 prel_2016_pe = sum(X2016_1.1, na.rm = TRUE)) %>% 
  st_drop_geometry()

massdo_prelev_plando <- massdo_total %>% 
  left_join(sum_values)

#new_names <- stringr::str_replace(string = names(massdo_prelev),
#                                  pattern = "_1.1",
#                                  replacement = "_tot")

#names(massdo_prelev) <- new_names

massdo_prelev <- massdo_prelev %>% 
  select(cdbvspemdo,
         X2008_tot:X2016_tot,
         geom)

### Sauvegarde finale ----
st_write(massdo_prelev,
         dsn = "chemin/vers/mon/fichier/massdo_tout_prelev.gpkg")

save(massdo_prelev,
     file = 'chemin/vers/mon/fichier/massdo_tout_prelev.RData')
