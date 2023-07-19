# Package ----
library(hubeau)
library(tidyverse)
library(mapview)
library(sf)
library(aspe)

# Décompte des PE par BV ----
## Import des données (Qgis) ----
bv_ipr <- read_sf("../../SIG/2-Exploitation/BV/BV_stations_IPR.gpkg")

pe_bv_ipr <- read_sf("../../SIG/2-Exploitation/BV/IPR/plando_BV_IPR.gpkg") %>% 
  filter(is.na(ERU),
         is.na(Transition),
         is.na(Ecoul_nat),
         is.na(Orage))

#pe_bv_ipr <- pe_bv_ipr %>%
#  sf :: st_make_valid()

#pe_bv_ipr <- pe_bv_ipr %>%
#  fortify()

## Attribution ----
intersections <- st_intersection(pe_bv_ipr, bv_ipr)

## Comptage PE totaux ----
counts <- intersections %>% 
  group_by(id) %>%
  st_drop_geometry() %>% 
  summarise(n_PE = n())

ipr_plando <- bv_ipr %>% 
  left_join(y = counts)

## Comptage PE cours ----
plando_ipr_cours <- pe_bv_ipr %>% 
  filter(distance == 0)

inter_cours <- st_intersection(plando_ipr_cours, bv_ipr)

nb_ipr_cours <- inter_cours %>% 
  group_by(id) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_cours = n())

ipr_plando <- ipr_plando %>% 
  left_join(nb_ipr_cours)

## Comptage PE source ----
plando_ipr_source <- pe_bv_ipr %>% 
  filter(R0_Topage == 1)

inter_source <- st_intersection(plando_ipr_source, bv_ipr)

nb_ipr_source <- inter_source %>% 
  group_by(id) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_source = n())

ipr_plando <- ipr_plando %>% 
  left_join(nb_ipr_source)

## Comptage PE nappe ----
plando_ipr_nappe <- pe_bv_ipr %>% 
  filter(NAPPE == 1)

inter_nappe <- st_intersection(plando_ipr_nappe, bv_ipr)

nb_ipr_nappe <- inter_nappe %>% 
  group_by(id) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_nappe = n())

ipr_plando <- ipr_plando %>% 
  left_join(nb_ipr_nappe)

## Comptage PE sur zone humide probable ----
plando_ipr_zh <- pe_bv_ipr %>% 
  filter(ZH == 1)

inter_zh <- st_intersection(plando_ipr_zh, bv_ipr)

nb_ipr_zh <- inter_zh %>% 
  group_by(id) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_zh = n())

ipr_plando <- ipr_plando %>% 
  left_join(nb_ipr_zh)

## Comptage PE connectés ----
plando_ipr_conn <- pe_bv_ipr %>% 
  filter(distance == 0 |
           NAPPE == 1 |
           R0_Topage == 1)

inter_conn <- st_intersection(plando_ipr_conn, bv_ipr)

nb_ipr_conn <- inter_conn %>% 
  group_by(id) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_conn = n())

ipr_plando <- ipr_plando %>% 
  left_join(nb_ipr_conn)

## Comptage PE sans mares ----
plando_ipr_ssMare <- pe_bv_ipr %>% 
  filter(is.na(Mares))

inter_ssMare <- st_intersection(plando_ipr_ssMare, bv_ipr)

nb_ipr_ssMare <- inter_ssMare %>% 
  group_by(id) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_ssMare = n())

ipr_plando <- ipr_plando %>% 
  left_join(nb_ipr_ssMare)

## Densités numériques ----
ipr_plando <- ipr_plando %>% 
  mutate(dens_num_tot = (n_PE / (surface/1000000))) %>% 
  mutate(dens_num_cours = (n_PE_cours / (surface/1000000))) %>% 
  mutate(dens_num_source = (n_PE_source / (surface/1000000))) %>% 
  mutate(dens_num_nappe = (n_PE_nappe / (surface/1000000))) %>% 
  mutate(dens_num_zh = (n_PE_zh / (surface/1000000))) %>% 
  mutate(dens_num_conn = (n_PE_conn / (surface/1000000))) %>% 
  mutate(dens_num_ssMare = (n_PE_ssMare / (surface/1000000)))

## Thermie ----
stations_ipr <- read_sf("../../SIG/2-Exploitation/BV/IPR/station_thermie.gpkg") %>% 
  rename(id = ID) %>% 
  st_transform(crs = 2154) %>% 
  st_drop_geometry()

ipr_plando <- ipr_plando %>% 
  left_join(stations_ipr) %>% 
  select(id:dens_surf,
         join_MTa30J:join_MTw30J_MUL)

## Débit ----
stations_qmna5 <- read_sf("../../SIG/2-Exploitation/BV/IPR/station_QMNA5.gpkg") %>% 
  rename(id = ID) %>% 
  st_transform(crs = 2154) %>% 
  st_drop_geometry()

stations_module <- read_sf("../../SIG/2-Exploitation/BV/IPR/station_module.gpkg") %>% 
  rename(id = ID) %>% 
  st_transform(crs = 2154) %>% 
  st_drop_geometry()

ipr_plando <- ipr_plando %>% 
  left_join(stations_qmna5) %>% 
  select(id:dens_num_ssMare,
         join_Q5BASN:join_Q5HAUN)

ipr_plando <- ipr_plando %>% 
  left_join(stations_module) %>% 
  select(id:join_Q5HAUN,
         join_QABASN:join_QAHAUN)

### Sauvegarde intermédiaire ----
save(ipr_plando,
     file = "processed_data/ipr_plando.RData")

st_write(ipr_plando,
         dsn = "../../SIG/2-Exploitation/BV/IPR/nb_plando_ipr.gpkg")

## Linéaire de cours d'eau ----
### Import des cours d'eau ----
courdo_ipr <- read_sf("../../SIG/2-Exploitation/BV/CE_BV_IPR.gpkg")

### Traitements ----
inter_courdo <- st_intersection(courdo_ipr, bv_ipr)

inter_courdo <- inter_courdo %>% 
  mutate(long = st_length(geom))

long_ce_ipr <- inter_courdo %>% 
  group_by(id) %>% 
  st_drop_geometry() %>% 
  summarise(lineaire_total = sum(long),
            rang = max(StreamOrde))

ipr_plando <- ipr_plando %>% 
  left_join(long_ce_ipr) %>% 
  select(id:join_QAHAUN,
         lineaire_total:rang)

## Nombre de plando et linéaire de courdo à 1,2km ----
### Imports ----
plando_1.2km <- st_read("../../SIG/2-Exploitation/BV/IPR/plando_IPR_avecID.gpkg") %>% 
  rename(id = join_ID)

courdo_1.2km <- st_read("../../SIG/2-Exploitation/BV/courdo_BV_IPR.gpkg") %>% 
  mutate(long = st_length(geom))

### Traitements ----
#### Plando ----
nb_ipr_1.2km <- plando_1.2km %>% 
  group_by(id) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE_1.2km = n())

ipr_plando <- ipr_plando %>% 
  left_join(nb_ipr_1.2km)

#### Courdo ----
long_ce_ipr_1.2km <- courdo_1.2km %>% 
  group_by(id) %>% 
  st_drop_geometry() %>% 
  summarise(lineaire_1.2km = sum(long))

ipr_plando <- ipr_plando %>% 
  left_join(long_ce_ipr_1.2km)

## Densité surfacique ----
surf_pe_ipr <- intersections %>% 
  group_by(id) %>% 
  st_drop_geometry() %>% 
  summarise(surf_PE = sum(surface_plando))

ipr_plando <- ipr_plando %>% 
  left_join(surf_pe_ipr)

ipr_plando <- ipr_plando %>% 
  mutate(dens_surf = (surf_PE/surface)*100)

# Récupération des métriques IPR ----
load(file = "../../3-Statistiques/traitements_stage/raw_data/tables_sauf_mei_2023_03_08_15_08_27.RData")

passerelle <- mef_creer_passerelle()

metriques_ipr <- passerelle %>% 
  mef_ajouter_metriques() %>% 
  mef_ajouter_ope_date() %>% 
  mef_ajouter_dept() %>% 
  mef_ajouter_ipr() %>% 
  mef_ajouter_libelle_site(origine_libelle = "station_sandre")

metriques_ipr <- metriques_ipr %>% 
  rename(libelle_sandre = sta_libelle_sandre)

ipr_numero_stat <- metriques_ipr %>% 
  left_join(station) %>% 
  select(sta_id:libelle_sandre,
         sta_code_sandre)

## Sélection des départements uniquement ----
metriques_ipr_dept <- ipr_numero_stat %>% 
  filter(dept == c(22, 29, 35, 44, 49, 53, 56, 72, 85))

### Regroupement ----
metriques_groupe_dept <- metriques_ipr_dept %>% 
  group_by(across(-lop_id)) %>% 
  summarise(across(!matches("lop_id"), first))

metriques_groupe_dept <- metriques_groupe_dept %>% 
  group_by(across(-pre_id)) %>% 
  summarise(across(!matches("pre_id"), first))

### Sélection de la date la plus ancienne par station ----
metrique_date <- metriques_groupe_dept %>% 
  group_by(sta_code_sandre) %>% 
  filter(ope_date == max(ope_date),
         !is.na(ipr)) %>% 
  mutate(sta_code_sandre = paste("ID_",
                                 sta_code_sandre,
                                 sep = ""))

## Sauvegarde intermédiaire ----
save(metrique_date,
     file = "processed_data/metriques_plus_recentes_station.RData")

## Jointure entre BV IPR et métriques ----
# Nécessité de "metrique_date" et "ipr_plando"
ipr_plando <- ipr_plando %>%
  rename(sta_code_sandre = id)

bv_ipr_metrique <- ipr_plando %>% 
  left_join(metrique_date)

## Sauvegarde ----
save(bv_ipr_metrique,
     file = "processed_data/bv_ipr_metriques.RData")

save(ipr_plando,
     file = "processed_data/ipr_plando.RData")

st_write(bv_ipr_metrique,
         dsn = "../../SIG/2-Exploitation/BV/IPR/bv_ipr_metrique.gpkg")

st_write(ipr_plando,
         dsn = "../../SIG/2-Exploitation/BV/IPR/IPR_full.gpkg")
