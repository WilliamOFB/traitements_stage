### Attribution des communes, masses d'eau et SAGE aux plans d'eau ###
# Imports ----
plando <- read_sf("chemin/vers/ma/couche/plando.gpkg") %>% # ou .shp ou en .RData
  st_transform(crs = 2154)

communes <- read_sf("chemin/vers/ma/couche/communes.gpkg") %>% # ou .shp ou en .RData 
  st_transform(2154)

massdo <- read_sf("chemin/vers/ma/couche/massdo.gpkg") %>% # ou .shp ou en .RData
  st_transform(2154)

sage <- read_sf("chemin/vers/ma/couche/sage.gpkg") # ou .shp ou en .RData

## Visualisation ----
mapview (masses_eau)+
  mapview(communes,
          col.regions="pink",
          alpha = 0.5,
          alpha.region = 0.2)

### Si problème persistant ----
plando_pb <- read_sf("chemin/vers/ma/couche/plando_problematique.gpkg") %>% # ou .shp ou en .RData
  st_transform(crs = 2154) %>% 
  mutate(surf_plando = st_area(.)) %>% 
  select(-c("cdbvspemdo", "nombvspemd", "cdmassedea")) %>% 
  st_make_valid()

### Visualisation ----
mapview(plando_pb,
        col.region = "red") +
  mapview(masses_eau,
          alpha = 0.5,
          alpha.region = 0.2)

# Attribution des communes ----
plando_communes <- plando %>% 
  st_intersection(communes) %>% # découpage des PE selon les communes
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry() %>% 
  select(gid_plando,  # sélection des variables à conserver
         INSEE_COM,
         NOM,
         INSEE_DEP,
         surface_plando,
         surface_intersect) %>% 
  mutate(pc_plando_sur_comm = surface_intersect / surface_plando)

## Vérification ----
names(plando_communes)

## Affectation d'une seule commune à un plan d'eau ----
affectation_comm <- plando_communes %>% 
  group_by(gid_plando) %>%  # groupement pour chaque plan d'eau selon leur gid
  filter(pc_plando_sur_comm == max(pc_plando_sur_comm)) %>% # pourcentage maxi
  ungroup()

plando_communes <- plando %>% 
  left_join(y = affectation_comm)

## Sauvegarde ----
save(plando_communes,
     file = "processed_data/plando_communes.RData")

sf::st_write(plando_communes,
             dsn="chemin/vers/mon/fichier/plando.gpkg")

# Attribution des masses d'eau ----
plando_me <- plando %>% 
  st_intersection(massdo) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry() %>% 
  select(gid_plando,  # sélection des variables à conserver
         cdbvspemdo,
         nombvspemd,
         cdmassedea,
         surface_plando,
         surface_intersect) %>% 
  mutate(pc_plando_sur_me = surface_intersect / surface_plando)

## Vérification ----
names(plando_ME)

## Affectation d'une seule masse d'eau à un plan d'eau ----
affectation_me <- plando_me %>% 
  group_by(gid_plando) %>%  # groupement pour chaque plan d'eau selon leur gid
  filter(pc_plando_sur_me == max(pc_plando_sur_me)) %>% # pourcentage maxi
  ungroup()

plando_comm_me <- plando_communes %>% 
  left_join(y = affectation_me)

## Sauvegarde ----
save(plando_comm_me,
     file = "processed_data/plando_comm_me.RData")

sf::st_write(plando_comm_me,
             dsn = "chemin/vers/mon/fichier/plando.gpkg")

# Attribution des SAGE ----
## Traitements ----
plando_sage <- plando_comm_me %>% 
  st_intersection(sage) %>% # découpage des plando selon les masses d'eau
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry() %>% 
  select(gid_plando,  # sélection des variables à conserver
         code,
         nom,
         surface_plando,
         surface_intersect) %>% 
  mutate(pc_plando_sur_sage = surface_intersect / surface_plando)

## Vérification ----
names(plando_sage)

## Affectation d'un seul SAGE à un plan d'eau ----
affectation_sage <- plando_sage %>% 
  group_by(gid_plando) %>%  # groupement pour chaque plan d'eau selon leur gid
  filter(pc_plando_sur_sage == max(pc_plando_sur_sage)) %>% # pourcentage maxi
  ungroup() %>% 
  select(gid_plando:nom)

plando_sage <- plando %>% 
  left_join(y = affectation_sage)

## Sauvegarde ----
save(plando_sage,
     file = "processed_data/plando_sage.RData")

st_write(plando_sage,
         dsn = "chemin/vers/mon/fichier/plando.gpkg")
