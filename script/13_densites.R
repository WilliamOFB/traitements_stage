### Calcul des densités numériques et surfaciques des PE découpés par communes ###

# Imports ----
communes <- read_sf("chemin/vers/ma/couche/communes.gpkg") %>% # ou .shp ou en .RData
  st_transform(crs = 2154) %>% 
  mutate(surface_comm = st_area(.))

plando <- read_sf("chemin/vers/ma/couche/plando.gpkg") %>% # ou .shp ou en .RData
  filter(is.na(Orage),
         is.na(Ecoul_nat),
         is.na(Transition),
         is.na(ERU))

massdo <- read_sf("chemin/vers/ma/couche/massdo.gpkg") %>% # ou .shp ou en .RData
  st_transform(2154)

## Vérification ----
mapview(massdo)

# Densité par communes ----
## Renommage de certains attributs ----
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

## Attribution des communes ----
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

### Vérification ----
names(plando_communes)

## Surface par communes ----
surf_plando_comm <- plando_communes %>% 
  select(surface_intersect,
         INSEE_COM,
         NOM) %>% 
  group_by(INSEE_COM, NOM) %>% 
  dplyr::summarise(surf_plando_comm = sum(surface_intersect)) %>% 
  view()

## Densité surfacique ----
communes <- communes %>% 
  left_join(y = surf_plando_comm) %>% 
  mutate(dens_surf = (surf_plando_comm / surface_comm)*100)

### Cas particuliers ----
#### Certaines communes n'ont pas de PE ----
test <- communes %>% 
  select(INSEE_COM,
         NOM,
         surf_plando_comm) %>% 
  st_drop_geometry()

setdiff(test, surf_plando_comm)

#### On attribut 0 aux valeurs NA ----
communes <- communes %>% 
  mutate_if(is.numeric, ~replace(.,is.na(.),0))

rm(test)

## Sauvegarde ----
save(communes,
     file = "chemin/vers/ma/couche/communes_densites.RData")

# Densité par ME ----
## Attribution des ME ----
plando_me <- plando %>% 
  st_intersection(massdo) %>% # découpage des PE selon les ME
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry() %>% 
  select(gid_plando,  # sélection des variables à conserver
         cdbvspemdo,
         nombvspemd,
         cdmassedea,
         surface_plando,
         surface_intersect) %>% 
  mutate(pc_plando_sur_me = surface_intersect / surface_plando)

### Vérification ----
names(plando_me)

## Surface par ME ----
surf_plando_me <- plando_me %>% 
  select(surface_intersect,
         cdbvspemdo,
         nombvspemd) %>% 
  group_by(cdbvspemdo, nombvspemd) %>% 
  dplyr::summarise(surf_plando_me = sum(surface_intersect)) %>% 
  view()

massdo <- massdo %>% 
  left_join(y = surf_plando_me) %>% 
  mutate(dens_surf = (surf_PE / surfacebvs)*100)

## Sauvegarde intermédiaire ----
load(massdo,
     file = "chemin/vers/ma/couche/massdo_densites.RData")

## Densité surfacique ----
massdo <- massdo %>% 
  left_join(y = surf_plando_me) %>% 
  mutate(dens_surf = (surface_plando_me / surface_me)*100)

### Cas particuliers ----
#### Certaines ME n'ont pas de PE ----
test <- massdo %>% 
  select(cdbvspemdo,
         nombvspemd,
         surface_plando_me) %>% 
  st_drop_geometry()

setdiff(test, surf_plando_me)

#### On attribut 0 aux valeurs NA ----
massdo <- massdo %>% 
  mutate_if(is.numeric, ~replace(.,is.na(.),0))

rm(test)

## Sauvegarde ----
save(massdo,
     file = "chemin/vers/mon/fichier/massdo_densite.RData")

# Calcul des densités surfaciques par ME ----
## Application ----
### Surfaces ----
plando_par_me <- plando_select %>% 
  group_by(cdbvspemdo) %>% 
  summarize(sum_surf = sum(surface_plando)) %>% 
  st_drop_geometry()

plando_me_cours <- plando_cours %>% 
  group_by(cdbvspemdo) %>% 
  summarize(surf_cours = sum(surface_plando)) %>% 
  st_drop_geometry()

plando_me_source <- plando_source %>% 
  group_by(cdbvspemdo) %>% 
  summarize(surf_source = sum(surface_plando)) %>% 
  st_drop_geometry()

plando_me_nappe <- plando_nappe %>% 
  group_by(cdbvspemdo) %>% 
  summarize(surf_nappe = sum(surface_plando)) %>% 
  st_drop_geometry()

plando_me_zh <- plando_zh %>% 
  group_by(cdbvspemdo) %>% 
  summarize(surf_zh = sum(surface_plando)) %>% 
  st_drop_geometry()

### Left_join ----
massdo_ptage_pe <- massdo_total %>% 
  left_join(y = plando_par_me)

massdo_ptage_cours <- massdo_ptage_pe %>% 
  left_join(y = plando_me_cours)

massdo_ptage_source <- massdo_ptage_cours %>% 
  left_join(y = plando_me_source)

massdo_ptage_nappe <- massdo_ptage_source %>% 
  left_join(y = plando_me_nappe)

massdo_ptage_zh <- massdo_ptage_nappe %>% 
  left_join(y = plando_me_zh)

### Mutate pour dens_surf ----
massdo_surf <- massdo_ptage_zh %>% 
  mutate(surf_PE = sum_surf,
         surf_PE_cours = surf_cours,
         surf_PE_source = surf_source,
         surf_PE_nappe = surf_nappe,
         surf_PE_zh = surf_zh) %>% 
  select(gid_me:n_PE_zh_inf500,
         geom)

massdo_dens_surf <- massdo_surf %>% 
  mutate(dens_surf_PE = (surf_PE/surfacebvs)*100,
         dens_surf_PE_cours = (surf_PE_cours/surfacebvs)*100,
         dens_surf_PE_source = (surf_PE_source/surfacebvs)*100,
         dens_surf_PE_nappe = (surf_PE_nappe/surfacebvs)*100,
         dens_surf_PE_zh = (surf_PE_zh/surfacebvs)*100) %>% 
  select(gid_me:dens_surf_PE,
         lineaire_CE:dens_surf_PE_zh)

## Sauvegarde ----
save(massdo_dens_surf,
     file = "chemin/vers/mon/fichier/massdo_dens_surf.RData")

st_write(massdo_dens_surf,
         dsn = "chemin/vers/mon/fichier/massdo_dens_surf.gpkg")
