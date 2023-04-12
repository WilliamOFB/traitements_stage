### Calcul des densités numériques et surfaciques des PE découpés par communes

# Imports ----
## Communes ----
communes <- read_sf("raw_data/Communes_perimetre_etude.shp") %>% 
  st_transform(crs = 2154) %>% 
  mutate(surface_comm = st_area(.))

## Surfaces en eau ----
plando <- read_sf("../traitements_stage/raw_data/SE_tronc_prel_toutdebit_ROE.gpkg") %>% 
  st_transform(crs = 2154) %>%
  mutate(surface_plando = st_area(.)) %>%
  rename(gid_plando = gid)

## Masses d'eau ----
masse_eau <- read_sf("raw_data/BVMasseEauSansCoteTransition_edl2019_perimetre-etude.gpkg") %>% 
  st_transform(crs=2154) %>% 
  mutate (surface_me = st_area(.)) %>% 
  rename(gid_me = gid)

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
#  st_drop_geometry() %>% 
  group_by(INSEE_COM, NOM) %>% 
  dplyr::summarise(surface_plando_comm = sum(surface_intersect)) %>% 
  view()

## Densité surfacique ----
communes <- communes %>% 
  left_join(y = surf_plando_comm) %>% 
  mutate(densite_surf = (surface_plando_comm / surface_comm)*100)

### Cas particuliers ----
## Certaines communes n'ont pas de PE ##
test <- communes %>% 
  select(INSEE_COM,
         NOM,
         surface_plando_comm) %>% 
  st_drop_geometry()

setdiff(test, surf_plando_comm)

# On attribut 0 aux valeurs NA
communes <- communes %>% 
  mutate_if(is.numeric, ~replace(.,is.na(.),0))

rm(test)

## Sauvegarde ----
save(communes,
     file = "processed_data/communes_densite.RData")

# Densité par ME ----
## Attribution des ME ----
plando_me <- plando %>% 
  st_intersection(masse_eau) %>% # découpage des PE selon les ME
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
  #  st_drop_geometry() %>% 
  group_by(cdbvspemdo, nombvspemd) %>% 
  dplyr::summarise(surface_plando_me = sum(surface_intersect)) %>% 
  view()

## Densité surfacique ----
masse_eau <- masse_eau %>% 
  left_join(y = surf_plando_me) %>% 
  mutate(densite_surf = (surface_plando_me / surface_me)*100)

### Cas particuliers ----
## Certaines ME n'ont pas de PE ##
test <- masse_eau %>% 
  select(cdbvspemdo,
         nombvspemd,
         surface_plando_me) %>% 
  st_drop_geometry()

setdiff(test, surf_plando_me)

# On attribut 0 aux valeurs NA
masse_eau <- masse_eau %>% 
  mutate_if(is.numeric, ~replace(.,is.na(.),0))

rm(test)

## Sauvegarde ----
save(masse_eau,
     file = "processed_data/masse_eau_densite.RData")
