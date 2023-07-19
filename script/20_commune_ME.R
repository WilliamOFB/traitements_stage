# Imports ----
## Communes ----
communes <- read_sf("../traitements_stage/raw_data/Communes_perimetre_etude.shp") %>%
  st_transform(2154)

mapview (communes)

## Masses d'eau ----
masses_eau <- read_sf("../traitements_stage/raw_data/BVMasseEauSansCoteTransition_edl2019_perimetre-etude.gpkg") %>%
  st_transform(2154)

## Visualisation ----
#mapview (masses_eau)+
#  mapview(communes,
#          col.regions="pink",
#          alpha = 0.5,
#          alpha.region = 0.2)

## Plans d'eau ----
plando <- read_sf("../../SIG/2-Exploitation/Plando/plando_full_source_230707.gpkg") %>% 
  st_transform(crs = 2154)

### Si problème persistant ----
plando_pb <- read_sf("../../SIG/2-Exploitation/Problemes/PE_sans_me.gpkg") %>% 
  st_transform(crs = 2154) %>% 
  mutate(surface_plando = st_area(.)) %>% 
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

plando_comm_her2 <- plando_her2_2 %>% 
  left_join(y = affectation_comm) %>% 
  select(gid_plando,
         CdOH,
         CdPlanEau_,
         NatureSE,
         Persistanc,
         SaliniteSE,
         join_gid,
         join_CdOH,
         join_Persistanc,
         join_SensEcoule,
         join_CdCoursEau,
         join_StreamOrde,
         distance,
         Marais,
         ERU,
         Transition,
         AREA_SE,
         R0,
         NAPPE,
         join_QABASN,
         join_QAMOY_MN,
         join_QAHAUN,
         join_ROBUSTES_1,
         join_QA_LOCAL,
         distance_2,
         prel_VOLUME,
         prel_USAGE,
         prel_USAGE_BR,
         cinquante,
         join_join_ID_BDCARTH,
         join_Q5BASN,
         join_Q5HAUN,
         join_Q5MOY_MN,
         join_ROBUSTES_1_2,
         ROE_identifian,
         ROE_usage_nom1,
         ROE_usage_nom2,
         ROE_usage_nom3,
         ROE_usage_nom4,
         distance_3,
         surface_plando,
         CdHER2,
         NomHER2,
         INSEE_COM,
         NOM,
         INSEE_DEP)

## Sauvegarde ----
save(plando_comm_her2,
     file = "../traitements_stage/processed_data/plando_comm_her2.RData")

sf::st_write(plando_comm_her2,
             dsn="processed_data/plando_comm_her2.gpkg")

# Attribution des masses d'eau ----
plando_ME <- plando %>% 
  st_intersection(masses_eau) %>% # découpage des PE selon les masses d'eau
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
affectation_me <- plando_ME %>% 
  group_by(gid_plando) %>%  # groupement pour chaque plan d'eau selon leur gid
  filter(pc_plando_sur_me == max(pc_plando_sur_me)) %>% # pourcentage maxi
  ungroup()

plando_comm_her_me <- plando_comm_her2 %>% 
  left_join(y = affectation_me) %>% 
  select(gid_plando,
         CdOH,
         CdPlanEau_,
         NatureSE,
         Persistanc,
         SaliniteSE,
         join_gid,
         join_CdOH,
         join_Persistanc,
         join_SensEcoule,
         join_CdCoursEau,
         join_StreamOrde,
         distance,
         Marais,
         ERU,
         Transition,
         AREA_SE,
         R0,
         NAPPE,
         join_QABASN,
         join_QAMOY_MN,
         join_QAHAUN,
         join_ROBUSTES_1,
         join_QA_LOCAL,
         distance_2,
         prel_VOLUME,
         prel_USAGE,
         prel_USAGE_BR,
         cinquante,
         join_join_ID_BDCARTH,
         join_Q5BASN,
         join_Q5HAUN,
         join_Q5MOY_MN,
         join_ROBUSTES_1_2,
         ROE_identifian,
         ROE_usage_nom1,
         ROE_usage_nom2,
         ROE_usage_nom3,
         ROE_usage_nom4,
         distance_3,
         surface_plando,
         CdHER2,
         NomHER2,
         INSEE_COM,
         NOM,
         INSEE_DEP,
         cdbvspemdo,
         nombvspemd,
         cdmassedea)

## Sauvegarde ----
#st_write(plando_pb_me,
#         dsn = "../../SIG/2-Exploitation/Problemes/plando_mi-repare_me.gpkg")

save(plando_comm_her_me,
     file = "../traitements_stage/processed_data/plando_comm_her_me.RData")

sf::st_write(plando_comm_her_me,
             dsn="processed_data/plando_comm_her_me.gpkg")

# Attribution des SAGE ----
## Imports supplémentaires ----
sage <- st_read("../../SIG/2-Exploitation/Habillage/sage_perimetre.gpkg")

## Traitements ----
plando_sage <- plando %>% 
  st_intersection(sage) %>% # découpage des PE selon les masses d'eau
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

plando_avec_sage <- plando %>% 
  left_join(y = affectation_sage)

## Sauvegarde ----
save(plando_avec_sage,
     file = "processed_data/plando_avec_sage.RData")

st_write(plando_avec_sage,
         dsn = "../../SIG/2-Exploitation/Plando/plando_full_sage_230712.gpkg")
