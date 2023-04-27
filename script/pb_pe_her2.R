#surfaces en eau
plando_pb <- read_sf("../../SIG/2-Exploitation/Problemes/PE_sans_her2.gpkg") %>% 
  st_transform(crs = 2154) %>% 
  mutate(surface_plando = st_area(.)) %>% 
  select(-c("CdHER2", "NomHER2"))

mapviewOptions(fgb = FALSE)
mapview(plando_pb)

names(plando_pb)

names(hec2)

plando_her2 <- plando_pb %>% 
  st_intersection(hec2) %>% # découpage des PE selon les HER2
  mutate(surface_intersect = st_area(.)) %>% # superficie des intersects
  st_drop_geometry() %>% 
  select(gid_plando,
         CdHER2,
         NomHER2,
         surface_plando,
         surface_intersect) %>% 
  mutate(pc_plando_sur_her2 = surface_intersect / surface_plando)


## vérification
names(plando_her2)

# affectation d'une seule her2 à un plan d'eau
affectation_her2 <- plando_her2 %>% 
  group_by(gid_plando) %>%  # groupement pour chaque plan d'eau selon leur gid
  filter(pc_plando_sur_her2 == max(pc_plando_sur_her2)) %>% # pourcentage maxi
  ungroup()

plando_her2_2 <- plando_pb %>% 
  left_join(y = affectation_her2) %>% 
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
         NOM,
         INSEE_COM,
         INSEE_DEP,
         cdbvspemdo,
         nombvspemd,
         cdmassedea,
         CdHER2,
         NomHER2)

sf::st_write(plando_her2_2,
             dsn = "../../SIG/2-Exploitation/Problemes/plando_mi-repare_her2.gpkg")
