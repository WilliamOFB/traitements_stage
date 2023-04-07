plando_72 <- plando %>%
  st_intersection(communes72) %>% # découpage des plans d'eau selon les communes
  mutate(surface_intersect = st_area(.)) %>%   # superficie des intersects
  select(gid_plando,  # sélection des variables à conserver
         INSEE_COM,
         NOM,
         INSEE_DEP,
         surface_plando,
         surface_intersect) %>%   
  st_drop_geometry() %>% # suppression de la géométrie
  mutate(pc_du_plando_sur_com = surface_intersect / surface_plando) # calcul du % de chaque plando par communes

# affectation d'une seule commune à un plan d'eau
affectation_commune72 <- plando_72 %>% 
  group_by(gid_plando) %>%  # groupement pour chaque plan d'eau selon leur gid
  filter(pc_du_plando_sur_com == max(pc_du_plando_sur_com)) %>% # pourcentage maxi
  ungroup() %>% 
  select(gid_plando, # sélection des variables à conserver
         INSEE_COM,
         NOM,
         INSEE_DEP,
         surface_plando,
         pc_du_plando_sur_com)

plando_72 <- plando %>% 
  left_join(y = affectation_commune72) %>% 
  select(NatureSE,
         OrigineSE,
         Persistanc,
         surface_plando,
         INSEE_COM,
         INSEE_DEP,
         NOM,
         pc_du_plando_sur_com) %>% 
  filter(INSEE_DEP == "72")

mapview(plando_72,
        col.region = "red")+
  mapview(communes72,
          alpha = 0.5,
          alpha.region = 0.2)

save(plando_72,
     file = "../traitements_stage/processed_data/plando72.RData")