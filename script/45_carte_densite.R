### Carte de densité de points ###

load("processed_data/plando_comm_her_me.RData")

dept <- read_sf("raw_data/Departements_perimetre_etude.shp")

centre_plando <- st_centroid(plando_comm_her_me)
  

centre_plando_dept <- centre_plando %>% 
  filter(INSEE_DEP == "35")

coord <- centre_plando_dept %>% 
  st_coordinates()

centre_plando_dept <- centre_plando_dept %>% 
  bind_cols(coord)

ggplot(data=centre_plando_dept,
       aes(x = X,
           y = Y)) +
#  geom_point(size = 0.05) +
  geom_density_2d_filled()
