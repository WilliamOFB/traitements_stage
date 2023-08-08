### Carte de densité de points ###
# Packages ----
library(tidyverse)
library(sf)
library(dplyr)

# Imports ----
plando <- read_sf("chemin/vers/ma/couche/plando.gpkg") %>% # ou .shp ou en .RData 
  filter(is.na(ERU),
         is.na(Orage),
         is.na(Ecoul_nat),
         is.na(Transition))

dept <- read_sf("chemin/vers/ma/couche/departements.gpkg") %>% # ou .shp ou en .RData 
  st_transform(crs = 2154)

# Centroïdes ----
centre_plando <- st_centroid(plando)

centre_plando_dept <- centre_plando %>% 
  filter(INSEE_DEP == "29") # Entrer le numéro de département voulu

coord <- centre_plando_dept %>% 
  st_coordinates()

centre_plando_dept <- centre_plando_dept %>% 
  bind_cols(coord)

# Visualisation ----
ggplot(data=centre_plando_dept,
       aes(x = X,
           y = Y)) +
  geom_density_2d_filled()
