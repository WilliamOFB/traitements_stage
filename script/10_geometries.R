# Packages ----
library(tidyverse)
library(mapview)
library(sf)
library(dplyr)

# Imports ----
surf_eau_old <- read_sf("../../SIG/Sauvegardes/SurfaceElementaire_perimetre_etude.gpkg") %>% 
  st_transform(crs = 2154)

plando_now <- read_sf("../../SIG/2-Exploitation/Plando/plando_prelev.gpkg") %>% 
  st_transform (crs = 2154)

plando_diff <- st_difference(surf_eau_old, plando_now)
