# Packages ----
library(sf)
library(tidyverse)
library(mapview)

# Import des couches ----
zh <- read_sf('../../SIG/2-Exploitation/ZH/Vectorise/ZH_perimetre.gpkg') %>% 
  st_transform(crs = 2154)

zh35 <- read_sf('../../SIG/2-Exploitation/ZH/Vectorise/ZH35_vectorise.gpkg') %>% 
  st_transform(crs = 2154)

plando_unique <- read_sf('../../SIG/2-Exploitation/plando_delim_roe_unique.gpkg')

# Traitements ----
plando_index <- plando_unique %>% 
  st_join(zh,
          join = st_intersects,
          largest = FALSE)

plando_select <- plando_index %>% 
  mutate(ZH = if_else(!is.na(zh), 1, 0)) %>% 
  st_drop_geometry()

plando_zh35 <- plando_unique %>% 
  st_join(zh35,
          join = st_intersects) %>% 
  mutate(ZH = if_else(!is.na(zh35), 1, 0))
