### Représentation des intersections entre différents types de plans d'eau ###
# Packages ----
library(tidyverse)
library(sf)
library(dplyr)
library(UpSetR)
library(ggplot2)

# Imports ----
plando <- read_sf("../../SIG/0-Couche_projet_final/Couche_plando/plando_full_source_230802.gpkg") %>% 
  filter(is.na(ERU),
         is.na(Orage),
         is.na(Ecoul_nat),
         is.na(Transition))

# Sélection ----
plando_select <- plando %>% 
  select(gid_plando,
         dist_courdo,
         R0_Topage,
         Nappe,
         ZH) %>% 
  st_drop_geometry() %>% 
  column_to_rownames("gid_plando")
  
plando_select <- plando_select %>% 
  mutate(cours = ifelse(dist_courdo == 0, 1, 0),
         source = R0_Topage,
         nappe = Nappe) %>%
  select(-dist_courdo,
         -R0_Topage,
         -Nappe)

plando_select <- plando_select %>% 
  mutate(connecte = ifelse(cours == 1 | source == 1 | nappe == 1, 1, 0))

upset(plando_select,
      sets = c("cours", "source", "nappe", "ZH"),
      order.by = "freq",
      mainbar.y.label = "Nombre de plans d'eau",
      sets.x.label = NULL,
      main.bar.color = "grey53",
      color.pal = 4)

# Sauvegarde ----
save(plando_select,
     file = "processed_data/plando_conn_zh.RData")

write_csv(plando_select,
          file = "processed_data/Stats/plando_conn_zh.csv")
