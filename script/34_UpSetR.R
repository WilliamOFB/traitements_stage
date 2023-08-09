### Représentation des intersections entre différents types de plans d'eau ###
# Packages ----
library(tidyverse)
library(sf)
library(dplyr)
library(UpSetR)
library(ggplot2)
library(ComplexUpset)

# Imports ----
plando <- read_sf("chemin/vers/ma/couche/plando.gpkg") %>% # ou .shp ou en .RData 
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
  mutate(Connecte = ifelse(dist_courdo == 0 | R0_Topage == 1 | Nappe == 1, 1, 0)) %>% 
  replace(is.na(.), 0)

plando_select <- plando_select %>% 
  mutate("Sur cours" = ifelse(dist_courdo == 0, 1, 0),
         "Sur source" = R0_Topage,
         "Sur nappe" = Nappe,
         "Sur zone humide" = ZH) %>%
  select(-dist_courdo,
         -R0_Topage,
         -Nappe,
         -ZH)

upset(plando_select,
      c("Sur cours", "Sur source", "Sur nappe", "Sur zone humide"),
      name = "Alimentation des plans d'eau",
      queries = list(upset_query(set = 'Sur cours', fill='orange'),
                     upset_query(set = 'Sur source', fill='#b52b8e'),
                     upset_query(set = 'Sur nappe', fill='coral3'),
                     upset_query(set = 'Sur zone humide', fill='#259d75')),
      base_annotations = list("Nombre de plans d'eau" = (intersection_size(bar_number_threshold = 1,
                                                                           width = 0.5)) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                             labels = scales::number_format(big.mark = " ")) + 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.line = element_line(colour='black'))),
      stripes = upset_stripes(geom = geom_segment(size = 12),
                              colors = c('grey95', 'white')),
      matrix = intersection_matrix(geom = geom_point(shape = 'circle filled',
                                                     size = 4,
                                                     stroke = 0.45)),
      set_sizes = (upset_set_size(geom = geom_bar(width = 0.4)) + 
                     theme(axis.line.x = element_line(colour = 'black'),
                           axis.ticks.x = element_line())) +
        labs(y = "Nombre de plans d'eau") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                           labels = scales::number_format(big.mark = " "),
                           trans = "reverse"),
      sort_sets = 'descending',
      sort_intersections = 'descending')

# Sauvegarde ----
save(plando_select,
     file = "chemin/vers/mon/fichier/plando_upsetr.RData")

write_csv(plando_select,
          file = "chemin/vers/mon/fichier/plando_conn_zh.csv")
