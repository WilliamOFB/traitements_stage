### Décompte du nombre de plans d'eau par type de géologie ###
# Packages ----
library(sf)
library(tidyverse)
library(mapview)
library(dplyr)

# Imports ----
geol_tot <- st_read("chemin/vers/ma/couche/geologie.gpkg") %>% # ou .shp ou en .RData  
  mutate(surf = st_area(geom))

plando <- read_sf("chemin/vers/ma/couche/plando.gpkg") %>% # ou .shp ou en .RData 
  filter(is.na(ERU),
         is.na(Orage),
         is.na(Ecoul_nat),
         is.na(Transition))

# Compte du nombre de PE par type de géologie et densité numérique ----
intersect_pe_geol <- st_intersection(plando, geol_tot)

## Décompte des plans d'eau ----
plando_geol <- intersect_pe_geol %>% 
  group_by(value) %>% 
  st_drop_geometry() %>% 
  summarise(n_PE = n())

geol <- geol_tot %>% 
  left_join(plando_geol) %>% 
  mutate(dens_num = n_PE / (surf / 1000000))

## Représentation graphique ----
geol <- geol %>% 
  mutate(descr = fct_rev(as.factor(descr)))

ggplot(geol,
       aes(y = dens_num,
           x = descr)) + 
  geom_point() +
  labs(x = "Géologie",
       y = "Densité numérique de plans d'eau, mares comprises",
       title = "Densité numérique de plans d'eau selon la géologie en Bretagne et Pays de la Loire élargis",
       subtitle = "Sans plans d'eau ERU et bassins d'orage") +
  coord_flip() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 13.5),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))

## Sauvegarde ----
save(geol,
     file = "chemin/vers/mon/fichier/geologie_avec_densite.RData")

st_write(geol,
         dsn = "chemin/vers/mon/fichier/geologie_avec_densite.gpkg")

# Test ANOVA ----
## ANOVA ----
densnum_geol <- geol %>% 
  select(value,
         dens_num,
         descr) %>% 
  st_drop_geometry() %>% 
  column_to_rownames("value")

densnum_geol %>%
  group_by(descr) %>%
  identify_outliers() # il y a des valeurs aberrantes...

model_densnum_geol <- lm(dens_num ~ descr,
                         data = densnum_geol)

ggqqplot(residuals(model_densnum_geol))

shapiro_test(residuals(model_densnum_geol))
