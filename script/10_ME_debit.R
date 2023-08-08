### Visualisation des débits interceptés par les plando sur courdo et à l'exutoire des massdo ###
# Packages ----
library(sf)
library(tidyverse)
library(mapview)
library(dplyr)
library(devtools)
library(classInt)
library(ggplot2)
library(scales)

# Import ----
massdo <- read_sf("chemin/vers/ma/couche/massdo.gpkg") %>% # ou .shp ou en .RData
  st_transform(crs = 2154)

plando <- st_read("chemin/vers/ma/couche/plando.gpkg") %>% # ou .shp ou en .RData
  filter(PERIM == 1,
         is.na(Orage),
         is.na(Ecoul_nat),
         is.na(Transition),
         is.na(ERU))

plando_ssMarais <- plando %>% 
  filter(is.na(Marais))

# Visualisation des débits interceptés par les PE sur CE ----
## Application ----
plandoQ5 <- plando %>% 
  filter(join_Q5MOY_MN <= 500 &
         !is.na(join_Q5MOY_MN) &
           join_Q5MOY_MN >= 0,
         distance_2 <= 100) %>% 
  st_drop_geometry()

plandoQA <- plando %>% 
  filter(!is.na(QAMOY_MN) &
           QAMOY_MN >= 0,
         distance_2 <= 100) %>% 
  st_drop_geometry()

good_plandoQ5_ssMarais <- plando_ssMarais %>% 
  filter(Q5MOY_MN <= 500 &
           !is.na(Q5MOY_MN) &
           Q5MOY_MN >= 0,
         distance_2 <= 100) %>% 
  st_drop_geometry()

plandoQA_ssMarais <- plando_ssMarais %>% 
  filter(!is.na(QAMOY_MN) &
           QAMOY_MN >= 0,
         distance_2 <= 100) %>% 
  st_drop_geometry()

### Interégional ----
#### QMNA5 ----
ggplot(data = plandoQ5,
       aes(x = Q5MOY_MN)) +
  geom_histogram(bins = 50,
                 fill = "lightblue",
                 color = "grey") +
  scale_x_continuous(limits = c(NA,0.11)) + 
  labs(x = "QMNA5 (m³/s)",
       y = "Nombre",
       title = "QMNA5 intercepté par les plans d'eau sur cours d'eau en Bretagne et Pays de la Loire") + 
  theme_gray(base_size = 20)

#### Module ----
ggplot(data = plandoQA,
       aes(x = QAMOY_MN)) +
  geom_histogram(bins = 50,
                 fill = "lightblue",
                 color = "grey") +
  scale_x_continuous(limits = c(NA,2)) + 
  labs(x = "Module (m³/s)",
       y = "Nombre",
       title = "Débit de module intercepté par les plans d'eau sur cours d'eau en Bretagne et Pays de la Loire") + 
  theme_gray(base_size = 20)

### Bretagne ----

# Pareil qu'au-dessus avec le tri

plando_bretagne <- plando %>% 
  filter(INSEE_DEP %in% c(29, 35, 22, 56))

### Pays de la Loire ----

# Pareil qu'au-dessus avec le tri

plando_pdl <- plando %>% 
  filter(INSEE_DEP %in% c(44, 49, 53, 72, 85))

# Débits à l'exutoire des ME ----
## Packages supplémentaires ----

## Traitement préalable ----
massdo <- massdo %>% 
  filter(is.na(HORS_PERIM) &
           !is.na(QAMOY_MN_m))

## Application ----
### Numérique ---- 
# Remplacer "QAMOY_MN_m" par "Q5MOY_MN_m" pour avoir les QMNA5
nb_classes_deb <- 10

discr_deb <- classIntervals(massdo$Q5MOY_MN_m,
                        nb_classes_deb,
                        style = "jenks")

lim_classes_deb <- discr_deb$brks

data_discr_deb <- cut(massdo$Q5MOY_MN_m,
                  breaks = lim_classes_deb,
                  include.lowest = TRUE)

etiquettes_deb <- paste0("[", lim_classes_deb[-nb_classes_deb],
                         " ; ", lim_classes_deb[-1], "]")

graphique_deb <- ggplot(data.frame(data_discr_deb),
                    aes(x = data_discr_deb)) + 
  geom_bar(fill = "lightblue",
           color = "grey") + 
  labs(x = "QMNA5 (L/s)",
       y = "Nombre") + 
  ggtitle("Répartition des QMNA5 à l'exutoire des ME en Bretagne et PdL") + 
  theme_gray() + 
  scale_x_discrete(labels = etiquettes_deb) + 
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

print(graphique_deb)

### Bretagne ----
massdo_bret <- massdo %>% 
  filter(!is.na(In_Bret))

discr_bret <- classIntervals(massdo_bret$Q5MOY_MN_m,
                             nb_classes_deb,
                             style = "jenks")

lim_classes_bret <- discr_bret$brks

data_discr_bret <- cut(massdo_bret$Q5MOY_MN_m,
                       breaks = lim_classes_bret,
                       include.lowest = TRUE)

etiquettes_bret <- paste0("[", lim_classes_bret[-nb_classes_deb], " ; ",
                          lim_classes_bret[-1], "]")

graphique_bret <- ggplot(data.frame(data_discr_bret),
                         aes(x = data_discr_bret)) + 
  geom_bar(fill = "lightblue",
           color = "grey") + 
  labs(x = "QMNA5 (L/s)",
       y = "Nombre de ME") + 
  ggtitle("Répartition des QMNA5 à l'exutoire des ME en Bretagne") + 
  theme_gray() + 
  scale_x_discrete(labels = etiquettes_bret) + 
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

print(graphique_bret)

### Pays de la Loire ----
massdo_pdl <- massdo %>% 
  filter(!is.na(In_PdL))

discr_pdl <- classIntervals(massdo_pdl$Q5MOY_MN_m,
                             nb_classes_deb,
                             style = "jenks")

lim_classes_pdl <- discr_pdl$brks

data_discr_pdl <- cut(massdo_pdl$Q5MOY_MN_m,
                       breaks = lim_classes_pdl,
                       include.lowest = TRUE)

etiquettes_pdl <- paste0("[", lim_classes_pdl[-nb_classes_deb], " ; ",
                          lim_classes_pdl[-1], "]")

graphique_pdl <- ggplot(data.frame(data_discr_pdl),
                         aes(x = data_discr_pdl)) + 
  geom_bar(fill = "lightblue",
           color = "grey") + 
  labs(x = "QMNA5 (L/s)",
       y = "Nombre de ME") + 
  ggtitle("Répartition des QMNA5 à l'exutoire des ME en Pays de la Loire") + 
  theme_gray() + 
  scale_x_discrete(labels = etiquettes_pdl) + 
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

print(graphique_pdl)
