# Attribution du module et du QMNA5 aux bonnes ME ----
## Packages ----
library(sf)
library(tidyverse)
library(mapview)
library(dplyr)

## Import ----
me_riviere <- read_sf("../../SIG/2-Exploitation/Masses_eau/ME_plando_courdo_riviere.gpkg")

me_old <- read_sf("../../SIG/2-Exploitation/Masses_eau/BVME_DIR02_PE_QA_Q5_QMNA5_inf_Dixieme_Module.shp")

### Vérification ----
mapview(me_riviere,
        col.regions = "pink",
        alpha = 0.5,
        alpha.region = 0.2) + 
  mapview(me_old)

## Traitements ----
me_riviere_debit <- me_riviere %>%
  st_intersection(me_old) %>%
  mutate(surface_intersect = st_area(.)) %>%
  select(gid_me,
         surfacebvs,
         surface_intersect,
         QABASN_max,
         QAMOY_MN_m,
         QAHAUN_max,
         Q5BASN_max,
         Q5MOY_MN_m,
         Q5HAUN_max) %>% 
  st_drop_geometry() %>%
  mutate(pc_new_sur_old = surface_intersect / surfacebvs)

affectation_me_new <- me_riviere_debit %>% 
  group_by(gid_me) %>%
  filter(pc_new_sur_old == max(pc_new_sur_old)) %>%
  ungroup() %>% 
  select(gid_me,
         QABASN_max,
         QAMOY_MN_m,
         QAHAUN_max,
         Q5BASN_max,
         Q5MOY_MN_m,
         Q5HAUN_max)

me_riviere_debit <- me_riviere %>% 
  left_join(y = affectation_me_new)

## Sauvegarde ----
save(me_riviere_debit,
     file = "processed_data/masse_eau_debit.RData")

st_write(me_riviere_debit,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_riviere_debit.gpkg")

# Visualisation des débits interceptés par les PE sur CE ----
## Packages supplémentaires ----
library(devtools)
library(classInt)
library(ggplot2)
library(scales)

## Imports supplémentaires ----
plando_debit_me <- st_read("../../SIG/2-Exploitation/Plando/plando_full_source_230712.gpkg") %>% 
  filter(PERIM == 1,
         is.na(Orage),
         is.na(Ecoul_nat),
         is.na(Transition),
         is.na(ERU))

## Application ----
plando_debit_ssMarais <- plando_debit_me %>% 
  filter(is.na(Marais))

good_plandoQ5 <- plando_debit_me %>% 
  filter(join_Q5MOY_MN <= 500 &
         !is.na(join_Q5MOY_MN) &
           join_Q5MOY_MN >= 0,
         distance_2 <= 100) %>% 
  st_drop_geometry()

good_plandoQ5_ssMarais <- plando_debit_ssMarais %>% 
  filter(join_Q5MOY_MN <= 500 &
           !is.na(join_Q5MOY_MN) &
           join_Q5MOY_MN >= 0,
         distance_2 <= 100) %>% 
  st_drop_geometry()

good_plandoQA <- plando_debit_me %>% 
  filter(!is.na(join_QAMOY_MN) &
           join_QAMOY_MN >= 0,
         distance_2 <= 100) %>% 
  st_drop_geometry()

good_plandoQA_ssMarais <- plando_debit_ssMarais %>% 
  filter(!is.na(join_QAMOY_MN) &
           join_QAMOY_MN >= 0,
         distance_2 <= 100) %>% 
  st_drop_geometry()

### Interégional ----
#### QMNA5 ----
ggplot(data = good_plandoQ5,
       aes(x = join_Q5MOY_MN)) +
  geom_histogram(bins = 50,
                 fill = "lightblue",
                 color = "grey") +
  scale_x_continuous(limits = c(NA,0.11)) + 
  labs(x = "QMNA5 (m³/s)",
       y = "Nombre",
       title = "QMNA5 intercepté par les plans d'eau sur cours d'eau en Bretagne et Pays de la Loire") + 
  theme_gray(base_size = 20)

#### Module ----
ggplot(data = good_plandoQA,
       aes(x = join_QAMOY_MN)) +
  geom_histogram(bins = 50,
                 fill = "lightblue",
                 color = "grey") +
  scale_x_continuous(limits = c(NA,2)) + 
  labs(x = "Module (m³/s)",
       y = "Nombre",
       title = "Débit de module intercepté par les plans d'eau sur cours d'eau en Bretagne et Pays de la Loire") + 
  theme_gray(base_size = 20)

### Bretagne ----

### Pays de la Loire ----

# Débits à l'exutoire des ME ----
## Packages supplémentaires ----

## Imports supplémentaires ----
massdo_perim <- read_sf("../../SIG/2-Exploitation/Masses_eau/ME_courdo_nb.gpkg") %>% 
  st_transform(crs = 2154)

massdo_perim <- massdo_perim %>% 
  filter(is.na(HORS_PERIM) &
           !is.na(QAMOY_MN_m))

## Application ----
### Numérique ---- 
# Remplacer "QAMOY_MN_m" par "Q5MOY_MN_m" pour avoir les QMNA5
nb_classes_deb <- 10

discr_deb <- classIntervals(massdo_perim$Q5MOY_MN_m,
                        nb_classes_deb,
                        style = "jenks")

lim_classes_deb <- discr_deb$brks

data_discr_deb <- cut(massdo_perim$Q5MOY_MN_m,
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
  theme_gray()# + 
#  coord_flip() + 
#  scale_x_reverse()

graphique_deb <- graphique_deb + 
  scale_x_discrete(labels = etiquettes_deb) + 
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

print(graphique_deb)

### Bretagne ----
massdo_bret <- massdo_perim %>% 
  filter(!is.na(IN_Bret))

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
  theme_gray()

graphique_bret <- graphique_bret + 
  scale_x_discrete(labels = etiquettes_bret) + 
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

print(graphique_bret)

### Pays de la Loire ----
massdo_pdl <- massdo_perim %>% 
  filter(!is.na(IN_PdL))

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
  theme_gray()

graphique_pdl <- graphique_pdl + 
  scale_x_discrete(labels = etiquettes_pdl) + 
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

print(graphique_pdl)
