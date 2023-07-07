# Packages ----
library(tidyverse)
library(mapview)
library(sf)
library(dplyr)

# Linéaire de CE par ME ----
## Import ----
me <- read_sf("../../SIG/2-Exploitation/ME_nb_plando.gpkg")

courdo <- read_sf("../../SIG/Cours_eau/TronconHydrographique_Bretagne_Pays_de_la_Loire_non_aqueduc_strahler.shp") %>% 
  rename(gid_ce = gid) %>% 
  st_transform(crs = 2154)

## Attribution ----
intersections <- st_intersection(courdo, me) %>% 
  mutate(length_intersect = st_length(.)) %>%
  st_drop_geometry() %>% 
  select(gid_ce,
         CdOH,
         NatureTH,
         Persistanc,
         OrigineTH,
         ClasseLarg,
         StreamOrde,
         gid_me,
         cdbvspemdo,
         nombvspemd,
         cdmassedea,
         n,
         length_intersect)#%>% 
#  mutate(pc_courdo_in_me = length_intersect / surface_plando)

counts <- intersections %>% 
  group_by(gid_me) %>%
  summarise(long_tot = sum(length_intersect))

long_ce_me <- me %>% 
  left_join(y = counts) %>% 
  mutate(lineaire_mean = long_tot / n)

## Sauvegarde ----
save(long_ce_me,
     file = "processed_data/lineaire_mean_me.RData")

st_write(long_ce_me,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_plando_courdo.gpkg")

# Proportion & linéaire de CE par rang par ME ----
## Imports ----
massdo <- read_sf("../../SIG/2-Exploitation/Masses_eau/ME_riviere_debit.gpkg") %>% 
  st_transform(crs = 2154)

## Attribution ----
courdo_autres_me <- read_sf("../../SIG/2-Exploitation/Courdo/courdo_autre_ME.gpkg") %>% 
  rename(gid_ce = gid)

intersect <- st_intersection(courdo_autres_me, massdo_cote) %>%
  select(gid_ce,
         StreamOrde,
         cdbvspemdo,
         LONG,
         geom)

# Si calcul de la longueur trop lent : 
#intersect_length <- st_read("../../SIG/2-Exploitation/test_intersect_courdo_massdo.gpkg") %>% 
#  select(-HORS_PERIM)

length_by_ordre <- intersect %>% 
  group_by(cdbvspemdo,
           StreamOrde) %>% 
  summarise(long_ordre = sum(LONG))

st_write(length_by_ordre,
         dsn = "../../SIG/2-Exploitation/Courdo/long_courdo_ordre_cote.gpkg")

massdo_test <- massdo_cote %>% 
  mutate(long0 = if_else(length_by_ordre$StreamOrde = "0",
                          length_by_ordre$long_ordre,
                          ''))
# Discrétisation des longueurs de TBV par ME ----
## Packages supplémentaires ----
library(devtools)
library(classInt)
library(ggplot2)
library(scales)

## Imports supplémentaires ----
massdo <- st_read("../../SIG/2-Exploitation/Masses_eau/ME_riviere_rang.gpkg") %>% 
  st_transform(crs = 2154)

## Application ----
### Inter-régional ----
#### Numérique ----
massdo_tbv <- massdo %>% 
  mutate(long_tbv = X1_long_ordre + X2_long_ordre)

massdo_tbv_perim <- massdo_tbv %>% 
  filter(is.na(HORS_PERIM))

nb_classes <- 10

discr <- classIntervals(massdo_tbv_perim$long_tbv,
                        nb_classes,
                        style = "jenks")

lim_classes <- discr$brks

data_discr <- cut(massdo_tbv_perim$long_tbv,
                  breaks = lim_classes,
                  include.lowest = TRUE)

etiquettes <- paste0("[", round(lim_classes[-nb_classes]), " ; ", round(lim_classes[-1]), "]")

graphique <- ggplot(data.frame(data_discr),
       aes(x = data_discr)) + 
  geom_bar(fill = "lightblue",
           color = "grey") + 
  labs(x = "Linéaire de rangs 1 et 2 (en m)",
       y = "Nombre") + 
  ggtitle("Proportion de ME en fonction du linéaire de cours d'eau de TBV en Bretagne & PdL") + 
  theme_gray()# + 
#  coord_flip() + 
#  scale_x_reverse()

graphique <- graphique + 
  scale_x_discrete(labels = etiquettes) + 
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

print(graphique)

#### Proportion ----
massdo_prop <- massdo_tbv_perim %>% 
  mutate(prop_tbv = (long_tbv/long_tot)*100)

nb_classes_prop <- 5

discr_prop <- classIntervals(massdo_prop$prop_tbv,
                        nb_classes_prop,
                        style = "jenks")

lim_classes_prop <- discr_prop$brks

data_discr_prop <- cut(massdo_prop$prop_tbv,
                  breaks = lim_classes_prop,
                  include.lowest = TRUE)

etiquettes_prop <- paste0("[", round(lim_classes_prop[-nb_classes]), " ; ", round(lim_classes_prop[-1]), "]")

graphique_discr <- ggplot(data.frame(data_discr_prop),
                    aes(x = data_discr_prop)) + 
  geom_bar(fill = "lightblue",
           color = "grey") + 
  labs(x = "Pourcentage de linéaire de rangs 1 & 2 // au linéaire total de cours d'eau par ME",
       y = "Nombre de ME") + 
  ggtitle("Nombre de ME en fonction du pourcentage de cours d'eau de TBV en Bretagne & PdL") + 
  theme_gray()

graphique_discr <- graphique_discr + 
  scale_x_discrete(labels = etiquettes_prop) + 
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

print(graphique_discr)

### Bretagne ----
massdo_bret <- massdo_prop %>% 
  filter(!is.na(IN_Bret))

discr_bret <- classIntervals(massdo_bret$prop_tbv,
                             nb_classes_prop,
                             style = "jenks")

lim_classes_bret <- discr_bret$brks

data_discr_bret <- cut(massdo_bret$prop_tbv,
                       breaks = lim_classes_bret,
                       include.lowest = TRUE)

etiquettes_bret <- paste0("[", round(lim_classes_bret[-nb_classes]), " ; ",
                          round(lim_classes_bret[-1]), "]")

graphique_bret <- ggplot(data.frame(data_discr_bret),
                          aes(x = data_discr_bret)) + 
  geom_bar(fill = "lightblue",
           color = "grey") + 
  labs(x = "Pourcentage de linéaire de rangs 1 & 2 // au linéaire total de cours d'eau par ME",
       y = "Nombre de ME") + 
  ggtitle("Nombre de ME en fonction du pourcentage de cours d'eau de TBV en Bretagne") + 
  theme_gray()

graphique_bret <- graphique_bret + 
  scale_x_discrete(labels = etiquettes_bret) + 
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

print(graphique_bret)

### Pays de la Loire ----
massdo_pdl <- massdo_prop %>% 
  filter(!is.na(IN_PdL))

discr_pdl <- classIntervals(massdo_pdl$prop_tbv,
                             nb_classes_prop,
                             style = "jenks")

lim_classes_pdl <- discr_pdl$brks

data_discr_pdl <- cut(massdo_pdl$prop_tbv,
                       breaks = lim_classes_pdl,
                       include.lowest = TRUE)

etiquettes_pdl <- paste0("[", round(lim_classes_pdl[-nb_classes]), " ; ",
                          round(lim_classes_pdl[-1]), "]")

graphique_pdl <- ggplot(data.frame(data_discr_pdl),
                         aes(x = data_discr_pdl)) + 
  geom_bar(fill = "lightblue",
           color = "grey") + 
  labs(x = "Pourcentage de linéaire de rangs 1 & 2 // au linéaire total de cours d'eau par ME",
       y = "Nombre de ME") + 
  ggtitle("Nombre de ME en fonction du pourcentage de cours d'eau de TBV en PdL") + 
  theme_gray()

graphique_pdl <- graphique_pdl + 
  scale_x_discrete(labels = etiquettes_pdl) + 
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

print(graphique_pdl)
