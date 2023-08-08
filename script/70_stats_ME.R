### Traitements stats des débits des ME ###
# Packages ----
library(mapview)
library(tidyverse)
library(sf)
library(dplyr)
library(NbClust)
library(geostats)
library(ggplot2)
library(ggpubr)
library(FactoClass)
library(factoextra)

# Imports ----
massdo <- read_sf("chemin/vers/ma/couche/massdo.gpkg") %>% # ou .shp ou en .RData
  st_transform(2154)

# Application ----
massdo_courdo_ssmax <- massdo_courdo %>% 
  filter(QAMOY_MN_m < 100)

regr_lin <- lm(massdo_courdo$QAMOY_MN_m ~ massdo_courdo$surfacebvs)

## Module par surface ----
lm_QA <- lm(surfacebvs ~ QAMOY_MN_m,
               data = massdo_courdo_ssmax)

r2_QA <- summary(lm_QA)$r.squared


ggplot(massdo_courdo_ssmax,
                        aes(x = QAMOY_MN_m,
                            y = surfacebvs)) + 
                  geom_point() + 
                  geom_smooth(method = "lm",
                              se = FALSE) + 
  labs(x = "Module moyen (m³/s)",
       y = "Surface (en km²)",
       title = "Module moyen à l'exutoire des ME par leur surface en Bretagne et PdL") +
  scale_y_continuous(labels = function(x) {
    x <- x / 1000000
    x
  }) + 
  geom_text(aes(x = max(QAMOY_MN_m),
                y = max(surfacebvs),
                label = paste0("y = ",
                               format(coef(lm_QA)[1],
                                      scientific = FALSE,
                                      big.mark = " "),
                               " + ", format(coef(lm_QA)[2],
                                             scientific = FALSE,
                                             big.mark = " "),
                               " x", "\n",
                               "R² = ", format(r2_QA, digits = 3))),
            hjust = 1, vjust = 1)

## QMNA5 par surface ----
lm_QMNA5 <- lm(surfacebvs ~ Q5MOY_MN_m,
               data = massdo_courdo_ssmax)

r2_QMNA5 <- summary(lm_QMNA5)$r.squared

ggplot(massdo_courdo_ssmax,
                        aes(x = Q5MOY_MN_m,
                            y = surfacebvs)) + 
  geom_point() + 
  geom_smooth(method = "lm",
              se = FALSE) +
  labs(x = "QMNA5 moyen (m³/s)",
       y = "Surface (en km²)",
       title = "QMNA5 moyen à l'exutoire des ME par leur surface en Bretagne et PdL") +
  scale_y_continuous(labels = function(x) {
    x <- x / 1000000
    x
  }) + 
  geom_text(aes(x = max(Q5MOY_MN_m),
                y = max(surfacebvs),
                label = paste0("y = ",
                               format(coef(lm_QMNA5)[1],
                                      scientific = FALSE,
                                      big.mark = " "),
                               " + ", format(coef(lm_QMNA5)[2],
                                             scientific = FALSE,
                                             big.mark = " "),
                               " x", "\n",
                               "R² = ", format(r2_QMNA5, digits = 3))),
            hjust = 1, vjust = 1)

## Module par QMNA5 ----
ggplot(massdo_courdo_ssmax,
       aes(x = QAMOY_MN_m,
           y = Q5MOY_MN_m)) + 
  geom_point() + 
  geom_smooth(method = "lm",
              se = FALSE)

## Temperature de l'eau par nombre de PE ----
massdo_good_temp <- massdo_courdo %>% 
  filter(MTw30J_MUL > 0)

massdo_courdo_AW <- massdo_good_temp %>% 
  mutate(Ta_Tw = MTa30J-MTw30J_MUL)

res <- NbClust(massdo_courdo_AW$Ta_Tw, # Détermination des clusters selon k-means
               distance = "euclidean",
               method = "kmeans",
               index = "all")

massdo_courdo_AW$cluster_AW <- res$Best.partition

massdo_courdo$cluster_densPE <- res$Best.partition

massdo_courdo_TBV <- massdo_courdo_AW %>% 
  filter(lineaire_Loire == 0 & lineaire_R4 == 0 &
           lineaire_R5 == 0 & lineaire_R6 == 0 &
           lineaire_R7 == 0 & lineaire_R8 == 0)

ggplot(massdo_courdo_AW,
       aes(x = Ta_Tw,
           y = dens_num_PE,
           color = factor(cluster_AW))) + 
  geom_point() + 
  geom_smooth(method = "lm",
              se = FALSE) +
  stat_regline_equation(label.y = c(14, 15, 16),
                        label.x = c(-6, -6, -6),
                        formula = y ~ x,
                        aes(label = paste(..eq.label..,
                                          ..rr.label..,
                                          sep = "~~~~"))) +
  labs(x = "Différence entre la T° de l'air et celle de l'eau (°C)",
       y = "Densité de plans d'eau (nb/km²)",
       title = "T° de l'air - T° de l'eau en fonction de la densité de PE par ME en Bretagne et PdL selon 3 clusters de différence température",
       color = "Clusters")

## Nombre de PE ou densités par géologie majoritaire ----
massdo_ssmax_dens <- massdo %>% 
  filter(dens_num_PE < 15)

massdo_ssLac <- massdo %>% 
  filter(layer != "lacustre")

ggplot(massdo_ssLac,
       aes(x = dens_num, # à modifier en fonction de la comparaison voulue
           y = top_geol)) + 
  geom_boxplot() + 
  geom_text(data = aggregate(dens_num ~ top_geol,
                             massdo_ssLac,
                             median),
            aes(label = round(dens_num, 2),
                x = max(dens_num + 5.5),
                y = top_geol),
            vjust = 0.5,
            hjust = 0) +
  labs(y = "Géologie majoritaire",
       x = "Densité numérique de plans d'eau",
       title = "Densité numérique de plans d'eau selon la géologie majoritaire de la masse d'eau en Bretagne et Pays de la Loire",
       subtitle = "Sans plans d'eau ERU, en marais, bassins d'orage ni masses d'eau lacustres")
