library(tidyverse)
library(sf)
library(mapview)
library(dplyr)
library(ade4)
library(ade4TkGUI)
library(spdep)
library(tmap)

# Import ----
centroide_pe <- read_sf("../../SIG/2-Exploitation/Agregation/Centroide_PE_each_part.gpkg")

load("processed_data/lineaire_mean_me.RData")

long_ce_me <- long_ce_me %>% 
  filter(gid_me != 297) # Une ME n'a pas de géométrie

long_ce_me <- long_ce_me %>% 
  replace_na(list(n=0))

# Traitements ----
#ade4::gearymoran()
#moran.test()

## Test ----
tm_shape(long_ce_me) +
  tm_fill(col="n",
          style="quantile",
          n=8,
          palette="Greens") +
  tm_legend(outside=TRUE)

test_nb <- poly2nb(long_ce_me,
                   queen = TRUE)

test_lw <- nb2listw(test_nb,
                    style = "W",
                    zero.policy = TRUE)

test_I <- moran(long_ce_me$n,
                test_lw,
                length(test_nb),
                Szero(test_lw))[1]

# Analyse de Moran
moran.test(long_ce_me$n,
           test_lw,
           alternative = "greater")

# Méthode de Monte-Carlo
test_mc <- moran.mc(long_ce_me$n,
                    test_lw,
                    nsim = 999,
                    alternative = "greater")
