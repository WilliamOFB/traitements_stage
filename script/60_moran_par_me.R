library(tidyverse)
library(sf)
library(mapview)
library(dplyr)
library(ade4)

# Import ----
centroide_pe <- read_sf("../../SIG/2-Exploitation/Agregation/Centroide_PE_each_part.gpkg")

load("processed_data/lineaire_mean_me.RData")

# Traitements ----
