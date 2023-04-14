library(tidyverse)
library(mapview)
library(sf)
library(rvest)
library(httr)
library(xml2)
library(dplyr)

load("processed_data/plando_comm_her_me.RData")

paysages_pdl <- read_sf("raw_data/r_unite_paysagere_r52.shp")

paysages_22 <- read_sf("raw_data/unites_paysages_22.shp")

paysages_bret <- read_sf("../../SIG/2-Exploitation/Paysages_293556.gpkg")

paysages_full <- read_sf("../../SIG/2-Exploitation/Paysages/paysages_full.gpkg")

mapview(paysages_full)

paysages_full <- paysages_full %>%
  sf :: st_make_valid()

paysages_full <- paysages_full %>%
  fortify()

paysages_full <- paysages_full %>% 
  