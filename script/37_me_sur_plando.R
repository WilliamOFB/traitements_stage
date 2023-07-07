# Pourcentage de surface des PE dans chaque ME ----
## Packages nécessaires ----
library(mapview)
library(dplyr)
library(tidyverse)
library(sf)
library(ggplot2)
library(sp)

## Importations ----
massdo <- read_sf("../../SIG/2-Exploitation/Masses_eau/ME_courdo_nb.gpkg")

massdo_lacustre <- read_sf("../../SIG/2-Exploitation/Masses_eau/ME_lac/massdo_lac_dens_prelev.gpkg")

massdo_trans <- read_sf("../../SIG/2-Exploitation/Masses_eau/ME_transition/massdo_trans_nbPE.gpkg")

massdo_cote <- read_sf("../../SIG/2-Exploitation/Masses_eau/ME_cotiere/massdo_cote_nbPE.gpkg")

plando_in_me <- read_sf("../../SIG/2-Exploitation/Plando/plando_inME.gpkg")

plando_in_lacust <- read_sf("../../SIG/2-Exploitation/Plando/ME_lacustre/plando_ME_lac.gpkg")

plando_in_trans <- read_sf("../../SIG/2-Exploitation/Plando/ME_transition/plando_transition.gpkg")

plando_in_cote <- read_sf("../../SIG/2-Exploitation/Plando/ME_cotiere/plando_ME_cote.gpkg")

## Applications ----
### ME courdo ----
#### PE généraux ----
plando_par_me <- plando_in_me %>% 
  group_by(cdbvspemdo) %>% 
  summarize(sum_surf = sum(surface_plando)) %>% 
  st_drop_geometry()

massdo_ptage_pe <- plando_par_me %>% 
  left_join(y = massdo)

st_write(massdo_ptage_pe,
         dsn = "../../SIG/2-Exploitation/Masses_eau/massdo_surf_PE.gpkg")

#### PE sur cours ----
plando_sur_cours <- st_read("../../SIG/2-Exploitation/Plando/plando_cours_inME.gpkg")

plando_me_cours <- plando_sur_cours %>% 
  group_by(cdbvspemdo) %>% 
  summarize(surf_cours = sum(surface_plando)) %>% 
  st_drop_geometry()

massdo_ptage_cours <- plando_me_cours %>% 
  left_join(y = massdo)

st_write(massdo_ptage_cours,
         dsn = "../../SIG/2-Exploitation/Masses_eau/massdo_surf_cours.gpkg")

#### PE sur source ----
plando_sur_source <- st_read("../../SIG/2-Exploitation/Plando/plando_source_inME.gpkg")

plando_me_source <- plando_sur_source %>% 
  group_by(cdbvspemdo) %>% 
  summarize(surf_source = sum(surface_plando)) %>% 
  st_drop_geometry()

massdo_ptage_source <- plando_me_source %>% 
  left_join(y = massdo)

st_write(massdo_ptage_source,
         dsn = "../../SIG/2-Exploitation/Masses_eau/massdo_surf_source.gpkg")

#### PE sur nappe ----
plando_sur_nappe <- st_read("../../SIG/2-Exploitation/Plando/plando_nappe_inME.gpkg")

plando_me_nappe <- plando_sur_nappe %>% 
  group_by(cdbvspemdo) %>% 
  summarize(surf_nappe = sum(surface_plando)) %>% 
  st_drop_geometry()

massdo_ptage_nappe <- plando_me_nappe %>% 
  left_join(y = massdo)

st_write(massdo_ptage_nappe,
         dsn = "../../SIG/2-Exploitation/Masses_eau/massdo_surf_nappe.gpkg")

#### PE sur ZH ----
plando_sur_zh <- st_read("../../SIG/2-Exploitation/Plando/plando_ZH_inME.gpkg")

plando_me_zh <- plando_sur_zh %>% 
  group_by(cdbvspemdo) %>% 
  summarize(surf_zh = sum(surface_plando)) %>% 
  st_drop_geometry()

massdo_ptage_zh <- plando_me_zh %>% 
  left_join(y = massdo)

st_write(massdo_ptage_zh,
         dsn = "../../SIG/2-Exploitation/Masses_eau/massdo_surf_zh.gpkg")

### ME plan d'eau ----
#### PE généraux ----
plando_par_me_lac <- plando_in_lacust %>% 
  group_by(cdbvspemdo) %>% 
  summarize(sum_surf = sum(surface_plando)) %>% 
  st_drop_geometry()

massdo_lac_ptage_pe <- plando_par_me_lac %>% 
  left_join(y = massdo_lacustre)

st_write(massdo_lac_ptage_pe,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_lac/massdo_lac_surf_PE.gpkg")

#### PE sur cours ----
plando_sur_cours_lac <- plando_in_lacust %>% 
  filter(distance == 0 | sur_cours_Police == 0)

plando_me_lac_cours <- plando_sur_cours_lac %>% 
  group_by(cdbvspemdo) %>% 
  summarize(surf_cours = sum(surface_plando)) %>% 
  st_drop_geometry()

massdo_lac_ptage_cours <- plando_me_lac_cours %>% 
  left_join(y = massdo_lacustre)

st_write(massdo_lac_ptage_cours,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_lac/massdo_lac_surf_cours.gpkg")

#### PE sur source ----
plando_sur_source_lac <- plando_in_lacust %>% 
  filter(R0 == 1)

plando_me_lac_source <- plando_sur_source_lac %>% 
  group_by(cdbvspemdo) %>% 
  summarize(surf_source = sum(surface_plando)) %>% 
  st_drop_geometry()

massdo_lac_ptage_source <- plando_me_lac_source %>% 
  left_join(y = massdo_lacustre)

st_write(massdo_lac_ptage_source,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_lac/massdo_lac_surf_source.gpkg")

#### PE sur nappe ----
plando_sur_nappe_lac <- plando_in_lacust %>% 
  filter(NAPPE == 1)

plando_me_lac_nappe <- plando_sur_nappe_lac %>% 
  group_by(cdbvspemdo) %>% 
  summarize(surf_nappe = sum(surface_plando)) %>% 
  st_drop_geometry()

massdo_lac_ptage_nappe <- plando_me_lac_nappe %>% 
  left_join(y = massdo_lacustre)

st_write(massdo_lac_ptage_nappe,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_lac/massdo_lac_surf_nappe.gpkg")

#### PE sur ZH ----
plando_sur_zh_lac <- plando_in_lacust %>% 
  filter(ZH == 1)

plando_me_lac_zh <- plando_sur_zh_lac %>% 
  group_by(cdbvspemdo) %>% 
  summarize(surf_zh = sum(surface_plando)) %>% 
  st_drop_geometry()

massdo_lac_ptage_zh <- plando_me_lac_zh %>% 
  left_join(y = massdo_lacustre)

st_write(massdo_lac_ptage_zh,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_lac/massdo_lac_surf_zh.gpkg")

### ME de transition ----
#### PE généraux ----
plando_par_me_trans <- plando_in_trans %>% 
  group_by(cdbvspemdo) %>% 
  summarize(surf_PE = sum(surface_plando)) %>% 
  st_drop_geometry()

massdo_trans_ptage_pe <- plando_par_me_trans %>% 
  left_join(y = massdo_trans)

st_write(massdo_trans_ptage_pe,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_transition/massdo_trans_surf_PE.gpkg")

#### PE sur cours ----
plando_sur_cours_trans <- plando_in_trans %>% 
  filter(distance == 0 | sur_cours_Police == 0)

plando_me_trans_cours <- plando_sur_cours_trans %>% 
  group_by(cdbvspemdo) %>% 
  summarize(surf_PE_cours = sum(surface_plando)) %>% 
  st_drop_geometry()

massdo_trans_ptage_cours <- plando_me_trans_cours %>% 
  left_join(y = massdo_trans)

st_write(massdo_trans_ptage_cours,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_transition/massdo_trans_surf_cours.gpkg")

#### PE sur source ----
plando_sur_source_trans <- plando_in_trans %>% 
  filter(R0 == 1)

plando_me_trans_source <- plando_sur_source_trans %>% 
  group_by(cdbvspemdo) %>% 
  summarize(surf_PE_source = sum(surface_plando)) %>% 
  st_drop_geometry()

massdo_trans_ptage_source <- plando_me_trans_source %>% 
  left_join(y = massdo_trans)

st_write(massdo_trans_ptage_source,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_transition/massdo_trans_surf_source.gpkg")

#### PE sur nappe ----
plando_sur_nappe_trans <- plando_in_trans %>% 
  filter(NAPPE == 1)

plando_me_trans_nappe <- plando_sur_nappe_trans %>% 
  group_by(cdbvspemdo) %>% 
  summarize(surf_PE_nappe = sum(surface_plando)) %>% 
  st_drop_geometry()

massdo_trans_ptage_nappe <- plando_me_trans_nappe %>% 
  left_join(y = massdo_trans)

st_write(massdo_trans_ptage_nappe,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_transition/massdo_trans_surf_nappe.gpkg")

#### PE sur ZH ----
plando_sur_zh_trans <- plando_in_trans %>% 
  filter(ZH == 1)

plando_me_trans_zh <- plando_sur_zh_trans %>% 
  group_by(cdbvspemdo) %>% 
  summarize(surf_PE_zh = sum(surface_plando)) %>% 
  st_drop_geometry()

massdo_trans_ptage_zh <- plando_me_trans_zh %>% 
  left_join(y = massdo_trans)

st_write(massdo_trans_ptage_zh,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_transition/massdo_trans_surf_zh.gpkg")

### ME côtières ----
#### PE généraux ----
plando_par_me_cote <- plando_in_cote %>% 
  group_by(cdbvspemdo) %>% 
  summarize(surf_PE = sum(surface_plando)) %>% 
  st_drop_geometry()

massdo_cote_ptage_pe <- plando_par_me_cote %>% 
  left_join(y = massdo_cote)

st_write(massdo_cote_ptage_pe,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_cotiere/massdo_cote_surf_PE.gpkg")

#### PE sur cours ----
plando_sur_cours_cote <- plando_in_cote %>% 
  filter(distance == 0 | sur_cours_Police == 0)

plando_me_cote_cours <- plando_sur_cours_cote %>% 
  group_by(cdbvspemdo) %>% 
  summarize(surf_PE_cours = sum(surface_plando)) %>% 
  st_drop_geometry()

massdo_cote_ptage_cours <- plando_me_cote_cours %>% 
  left_join(y = massdo_cote)

st_write(massdo_cote_ptage_cours,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_cotiere/massdo_cote_surf_cours.gpkg")

#### PE sur source ----
plando_sur_source_cote <- plando_in_cote %>% 
  filter(R0 == 1)

plando_me_cote_source <- plando_sur_source_cote %>% 
  group_by(cdbvspemdo) %>% 
  summarize(surf_PE_source = sum(surface_plando)) %>% 
  st_drop_geometry()

massdo_cote_ptage_source <- plando_me_cote_source %>% 
  left_join(y = massdo_cote)

st_write(massdo_cote_ptage_source,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_cotiere/massdo_cote_surf_source.gpkg")

#### PE sur nappe ----
plando_sur_nappe_cote <- plando_in_cote %>% 
  filter(NAPPE == 1)

plando_me_cote_nappe <- plando_sur_nappe_cote %>% 
  group_by(cdbvspemdo) %>% 
  summarize(surf_PE_nappe = sum(surface_plando)) %>% 
  st_drop_geometry()

massdo_cote_ptage_nappe <- plando_me_cote_nappe %>% 
  left_join(y = massdo_cote)

st_write(massdo_cote_ptage_nappe,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_cotiere/massdo_cote_surf_nappe.gpkg")

#### PE sur ZH ----
plando_sur_zh_cote <- plando_in_cote %>% 
  filter(ZH == 1)

plando_me_cote_zh <- plando_sur_zh_cote %>% 
  group_by(cdbvspemdo) %>% 
  summarize(surf_PE_zh = sum(surface_plando)) %>% 
  st_drop_geometry()

massdo_cote_ptage_zh <- plando_me_cote_zh %>% 
  left_join(y = massdo_cote)

st_write(massdo_cote_ptage_zh,
         dsn = "../../SIG/2-Exploitation/Masses_eau/ME_cotiere/massdo_cote_surf_zh.gpkg")