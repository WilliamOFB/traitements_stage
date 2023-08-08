### Fonctions de répétition des traitements ###
# Packages ----
library(tidyverse)
library(mapview)
library(sf)
library(dplyr)
library(stringr)

# --------------------------- début ma_select() -------------------------
ma_select <- function(data,
                      type = NULL)
  
{
  
sel <- data
  
  # gestion des étiquettes des axes
  if(type == "cours") {sel <- sel %>% filter(dist_courdo == 0) }
  if(type == "source") {sel <- sel %>% filter(R0_Topage == 1) }
  if(type == "nappe") {sel <- sel %>% filter(Nappe == 1) }
  if(type == "zh") {sel <- sel %>% filter(ZH == 1) }
  if(type == "connecte") {sel <- sel %>% filter(dist_courdo == 0  | Nappe == 1 | R0_Topage == 1) }
  if(type == "tbv") {sel <- sel %>% filter(StreamOrder == c(1,2), dist_courdo <= 500) }
  
}

# --------------------------- fin ma_select() -------------------------

# --------------------------- début mon_compte() -------------------------
mon_compte <- function(data,
                        group = "code",
                        type = NULL)
  
{
  
  sel <- data %>% 
    group_by({{ group }}) %>% 
    st_drop_geometry()
  
  # gestion des étiquettes des axes
  if(is.null((type))) {sel <- sel %>% summarise(n_PE = n()) }
  if(type == "cours") {sel <- sel %>% summarise(n_PE_cours = n()) }
  if(type == "source") {sel <- sel %>% summarise(n_PE_source = n()) }
  if(type == "nappe") {sel <- sel %>% summarise(n_PE_nappe = n()) }
  if(type == "zh") {sel <- sel %>% summarise(n_PE_zh = n()) }
  if(type == "connecte") {sel <- sel %>% summarise(n_PE_conn = n()) }
  if(type == "tbv") {sel <- sel %>% summarise(n_PE_tbv = n()) }
  
}

# --------------------------- fin mon_compte() -------------------------