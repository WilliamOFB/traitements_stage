### Statistiques basiques concernant les indicateurs ###
# Packages ----
library(sf)
library(tidyverse)
library(dplyr)
library(readxl)
library(openxlsx2)

# Imports ----
plando <- read_sf("chemin/vers/ma/couche/plando.gpkg") %>% # ou .shp ou en .RData 
  filter(is.na(ERU),
         is.na(Orage),
         is.na(Ecoul_nat),
         is.na(Transition))

massdo <- read_sf("chemin/vers/ma/couche/massdo.gpkg") %>% # ou .shp ou en .RData
  st_transform(2154)

# Si fichier de stats déjà existant...
stat_existant <- read_xlsx("chemin/vers/mon/fichier/stats.xlsx")

# ME - Statistiques récapitulatives d'indicateurs simples ----
massdo_bret <- massdo %>% 
  filter(In_Bret == 1)

massdo_pdl <- massdo %>% 
  filter(In_PdL == 1)

diff_temp <- data.frame(col = massdo$Ta_Tw) # à adapter selon l'indicateur voulu (ici, exemple T°)

stats_indic_ME <- diff_temp %>% # modifier selon dataframe créé précédemment
  summarise(mean = mean(col, na.rm = TRUE),
            median = median(col, na.rm = TRUE),
            Q25 = quantile (col, probs = 0.25, na.rm = TRUE),
            Q75 = quantile (col, probs = 0.75, na.rm = TRUE),
            percent_90 = quantile (col, probs = 0.9, na.rm = TRUE),
            ecart_type = sd(col, na.rm = TRUE),
            min = min(col, na.rm = TRUE),
            max = max(col, na.rm = TRUE)) %>% 
#            mode = as.numeric(names(table(col))[table(col) == max(table(col))])) %>%
  unlist()

## Vérification ----
print(stats_indic_ME)

## Sauvegarde ----
# Possibilité de créer autant de stats basique que l'on veut
stats_diff_temp <- data.frame(nom_stat = names(stats_indic_ME),
                              val_diff_temp = stats_indic_ME)

stats_dens_num_cours <- data.frame(nom_stat = names(stats_indic_ME),
                              val_dens_num_cours = stats_indic_ME)

stats_dens_num_nappe <- data.frame(nom_stat = names(stats_indic_ME),
                             val_dens_num_nappe = stats_indic_ME) 

liste_df <- list(stats_diff_temp, # à modifier en fonction des df à lier
                 stats_dens_num_cours,
                 stats_dens_num_nappe,
#                 stats_existant
                 )

# Utiliser CECI...
stat_total_ME <- liste_df %>% 
  reduce(left_join,
         by = "nom_stat")

# ou CELA
stat_total_ME <- stat_dens_num_cours %>% # remplacer df attribué par le df auquel la jonction doit être faite
  left_join(stats_diff_temp)

write_xlsx(stat_total_ME,
          file = "chemin/vers/mon/fichier/stats_completes_massdo.xlsx")

save(stat_total_ME,
     file = "chemin/vers/mon/fichier/stats_completes_massdo.RData")

# PE - Statistiques récapitulatives d'indicateurs simples ----
plando_select_30000 <- plando_select %>% 
  filter(surface_plando >= 30000)

# à adapter selon l'indicateur voulu (ici, plans d'eau > 3 ha)
dist_PE30000_CE <- data.frame(col = plando_select_30000$distance)

stats_indic_PE <- dist_PE30000_CE %>% # modifier selon dataframe créé précédemment
  summarise(mean = mean(col),
            median = median(col),
            Q25 = quantile (col, probs = 0.25),
            Q75 = quantile (col, probs = 0.75),
            percent_90 = quantile (col, probs = 0.9),
            ecart_type = sd(col),
            max = max(col),
            min = min(col)) %>% 
  #            mode = as.numeric(names(table(col))[table(col) == max(table(col))])) %>%
  unlist()

## Vérification ----
print(stats_indic_PE) # modifier selon dataframe créé précédemment

## Sauvegarde ----
stats_dist_PE30000_CE <- data.frame(nom_stat = names(stats_indic_PE),
                                    val_dist_PE30000 = stats_indic_PE)

stats_dist_PE1000_CE <- data.frame(nom_stat = names(stats_indic_PE),
                                val_dist_PE1000 = stats_indic_PE)

liste_df <- list(stats_dist_PE1000_CE, # à modifier en fonction des df à lier
                 stats_dist_PE30000_CE)

# Utiliser CECI...
stat_total_PE <- liste_df %>% 
  reduce(left_join,
         by = "nom_stat")

# ou CELA
stat_total_PE <- stats_dist_PE1000_CE %>% # remplacer df attribué par le df auquel la jonction doit être faite
  left_join(stats_dist_PE30000_CE)

write_xlsx(stat_total_PE,
           file = "chemin/vers/mon/fichier/stats_completes_plando.xlsx")

save(stat_total_PE,
     file = "chemin/vers/mon/fichier/stats_completes_plando.RData")
