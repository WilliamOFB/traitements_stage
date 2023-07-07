# Packages ----
library(sf)
library(tidyverse)
library(dplyr)
library(readxl)
library(openxlsx2)

# Imports ----
plando_uniques <- read_sf("../../SIG/2-Exploitation/Plando/plando_prelev_uniques.gpkg")

# Si besoin...
plando_uniques <- plando_uniques %>% 
  distinct(gid_plando,
           surface_plando,
           .keep_all = TRUE)

# Sinon...
plando_select <- plando_uniques %>% 
  filter(Retirer == 0)

massdo <- read_sf("../../SIG/2-Exploitation/Masses_eau/ME_toutes/massdo_toutes_dens_surf.gpkg")

massdo <- massdo %>% 
  mutate(dens_num_PE500_cours = (n_PE_cours_sup500/ (surfacebvs/1000000)),
         dens_num_PE500_source = (n_PE_source_sup500/ (surfacebvs/1000000)),
         dens_num_PE500_nappe = (n_PE_nappe_sup500/ (surfacebvs/1000000)),
         dens_num_PE500_zh = (n_PE_zh_sup500/ (surfacebvs/1000000)),
         dens_num_PE500_conn = (n_PE_conn_sup500/ (surfacebvs/1000000)))

stat_dens_num <- read_xlsx("processed_data/Stats/stats_dens_parME_good.xlsx")

# ME - Statistiques récapitulatives d'indicateurs simples ----
massdo_bret <- massdo %>% 
  filter(IN_Bret == 1)

massdo_pdl <- massdo %>% 
  filter(IN_PdL == 1)

ptage_PE_zh <- data.frame(col = nb_me_plando$Ptage_PEzh_full)

stats_indic_ME <- ptage_PE_zh %>% # modifier selon dataframe créé précédemment
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
print(stats_indic_ME) # modifier selon dataframe créé précédemment

## Sauvegarde ----
stats_dens_num_ME <- data.frame(nom_stat = names(stats_indic_ME),
                                 val_dens_num = stats_indic_ME)

stats_ptage_pe_zh <- data.frame(nom_stat = names(stats_indic_ME),
                             val_ptage_zh = stats_indic_ME)

stats_dens_num_pdl <- data.frame(nom_stat = names(stats_indic_ME),
                              val_dens_num_pdl = stats_indic_ME)

stats_dens_num_nappe <- data.frame(nom_stat = names(stats_indic_ME),
                             val_dens_num_nappe = stats_indic_ME)

stats_dens_num_zh <- data.frame(nom_stat = names(stats_indic_ME),
                              val_dens_num_zh = stats_indic_ME)

stats_dens_num_conn <- data.frame(nom_stat = names(stats_indic_ME),
                                val_dens_num_conn = stats_indic_ME)

stats_dens_surf <- data.frame(nom_stat = names(stats_indic_ME),
                              val_dens_surf = stats_indic_ME)

liste_df <- list(stats_dens_num_ME, # à modifier en fonction des df à lier
                 stats_dens_num_cours,
                 stats_dens_num_source,
                 stats_dens_num_nappe,
                 stats_dens_num_zh,
                 stats_dens_num_conn,
                 stats_dens_surf)

# Utiliser CECI...
stat_total_ME <- liste_df %>% 
  reduce(left_join,
         by = "nom_stat")

# ou CELA
stat_total_ME <- stat_dens_num %>% # remplacer df attribué par le df auquel la jonction doit être faite
  left_join(stats_ptage_pe_zh)

write_xlsx(stat_total_ME,
          file = "processed_data/Stats/stats_dens_parME_good.xlsx")

save(stat_total_ME,
     file = "processed_data/Stats/stats_parME_good.RData")

# PE - Statistiques récapitulatives d'indicateurs simples ----
plando_select_30000 <- plando_select %>% 
  filter(surface_plando >= 30000)

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
stats_dist_PE1000_CE <- data.frame(nom_stat = names(stats_indic_PE),
                                val_dist_PE1000 = stats_indic_PE)

stats_dist_PE30000_CE <- data.frame(nom_stat = names(stats_indic_PE),
                                   val_dist_PE30000 = stats_indic_PE)


liste_df <- list(stats_dist_PE_CE, # à modifier en fonction des df à lier
                 stats_dist_PE1000_CE,
                 stats_dist_PE30000_CE)

# Utiliser CECI...
stat_total_PE <- liste_df %>% 
  reduce(left_join,
         by = "nom_stat")

# ou CELA
stat_total_PE <- stat_dens_num %>% # remplacer df attribué par le df auquel la jonction doit être faite
  left_join(stats_dens_num_500)

write_xlsx(stat_total_PE,
          file = "processed_data/Stats/stats_parPE_good.xlsx")

save(stat_total_PE,
     file = "processed_data/Stats/stats_parPE_good.RData")
