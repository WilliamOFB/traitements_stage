### Statistiques descriptives par indicateurs ###
# Packages ----
library(tidyverse)
library(ggplot2)
library(corrplot)
library(dplyr)
library(sf)
library(GGally)
library(broom)
library(ggpubr)
library(rstatix)

# Imports ----
massdo <- read_sf("chemin/vers/ma/couche/massdo.gpkg") %>% # ou .shp ou en .RData
  mutate(lineaire_tbv = lineaire_R1 + lineaire_R2)

ipr_plando <- read_sf("chemin/vers/ma/couche/bv_ipr.gpkg") %>% # ou .shp ou en .RData
  mutate(Ta_Tw = MTa30J - MTw30J_MUL,
         ratio_Q5QA = Q5MOY_MN / QAMOY_MN,
         lineaire_mean = lineaire_total / n_PE,
         lineaire_mean_1.2km = lineaire_1.2km / n_PE_1.2km) %>% 
  st_drop_geometry()

# Traitements ----
## Pivoter les données ----
### Données quantitatives massdo bien réparties ----
massdo_long <- massdo %>%
  select(cdbvspemdo,
         Gravelius,
         dens_num_PE:dens_surf_PE,
         lineaire_mean,
         dens_num_PEcours:dens_num_PEzh,
         Ratio_Q5QA,
         Ta_Tw,
         Ptage_PE_tbv,
         Pourc_prelPE_2013,
         Ptage_PEzh_full,
         lineaire_tbv) %>% 
  pivot_longer(cols = Gravelius:lineaire_tbv,
               names_to = "variable", values_to = "valeur")

massdo_long$variable <- fct_relevel(massdo_long$variable, "ptag_prelPE_2013", after = 11)

### Données quantitatives massdo pour log10 ----
debit_massdo_long <- massdo %>% 
  select(cdbvspemdo,
         Q5MOY_MN_m,
         QAMOY_MN_m,
         tx_etagement:dens_ouvrage) %>% 
  pivot_longer(cols = Q5MOY_MN_m:dens_ouvrage,
               names_to = "variable", values_to = "valeur")

debit_massdo_long$variable <- fct_relevel(debit_massdo_long$variable, "Q5MOY_MN_m", "QAMOY_MN_m", after = 3)

### Données quantitatives ipr bien réparties ----
data_ipr_long <- ipr_plando %>%
  select(id,
         dens_num_tot:dens_num_conn,
         dens_surf,
         Gravelius:lineaire_mean) %>% 
  pivot_longer(cols = dens_num_tot:lineaire_mean,
               names_to = "variable", values_to = "valeur")

data_ipr_long$variable <- fct_relevel(data_ipr_long$variable, "dens_num_conn", after = 5)
data_ipr_long$variable <- fct_relevel(data_ipr_long$variable, "dens_num_tot")

### Données quantitatives ipr pour log10 ----
debit_ipr_long <- ipr_plando %>% 
  select(id,
         join_Q5MOY_MN,
         join_QAMOY_MN) %>% 
  pivot_longer(cols = join_Q5MOY_MN:join_QAMOY_MN,
               names_to = "variable", values_to = "valeur")

### Données quantitatives métriques et note ipr ----
metriq_ipr_long <- ipr_plando %>% 
  select(id,
         ner:dti,
         ipr) %>% 
  pivot_longer(cols = ner:ipr,
               names_to = "variable", values_to = "valeur")

metriq_ipr_long$variable <- fct_relevel(metriq_ipr_long$variable, "ipr")

## Représenter les données en densité ----
# Lancer au préalable le script "..._fonctions_stats.R"
### Indicateurs massdo ----
var_quanti(data = massdo_long,
           x = valeur,
           x_lab = "Valeur",
           y_lab = "Densité")

var_quanti(data = debit_massdo_long,
           x = valeur,
           x_lab = "Valeur",
           y_lab = "Densité",
           log = TRUE)

### Indicateurs ipr ----
var_quanti(data = data_ipr_long,
           x = valeur,
           x_lab = "Valeur",
           y_lab = "Densité")

var_quanti(data = debit_ipr_long,
           x = valeur,
           x_lab = "Valeur",
           y_lab = "Densité",
           log = TRUE)

### Métriques ipr ----
var_quanti(data = metriq_ipr_long,
           x = valeur,
           x_lab = "Valeur",
           y_lab = "Densité")

## Représenter les données quali en diagramme ----
# Lancer au préalable la script "..._fonctions_stats.R"
### Données massdo ----
var_quali(data = massdo,
          x = top_geol,
          x_lab = "Type de géologie",
          y_lab = "Occurence",
          ordre_x = "frequence")

var_quali(data = massdo,
          x = cdnaturema,
          x_lab = "Nature de la masses d'eau",
          y_lab = "Occurence")

var_quali(data = massdo,
          x = c_struct_sub,
          x_lab = "Pression et altération de la structure et du substrat du lit des cours d'eau",
          y_lab = "Occurence")

var_quali(data = massdo,
          x = c_struct_riv,
          x_lab = "Pression sur la structure de la rive des cours d'eau",
          y_lab = "Occurence")

### Données ipr ----
var_quali(data = ipr_plando,
          x = rang,
          x_lab = "Rang de Strahler à la station IPR",
          y_lab = "Occurence")

## Matrices de corrélation ----
### Sélection des variables massdo ----
massdo_corr <- massdo %>% 
  select(cdbvspemdo,
         Gravelius,
         dens_num_PE,
         dens_num_PEcours,
         dens_surf_PE,
         Ptage_PE_tbv,
         Ptage_PEzh_full,
         lineaire_mean,
         lineaire_tbv,
         Ratio_Q5QA,
         Q5MOY_MN_m,
         Ta_Tw,
         tx_etagement,
         dens_ouvrage,
         Pourc_prelPE_2013,
         cdnaturema,
         c_struct_sub,
         c_struct_riv,
         top_geol) %>% 
  st_drop_geometry() %>% 
  mutate(top_geol = match(top_geol,
                          sort(unique(top_geol))),
         cdnaturema = as.numeric(cdnaturema)) %>% 
  column_to_rownames("cdbvspemdo")

cor_massdo = cor(massdo_corr,
               use = "pairwise.complete.obs")

corrplot.mixed(cor_massdo,
               tl.col = "#292e2b",
               tl.pos = "lt")

### Sélection des variables ipr ----
ipr_corr_red <- ipr_plando %>% 
  select(#id,
         dens_num_tot:dens_num_cours,
         dens_surf,
         n_PE_1.2km,
         lineaire_1.2km,
         lineaire_total,
         lineaire_mean,
         Gravelius,
         ratio_Q5QA,
         join_Q5MOY_MN,
         Ta_Tw,
         rang,
         ipr,
         ner:dti) %>% 
  st_drop_geometry()# %>% 
#  column_to_rownames("id")

cor_ipr_red = cor(ipr_corr_red,
               use = "pairwise.complete.obs")

corrplot.mixed(cor_ipr_red,
               tl.col = "#292e2b",
               tl.pos = "lt")

## Analyses précises entre indicateurs ----
# Déclinable pour tous les indicateurs voulus
### Régressions linéaires et significativités ----
tidy(lm(Ratio_Q5QA ~ ., massdo_corr),
     conf.int = TRUE)

glance(lm(Ratio_Q5QA ~ ., massdo_corr)) %>% 
  select(c("r.squared","adj.r.squared","statistic","p.value"))

tidy(lm(Ratio_Q5QA ~ Pourc_prelPE_2013, massdo_corr),
     conf.int = TRUE)

glance(lm(Ratio_Q5QA ~ Pourc_prelPE_2013, massdo_corr)) %>% 
  select(c("r.squared","adj.r.squared","statistic","p.value"))

tidy(lm(Ta_Tw ~ ., massdo_corr),
     conf.int = TRUE)

glance(lm(Ta_Tw ~ ., massdo_corr)) %>% 
  select(c("r.squared","adj.r.squared","statistic","p.value"))

lm_ipr <- tidy(lm(ipr ~ ., ipr_corr_red),
               conf.int = TRUE)

glance(lm(ipr ~ dens_num_tot, ipr_corr_red)) %>% 
  select(c("r.squared","adj.r.squared","statistic","p.value"))

lm_ner <- tidy(lm(ner ~ ., ipr_corr_red),
               conf.int = TRUE)

glance(lm(ner ~ dens_num_tot, ipr_corr_red)) %>% 
  select(c("r.squared","adj.r.squared","statistic","p.value"))

glance(lm(ner ~ dens_surf, ipr_corr_red)) %>% 
  select(c("r.squared","adj.r.squared","statistic","p.value"))

lm_nel <- tidy(lm(nel ~ ., ipr_corr_red),
               conf.int = TRUE)

glance(lm(nel ~ dens_surf, ipr_corr_red)) %>% 
  select(c("r.squared","adj.r.squared","statistic","p.value"))

### ANOVA ----
densnum_geol <- massdo %>% 
  select(cdbvspemdo,
         dens_num_PE,
         top_geol) %>% 
  st_drop_geometry() %>% 
  column_to_rownames("cdbvspemdo")

densnum_geol %>%
  group_by(top_geol) %>%
  identify_outliers() # y a-t-il des valeurs aberrantes ?

model_densnum_geol <- lm(dens_num_PE ~ top_geol,
                         data = densnum_geol)

ggqqplot(residuals(model_densnum_geol))

shapiro_test(residuals(model_densnum_geol)) # si non normalité -> Kruskal-Wallis

### Kruskal-Wallis ----
res.kruskal <- densnum_geol %>% 
  kruskal_test(dens_num_PE ~ top_geol)

densnum_geol %>% kruskal_effsize(dens_num_PE ~ top_geol)

#### Test de Dunn et Wilcoxon ----
wilcox_dens_geol <- densnum_geol %>%
  wilcox_test(dens_num_PE ~ top_geol,
            p.adjust.method = "bonferroni") # visualisation des paires significativement différentes