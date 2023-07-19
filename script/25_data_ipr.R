library(aspe)
library(tidyverse)

load(file = "raw_data/tables_sauf_mei_2023_03_08_15_08_27.RData")

passerelle <- mef_creer_passerelle() %>% 
  select(sta_id:ope_id) %>% 
  distinct()

ipr <- passerelle %>% 
  mef_ajouter_ipr() %>% 
  mef_ajouter_dept() %>% 
  mef_ajouter_ope_date() %>% 
  filter(dept %in% c("22","35","29","56","44","53","85","49","72")&
         !is.na(ipr),
         annee >= 2018)
