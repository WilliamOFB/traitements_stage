# Packages ----
library(hubeau)

load("processed_data/stations_onde.RData")

ecoulement_onde <- get_ecoulement_observations() %>% 
  filter(code_departement %in% c("22","35","29","56","44","53","85","49","72"))

ecoulement_35 <- get_ecoulement_observations() %>% 
  filter(libelle_commune = "SAINT-GILLES",
         date_maj_stations == "2022-01-07") %>% 
  filter(date_maj_stations == "2022-01-07") %>% 
  filter(code_departement %in% c("22","35","29","56"))

         