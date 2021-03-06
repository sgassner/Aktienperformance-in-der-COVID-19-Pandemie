# Notwendige Packages laden
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)

# NAICS SIC Crosswalk laden
crosswalk <- read_xlsx("SIC_NAICS_CROSSWALK.xlsx")

# Ratios laden
ratios <- read_xlsx("RATIOS_SIC.xlsx")

# Ratios zum Datensatz hinzufügen
library(plyr)
crosswalk <- join(crosswalk, ratios, by="SIC_L1")
detach("package:plyr", unload=TRUE)

# Data-Cleaning
crosswalk <- crosswalk[complete.cases(crosswalk), ]

# Durchschnittliche Ratios pro NAICS Gruppe
mean_ratios <- crosswalk %>% group_by(NAICS_L1) %>% 
  summarize(mean_de_ratio = mean(de_ratio),
            mean_q_ratio = mean(q_ratio))

# Excel-Datei erstellen
write_xlsx(mean_ratios, "mean_ratios_naics.xlsx")
