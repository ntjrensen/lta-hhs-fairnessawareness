## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Include_Voorbereidingen_Verdieping.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Lectoraat Learning Technology & Analytics De Haagse Hogeschool
## Copyright 2024 De HHs
## Web Page: http://www.hhs.nl
## Contact: Theo Bakker (t.c.bakker@hhs.nl)
## Verspreiding buiten De HHs: Nee
##
## Doel: Doel
##
## Afhankelijkheden: Afhankelijkheid
##
## Datasets: Datasets
##
## Opmerkingen:
## 1) Geen.
## 2) ___
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Geschiedenis:
## 21-08-2024: TB: Aanmaak bestand
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. LAAD LTABASE PACKAGE ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.1 Standaard packages die direct nodig zijn ####

## Installeer here, cli en icecream indien nodig
for (i in c("here", "cli", "icecream")) {
  if(!requireNamespace(i, quietly = TRUE)) {
    install.packages(i)
  }
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.2 ltabase package in (installeer indien nodig) ####

source("99. Functies & Libraries/Inladen_ltabase.R")
source("99. Functies & Libraries/Fairness_functies.R")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.3 Default datasets: dfOpleidigen, sectors, studytypes, studyforms ####

## Laad de default datasets: dfOpleidigen, sectors, studytypes, studyforms
ltabase::load_lta_datasets(message = TRUE)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.4 Laad extra bibliotheken ####

## Laad extra bibliotheken
library(conflicted)   # om conflicten op te lossen
library(tidymodels)   # voor machine learning

library(vip)          # voor variable importance plots
library(forcats)      # om factor variabelen te bewerken
library(performance)  # voor performance metingen op lr modellen
#library(dlookr)      # om data te inspecteren > geeft conflicten vanwege showtext_auto()
library(gtsummary)    # voor beschrijvende summary tabellen
library(flextable)    # voor flextables
library(officer)      # voor opmaak in tabellen
library(gt)           # voor tabellen
library(gtExtras)     # voor sparklines
library(cli)          # voor cli teksten
library(glue)         # voor string interpolatie
library(probably)     # voor probabilistische modellen
library(discrim)      # discriminant analysis
library(klaR)         # voor classificatie en visualisatie
library(betacal)      # voor beta calibration

library(doParallel)   # voor parallel processing
library(DALEX)        # voor explainable AI
library(DALEXtra)     # voor explainable AI
library(lobstr)       # voor het meten van objecten
library(butcher)      # voor het verkleinen van modellen
library(iBreakDown)   # voor het uitleggen van modellen
library(ggtext)       # voor het maken van opmaak in titels

library(showtext)     # voor het instellen van lettertypes
library(ggplot2)      # voor het maken van plots
library(cvms)         # voor confusion matrices
library(ggimage)      # voor confusion matrices
library(rsvg)         # voor confusion matrices
library(ggnewscale)   # voor confusion matrices

library(ggpubr)       # voor het bewaren van plots
library(bbplot)       # voor het bewaren van plots
library(grid)         # voor het bewaren van plots

library(gridGraphics) # voor het bewaren van plots
library(extrafont)    # voor het bewaren van plots
library(sysfonts)     # voor fonts

library(fairmodels)   # voor fairness in modellen

library(fs)          # voor file system functies

library(quartostamp)  # voor extra quarto add-in functionaliteit

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.5 Fonts ####

extrafont::loadfonts(quiet = TRUE)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.6 Laad extra functies ####

source("99. Functies & Libraries/Rapport_functies.R")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.7 Kleuren ####

source("01. Includes/Include_Colors.R")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.8 Bepaal de voorkeur voor de thema's ####

Set_LTA_Theme()

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.9 Laad extra functies ####

source("99. Functies & Libraries/Rapport_functies.R")

## Bepaal de volgorde van een aantal levels
Get_Levels()

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.10 Conflicts ####

conflicts_prefer(dplyr::select)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.11 Netwerkpaden ####

# Als LTA_ROOT, LTA_DATA of LTA_BOARD niet bestaan, dan wordt de omgeving opnieuw ingesteld
ltabase::set_lta_sys_env()

## Bepaal de netwerkdirectory
Network_directory <- ltabase::get_lta_network_directory()
