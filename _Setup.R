## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## _Setup.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Lectoraat Learning Technology & Analytics De Haagse Hogeschool
## Copyright 2024 De HHs
## Web Page: http://www.hhs.nl
## Contact: Theo Bakker (t.c.bakker@hhs.nl)
## Verspreiding buiten De HHs: Nee
##
## Doel: Een setup bestand om standaard funciontaliteit van het project uit te voeren.
##
## Afhankelijkheden: Geen
##
## Datasets: Geen
##
## Opmerkingen:
## 1) Geen.
## 2) ___
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Geschiedenis:
## 01-05-2024: TB: Aanmaak bestand
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 0. ONSTART ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 0.1 Reset Setup ####

# Zet deze variabele op T om deze pagina te resetten of herstart de sessie
bReset_Setup <- F

## De setup nog niet is uitgevoerd, voer deze dan alsnog uit, tenzij er een reset is
if(!exists("bSetup_executed") || bReset_Setup){
  bSetup_executed <- F
}

## Run op basis van de setup de rest van dit document wel of niet
if(bSetup_executed == F) {

  ## . ####
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1. LAAD LTABASE PACKAGE fS ####
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
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1.3 Default datasets: dfOpleidigen, sectors, studytypes, studyforms ####
  
  ## Laad de default datasets: dfOpleidigen, sectors, studytypes, studyforms
  ltabase::load_lta_datasets(message = TRUE)
  
  ## ++++dldiklbeDAtraovvvex+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1.4 Laad extra bibliotheken
  
  ## Laad extra bibliotheken
  library(tidymodels)  # voor machine learning
  library(vip)         # voor variable importance plots
  library(forcats)     # om factor variabelen te bewerken
  library(performance) # voor performance metingen op lr modellen
  library(dlookr)      # om data te inspecteren
  library(gtsummary)   # voor beschrijvende summary tabellen
  library(cli)         # voor cli teksten
  library(glue)        # voor string interpolatie
  library(probably)    # voor probabilistische modellen
  library(discrim)     # discriminant analysis
  library(klaR)        # voor classificatie en visualisatie
  library(betacal)     # voor beta calibration
  library(doParallel)  # voor parallel processing
  library(DALEX)       # voor explainable AI
  library(DALEXtra)    # voor explainable AI
  
  ## Geef de voorkeur bij conflicten aan het tidymodels package
  tidymodels_prefer(quiet = TRUE)
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1.5 Laad extra functies
  
  source("99. Functies & Libraries/Rapport_functies.R")
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1.6 Bepaal de voorkeur voor de thema's
  
  theme_set(theme_minimal())
  
  ## . ####
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 2. BASISVARIABELEN / PADEN ####
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 2.1 Netwerkpaden ####
  
  # Als LTA_ROOT, LTA_DATA of LTA_BOARD niet bestaan, dan wordt de omgeving opnieuw ingesteld
  ltabase::set_lta_sys_env()
  
  ## Bepaal de netwerkdirectory
  Network_directory <- ltabase::get_lta_network_directory()
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 2.2 Debuginstellingen ####
  
  ## Stel de debug opties in: icecream package settings
  ltabase::set_icecream_options()
  icecream::ic_disable()
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 2.3 Gtsummary instellingen ####
  
  ## Bepaal de standaard instelingen van gtsummary
  list("style_number-arg:big.mark" = ".",
       "style_number-arg:decimal.mark" = ",") |> 
    set_gtsummary_theme()
  invisible(theme_gtsummary_compact())
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 2.4 Levels van variabelen
  
  ## Bepaal de volgorde van een aantal levels
  Get_Levels()

}
