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
  ## 1.9 Tidymodels ####

  ## Geef de voorkeur bij conflicten aan het tidymodels package
  tidymodels_prefer(quiet = TRUE)

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

  ## Bepaal de standaard instellingen van gtsummary
  list("style_number-arg:big.mark" = ".",
       "style_number-arg:decimal.mark" = ",") |>
    set_gtsummary_theme()
  invisible(theme_gtsummary_compact())

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 2.4 Levels van variabelen ####

  ## Bepaal de volgorde van een aantal levels
  Get_Levels()

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 2.5  Config ####

  sSucces_model <- params$succes
  sPropedeusediploma <- params$propedeusediploma

  sSucces_model_text <- Get_Succes_Model_Text(sPropedeusediploma, sSucces_model)

  ## Maak de variabelen voor de huidige opleiding op basis van de opleidingsnaam en opleidingsvorm
  current_opleiding <- Get_Current_Opleiding(
    opleiding = params$opleiding,
    opleidingsvorm = params$opleidingsvorm_afkorting
  )

  ## Bepaal op basis hiervan afgeleide variabelen
  Set_Current_Opleiding_Vars(current_opleiding, debug = T)

  ## . ####
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 3. BASISQUERY ####
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 3.1 Inschrijvingen  ####

  dfOpleiding_inschrijvingen_base <- get_lta_studyprogram_enrollments_pin(
    board = "HHs/Inschrijvingen",
    faculty = faculteit,
    studyprogram = opleidingsnaam_huidig,
    studytrack = opleiding,
    studyform = toupper(opleidingsvorm),
    range = "eerstejaars")
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 3.2 Settings ####

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 3.2.1 Metadata ####

  lResearch_settings <- list()
  lResearch_settings[["sResearch_path"]] <- "Kansengelijkheid"
  lResearch_settings[["sDataset"]]       <- Get_sDataset(dfOpleiding_inschrijvingen_base)
  lResearch_settings[["sOpleiding"]]     <- Get_sOpleiding()

  lMetadata <- Get_Metadata()

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 3.2.2 Caption ####

  sCaption <- Get_sCaption()

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 3.2.3 Plots ####

  ## Bepaal de hoogte en breedte van afbeeldingen
  nPlotWidth  <- 640
  nPlotHeight <- 550
  
}
