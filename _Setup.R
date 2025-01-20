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
## 0. ON START ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 0.1 Reset Setup ####

# Set this variable to T to reset this page or restart the session
bReset_Setup <- F

## The setup has not yet been performed, please perform it unless there is a reset
if(!exists("bSetup_executed") || bReset_Setup){
  bSetup_executed <- F
}

## Based on the setup, do or do not run the rest of this document
if(bSetup_executed == F) {

  ## . ####
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1. LOAD LTABASE PACKAGE ####
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1.1 Standard packages needed immediately ####
  
  ## Install here, cli and icecream if necessary
  for (i in c("here", "cli", "icecream")) {
    if(!requireNamespace(i, quietly = TRUE)) {
      install.packages(i)
    }
  }
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1.2 ltabase package (install if necessary) ####
  
  source("R/functions/Inladen_ltabase.R")
  source("R/functions/fairness.helpers.R")
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1.3 Default datasets: dfOpleidigen, sectors, studytypes, studyforms ####
  
  ## Load the default datasets: dfOpleidigen, sectors, studytypes, studyforms
  ltabase::load_lta_datasets(message = TRUE)
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1.4 Laad extra bibliotheken ####
  
  ## Laad extra bibliotheken
  library(conflicted)   # to solve conflicts
  library(tidymodels)   # for machine learning
  library(rio)          # for reading files
  
  library(vip)          # for variable importance plots
  library(forcats)      # to edit factor variables
  library(performance)  # for performance measurements on lr models
  #library(dlookr)      # to inspect data > causes conflicts because of showtext_auto()
  library(gtsummary)    # for descriptive summary tables
  library(flextable)    # for flextables
  library(officer)      # for formatting in tables
  library(gt)           # for tables
  library(gtExtras)     # for sparklines
  library(cli)          # for cli texts
  library(glue)         # for string interpolation
  library(probably)     # for probabilistic models
  library(discrim)      # discriminant analysis
  library(klaR)         # for classification and visualization
  library(betacal)      # for beta calibration
  
  library(doParallel)   # for parallel processing
  library(DALEX)        # for explainable AI
  library(DALEXtra)     # for explainable AI
  library(lobstr)       # for measuring objects
  library(butcher)      # for shrinking models
  library(iBreakDown)   # for explaining models
  library(ggtext)       # for creating formatting in titles
  
  library(showtext)     # for setting fonts
  library(ggplot2)      # for creating plots
  library(cvms)         # for confusion matrices
  library(ggimage)      # for confusion matrices
  library(rsvg)         # for confusion matrices
  library(ggnewscale)   # for confusion matrices
  
  library(ggpubr)       # for saving plots
  library(bbplot)       # for saving plots
  library(grid)         # for saving plots
  
  library(gridGraphics) # for saving plots
  library(extrafont)    # for saving plots
  library(sysfonts)     # for fonts
  
  library(fairmodels)   # for fairness in models
  
  library(fs)           # for file system functions
  
  library(quartostamp)  # for additional quarto add-in functionality
  
  ## Added from pages
  library(ingredients)
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1.5 Fonts ####
  
  extrafont::loadfonts(quiet = TRUE)

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1.6 Load additional features ####

  source("R/functions/report.helpers.R")
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1.7 Colors ####
  
  source("R/scripts/Include_Colors.R")

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1.8 Determine preferred themes ####

  Set_LTA_Theme()

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1.9 Tidymodels ####

  ## Geef de voorkeur bij conflicten aan het tidymodels package
  tidymodels_prefer(quiet = TRUE)

  ## . ####
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 2. BASIC VARIABLES / PATHS ####
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 2.1 Network paths ####

  # If LTA_ROOT, LTA_DATA or LTA_BOARD do not exist, the environment is reset
  ltabase::set_lta_sys_env()

  ## Define the network directory
  Network_directory <- ltabase::get_lta_network_directory()

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 2.2 Debug settings ####

  ## Set debug options: icecream package settings
  ltabase::set_icecream_options()
  icecream::ic_disable()

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 2.3 Gtsummary settings ####

  ## Define the default settings of gtsummary
  list("style_number-arg:big.mark" = ".",
       "style_number-arg:decimal.mark" = ",") |>
    set_gtsummary_theme()
  invisible(theme_gtsummary_compact())

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 2.4 Levels of variables ####

  ## Determine the order of some levels
  Get_Levels()

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 2.5  Config ####

  sSucces_model      <- params$succes
  sPropedeusediploma <- params$propedeusediploma

  sSucces_model_text <- Get_Succes_Model_Text(sPropedeusediploma, sSucces_model)

  ## Create the variables for the current study programme based on the programme name and type of education
  current_opleiding <- Get_Current_Opleiding(
    opleiding = params$opleiding,
    opleidingsvorm = params$opleidingsvorm_afkorting
  )

  ## Based on this, determine derived variables
  Set_Current_Opleiding_Vars(current_opleiding, debug = T)

  ## . ####
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 3. BASIC QUERY ####
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 3.1 Enrollments  ####

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

  ## Determine the height and width of images
  nPlotWidth  <- 640
  nPlotHeight <- 550

}
