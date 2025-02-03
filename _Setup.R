# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# _Setup.R ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R code of the Learning Technology & Analytics Research Group of THUAS
# Copyright 2025 THUAS
# Web Page: http://www.thuas.com
# Contact: Theo Bakker (t.c.bakker@hhs.nl)
# Distribution outside THUAS: No
#
# Purpose: A setup file to perform standard funciontality of the project.
#
# Dependencies: None
#
# Datasets: None
#
# Remarks:
# 1) None.
# 2) ___
#
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Version history:
# 01-05-2024: TB: File creation
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0. ON START ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0.1 Reset Setup ####

# Set this variable to T to reset this page or restart the session
bReset_Setup <- F

# The setup has not yet been performed, please perform it unless there is a reset
if(!exists("bSetup_executed") || bReset_Setup){
  bSetup_executed <- F
}

# Based on the setup, do or do not run the rest of this document
if(bSetup_executed == F) {

  # . ####
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1. PACKAGE & FUNCTIONS ####
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.1 Standard packages needed immediately ####
  
  # Install here, cli and icecream if necessary
  for (i in c("here", "cli", "icecream")) {
    if(!requireNamespace(i, quietly = TRUE)) {
      install.packages(i)
    }
  }
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.2 ltabase package (install if necessary) ####
  
  source("R/functions/load.ltabase.R")
  source("R/functions/fairness.helpers.R")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.3 Default datasets: dfOpleidigen, sectors, studytypes, studyforms ####
  
  # Load the default datasets: dfOpleidigen, sectors, studytypes, studyforms
  ltabase::load_lta_datasets(message = TRUE)
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.4 Load additional libraries ####
  
  # Laad extra bibliotheken
  library(conflicted)   # to solve conflicts
  library(rio)          # for reading files
  library(doParallel)   # for parallel processing
  library(fs)           # for file system functions
  
  #library(dlookr)      # to inspect data > causes conflicts because of showtext_auto()
  library(gtsummary)    # for descriptive summary tables
  library(flextable)    # for flextables
  library(officer)      # for formatting in tables
  library(gt)           # for tables
  library(gtExtras)     # for sparklines
  library(cli)          # for cli texts
  library(glue)         # for string interpolation
  
  library(tidymodels)   # for machine learning
  library(forcats)      # to edit factor variables
  library(performance)  # for performance measurements on lr models
  library(vip)          # for variable importance plots
  library(probably)     # for probabilistic models
  library(discrim)      # discriminant analysis
  library(klaR)         # for classification and visualization
  library(betacal)      # for beta calibration
  library(DALEX)        # for explainable AI
  library(DALEXtra)     # for explainable AI
  library(lobstr)       # for measuring objects
  library(butcher)      # for shrinking models
  library(iBreakDown)   # for explaining models
  library(ingredients)  # for feature importance
  library(fairmodels)   # for fairness in models
  
  library(ggtext)       # for creating formatting in titles
  library(showtext)     # for setting fonts
  library(ggplot2)      # for creating plots
  library(ggpubr)       # for saving plots
  library(bbplot)       # for saving plots
  library(grid)         # for saving plots
  library(gridGraphics) # for saving plots
  library(extrafont)    # for saving plots
  library(sysfonts)     # for fonts
  library(systemfonts)  # for fonts
  
  library(cvms)         # for confusion matrices
  library(ggimage)      # for confusion matrices
  library(rsvg)         # for confusion matrices
  library(ggnewscale)   # for confusion matrices
  
  library(quartostamp)  # for additional quarto add-in functionality
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.5 Fonts ####
  
  extrafont::loadfonts(quiet = TRUE)
  systemfonts::get_from_google_fonts("GT Walsheim")
  systemfonts::get_from_google_fonts("Aktiv Grotesk")

  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.6 Load additional features ####

  source("R/functions/report.helpers.R")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.7 Colors ####
  
  source("R/scripts/colors.R")

  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.8 Determine preferred themes ####

  Set_LTA_Theme()

  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.9 Tidymodels preferences ####

  # When conflicts arise, give preference to the tidymodels package
  tidymodels_prefer(quiet = TRUE)

  # . ####
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2. CONFIG ####
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.1 Network paths ####

  # If LTA_ROOT, LTA_DATA or LTA_BOARD do not exist, the environment is reset
  ltabase::set_lta_sys_env()

  # Define the network directory
  Network_directory <- ltabase::get_lta_network_directory()
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.2 Debug ####

  # Set debug options: icecream package settings
  ltabase::set_icecream_options()
  icecream::ic_disable()

  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.3 Gtsummary ####

  # Define the default settings of gtsummary
  list("style_number-arg:big.mark" = ".",
       "style_number-arg:decimal.mark" = ",") |>
    set_gtsummary_theme()
  invisible(theme_gtsummary_compact())

  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.4  Parameters ####

  if(!exists("params")) {
    params <- list()
    params$succes                   <- "Retentie na 1 jaar"
    params$model                    <- "Retentie na 1 jaar"
    params$propedeusediploma        <- "Nvt"
    params$use_synthetic_data       <- TRUE
    params$recreateplots            <- FALSE
    params$faculteit                <- "ITD"
    params$opleidingsnaam           <- "B Communication and Multimedia Design"
    params$opleiding                <- "CMD"
    params$opleidingsvorm           <- "voltijd"
    params$opleidingsvorm_afkorting <- "VT"
    params$selectie                 <- FALSE
  }
  
  sSucces_model      <- params$succes
  sPropedeusediploma <- params$propedeusediploma
  sSucces_label      <- "Kans op retentie"
  
  sSucces_model_text <- Get_Succes_Model_Text(sPropedeusediploma, sSucces_model)

  # Create the variables for the current study programme based on the programme name and type of education
  current_opleiding <- Get_Current_Opleiding(
    opleiding = params$opleiding,
    opleidingsvorm = params$opleidingsvorm_afkorting
  )

  # Based on this, determine derived variables
  Set_Current_Opleiding_Vars(current_opleiding, debug = T)

  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.5 Enrollments  ####

  dfOpleiding_inschrijvingen_base <- get_lta_studyprogram_enrollments_pin(
    board = "HHs/Inschrijvingen",
    faculty = faculteit,
    studyprogram = opleidingsnaam_huidig,
    studytrack = opleiding,
    studyform = toupper(opleidingsvorm),
    range = "eerstejaars")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.6 Settings ####
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.6.1 Metadata ####

  lResearch_settings <- list()
  lResearch_settings[["sResearch_path"]] <- "Kansengelijkheid"
  lResearch_settings[["sDataset"]]       <- Get_sDataset(dfOpleiding_inschrijvingen_base)
  lResearch_settings[["sOpleiding"]]     <- Get_sOpleiding()

  lMetadata <- Get_Metadata()

  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.6.2 Caption ####

  sCaption <- Get_sCaption()

  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.6.3 Plot height and width ####

  # Determine the height and width of images
  nPlotWidth  <- 640
  nPlotHeight <- 550
  
  # . ####
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3. CONTENT ####
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3.1 Names of the study programme and type of education ####
  
  # Determine the long name of the type of education and faculty
  opleidingsvorm_lang <- Get_Opleidingsvorm_Lang(params$opleidingsvorm_afkorting)
  faculteit_lang      <- Get_Faculteitsnaam_Lang(params$faculteit)
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3.2 Variables and levels ####
  
  dfVariables    <- Get_dfVariables()
  dfLevels       <- Get_dfLevels()
  lLevels        <- Get_lLevels(dfLevels)
  lLevels_formal <- Get_lLevels(dfLevels, formal = TRUE)
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3.3 Sensitive variables and labels ####
  
  lSentitive_formal_variables <- Get_lSensitive(dfVariables, "VAR_Formal_variable")
  lSentitive_variables        <- Get_lSensitive(dfVariables, "VAR_Simple_variable")
  lSensitive_labels           <- Get_lSensitive(dfVariables, "VAR_Variable_label")
  lSensitive_levels_breakdown <- Get_lSensitive_Levels_Breakdown(dfLevels, lSentitive_formal_variables)
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3.4 Paths for data, last fits and model results ####
  
  # Define the paths
  sData_outputpath            <- Get_Model_Outputpath(mode = "data")
  sFittedmodels_outputpath    <- Get_Model_Outputpath(mode = "last-fits")
  sModelresults_outputpath    <- Get_Model_Outputpath(mode = "modelresults")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3.5 Data for training, last fits and results ####
  
  # Load data for training: data, last fits and model results
  # Adjust the Retention variable to numeric (0/1),
  dfOpleiding_inschrijvingen <- rio::import(sData_outputpath, trust = TRUE) |> 
    mutate(across(all_of(names(lLevels)), ~ factor(.x, 
                                                   levels = lLevels[[cur_column()]]))) |> 
    mutate(Retentie = as.numeric(Retentie) - 1)
  
  lLast_fits                 <- rio::import(sFittedmodels_outputpath, trust = TRUE)
  dfModel_results            <- rio::import(sModelresults_outputpath, trust = TRUE)
  # 
  # # Adjust the Retention variable to numeric (0/1), 
  # # so it can be used for an explainer
  # dfOpleiding_inschrijvingen$Retentie <- as.numeric(dfOpleiding_inschrijvingen$Retentie) - 1
  
}

