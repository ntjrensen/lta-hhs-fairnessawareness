# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# _Setup.R ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R code of the Learning Technology & Analytics Research Group of THUAS
# Copyright 2025 THUAS
# Web Page: http://www.thuas.com
# Contact: Theo Bakker (t.c.bakker@hhs.nl)
# Distribution outside THUAS: Yes
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
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0. ON START ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0.1 Load Setup config #### 

source("_Setup_config.R")

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0.2 Set environment profile #### 

if(!is.null(rmarkdown::metadata$config$environment)) {
  sEnvironment <- rmarkdown::metadata$config$environment
} else {
  sEnvironment <- "ceda"
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0.3 Reset Setup ####

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
  # 1.2 Base and fairness functions ####
  
  source("R/functions/base.helpers.R")
  
  if(sEnvironment == "ceda") {
    source("R/functions/base.helpers.R")
  } else {
    source("R/functions/load.ltabase.R")
  }
  source("R/functions/fairness.helpers.R")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.3 Default datasets ####
  
  # Load the default datasets: dfOpleidigen, sectors, studytypes, studyforms
  if(sEnvironment == "ceda") {
    Load_Datasets(message = TRUE)
  } else {
    ltabase::load_ltabase_datasets(message = TRUE)
  }
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.4 Load additional libraries ####
  
  # Install devtools to install bbplot
  if(!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
  }
  library(devtools)
  if(!requireNamespace("bbplot", quietly = TRUE)) {
    devtools::install_github('bbc/bbplot')
  }
  
  # Load aditional bibliotheken
  library(conflicted)   # to solve conflicts
  library(rio)          # for reading files
  library(doParallel)   # for parallel processing
  library(fs)           # for file system functions
  
  library(gtsummary)    # for descriptive summary tables
  library(flextable)    # for flextables
  library(officer)      # for formatting in tables
  library(gt)           # for tables
  library(gtExtras)     # for sparklines
  library(cli)          # for cli texts
  library(glue)         # for string interpolation
  library(yaml)         # for yaml files
  
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
  library(ranger)       # for random forest
  
  library(ggtext)       # for creating formatting in titles
  library(showtext)     # for setting fonts
  library(ggplot2)      # for creating plots
  library(ggpubr)       # for saving plots
  library(bbplot)       # for saving plots > TODO: phase out [?]
  library(grid)         # for saving plots
  library(gridGraphics) # for saving plots
  library(extrafont)    # for saving plots
  library(sysfonts)     # for fonts
  library(systemfonts)  # for fonts
  library(janitor)      # for cleaning names
  library(pins)         # for data sharing
  
  library(cvms)         # for confusion matrices
  library(ggimage)      # for confusion matrices
  library(rsvg)         # for confusion matrices
  library(ggnewscale)   # for confusion matrices
  
  library(cardx)        # for extra analysis results data utilities
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.5 Brand #### 
  
  # Load the brand settings
  brand_data <- read_yaml("brand/_brand.yml")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.6 Fonts ####
  
  # Load fonts
  extrafont::loadfonts(quiet = TRUE)
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.7 Load additional features ####

  source("R/functions/report.helpers.R")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.8 Colors ####
  
  source("brand/colors/colors.R")

  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.9 Determine preferred themes ####

  Set_Theme()

  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.10 Tidymodels preferences ####

  # When conflicts arise, give preference to the tidymodels package
  tidymodels_prefer(quiet = TRUE)

  # . ####
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2. GENERAL CONFIG ####
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.1 Network paths ####
  
  # Define the network directory
  if(sEnvironment == "ceda") {
    Network_directory <- "R/data"
  } else {
    ltabase::set_lta_sys_env()
    Network_directory <- ltabase::get_lta_network_directory()
  }
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.2 Debug ####
  
  # Set debug options: icecream package settings
  Set_Icecream_Options()
  icecream::ic_disable()
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.3 Gtsummary ####
  
  # Define the default settings of gtsummary
  list("style_number-arg:big.mark" = ".",
       "style_number-arg:decimal.mark" = ",") |>
    set_gtsummary_theme()
  invisible(theme_gtsummary_compact())
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.4 Succes model ####
  
  sSucces_model                     <- params$succes
  sPropedeusediploma                <- params$propedeusediploma
  sSucces_model_text                <- Get_Succes_Model_Text(sPropedeusediploma, 
                                                             sSucces_model)
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.5  Educational programme information ####
  
  # Create the variables for the current study programme based on the programme name and type of education
  current_opleiding <- Get_Current_Opleiding(
    opleiding = params$opleiding,
    opleidingsvorm = params$opleidingsvorm_afkorting
  )
  
  # Based on this, determine derived variables
  Set_Current_Opleiding_Vars(current_opleiding, debug = T)
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.6 Enrollment data  ####
  
  if(sEnvironment == "ceda") {
    dfOpleiding_inschrijvingen_base <- Get_dfOpleiding_inschrijvingen_base_syn()
  } else {
    dfOpleiding_inschrijvingen_base <- ltabase::get_lta_studyprogram_enrollments_pin(
      board = sPinBoard,
      faculty = params$faculteit,
      studyprogram = current_opleiding$INS_Opleidingsnaam_huidig,
      studytrack = current_opleiding$INS_Opleiding,
      studyform = toupper(current_opleiding$INS_Opleidingsvorm),
      range = "eerstejaars")
  }
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.7 Metadata & research settings ####
  
  lMetadata <- Get_Metadata()
  
  if(is.null(lResearch_settings)) {
    cli_alert_warning("lResearch_settings is not defined. Please define it in the _Setup_config.R file.")
  }
  
  lResearch_settings[["sDataset"]]       <- Get_sDataset(dfOpleiding_inschrijvingen_base)
  lResearch_settings[["sOpleiding"]]     <- Get_sOpleiding()
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.8 Plot information ####
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.8.1 Caption
  
  sCaption <- Get_sCaption()
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.8.2 Plot height and width ####
  
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
  sOpleidingsvorm_lang        <- Get_Opleidingsvorm_Lang(params$opleidingsvorm_afkorting)
  sFaculteit_lang             <- Get_Faculteitsnaam_Lang(params$faculteit)
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3.2 Variables and levels ####
  
  dfVariables                 <- Get_dfVariables()
  dfLevels                    <- Get_dfLevels()
  lLevels                     <- Get_lLevels(dfLevels)
  lLevels_formal              <- Get_lLevels(dfLevels, formal = TRUE)
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3.3 Sensitive variables and labels ####
  
  lSentitive_formal_variables <- Get_lSensitive(dfVariables, var = "VAR_Formal_variable")
  lSentitive_variables        <- Get_lSensitive(dfVariables, var = "VAR_Simple_variable")
  lSensitive_labels           <- Get_lSensitive(dfVariables, var = "VAR_Variable_label")
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
  
  ## TODO: if these paths and objects don't exist
  lLast_fits                 <- rio::import(sFittedmodels_outputpath, trust = TRUE)
  dfModel_results            <- rio::import(sModelresults_outputpath, trust = TRUE)
  
}

