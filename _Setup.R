# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# _Setup.R ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R code of the Learning Technology & Analytics Research Group of THUAS
# Copyright 2025 THUAS
# Web Page: http://www.thuas.com
# Contact: Theo Bakker (t.c.bakker@hhs.nl)
# Distribution outside THUAS: Yes
#
# Purpose: A setup file to perform standard functionality of the project.
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

# Install renv, cli, glue, knitr, and markdown if not yet installed and load the library
for(package in c("renv", "cli", "glue", "knitr", "markdown")) {
  if(!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

library(renv)
library(cli)
library(glue)
library(knitr)
library(markdown)

# Show the start of the document
cli_h1("0. ON START")

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0.1 Restore renv ####

if (!exists("bRenv_restored") || bRenv_restored == FALSE) {
  if (!renv::status()$synchronized) {
    renv::restore()
  } 
  bRenv_restored <- TRUE  
  
}

cli_h1("Installing packages")
cli_alert_success("All packages are installed")

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0.2 Load Setup config #### 

source("_Setup_config.R")

cli_h1("Load configuration")
cli_alert_success("Configuration has been loaded.")

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0.3 Set environment profile #### 

if(!is.null(rmarkdown::metadata$config$environment)) {
  sEnvironment <- rmarkdown::metadata$config$environment
} else {
  sEnvironment <- "ceda"
}

cli_h1("Setting environment")
cli_alert_success(glue("Environment is {col_red(sEnvironment)}"))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0.4 Determine the current file name ####

if(!exists("sCurrent_file")) {
  sCurrent_file <- "unknown"
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0.5 Reset Setup ####

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
  # 1. PACKAGES & FUNCTIONS ####
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  cli_h1("1. PACKAGES & FUNCTIES")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.2 Base and fairness functions ####
  
  source("R/functions/base.helpers.R")
  source("R/functions/fairness.helpers.R")
  
  cli_h1("Load functions")
  cli_alert_success("Functions have been loaded: basis and fairness")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.3 Default datasets ####
  
  cli_h1("Load standard datasets")
  
  # Load the default datasets: dfOpleidigen, sectors, studytypes, studyforms
  Load_Datasets(message = TRUE)
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.4 Load libraries ####
  
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
  
  cli_h1("Load libraries")
  cli_alert_success("Libraries have been loaded.")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.5 Brand #### 
  
  # Load the brand settings
  brand_data <- read_yaml("brand/_brand.yml")
  
  cli_h1("Load brand setting")
  cli_alert_success("Brand settings have been loaded.")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.6 Fonts ####
  
  # Load fonts
  extrafont::loadfonts(quiet = TRUE)
  
  cli_h1("Load fonts")
  cli_alert_success("Fonts have been loaded.")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.7 Load additional features ####

  source("R/functions/report.helpers.R")
  
  cli_h1("Load functions")
  cli_alert_success("Functions are loaded: report")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.8 Colors ####
  
  source("brand/colors/colors.R")
  
  cli_h1("Load colors")
  cli_alert_success("Colors have been loaded.")

  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.9 Determine preferred themes ####

  Set_Theme()
  
  cli_h1("Load theme")
  cli_alert_success("Theme has been loaded")

  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.10 Tidymodels preferences ####

  # When conflicts arise, give preference to the tidymodels package
  tidymodels_prefer(quiet = TRUE)
  
  cli_h1("Load tidymodels preferences")
  cli_alert_success("Tidymodels preference set")

  # . ####
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2. GENERAL SETTINGS ####
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  cli_h1("2. GENERAL SETTINGS")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.1 Network paths ####
  
  # Define the network directory
  Network_directory <- "R/data"
  
  cli_h1("Set network path")
  cli_alert_success(glue("Network path is set: {Network_directory}."))
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.2 Debug ####
  
  # Set debug options: icecream package settings
  Set_Icecream_Options()
  icecream::ic_disable()
  
  cli_h1("Debug settings")
  cli_alert_success("Debug is set.")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.3 Gtsummary ####
  
  # Define the default settings of gtsummary
  list("style_number-arg:big.mark" = ".",
       "style_number-arg:decimal.mark" = ",") |>
    set_gtsummary_theme()
  invisible(theme_gtsummary_compact())
  
  cli_h1("Gtsummary settings")
  cli_alert_success("Gtsummary preferences set.")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.4 Succes model ####
  
  sSucces_model                     <- params$succes
  sPropedeusediploma                <- params$propedeusediploma
  sSucces_model_text                <- Get_Succes_Model_Text(sPropedeusediploma, 
                                                             sSucces_model)
  
  cli_h1("Succes model settings")
  cli_alert_success("Model settings set")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.5  Educational programme information ####
  
  cli_h1("Current study programme")
  cli_alert_success("Current study programme set")
  
  # Create the variables for the current study programme based on the programme name and type of education
  current_opleiding <- Get_Current_Opleiding(
    opleiding = params$opleiding,
    opleidingsvorm = params$opleidingsvorm_afkorting
  )
  
  # Based on this, determine derived variables
  Set_Current_Opleiding_Vars(current_opleiding, debug = T)
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.6 Enrollment data  ####
  
  dfOpleiding_inschrijvingen_base <- Get_dfOpleiding_inschrijvingen_base_syn()
  
  cli_h1("Enrollments")
  cli_alert_success("Enrollments loaded")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.7 Metadata & research settings ####
  
  lMetadata <- Get_Metadata()
  
  if(is.null(lResearch_settings)) {
    cli_alert_warning("lResearch_settings is not defined. Please define it in the _Setup_config.R file.")
  }
  
  lResearch_settings[["sDataset"]]       <- Get_sDataset(dfOpleiding_inschrijvingen_base)
  lResearch_settings[["sOpleiding"]]     <- Get_sOpleiding()
  
  cli_h1("Metadata & research settings")
  cli_alert_success("Metadata & research settings loaded")
  
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
  
  cli_h1("Plot settings")
  cli_alert_success("Plot caption, width and height set")
  
  # . ####
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3. CONTENT ####
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  cli_h1("3. CONTENT")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3.1 Names of the study programme and type of education ####
  
  # Determine the long name of the type of education and faculty
  sOpleidingsvorm_lang        <- Get_Opleidingsvorm_Lang(params$opleidingsvorm_afkorting)
  sFaculteit_lang             <- Get_Faculteitsnaam_Lang(current_opleiding$INS_Faculteit)
  
  cli_h1("Long names")
  cli_alert_success("Long names of study programme and faculty set.")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3.2 Variables and levels ####
  
  dfVariables                 <- Get_dfVariables()
  dfLevels                    <- Get_dfLevels()
  lLevels                     <- Get_lLevels(dfLevels)
  lLevels_formal              <- Get_lLevels(dfLevels, formal = TRUE)
  
  cli_h1("Variables and levels")
  cli_alert_success("Variables and levels set")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3.3 Sensitive variables and labels ####
  
  lSentitive_formal_variables <- Get_lSensitive(dfVariables, var = "VAR_Formal_variable")
  lSentitive_variables        <- Get_lSensitive(dfVariables, var = "VAR_Simple_variable")
  lSensitive_labels           <- Get_lSensitive(dfVariables, var = "VAR_Variable_label")
  lSensitive_levels_breakdown <- Get_lSensitive_Levels_Breakdown(dfLevels, lSentitive_formal_variables)
  
  cli_h1("Sensitive variables and labels")
  cli_alert_success(glue("Sensitive variables set: {paste(unlist(lSentitive_variables), collapse = ', ')}"))
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3.4 Paths for data, last fits and model results ####
  
  # Define the paths
  sData_outputpath            <- Get_Model_Outputpath(mode = "data")
  sFittedmodels_outputpath    <- Get_Model_Outputpath(mode = "last-fits")
  sModelresults_outputpath    <- Get_Model_Outputpath(mode = "modelresults")
  
  # If these folders don't exist yet, create them
  for (i in c(sData_outputpath, 
              sFittedmodels_outputpath, 
              sModelresults_outputpath)) {
    if(!dir.exists(dirname(i))) {
      dir.create(dirname(i), recursive = TRUE)
    }
  }
  
  cli_h1("Paths for data, last fits and model results")
  cli_alert_success("Paths for data, last fits and model results set.")
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3.5 Data for training, last fits and results ####
  
  # Check if we need to check the data
  if(sCurrent_file != "ch4-models.qmd") {
    bCheck_data <- TRUE
  } else {
    bCheck_data <- FALSE
  }
  
  # If one of these files does not exist and we are not in ch4-models.qmd, 
  # give a cli warning
  if((
    !file.exists(sData_outputpath) ||
    !file.exists(sFittedmodels_outputpath) ||
    !file.exists(sModelresults_outputpath)
  ) ) {
    
    if(bCheck_data) {
      
      cli_h1("Data and model files.")
      cli_alert_warning(
        glue(
          "One or more data or model files do not yet exist.",
          "\n\n First, run the template in 'advanced-report' mode in the terminal:",
          "\n quarto render --profile advanced-report"
        )
      )
    }
    
  } else {
    
    cli_h1("Data and model files.")
    # Data - adjust the Retention variable to numeric (0/1),
    dfOpleiding_inschrijvingen <- rio::import(sData_outputpath, trust = TRUE) |> 
      mutate(across(all_of(names(lLevels)), ~ factor(.x, 
                                                     levels = lLevels[[cur_column()]]))) |> 
      mutate(Retentie = as.numeric(Retentie) - 1)
    
    ## Last fits and model results
    lLast_fits                 <- rio::import(sFittedmodels_outputpath, trust = TRUE)
    dfModel_results            <- rio::import(sModelresults_outputpath, trust = TRUE)
    
    cli_alert_success(
      glue(
        "Data and model files have been loaded."
      )
    )
    
    
  } 
  
  
}


