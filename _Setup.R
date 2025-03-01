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
for (package in c("renv", "cli", "glue", "knitr", "markdown")) {
  if (!requireNamespace(package, quietly = TRUE)) {
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

if (!exists("renv_restored") || renv_restored == FALSE) {
  if (!renv::status()$synchronized) {
    renv::restore()
  }
  renv_restored <- TRUE
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

if (!is.null(rmarkdown::metadata$config$environment)) {
  environment <- rmarkdown::metadata$config$environment
} else {
  environment <- "ceda"
}

cli_h1("Setting environment")
cli_alert_success(glue("Environment is {col_red(environment)}"))

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0.4 Determine the current file name ####

if (!exists("current_file")) {
  current_file <- "unknown"
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0.5 Reset Setup ####

# Set this variable to T to reset this page or restart the session
current_file <- FALSE

# The setup has not yet been performed, please perform it unless there is a reset
if (!exists("setup_executed") || current_file) {
  setup_executed <- FALSE
}

# Based on the setup, do or do not run the rest of this document
if (setup_executed == FALSE) {

  # . ####
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1. PACKAGES & FUNCTIONS ####
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  cli_h1("1. PACKAGES & FUNCTIONS")
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.2 Base and fairness functions ####
  
  source("R/functions/base.helpers.R")
  source("R/functions/fairness.helpers.R")
  
  cli_h1("Load functions")
  cli_alert_success("Functions have been loaded: basis and fairness")
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.3 Default datasets ####
  
  cli_h1("Load standard datasets")
  
  # Load the default datasets: df_studyprogrammes, df_sectors, df_studytypes, df_studyforms
  load_datasets(message = TRUE)
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.5 Brand #### 
  
  # Load the brand settings
  brand_data <- read_yaml("brand/_brand.yml")
  
  cli_h1("Load brand setting")
  cli_alert_success("Brand settings have been loaded.")
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.6 Fonts ####
  
  # Load fonts
  extrafont::loadfonts(quiet = TRUE)
  
  cli_h1("Load fonts")
  cli_alert_success("Fonts have been loaded.")
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.7 Load additional features ####

  source("R/functions/report.helpers.R")
  
  cli_h1("Load functions")
  cli_alert_success("Functions are loaded: report")
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.8 Colors ####
  
  source("brand/colors/colors.R")
  
  cli_h1("Load colors")
  cli_alert_success("Colors have been loaded.")

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.9 Determine preferred themes ####

  set_theme()
  
  cli_h1("Load theme")
  cli_alert_success("Theme has been loaded")

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1.10 Tidymodels preferences ####

  # When conflicts arise, give preference to the tidymodels package
  tidymodels_prefer(quiet = TRUE)
  
  cli_h1("Load tidymodels preferences")
  cli_alert_success("Tidymodels preference set")

  # . ####
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2. GENERAL SETTINGS ####
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  cli_h1("2. GENERAL SETTINGS")
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.1 Network paths ####
  
  # Define the network directory
  network_directory <- "R/data"
  
  cli_h1("Set network path")
  cli_alert_success(glue("Network path is set: {network_directory}."))
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.2 Debug ####
  
  # Set debug options: icecream package settings
  set_icecream_options()
  icecream::ic_disable()
  
  cli_h1("Debug settings")
  cli_alert_success("Debug is set.")
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.3 Gtsummary ####
  
  # Define the default settings of gtsummary
  list("style_number-arg:big.mark" = ".",
       "style_number-arg:decimal.mark" = ",") |>
    set_gtsummary_theme()
  invisible(theme_gtsummary_compact())
  
  cli_h1("Gtsummary settings")
  cli_alert_success("Gtsummary preferences set.")
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.4 Succes model ####
  
  succes_model      <- params$succes
  pd                <- params$pd
  succes_model_text <- get_succes_model_text(pd, succes_model)
  
  cli_h1("Succes model settings")
  cli_alert_success("Model settings set")
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.5  Educational programme information ####
  
  cli_h1("Current study programme")
  cli_alert_success("Current study programme set")
  
  # Create the variables for the current study programme based on the programme name and type 
  # of education
  current_sp <- get_current_sp(
    sp = params$sp,
    sp_form = params$sp_abbreviation
  )
  
  # Based on this, determine derived variables
  set_current_sp_vars(current_sp, debug = TRUE)
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.6 Enrollment data  ####
  
  df_sp_enrollments <- get_sp_enrollments_base_syn()
  
  cli_h1("Enrollments")
  cli_alert_success("Enrollments loaded")
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.7 Metadata & research settings ####
  
  metadata <- get_metadata()
  
  if (is.null(research_settings)) {
    cli_alert_warning(glue("research_settings is not defined. ",
                           "Please define it in the _Setup_config.R file."))
  }
  
  research_settings[["dataset"]] <- get_dataset(df_sp_enrollments)
  research_settings[["sp"]]      <- get_sp()
  
  cli_h1("Metadata & research settings")
  cli_alert_success("Metadata & research settings loaded")
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.8 Plot information ####
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.8.1 Caption
  
  caption <- get_caption()
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2.8.2 Plot height and width ####
  
  # Determine the height and width of images
  plot_width  <- 640
  plot_height <- 550
  
  cli_h1("Plot settings")
  cli_alert_success("Plot caption, width and height set")
  
  # . ####
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3. CONTENT ####
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  cli_h1("3. CONTENT")
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3.1 Names of the study programme and type of education ####
  
  # Determine the long name of the type of education and faculty
  studyprogramme_form_long  <- get_sp_form_long(params$sp_abbreviation)
  faculty_long              <- get_faculty_name_long(current_sp$INS_Faculteit)
  
  cli_h1("Long names")
  cli_alert_success("Long names of study programme and faculty set.")
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3.2 Variables and levels ####
  
  df_variables   <- get_df_variables()
  df_levels      <- get_df_levels()
  levels         <- get_levels(df_levels)
  levels_formal  <- get_levels(df_levels, formal = TRUE)
   
  cli_h1("Variables and levels")
  cli_alert_success("Variables and levels set")
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3.3 Sensitive variables and labels ####
  
  sensitive_formal_variables <- get_list_sensitive(df_variables, var = "VAR_Formal_variable")
  sensitive_variables        <- get_list_sensitive(df_variables, var = "VAR_Simple_variable")
  sensitive_labels           <- get_list_sensitive(df_variables, var = "VAR_Variable_label")
  sensitive_levels_breakdown <- get_sensitive_levels_breakdown(df_levels, 
                                                               sensitive_formal_variables)
  
  cli_h1("Sensitive variables and labels")
  cli_alert_success(glue("Sensitive variables set: ",
                         "{paste(unlist(sensitive_variables), collapse = ', ')}"))
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3.4 Paths for data, last fits and model results ####
  
  # Define the paths
  data_outputpath            <- get_model_outputpath(mode = "data")
  fittedmodels_outputpath    <- get_model_outputpath(mode = "last-fits")
  modelresults_outputpath    <- get_model_outputpath(mode = "modelresults")
  
  # If these folders don't exist yet, create them
  for (i in c(data_outputpath, 
              fittedmodels_outputpath, 
              modelresults_outputpath)) {
    if (!dir.exists(dirname(i))) {
      dir.create(dirname(i), recursive = TRUE)
    }
  }
  
  cli_h1("Paths for data, last fits and model results")
  cli_alert_success("Paths for data, last fits and model results set.")
  
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3.5 Data for training, last fits and results ####
  
  # Check if we need to check the data
  if (current_file != "ch-models.qmd") {
    check_data <- TRUE
  } else {
    check_data <- FALSE
  }
  
  # If one of these files does not exist and we are not in ch4-models.qmd, 
  # give a cli warning
  if ((
    !file.exists(data_outputpath) ||
      !file.exists(fittedmodels_outputpath) ||
      !file.exists(modelresults_outputpath)
  )) {
    
    
    if (check_data) {
      
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
    df_sp_enrollments <- rio::import(data_outputpath, trust = TRUE) |> 
      mutate(across(all_of(names(levels)), ~ factor(.x, 
                                                    levels = levels[[cur_column()]]))) |> 
      mutate(Retentie = as.numeric(Retentie) - 1)
    
    ## Last fits and model results
    last_fits         <- rio::import(fittedmodels_outputpath, trust = TRUE)
    df_model_results  <- rio::import(modelresults_outputpath, trust = TRUE)
    
    cli_alert_success(
      glue(
        "Data and model files have been loaded."
      )
    )
    
    
  } 
}
