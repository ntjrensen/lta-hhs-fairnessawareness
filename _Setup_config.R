# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# _Setup_config.R ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R code of the Learning Technology & Analytics Research Group of THUAS
# Copyright 2025 THUAS
# Web Page: http://www.thuas.com
# Contact: Theo Bakker (t.c.bakker@hhs.nl)
# Distribution outside THUAS: Yes
#
# Purpose: A config file to create settings for this proejct.
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
# 1. CONFIG ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1.1  Defaults if params don't exist ####

if (!exists("params")) {
  params <- list()
  params$succes                        <- "Retentie na 1 jaar"
  params$model                         <- "Retentie na 1 jaar"
  params$pd           <- "Nvt"
  params$use_synthetic_data            <- TRUE
  params$recreateplots                 <- FALSE
  params$sp               <- "CMD"
  params$sp_abbreviation  <- "VT"
  params$enrollment_selection          <- FALSE
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1.2 Research settings ####

research_settings <- list()
research_settings[["research_path"]]  <- "Kansengelijkheid"
research_settings[["succes_label"]]   <- "Kans op retentie"
research_settings[["institution"]]    <- "HHs"
research_settings[["data_provider"]]  <- "De HHs, IR & Analytics"
research_settings[["analysis"]]       <- "De HHs, Lectoraat Learning Technology & Analytics"

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1.3 Data settings ####

pin_board <- "HHs/Inschrijvingen"
