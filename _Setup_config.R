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

if(!exists("params")) {
  params <- list()
  params$succes                        <- "Retentie na 1 jaar"
  params$model                         <- "Retentie na 1 jaar"
  params$propedeusediploma             <- "Nvt"
  params$use_synthetic_data            <- TRUE
  params$recreateplots                 <- FALSE
  params$faculteit                     <- "ITD"
  params$opleidingsnaam                <- "B Communication and Multimedia Design"
  params$opleiding                     <- "CMD"
  params$opleidingsvorm                <- "voltijd"
  params$opleidingsvorm_afkorting      <- "VT"
  params$instroomselectie              <- FALSE
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1.2 Research settings ####

lResearch_settings <- list()
lResearch_settings[["sResearch_path"]] <- "Kansengelijkheid"
lResearch_settings[["sSucces_label"]]  <- "Kans op retentie"
lResearch_settings[["sInstelling"]]    <- "HHs"
lResearch_settings[["sBron"]]          <- "De HHs, IR & Analytics"
lResearch_settings[["sAnalyse"]]       <- "De HHs, Lectoraat Learning Technology & Analytics"

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1.3 Data settings ####

sPinBoard <- "HHs/Inschrijvingen"

