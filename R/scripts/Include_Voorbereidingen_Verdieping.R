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
## 1.4 Load additional libraries ####

## Load additional libraries
library(conflicted)   # to solve conflicts
library(tidymodels)   # for machine learning

library(vip)          # for variable importance plots
library(forcats)      # to edit factor variables
library(performance)  # for performance measures on lr models
#library(dlookr)      # to inspect data > gives conflicts because of showtext_auto()
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

library(ggpubr)       # for storing plots
library(bbplot)       # for storing plots
library(grid)         # for storing plots

library(gridGraphics) # for storing plots
library(extrafont)    # for saving plots
library(sysfonts)     # for fonts

library(fairmodels)   # for fairness in models

library(fs)           # for file system functions

library(quartostamp)  # for additional quarto add-in functionality

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
## 1.9 Load additional features ####

source("R/functions/report.helpers.R")

## Determine the order of some levels
Get_Levels()

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.10 Conflicts ####

conflicts_prefer(dplyr::select)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.11 Network paths ####

# If LTA_ROOT, LTA_DATA or LTA_BOARD do not exist, the environment is reset
ltabase::set_lta_sys_env()

## Define the network directory
Network_directory <- ltabase::get_lta_network_directory()
