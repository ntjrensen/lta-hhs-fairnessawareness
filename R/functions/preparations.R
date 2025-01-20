# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# preparations.R
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R code of the Learning Technology & Analytics Research Group of THUAS
# Copyright 2025 THUAS
# Web Page: http://www.thuas.com
# Contact: Theo Bakker (t.c.bakker@hhs.nl)
# Distribution outside THUAS: No
#
# Purpose: De specifieke functies en libraries voor dit project worden ingeladen
#
# Dependencies: None
#
# Datasets: geen
#
# Remarks:
# 1) ...
#
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TODO:
# 1) ...
#
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Version history:
# 24-02-2023: TB: File creation; een aantal onderdelen uitgezet
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0. LOAD LTABASE PACKAGE + STANDARD DATABASES ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0.1 Standard general package needed immediately ####

# Install here, cli and icecream if necessary
packages <- c("here", "cli", "icecream")

purrr::walk(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
})

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0.2 ltabase package (install if necessary) ####

source("R/functions/load.ltabase.R")

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 0.3 Default datasets: dfOpleidigen, sectors, studytypes, studyforms ####

# Load the default datasets: dfOpleidigen, sectors, studytypes, studyforms
ltabase::load_lta_datasets(message = TRUE)

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. BASIC VARIABLES / PATHS ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# If LTA_ROOT, LTA_DATA or LTA_BOARD do not exist, the environment is reset
ltabase::set_lta_sys_env()

# Define the network directory
Network_directory <- ltabase::get_lta_network_directory()

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. DEBUG ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Set debug options: icecream package settings
ltabase::set_icecream_options()
icecream::ic_disable()

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. PACKAGES ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Install and load the packages + default settings
install_and_load_packages(plot = TRUE, spatial = TRUE, message = TRUE)

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4. SET SEED ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Determine the seed used for hashing
set.seed(79515660)

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5. DOCUMENTATION ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Read the documentation in to memory
Documentatie <- ltabase::get_lta_documentation_enrollments(network_directory = Network_directory)

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 6. COLORS ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Read in general Tableau colors: these must be known in advance.
lColors_Tableau <- ltabase::get_colors_tableau()

# [1] "#fc7d0b" "#1170aa" "#a3acb9" "#c85200" "#a3cce9" "#57606c" "#ffbc79" "#5fa2ce" "#7b848f" "#f28e2b" "#c8d0d9"
# [12] "#b6992d" "#f1ce63" "#9467BD" "#C5B0D5" "#E377C2" "#F7B6D2" "#17BECF" "#9EDAE5" "#8C564b" "#C49C94" "#2CA02C"

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 7. CONFLICTS ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

conflicts_prefer(rlang::set_names, .quiet = TRUE)

cli::cli_alert_warning("rlang::set_names is preferred")

conflicts_prefer(purrr::`%||%`)

cli::cli_alert_warning("purrr::`%||%` is preferred")
