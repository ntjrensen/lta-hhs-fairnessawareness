# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# preparations.R
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R code of the Learning Technology & Analytics Research Group of THUAS
# Copyright 2025 THUAS
# Web Page: http://www.thuas.com
# Contact: Theo Bakker (t.c.bakker@hhs.nl)
# Distribution outside THUAS: Yes
#
# Purpose: De specifieke functies en libraries voor dit project worden ingeladen
#
# Dependencies: None
#
# Datasets: None
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
# 0. LOAD PACKAGE + STANDARD DATABASES ####
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

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. BASIC VARIABLES / PATHS ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Define the network directory
if(sEnvironment_profile == "production") {
  Network_directory <- "R/data"
} else {
  ltabase::set_lta_sys_env()
  Network_directory <- ltabase::get_lta_network_directory()
}

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. DEBUG ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Set debug options: icecream package settings
if(sEnvironment_profile == "production") {
  Set_Icecream_Options()
} else {
  ltabase::set_icecream_options()
}
icecream::ic_disable()

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. PACKAGES ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Install and load the packages + default settings
if(sEnvironment_profile == "production") {
  Install_and_Load_Packages(plot = FALSE, spatial = FALSE, message = FALSE)
} else {
  ltabase::install_and_load_packages(plot = TRUE, spatial = TRUE, message = TRUE)
}

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
if(sEnvironment_profile == "production") {
  Documentatie <- Get_Documentation()
} else {
  Documentatie <- ltabase::get_lta_documentation_enrollments(network_directory = Network_directory)
}

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 6. COLORS ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Read in general Tableau colors: these must be known in advance.
if(sEnvironment_profile == "production") {
  lColors_Tableau <- Get_Colors_Tableau()
} else {
  lColors_Tableau <- ltabase::get_colors_tableau()
}

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
