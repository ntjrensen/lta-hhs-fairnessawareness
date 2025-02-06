# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ltabase.helpers.R ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R code of the Learning Technology & Analytics Research Group of THUAS
# Copyright 2025 THUAS
# Web Page: http://www.thuas.com
# Contact: Theo Bakker (t.c.bakker@hhs.nl)
# Distribution outside THUAS: Yes
#
# Purpose: Necessary functions to replace the original ltabase functions
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
# 04-02-2025: TB: File creation
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. DEBUGGING FUNCTIONS ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to set debug options
Set_Icecream_Options() <- function()
{
  options(
    icecream.enabled = FALSE,
    icecream.peeking.function = head,
    icecream.max.lines = 5,
    icecream.prefix = "🍦",
    icecream.always.include.context = FALSE
  )
}

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. DOCUMENTATION FUNCTIONS ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to get the latest version of the documentation
Get_Documentation <- function (folder = "R/data", file = "Documentatie_HHs_LTA.xlsx")
{
  documentation <- rio::import(file.path(folder, file)) |>
    select(
      DOC_Categorie,
      DOC_Veldnaam_OKC,
      DOC_Veldnaam_LTA,
      DOC_Label_LTA,
      DOC_Omschrijving,
      DOC_Voorkomende_waarden,
      DOC_Privacygevoelig,
      DOC_Oorspronkelijke_bron,
      DOC_Datum_levering,
      DOC_Datum_mutatie_LTA,
      DOC_Commentaar
    )
  return(documentation)
}

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. PACKAGE FUNCTIONS ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to install and load packages
Install_and_Load_Packages <- function (base = TRUE,
                                       plots = FALSE,
                                       reports = FALSE,
                                       spatial = FALSE,
                                       models = FALSE,
                                       testpackage = FALSE,
                                       git = FALSE,
                                       debug = FALSE,
                                       message = FALSE) {
  lmode <- c()
  if (base)
    lmode <- c(lmode, "base")
  if (plots)
    lmode <- c(lmode, "plots")
  if (reports)
    lmode <- c(lmode, "reports")
  if (spatial)
    lmode <- c(lmode, "spatial")
  if (models)
    lmode <- c(lmode, "models")
  if (testpackage)
    lmode <- c(lmode, "testpackage")
  if (git)
    lmode <- c(lmode, "git")
  for (i in lmode) {
    packages <- Get_Packagelist(i)
    if (message) {
      cli::cli_h1(paste0("Installeren en laden ", i, "-lijst"))
      cli::cli_alert_info(c(
        "De volgende packages worden geïnstalleerd en geladen: {.var {packages}}"
      ))
    }
    Install_Packages(packages)
    Load_Packages(packages, debug = debug, message = message)
    rm(packages)
    if (message) {
      cli::cli_alert_success(c(
        "Packages van de {.var {i}}-lijst ",
        "zijn geïnstalleerd en geladen"
      ))
    }
  }
  if ("dplyr" %in% .packages()) {
    conflicted::conflict_prefer("filter", "dplyr", quiet = message)
    conflicted::conflict_prefer("select", "dplyr", quiet = message)
    conflicted::conflict_prefer("dplyr", "pull", quiet = message)
    options(dplyr.summarise.inform = FALSE)
    if (message) {
      cli::cli_alert_warning("dplyr::summarise.inform is uitgezet")
    }
  }
  if ("flextable" %in% .packages()) {
    conflicted::conflict_prefer("flextable", "separate_header", quiet = message)
  }
  if ("testthat" %in% .packages() && "vroom" %in% .packages()) {
    conflicted::conflict_prefer("testthat", "matches", quiet = message)
  }
  if ("purrr" %in% .packages()) {
    conflicted::conflict_prefer("purrr", "is_empty", quiet = message)
    conflicted::conflict_prefer("purrr", "%||%", quiet = message)
  }
  conflicted::conflict_prefer("readr", "col_factor", quiet = message)
  conflicted::conflict_prefer("rlang", "set_names", quiet = message)
  conflicted::conflict_prefer("testthat", "matches", quiet = message)
}

# Function to get the packagelist
Get_Packagelist <- function (mode = "base") {
  stopifnot(mode %in% c(
    "base",
    "plots",
    "reports",
    "spatial",
    "models",
    "testpackage",
    "git"
  ))
  if (mode == "base") {
    packages <- c(
      "conflicted",
      "cli",
      "digest",
      "htmltools",
      "devtools",
      "httr",
      "purrr",
      "furrr",
      "pins",
      "rio",
      "readr",
      "vroom",
      "readxl",
      "xlsx",
      "readtext",
      "fst",
      "haven",
      "stringr",
      "testthat",
      "icecream",
      "checkmate",
      "utils",
      "rvest",
      "scales",
      "ggplot2",
      "magrittr",
      "janitor",
      "snakecase",
      "glue",
      "janitor",
      "textutils",
      "lubridate",
      "scriptName",
      "tibble",
      "tidyr",
      "tidyverse",
      "dplyr",
      "stringi"
    )
  }
  if (mode == "plots") {
    packages <- c(
      "ggthemes",
      "ggtext",
      "gghighlight",
      "ggmosaic",
      "extrafont",
      "extrafontdb",
      "showtext",
      "ggrepel",
      "ggbump",
      "cowplot",
      "geomtextpath",
      "lemon",
      "ggblanket",
      "patchwork",
      "ggpubr",
      "gridExtra",
      "grid",
      "Cairo",
      "ragg",
      "daiR",
      "ggpubr",
      "hrbrthemes",
      "viridis",
      "paletteer",
      "circlize",
      "magick",
      "shadowtext"
    )
  }
  if (mode == "reports") {
    packages <- c(
      "yaml",
      "knitr",
      "kableExtra",
      "simplermarkdown",
      "gt",
      "flextable",
      "ftExtra",
      "huxtable",
      "xtable",
      "gt",
      "officer"
    )
  }
  if (mode == "spatial") {
    packages <- c(
      "sp",
      "tmap",
      "sf",
      "ggmap",
      "mapproj",
      "monochromeR",
      "cbsodataR",
      "ggspatial",
      "gridExtra"
    )
  }
  if (mode == "models") {
    packages <- c(
      "stats",
      "BurStMisc",
      "caret",
      "rpart",
      "randomForest",
      "xgboost",
      "glmnet",
      "nnet",
      "ranger",
      "caret",
      "e1071",
      "kernlab",
      "xgboost",
      "gbm",
      "lightgbm",
      "catboost"
    )
  }
  if (mode == "testpackage") {
    packages <- c("datapasta", "intendo")
  }
  if (mode == "git") {
    packages <- c("git2r")
  }
  return(packages)
}

# Function to install packages
Install_Packages <- function (packages, message = FALSE)
{
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages)) {
    if (message) {
      cli::cli_alert_warning(
        c(
          "De volgende packages zijn nog niet geïnstalleerd: {.var {new.packages}} \n",
          "Deze worden nu alsnog geïnstalleerd"
        )
      )
    }
    install.packages(new.packages, repos = "http://cran.us.r-project.org", quiet = TRUE)
  }
  else {
    if (message) {
      cli::cli_alert_success("Alle packages zijn al geïnstalleerd")
    }
  }
  if (!all(packages %in% installed.packages())) {
    cli::cli_alert_warning("Packages zijn NIET geïnstalleerd")
  }
}

# Function to load packages
Load_Packages <- function (packages,
                           debug = FALSE,
                           message = FALSE)
{
  load_all_packages <- function(packages) {
    lapply(packages, library, character.only = TRUE)
  }
  if (debug) {
    load_all_packages(packages)
  }
  else {
    suppressMessages(invisible(load_all_packages(packages)))
  }
  if (!all(packages %in% .packages())) {
    missing_packages <- packages[!(packages %in% .packages())]
    if (message) {
      cli::cli_alert_warning(
        c(
          "Niet alle packages zijn ingeladen; ",
          "De volgende packages zijn niet ingeladen: {.var {missing_packages}}"
        )
      )
    }
  }
  else {
    if (message) {
      cli::cli_alert_success("Alle packages zijn ingeladen: {.var {packages}}")
    }
  }
}

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. COLOR FUNCTIONS ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to get the tableau colors
Get_Colors_Tableau() <- 
  function () 
  {
    lcolors_tableau <- c("#fc7d0b", "#1170aa", "#a3acb9", "#c85200", 
                         "#a3cce9", "#57606c", "#ffbc79", "#5fa2ce", "#7b848f", 
                         "#f28e2b", "#c8d0d9", "#b6992d", "#f1ce63", "#9467BD", 
                         "#C5B0D5", "#E377C2", "#F7B6D2", "#17BECF", "#9EDAE5", 
                         "#8C564b", "#C49C94", "#2CA02C")
    return(lcolors_tableau)
  }


