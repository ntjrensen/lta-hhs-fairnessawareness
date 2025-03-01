# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# base.helpers.R ####
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
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. DEBUGGING FUNCTIONS ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to set debug options
set_icecream_options <- function() {
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
get_documentation <- function(folder = "R/data", file = "Documentatie_HHs_LTA.xlsx") {
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
  
  documentation
}

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. PACKAGE FUNCTIONS ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to install and load packages
install_and_load_packages <- function(base = TRUE,
                                      plots = TRUE,
                                      reports = FALSE,
                                      spatial = FALSE,
                                      models = TRUE,
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
    packages <- get_packagelist(i)
    if (message) {
      cli::cli_h1(paste0("Installeren en laden ", i, "-lijst"))
      cli::cli_alert_info(c(
        "De volgende packages worden geïnstalleerd en geladen: {.var {packages}}"
      ))
    }
    install_packages(packages)
    load_packages(packages, debug = debug, message = message)
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
get_packagelist <- function(mode = "fairness") {
  stopifnot(mode %in% c(
    "fairness"
  ))
  if (mode == "fairness") {
    packages <- c(
      "glue",
      "abind",
      "backports",
      "bbplot",
      "broom",
      "car",
      "carData",
      "cli",
      "colorspace",
      "conflicted",
      "corrplot",
      "cowplot",
      "cpp11",
      "cvms",
      "data.table",
      "Deriv",
      "doBy",
      "doParallel",
      "dplyr",
      "fansi",
      "farver",
      "flextable",
      "Formula",
      "generics",
      "ggplot2",
      "ggpubr",
      "ggrepel",
      "ggsci",
      "ggsignif",
      "gridExtra",
      "gtable",
      "gtExtras",
      "gtsummary",
      "isoband",
      "labeling",
      "lifecycle",
      "lme4",
      "magrittr",
      "MatrixModels",
      "microbenchmark",
      "minqa",
      "modelr",
      "munsell",
      "nloptr",
      "numDeriv",
      "pbkrtest",
      "performance",
      "pillar",
      "pkgconfig",
      "png",
      "polynom",
      "purrr",
      "quantreg",
      "R6",
      "rbibutils",
      "RColorBrewer",
      "Rcpp",
      "RcppEigen",
      "Rdpack",
      "reformulas",
      "rio",
      "rlang",
      "rstatix",
      "scales",
      "SparseM",
      "stringi",
      "stringr",
      "tibble",
      "tidyr",
      "tidyselect",
      "tidymodels",
      "utf8",
      "vctrs",
      "viridisLite",
      "vip",
      "withr",
      "base",
      "boot",
      "class",
      "cluster",
      "codetools",
      "compiler",
      "datasets",
      "foreign",
      "graphics",
      "grDevices",
      "grid",
      "KernSmooth",
      "lattice",
      "MASS",
      "Matrix",
      "methods",
      "mgcv",
      "nlme",
      "nnet",
      "parallel",
      "rpart",
      "spatial",
      "splines",
      "stats",
      "stats4",
      "survival",
      "tcltk",
      "tools",
      "utils",
    )
  }
  return(packages)
}

# Function to install packages
install_packages <- function(packages, message = FALSE) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) {
    if (message) {
      cli::cli_alert_warning(
        c(
          "The following packages are not yet installed: {.var {new_packages}} \n",
          "These are now being installed as yet"
        )
      )
    }
    install.packages(new_packages, repos = "http://cran.us.r-project.org", quiet = TRUE)
  } else {
    if (message) {
      cli::cli_alert_success("All packages are already installed")
    }
  }
  if (!all(packages %in% installed.packages())) {
    cli::cli_alert_warning("Packages are NOT installed")
  }
}

# Function to load packages
load_packages <- function(packages,
                          debug = FALSE,
                          message = FALSE) {
  load_all_packages <- function(packages) {
    lapply(packages, library, character.only = TRUE)
  }
  if (debug) {
    load_all_packages(packages)
  } else {
    suppressMessages(invisible(load_all_packages(packages)))
  }
  if (!all(packages %in% .packages())) {
    missing_packages <- packages[!(packages %in% .packages())]
    if (message) {
      cli::cli_alert_warning(
        c(
          "Not all packages are loaded; ",
          "The following packages are not loaded: {.var {missing_packages}}"
        )
      )
    }
  } else {
    if (message) {
      cli::cli_alert_success("All packages are loaded: {.var {packages}}")
    }
  }
}

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. COLOR FUNCTIONS ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to get the tableau colors
get_colors_tableau <- 
  function()  {
    lcolors_tableau <- c("#fc7d0b", "#1170aa", "#a3acb9", "#c85200", 
                         "#a3cce9", "#57606c", "#ffbc79", "#5fa2ce", "#7b848f", 
                         "#f28e2b", "#c8d0d9", "#b6992d", "#f1ce63", "#9467BD", 
                         "#C5B0D5", "#E377C2", "#F7B6D2", "#17BECF", "#9EDAE5", 
                         "#8C564b", "#C49C94", "#2CA02C")
    lcolors_tableau
  }


# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4. ADDITIONAL FUNCTIONS ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sort_distinct <- function(df) {
  if (!is.data.frame(df)) {
    stop("Input is not a data frame")
  }
  
  dplyr::select(dplyr::distinct(df), base::sort(tidyselect::peek_vars()))
}

load_datasets <- function(message = FALSE) {
  datasets <- list(
    df_documentation = get_df_documentation(),
    df_studyprogrammes = get_df_studyprogrammes(),
    df_institutions = get_df_institutions(),
    df_sectors = get_df_sectors(),
    df_studytypes = get_df_studytypes(),
    df_studyforms = get_df_studyforms()
  )
  set_variables_from_list(datasets)
  if (message == TRUE) {
    cli::cli_alert(c(
      "The following datasets have been loaded: \n",
      "\n",
      paste0(names(datasets), collapse = "\n")
    ))
  }
}  

set_variables_from_list <- function(list) {
  list2env(stats::setNames(list, nm = names(list)), envir = .GlobalEnv)
}

get_df_documentation <- function() {
  
  df <- rio::import("R/data/documentation_hhs_lta.xlsx")
  
  df
}

get_df_studyprogrammes <- function() {
  
  df <- rio::import("R/data/studyprogrammes.rda", trust = TRUE)
  
  df
}

get_df_institutions <- function() {
  
  df <- rio::import("R/data/institutions.rda", trust = TRUE)
  
  df
}

get_df_sectors <- function() {
  
  df <- rio::import("R/data/sectors.rda", trust = TRUE)
  
  df
}

get_df_studyforms <- function() {
  
  df <- rio::import("R/data/studyforms.rda", trust = TRUE)
  
  df
}

get_df_studytypes <- function() {
  
  df <- rio::import("R/data/studytypes.rda", trust = TRUE)
  
  df
}

get_sp_enrollments_base_syn <- function() {
  
  df_sp_enrollments <- rio::import("R/data/syn/studyprogrammes_enrollments_syn.rds", trust = TRUE)
  df_sp_enrollments <- df_sp_enrollments |>
    filter(INS_Faculteit == current_sp$INS_Faculteit,
           INS_Opleidingsnaam_huidig == current_sp$INS_Opleidingsnaam_huidig,
           INS_Opleiding == current_sp$INS_Opleiding,
           INS_Opleidingsvorm == toupper(current_sp$INS_Opleidingsvorm)) |> 
    mutate(LTA_Dataset = "ASI-Syn 20240124")
  
  df_sp_enrollments
}
