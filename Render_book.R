# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Render_book.R ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R code of the Learning Technology & Analytics Research Group of THUAS
# Copyright 2025 THUAS
# Web Page: http://www.thuas.com
# Contact: Theo Bakker (t.c.bakker@hhs.nl)
# Distribution outside THUAS: No
#
# Purpose: Rendering analyses for multiple study programmes in book form
#
# Dependencies: ltabase package
#
# Datasets: None
#
# Remarks:
# 1) None.
# 2) ___
#
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Version history:
# 01-05-2024: TB: File creation
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. LOAD LTABASE PACKAGE + DEFAULT DATABASES ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1.1 Standard packages needed immediately ####

# Install here, cli en icecream if required
for (i in c("here", "cli", "icecream")) {
  if(!requireNamespace(i, quietly = TRUE)) {
    install.packages(i)
  }
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1.2 ltabase package (install if necessary) ####

source("R/functions/load.ltabase.R")

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1.3 Default datasets: dfOpleidigen, sectors, studytypes, studyforms ####

# Load the default datasets: dfOpleidigen, sectors, studytypes, studyforms
ltabase::load_lta_datasets(message = TRUE)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1.4 Load additional libraries

# Load additional libraries
library(cli)         # for cli texts
library(quarto)      # for quarto files
library(tidyverse)   # for data manipulation
library(conflicted)  # to resolve conflicts
library(janitor)     # for make_clean_names
library(fs)          # for file system functions

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1.4 Default datasets: dfOpleidigen, sectors, studytypes, studyforms ####

# Laad de default datasets: dfOpleidigen, sectors, studytypes, studyforms
ltabase::load_lta_datasets(message = TRUE)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1.5 Load additional features ####

source("R/functions/report.helpers.R")

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. RENDER THE OUTPUT ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2.1 Determine the selection ####

dfRender <- dfOpleidingen |>
  
  # Combine the study programme and type of education into a new variable
  mutate(INS_Opleiding_en_Opleidingsvorm = paste(INS_Opleiding, INS_Opleidingsvorm, sep = "_")) |>
  # dplyr::filter(
  #   INS_Opleiding_en_Opleidingsvorm %in% c(
  #     # "HBO-V_VT",
  #     # "BO_DU",
  #     # "ICT_VT"
  #     # "HBO-V_DT",
  #     # "CMD_VT", 
  #     # "IPO_VT",
  #     # "E_VT",
  #     # "B_VT",
  #     # "W_VT",
  #     # "MT_VT", 
  #     # "SW_VT", 
  #     # "ORM_DT",
  #     # "ORM_VT",
  #     # "AC_VT",
  #     "CMD_VT"
  #     ) 
  # ) |>
  # dplyr::filter(INS_Faculteit %in% c("GVS","MO","SWE","TIS"),
  #               INS_Opleiding %in% c("HDT")) |>
  
  # Limit selection to study programmes still in existence in 2022 
  # and have more than 1 year's worth of data
  dplyr::filter(INS_Collegejaar_max == 2022,
                INS_Collegejaar_min < 2022) |>
  dplyr::select(INS_Opleiding_en_Opleidingsvorm, 
                INS_Faculteit,
                INS_Opleiding,
                INS_Opleidingsvorm,
                everything())

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2.2 Determine if a refresh is needed ####

bRefresh_books <- FALSE

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2.3 Determine whether synthetic data will be used ####

bUse_Synthetic_data <- FALSE

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2.4 Walk across study programmes and render outputs ####

# Determine the success variables
lSucces <-  c("Retentie na 1 jaar")

# Walk across the study programmes
for(i in 1:nrow(dfRender)) {
  
  # Determine study programme and type of education
  .opleiding      <- dfRender$INS_Opleiding[i] 
  .opleidingsvorm <- dfRender$INS_Opleidingsvorm[i]
  
  # Walk over the success variables
  for (j in lSucces) {
    
    # Report the action
    cli::cli_h1(
      paste(
        .opleiding,
        .opleidingsvorm,
        j
      )
    )
    cli::cli_alert_info(
      paste(
        "Render the output for the study programme:",
        .opleiding,
        "and type of education:",
        .opleidingsvorm
      )
    )
    
    # If this combination already exists, skip
    # Create the variables for the current study programme based on the programme name and type of education
    current_render_opleiding <- Get_Current_Opleiding(opleiding = .opleiding,
                                                      opleidingsvorm = .opleidingsvorm)

    # Determine the output dir:
    # faculteit/opleiding-opleidingsvorm/retentie-na-1-jaar/index.html
    
    .output_dir <- tolower(file.path(
      current_render_opleiding$INS_Faculteit,
      paste(
        current_render_opleiding$INS_Opleidingstype_LTA,
        current_render_opleiding$INS_Opleiding,
        current_render_opleiding$INS_Opleidingsvorm,
        sep = "-"
      ),
      .succes <- janitor::make_clean_names(j)
    ))
    
    if(bUse_Synthetic_data) {
      .output_dir <- paste0(.output_dir, "-synth")
    }
    
    # Define the parameters for the quarto file
    .execute_params <- list(
      succes                   = j,
      faculteit                = current_render_opleiding$INS_Faculteit,
      opleidingsnaam           = current_render_opleiding$INS_Opleidingsnaam_huidig,
      opleiding                = current_render_opleiding$INS_Opleiding,
      opleidingsvorm           = Get_Opleidingsvorm_Lang(current_render_opleiding$INS_Opleidingsvorm),
      opleidingsvorm_afkorting = current_render_opleiding$INS_Opleidingsvorm,
      selectie                 = ifelse(current_render_opleiding$INS_Opleiding == "HDT", 
                                        TRUE, 
                                        FALSE),
      use_synthetic_data       = bUse_Synthetic_data
    )
    
    # If the output directory of this book already exists, skip
    bBook_exists <- file.exists(Get_Output_Dir_Repo(.output_dir))
    if(bBook_exists & !bRefresh_books) {
      
      cli::cli_alert_info(
        paste(
          "The output for the study programme.:",
          .opleiding,
          "and type of education:",
          .opleidingsvorm,
          "and success:",
          j,
          "already exists in the directory:",
          .output_dir
        )
      )
      
      next
      } else {
    
        # Report the action
        cli::cli_alert_info(
          paste(
            "Render the output for the study programme:",
            .opleiding,
            "and type of education:",
            .opleidingsvorm,
            "and success:",
            j,
            "\n to the directory:",
            .output_dir
          )
        ) 
        
        }

    # Render the quarto files and move them to the asset repo
    for(k in c("index.qmd",
               "H1_Index_basis.qmd",
               "H2_Index_verdieping_factoren.qmd",
               "H3_Index_verdieping_kansengelijkheid.qmd")) {

      # Render the qmd input file to the input_dir
      quarto::quarto_render(input = k,
                            execute_params = .execute_params,
                            output_format = 'html')
    }

    # Copy the _book directory to the output directory
    Copy_Book_To_Reports(output_dir = .output_dir)
    
    # Collect garbage
    invisible(gc())
  
  }
  
}

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. END ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cli::cli_alert_success("End of script")

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


