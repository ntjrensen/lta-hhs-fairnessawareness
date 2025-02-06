# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Render.R ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R code of the Learning Technology & Analytics Research Group of THUAS
# Copyright 2025 THUAS
# Web Page: http://www.thuas.com
# Contact: Theo Bakker (t.c.bakker@hhs.nl)
# Distribution outside THUAS: Yes
#
# Purpose: Rendering analyses for multiple study programmes
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

# Install here, cli and icecream if necessary
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
library(tidymodels)  # for machine learning
library(vip)         # for variable importance plots
library(forcats)     # to edit factor variables
library(performance) # for performance measurements on lr models
library(dlookr)      # to inspect data
library(gtsummary)   # for descriptive summary tables
library(cli)         # for cli texts
library(quarto)      # for quarto files

# When conflicts arise, give preference to the tidymodels package
tidymodels_prefer(quiet = TRUE)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1.5 Default datasets: dfOpleidigen, sectors, studytypes, studyforms ####

# Load the default datasets: dfOpleidigen, sectors, studytypes, studyforms
ltabase::load_lta_datasets(message = TRUE)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1.6 Load additional features ####

source("R/functions/report.helpers.R")

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. RENDER THE OUTPUT ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2.1 Determine the selection ####

dfRender <- dfOpleidingen |>
  mutate(INS_Opleiding_en_Opleidingsvorm = paste(INS_Opleiding, INS_Opleidingsvorm, sep = "_")) |>
  dplyr::filter(
    INS_Opleiding_en_Opleidingsvorm %in% c(
      # "HBO-V_VT", 
      # "HBO-V_DT",
      # "CMD_VT", 
      # "IPO_VT",
      # "E_VT",
      # "B_VT",
      "W_VT"#,
      # "MT_VT", 
      # "SW_VT", 
      # "ORM_DT",
      # "ORM_VT"#,
      #"AC_VT"
      ) 
  ) |> 
  dplyr::select(INS_Opleiding_en_Opleidingsvorm, 
                INS_Faculteit,
                INS_Opleiding,
                INS_Opleidingsvorm,
                everything())

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2.2 Walk across study programmes and render outputs ####

lUitval <-  c("Uitval na 1 jaar", "Alle uitval")

for(i in 1:nrow(dfRender)) {
  
  # Determine study programme and type of education
  .opleiding      <- dfRender$INS_Opleiding[i] 
  .opleidingsvorm <- dfRender$INS_Opleidingsvorm[i]
  
  cli::cli_alert_info(
    paste(
      "Render the output for the study programme:",
      .opleiding,
      "and type of education:",
      .opleidingsvorm
    )
  )
  
  for (j in lUitval) {
  
    # Create the variables for the current study programme based on the programme name and type of education
    current_render_opleiding <- Get_Current_opleiding(opleiding = .opleiding,
                                                      opleidingsvorm = .opleidingsvorm)

    # Define the output file:
    # faculteit-opleiding-opleidingsvorm-uitval.html
    .uitval <- janitor::make_clean_names(j)
    .output_file <- paste(
      paste(
        current_render_opleiding$INS_Faculteit,
        current_render_opleiding$INS_Opleidingstype_LTA,
        current_render_opleiding$INS_Opleiding,
        current_render_opleiding$INS_Opleidingsvorm,
        sep = "-"
      ),
      "_",
      .uitval,
      ".html",
      sep = ""
    )
    
    # Define the output directory
    .output_dir <- file.path("_output",
                             tolower(current_render_opleiding$INS_Faculteit))
    
    # Define the parameters for the quarto file
    .execute_params <- list(
      uitval                   = j,
      faculteit                = current_render_opleiding$INS_Faculteit,
      opleidingsnaam           = current_render_opleiding$INS_Opleidingsnaam_huidig,
      opleiding                = current_render_opleiding$INS_Opleiding,
      opleidingsvorm           = Get_Opleidingsvorm_lang(current_render_opleiding$INS_Opleidingsvorm),
      opleidingsvorm_afkorting = current_render_opleiding$INS_Opleidingsvorm,
      selectie                 = ifelse(current_render_opleiding$INS_Opleiding == "HDT", 
                                        TRUE, 
                                        FALSE)
    )
    
    # Render the quarto file and move it to the output directory
    Quarto_Render_Move(input = "Index_basis.qmd",
                       output_file = .output_file,
                       output_dir = .output_dir,
                       execute_params = .execute_params,
                       output_format = 'html')
    
    # Collect garbage
    invisible(gc())
  
  }
  
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2.2 Move the output to the output directory of the repo outside this project ####

bRemove_originals <- FALSE

# Copy the rendered files to the output directory of the repo outside this project
# Delete the original files if bRemove_originals = TRUE
Copy_Reports(remove_orgials = bRemove_originals)

