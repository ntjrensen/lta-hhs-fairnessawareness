## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## quarto-render-basic-report.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R code of the Learning Technology & Analytics Research Group of THUAS
# Copyright 2025 THUAS
# Web Page: http://www.thuas.com
# Contact: Theo Bakker (t.c.bakker@hhs.nl)
# Distribution outside THUAS: Yes
#
# Purpose: Rendering quarto profile: basic-report
#
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Set minimal libraries
library(here)
library(cli)
library(rvest)
library(stringr)
library(glue)
library(quarto)

# Define the parameters for the quarto file
lExecute_params <- list(
  opleiding                 = "CMD", # CMD or VD
  opleidingsvorm_afkorting  = "VT"
)

# Delete the _freeze folder (bug in Quarto)
if (dir.exists("_freeze")) {
  unlink("_freeze", recursive = TRUE)
}

# Test if model building for the current study programme has been executed, if not, alert
if(file.exists(here::here("_advanced-report", "ch-models.html"))) {

  # Define text and search string
  sText_content  <- html_text(read_html(here::here("_advanced-report", "ch-models.html")))
  sSearch_string <- sprintf("(%s)", lExecute_params$opleiding)
  
  # Check if the search string is found in the text content
  if (!str_detect(sText_content, sSearch_string)) {
    cli_alert_warning(glue("Voor de opleiding {lExecute_params$opleiding} ",
                           "moet eerst het advanced-report worden uitgevoerd."))
  } else {
    
    cli_alert_info(glue("Voor de opleiding {lExecute_params$opleiding} ",
                        "is het advanced-report uitgevoerd. \n",
                        "Het basic-report wordt nu uitgevoerd."))
    
    # Render a basic report
    withr::with_envvar(new = c("QUARTO_PROFILE" = "basic-report"), {
      quarto::quarto_render(execute_params = lExecute_params,
                            as_job = FALSE)
    })
    
  }
  
} else {
  cli_alert_warning(glue("Voor de opleiding {lExecute_params$opleiding} ",
                         "moet eerst het advanced-report worden uitgevoerd."))
}

