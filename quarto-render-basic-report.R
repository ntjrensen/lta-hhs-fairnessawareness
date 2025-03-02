# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# quarto-render-basic-report.R ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R code of the Learning Technology & Analytics Research Group of THUAS
# Copyright 2025 THUAS
# Web Page: http://www.thuas.com
# Contact: Theo Bakker (t.c.bakker@hhs.nl)
# Distribution outside THUAS: Yes
#
# Purpose: Rendering quarto profile: basic-report
#
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Set minimal libraries
library(here)
library(cli)
library(rvest)
library(stringr)
library(glue)
library(quarto)

# Define the parameters for the quarto file
execute_params_list <- list(
  sp       = "CMD", # CMD or VD
  sp_form  = "VT"
)

# Delete the _freeze folder (bug in Quarto)
if (dir.exists("_freeze")) {
  unlink("_freeze", recursive = TRUE)
}

# Test if model building for the current study programme has been executed, if not, alert
if (file.exists(here::here("_advanced-report", "ch-models.html"))) {

  # Define text and search string
  text_content  <- html_text(read_html(here::here("_advanced-report", "ch-models.html")))
  search_string <- sprintf("(%s)", execute_params_list$sp)
  
  # Check if the search string is found in the text content
  if (!str_detect(text_content, search_string)) {
    cli_alert_warning(glue("First, render the advanced-report ",
                           "for the study programme {execute_params_list$sp}."))
  } else {
    
    cli_alert_info(glue("The advanced-report was rendered for the study programme ",
                        "{execute_params_list$sp} \n",
                        "The basic-report is rendered now."))
    
    # Render a basic report
    withr::with_envvar(new = c("QUARTO_PROFILE" = "basic-report"), {
      quarto::quarto_render(execute_params = execute_params_list,
                            as_job = FALSE)
    })
    
  }
  
} else {
  cli_alert_warning(glue("First, render the advanced-report ",
                         "for the study programme {execute_params_list$sp}."))
}
