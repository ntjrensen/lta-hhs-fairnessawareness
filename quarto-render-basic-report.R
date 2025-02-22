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
library(quarto)
library(here)

# Define the parameters for the quarto file
lExecute_params <- list(
  opleiding                 = "CMD", # CMD or VD
  opleidingsvorm_afkorting  = "VT"
)

# Delete the _freeze folder (bug in Quarto)
if (dir.exists("_freeze")) {
  unlink("_freeze", recursive = TRUE)
}

# Render a basic report
withr::with_envvar(new = c("QUARTO_PROFILE" = "basic-report"), {
  quarto::quarto_render(execute_params = lExecute_params,
                        as_job = FALSE)
})
