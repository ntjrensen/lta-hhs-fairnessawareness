# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# butcher.models.R ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R code of the Learning Technology & Analytics Research Group of THUAS
# Copyright 2025 THUAS
# Web Page: http://www.thuas.com
# Contact: Theo Bakker (t.c.bakker@hhs.nl)
# Distribution outside THUAS: Yes
#
# Purpose: Calculate the size of the objects and save the last fit
#
# Dependencies: butcher library
#
# Datasets: None
#
# Remarks:
# 1) None.
# 2) ___
#
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Version history:
# 07-06-2024: TB: File creation
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(butcher)

# Calculate the size of objects
obj_size(lr_auc)
obj_size(lr_auc_highest)
obj_size(lr_best)
obj_size(lr_mod)
obj_size(lr_plot)
obj_size(lr_plot_plus)
obj_size(lr_recipe)
obj_size(lr_res)
obj_size(lr_workflow)

weigh(lr_res, threshold = 0, units = "MB")

# List all lr objects except lr_plot and lr_plot_plus
lr_list <- list(lr_auc_highest,
                lr_best,
                lr_mod,
                lr_recipe,
                lr_res,
                lr_workflow)

obj_size(lr_list)

# Save - last_fit

