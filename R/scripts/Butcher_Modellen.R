## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Butcher_Modellen.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Lectoraat Learning Technology & Analytics De Haagse Hogeschool
## Copyright 2024 De HHs
## Web Page: http://www.hhs.nl
## Contact: Theo Bakker (t.c.bakker@hhs.nl)
## Verspreiding buiten De HHs: Nee
##
## Doel: Doel
##
## Afhankelijkheden: Afhankelijkheid
##
## Datasets: Datasets
##
## Opmerkingen:
## 1) Geen.
## 2) ___
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Geschiedenis:
## 07-06-2024: TB: Aanmaak bestand
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(butcher)

## Calculate the size of objects
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

## List all lr objects except lr_plot and lr_plot_plus
lr_list <- list(lr_auc_highest,
                lr_best,
                lr_mod,
                lr_recipe,
                lr_res,
                lr_workflow)

obj_size(lr_list)

## Save - last_fit

