## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 00. Voorbereidingen.R
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Lectoraat Learning Technology & Analytics De Haagse Hogeschool
## Copyright 2024 De HHs
## Web Page: http://www.hhs.nl
## Contact: Theo Bakker (t.c.bakker@hhs.nl)
## Verspreiding buiten De HHs: Nee
##
## Doel: De working directory wordt bepaald door de locatie van het project
## De specifieke functies en libraries voor dit project worden ingeladen
##
## Afhankelijkheden: Geen
##
## Datasets: Geen
##
## Opmerkingen:
## 1) ...
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## TODO:
## 1) Geen
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Geschiedenis:
## 24-02-2023: TB: Aanmaak bestand
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 00. Voorbereidingen.R
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Lectoraat Learning Technology & Analytics De Haagse Hogeschool
## Copyright 2024 De HHs
## Web Page: http://www.hhs.nl
## Contact: Theo Bakker (t.c.bakker@hhs.nl)
## Verspreiding buiten De HHs: Nee
##
## Doel: De specifieke functies en libraries voor dit project worden ingeladen
##
## Afhankelijkheden: geen
##
## Datasets: geen
##
## Opmerkingen:
## 1) ...
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## TODO:
## 1) Geen
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Geschiedenis:
## 24-02-2023: TB: Aanmaak bestand; een aantal onderdelen uitgezet
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 0. LAAD LTABASE PACKAGE + DEFAULT DATABASES ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 0.1 Standaard algemene package die direct nodig zijn ####

## Installeer here, cli en icecream indien nodig
packages <- c("here", "cli", "icecream")

purrr::walk(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
})

# for (i in c("here", "cli", "icecream")) {
#   if(!requireNamespace(i, quietly = TRUE)) {
#     install.packages(i)
#   }
#   library(i, character.only = TRUE)
# }

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 0.2 ltabase package in (installeer indien nodig) ####

source("99. Functies & Libraries/Inladen_ltabase.R")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 0.3 Default datasets: dfOpleidigen, sectors, studytypes, studyforms ####

## Laad de default datasets: dfOpleidigen, sectors, studytypes, studyforms
ltabase::load_lta_datasets(message = TRUE)

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. BASISVARIABELEN / PADEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Als LTA_ROOT, LTA_DATA of LTA_BOARD niet bestaan, dan wordt de omgeving opnieuw ingesteld
ltabase::set_lta_sys_env()

## Bepaal de netwerkdirectory
Network_directory <- ltabase::get_lta_network_directory()

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. DEBUG ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Stel de debug opties in: icecream package settings
ltabase::set_icecream_options()
icecream::ic_disable()

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. PACKAGES ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Installeer en laad de packages + default settings
install_and_load_packages(plot = TRUE, spatial = TRUE, message = TRUE)

## Extra packages voor repo (tijdelijk)
# library(git2r)
#
# conflicts_prefer(testthat::matches)

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. SET SEED ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Bepaal de seed die gebruikt wordt voor hashing
set.seed(79515660)

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5. DOCUMENTATIE ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Lees de documentatie in naar het geheugen
Documentatie <- ltabase::get_lta_documentation_enrollments(network_directory = Network_directory)

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 6. COLORS ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Lees algemene Tableau kleuren in: deze moeten vooraf bekend zijn.
lColors_Tableau <- ltabase::get_colors_tableau()

# [1] "#fc7d0b" "#1170aa" "#a3acb9" "#c85200" "#a3cce9" "#57606c" "#ffbc79" "#5fa2ce" "#7b848f" "#f28e2b" "#c8d0d9"
# [12] "#b6992d" "#f1ce63" "#9467BD" "#C5B0D5" "#E377C2" "#F7B6D2" "#17BECF" "#9EDAE5" "#8C564b" "#C49C94" "#2CA02C"

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 7. CONFLICTS ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

conflicts_prefer(rlang::set_names, .quiet = TRUE)

cli::cli_alert_warning("rlang::set_names heeft de voorkeur")

conflicts_prefer(purrr::`%||%`)

cli::cli_alert_warning("purrr::`%||%` heeft de voorkeur")
