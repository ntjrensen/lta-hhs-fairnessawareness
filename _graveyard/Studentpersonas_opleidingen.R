## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Studentpersonas_opleidingen.R ####
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
## 06-06-2024: TB: Aanmaak bestand
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INSPECTEER dfOpleiding_inschrijvingen ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.1 Namen en inhoud ####

# names(dfOpleiding_inschrijvingen)

# [1] "Uitval"                        "Aanmelding"                    "Aansluiting"                  
# [4] "APCG"                          "Cijfer_CE_Engels"              "Cijfer_CE_Engels_missing"     
# [7] "Cijfer_CE_Natuurkunde"         "Cijfer_CE_Natuurkunde_missing" "Cijfer_CE_Nederlands"         
# [10] "Cijfer_CE_Nederlands_missing"  "Cijfer_CE_VO"                  "Cijfer_CE_VO_missing"         
# [13] "Cijfer_CE_Wiskunde"            "Cijfer_CE_Wiskunde_missing"    "Cijfer_SE_VO"                 
# [16] "Cijfer_SE_VO_missing"          "Collegejaar"                   "Dubbele_studie"               
# [19] "Geslacht"                      "ID"                            "Leeftijd"                     
# [22] "Reistijd"                      "SES_Arbeid"                    "SES_Totaal"                   
# [25] "SES_Welvaart"                  "Studiekeuzeprofiel"            "Vooropleiding"    

# glimpse(dfOpleiding_inschrijvingen)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.2 Functies ####


## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BOUW PERSONAS ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Maak een lijst van dfPersonas
lDfPersona <- list()

## Loop over de variabelen
lDfPersona <- map(c("Geslacht", "Vooropleiding", "Aansluiting"),
                  ~ Get_dfPersona(.x)) |>
  set_names(c("Geslacht", "Vooropleiding", "Aansluiting"))
