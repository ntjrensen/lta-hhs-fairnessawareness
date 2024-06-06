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
# 
# student_x <- dfOpleiding_inschrijvingen[3,]

# Uitval 
# Aanmelding 
# Aansluiting 
# APCG 
# Cijfer_CE_Engels 
# Cijfer_CE_Engels_missing 
# Cijfer_CE_Natuurkunde
# Cijfer_CE_Natuurkunde_missing 
# Cijfer_CE_Nederlands 
# Cijfer_CE_Nederlands_missing 
# Cijfer_CE_VO 
# Cijfer_CE_VO_missing
# Cijfer_CE_Wiskunde 
# Cijfer_CE_Wiskunde_missing 
# Cijfer_SE_VO 
# Cijfer_SE_VO_missing 
# Collegejaar 
# Dubbele_studie
# Geslacht
# ID
# Leeftijd
# Reistijd 
# SES_Arbeid 
# SES_Totaal 
# SES_Welvaart 
# Studiekeuzeprofiel
# Vooropleiding

dfPersona <- dfOpleiding_inschrijvingen |>
  
  ## Split de opleidingen op basis van de Vooropleiding
  group_by(Vooropleiding) |>
  
  ## Maak een persona aan op basis van de overige variabelen: 
  ## kies de meest voorkomende waarden per variabele bij categorieën en de mediaan bij numerieke variabelen
  summarise(
    
    ## Categorische variabelen
    Aansluiting                   = names(which.max(table(Aansluiting))),
    APCG                          = names(which.max(table(APCG))),
    Dubbele_studie                = names(which.max(table(Dubbele_studie))),
    Cijfer_CE_Engels_missing      = names(which.max(table(Cijfer_CE_Engels_missing))),
    Cijfer_CE_Natuurkunde_missing = names(which.max(table(Cijfer_CE_Natuurkunde_missing))),
    Cijfer_CE_Nederlands_missing  = names(which.max(table(Cijfer_CE_Nederlands_missing))),
    Cijfer_CE_VO_missing          = names(which.max(table(Cijfer_CE_VO_missing))),
    Cijfer_CE_Wiskunde_missing    = names(which.max(table(Cijfer_CE_Wiskunde_missing))),
    Cijfer_SE_VO_missing          = names(which.max(table(Cijfer_SE_VO_missing))),
    Geslacht                      = names(which.max(table(Geslacht))),
    Studiekeuzeprofiel            = names(which.max(table(Studiekeuzeprofiel))),
    
    ## Numerieke variabelen
    Aanmelding            = round(median(Aanmelding, na.rm = TRUE), 1),
    Cijfer_CE_Engels      = round(median(Cijfer_CE_Engels, na.rm = TRUE), 1),
    Cijfer_CE_Natuurkunde = round(median(Cijfer_CE_Natuurkunde, na.rm = TRUE), 1),
    Cijfer_CE_Nederlands  = round(median(Cijfer_CE_Nederlands, na.rm = TRUE), 1),
    Cijfer_CE_VO          = round(median(Cijfer_CE_VO, na.rm = TRUE), 1),
    Cijfer_CE_Wiskunde    = round(median(Cijfer_CE_Wiskunde, na.rm = TRUE), 1),
    Cijfer_SE_VO          = round(median(Cijfer_SE_VO, na.rm = TRUE), 1),
    Leeftijd              = round(median(Leeftijd, na.rm = TRUE), 1),
    Reistijd              = round(median(Reistijd, na.rm = TRUE), 1),
    SES_Arbeid            = round(median(SES_Arbeid, na.rm = TRUE), 1),
    SES_Welvaart          = round(median(SES_Welvaart, na.rm = TRUE), 1),
    SES_Totaal            = round(median(SES_Totaal, na.rm = TRUE), 1),
    
    ## Overige variabelen
    Uitval                = names(which.max(table(Uitval))),
    Collegejaar           = median(Collegejaar),
    ID                    = NA,                              
    
    .groups = "drop")
