## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Lift_HHs.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Lectoraat Learning Technology & Analytics De Haagse Hogeschool
## Copyright 2024 De HHs
## Web Page: http://www.hhs.nl
## Contact: Theo Bakker (t.c.bakker@hhs.nl)
## Verspreiding buiten De HHs: Nee
##
## Doel: Een totaal overzicht van de lift per opleiding
##
## Afhankelijkheden: ltabase package en 02. Fairness_Gemiddelde_succesvariabelen.R
##
## Datasets: Geen
##
## Opmerkingen:
## 1) Geen.
## 2) ___
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Geschiedenis:
## 04-08-2024: TB: Aanmaak bestand
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INCLUDE VOORBEREIDINGEN  ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("R/scripts/Include_Voorbereidingen_Verdieping.R")

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. LAAD BESTANDEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.1 Maak een lijst van last-fits.rds ####

dir <- "10. Output"
lFiles <- list.files(path = dir, recursive = TRUE, pattern = "*last-fits.rds", full.names = TRUE)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.2 Laad de data ####

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.2.1 Basis models ####

# Functie om de faculteit, opleiding, vorm en soort analyse te extraheren uit het pad
dfModels_HHs <- bind_rows(lapply(lFiles, function(file) {
  df <- readRDS(file)[["Logistic Regression"]][[".metrics"]] 
  return(df)
}))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.2.2 Retentie na 1 jaar per opleiding ####
dfRetentie_per_opleiding_jr1 <- readRDS("10. Output/all/dfRetentie_per_opleiding_jr1.rds")

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. BEWERK BESTANDEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.1 Behoud alleen accuracy en waarde ####

dfModels_HHs.1 <- dfModels_HHs |> 
  dplyr::filter(`.metric` == "accuracy") |> 
  rename(MDL_Metric = `.metric`,
         MDL_Value = `.estimate`) |>
  select(MDL_Metric, MDL_Value)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.2 Koppel aan opleidingen ####

# Functie om de namen te herleiden uit het pad
Split_files_to_opleidingsnamen <- function(path) {
  
  # Extract de relevante string uit het bestandspad
  filename <- basename(path)
  
  # Verwijder de extensie en andere irrelevante delen uit de bestandsnaam
  pattern <- "_retentie_na_1_jaar_nvt_last-fits.rds"
  model_name <- sub(pattern, "", filename)
  
  # Splits de string: 
  # tot aan de 1e - = MDL_Faculteit
  # tot aan de 2e - = MDL_Opleidingssoort
  # Het laatste deel (na de laatste -) = MDL_Opleidingvorm
  # De rest is MDL_Opleiding
  
  # Splits de string op de - en haal de eerste 3 delen eruit
  split <- strsplit(model_name, "-")[[1]]
  
  MDL_Faculteit <- split[1]
  MDL_Opleidingssoort <- split[2]
  MDL_Opleidingvorm <- split[length(split)]
  MDL_Opleiding <- paste(split[3:(length(split) - 1)], collapse = "-")
  
  ## Geeft de resultaten terug als een df
  return(tibble(
    MDL_Faculteit = MDL_Faculteit,
    MDL_Opleidingssoort = MDL_Opleidingssoort,
    MDL_Opleiding = MDL_Opleiding,
    MDL_Opleidingvorm = MDL_Opleidingvorm
  ))
  
}

# Combineer tot een df met opleidingsnamen
dfModels_HHs_Opleidingsnamen <- bind_rows(lapply(lFiles, function(file) {
  df <- Split_files_to_opleidingsnamen(file)
  return(df)
}))

## Combineer beide dataset
dfModels_HHs.2 <- dfModels_HHs.1 |> 
  bind_cols(dfModels_HHs_Opleidingsnamen) |> 
  select(MDL_Faculteit, MDL_Opleidingssoort, MDL_Opleiding, MDL_Opleidingvorm, MDL_Metric, MDL_Value) |> 
  arrange(MDL_Value)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.3 Koppel aan de retentie ####

## Bereken de lift
dfLift <- dfModels_HHs.2 |> 
  left_join(dfRetentie_per_opleiding_jr1, 
            by = c("MDL_Opleiding" = "INS_Opleiding",
                   "MDL_Opleidingvorm" = "INS_Opleidingsvorm")) |> 
  mutate(MDL_Basemodel = ifelse(INS_Retentie_jr1_M > 0.5, 
                                INS_Retentie_jr1_M, 1 - INS_Retentie_jr1_M)) |> 
  mutate(MDL_Lift = MDL_Value - MDL_Basemodel)

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. MAAK BEREKENINGEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4.1 Bereken de M en SD van accuraatheid ####

## Gemiddelde accuraatheid en SD
mean(dfModels_HHs.2$MDL_Value)
sd(dfModels_HHs.2$MDL_Value)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4.2 Bereken de M en SD van lift ####

## Gemiddelde lift en SD
mean(dfLift$MDL_Lift) * 100
sd(dfLift$MDL_Lift) * 100

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5. RUIM OP ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(list = ls())
