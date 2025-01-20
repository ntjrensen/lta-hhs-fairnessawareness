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
## 2. LOAD BESTANDEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.1 Make a list of last-fits.rds ####

dir <- "10. Output"
lFiles <- list.files(path = dir, recursive = TRUE, pattern = "*last-fits.rds", full.names = TRUE)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.2 Load the data ####

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.2.1 Basic models ####

# Function to extract faculty, study programmes, form and type of analysis from the path
dfModels_HHs <- bind_rows(lapply(lFiles, function(file) {
  df <- readRDS(file)[["Logistic Regression"]][[".metrics"]] 
  return(df)
}))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.2.2 Retention after 1 year per study programme ####
dfRetentie_per_opleiding_jr1 <- readRDS("10. Output/all/dfRetentie_per_opleiding_jr1.rds")

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. EDIT FILES ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.1 Conservation only accuracy and value ####

dfModels_HHs.1 <- dfModels_HHs |> 
  dplyr::filter(`.metric` == "accuracy") |> 
  rename(MDL_Metric = `.metric`,
         MDL_Value = `.estimate`) |>
  select(MDL_Metric, MDL_Value)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.2 Link to study programmes ####

# Function to retrieve the names from the path
Split_files_to_opleidingsnamen <- function(path) {
  
  # Extract the relevant string from the file path
  filename <- basename(path)
  
  # Remove the extension and other irrelevant parts from the file name
  pattern <- "_retentie_na_1_jaar_nvt_last-fits.rds"
  model_name <- sub(pattern, "", filename)
  
  # split the string: 
  # up to the 1st - = MDL_Faculteit
  # up to the 2nd - = MDL_Opleidingssoort
  # The last part (after the last -) = MMDL_Opleidingvorm
  # The rest is MDL_Opleiding
  
  # Splits de string op de - en haal de eerste 3 delen eruit
  split <- strsplit(model_name, "-")[[1]]
  
  MDL_Faculteit <- split[1]
  MDL_Opleidingssoort <- split[2]
  MDL_Opleidingvorm <- split[length(split)]
  MDL_Opleiding <- paste(split[3:(length(split) - 1)], collapse = "-")
  
  ## Return the results as a df
  return(tibble(
    MDL_Faculteit = MDL_Faculteit,
    MDL_Opleidingssoort = MDL_Opleidingssoort,
    MDL_Opleiding = MDL_Opleiding,
    MDL_Opleidingvorm = MDL_Opleidingvorm
  ))
  
}

# Combine to a df with training names
dfModels_HHs_Opleidingsnamen <- bind_rows(lapply(lFiles, function(file) {
  df <- Split_files_to_opleidingsnamen(file)
  return(df)
}))

## Combine both dataset
dfModels_HHs.2 <- dfModels_HHs.1 |> 
  bind_cols(dfModels_HHs_Opleidingsnamen) |> 
  select(MDL_Faculteit, MDL_Opleidingssoort, MDL_Opleiding, MDL_Opleidingvorm, MDL_Metric, MDL_Value) |> 
  arrange(MDL_Value)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.3 Link to retention ####

## Calculate the lift
dfLift <- dfModels_HHs.2 |> 
  left_join(dfRetentie_per_opleiding_jr1, 
            by = c("MDL_Opleiding" = "INS_Opleiding",
                   "MDL_Opleidingvorm" = "INS_Opleidingsvorm")) |> 
  mutate(MDL_Basemodel = ifelse(INS_Retentie_jr1_M > 0.5, 
                                INS_Retentie_jr1_M, 1 - INS_Retentie_jr1_M)) |> 
  mutate(MDL_Lift = MDL_Value - MDL_Basemodel)

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. MAKE CALCULATIONS ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4.1 Calculate the M and SD of accuracy ####

## Mean accuracy and SD
mean(dfModels_HHs.2$MDL_Value)
sd(dfModels_HHs.2$MDL_Value)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4.2 Calculate the M and SD of lift ####

## Mean Lift and SD
mean(dfLift$MDL_Lift) * 100
sd(dfLift$MDL_Lift) * 100

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5. CLEAN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(list = ls())
