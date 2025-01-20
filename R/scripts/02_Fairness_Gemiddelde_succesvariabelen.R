## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Fairness_Gemiddelde_succesvariabelen.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Lectoraat Learning Technology & Analytics De Haagse Hogeschool
## Copyright 2024 De HHs
## Web Page: http://www.hhs.nl
## Contact: Theo Bakker (t.c.bakker@hhs.nl)
## Verspreiding buiten De HHs: Nee
##
## Doel: Een totaal overzicht van bias per opleiding
##
## Afhankelijkheden: ltabase package
##
## Datasets: Geen
##
## Opmerkingen:
## 1) Geen.
## 2) ___
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Geschiedenis:
## 19-08-2024: TB: Aanmaak bestand
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INCLUDE PREPARATIONS  ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("R/scripts/Include_Voorbereidingen_Verdieping.R")

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. LOAD DATA ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.1 Active trainers ####

Get_Actieve_Opleidingen <- function() {
  
  dfOpleidingen |>
    
    ## Combine the study programme and type of education into a new variable
    mutate(INS_Opleiding_en_Opleidingsvorm = paste(INS_Opleiding, INS_Opleidingsvorm, sep = "_")) |>
    
    ## Limit selection to study programmes that are still in existence in 2022 
    ## and have more than 1 year's worth of data
    dplyr::filter(INS_Collegejaar_max == 2022,
                  INS_Collegejaar_min < 2022) |>
    dplyr::select(INS_Opleiding_en_Opleidingsvorm, 
                  INS_Faculteit,
                  INS_Opleiding,
                  INS_Opleidingsvorm,
                  everything())
}

## List active study programmes.
dfOpleidingen_actief <- Get_Actieve_Opleidingen()

lDfOpleidingen <- list()

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.3 All freshmen ####

## Loop over dfOpleidingen_actief and load the dataset with all freshmen
for(i in 1:nrow(dfOpleidingen_actief)) {
  
  ## Load the dataset with all freshmen
  lDfOpleidingen[[i]] <- get_lta_studyprogram_enrollments_pin(
    board = "HHs/Inschrijvingen",
    faculty = dfOpleidingen_actief$INS_Faculteit[i],
    studyprogram = dfOpleidingen_actief$INS_Opleidingsnaam_huidig[i],
    studytrack = dfOpleidingen_actief$INS_Opleiding[i],
    studyform = toupper(dfOpleidingen_actief$INS_Opleidingsvorm[i]),
    range = "eerstejaars")
}

## Combine the datasets into 1 large df
dfOpleidingen_alle_eerstejaars <- bind_rows(lDfOpleidingen)

names(dfOpleidingen_alle_eerstejaars) 

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. DETERMINE STUDY SUCCESS DATA ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.1 Switch ####

## Determine the average switch by study programme
dfSwitch <- dfOpleidingen_alle_eerstejaars |>
  mutate(INS_Switch = ifelse(INS_Aansluiting == "Switch", TRUE, FALSE),
         INS_Switch_extern = ifelse(INS_Aansluiting_LTA == "Switch extern", TRUE, FALSE),
         INS_Switch_intern = ifelse(INS_Aansluiting_LTA == "Switch intern", TRUE, FALSE))

dfSwitch_per_opleiding <- dfSwitch |>
  dplyr::group_by(INS_Opleiding,
                  INS_Opleidingsvorm) |>
  dplyr::summarise(INS_Switch_M = mean(INS_Switch, na.rm = TRUE),
                   INS_Switch_extern_M = mean(INS_Switch_extern, na.rm = TRUE),
                   INS_Switch_intern_M = mean(INS_Switch_intern, na.rm = TRUE),
                   INS_Switch_SD = sd(INS_Switch, na.rm = TRUE),
                   INS_Switch_extern_SD = sd(INS_Switch_extern, na.rm = TRUE),
                   INS_Switch_intern_SD = sd(INS_Switch_intern, na.rm = TRUE)) |>
  dplyr::ungroup() |> 
  arrange(-INS_Switch_M)

## Average switch by study programme across THUAS
dfSwitch_per_opleiding_gemiddeld <- dfSwitch_per_opleiding |>
  dplyr::summarise(INS_Switch_M_HHs = mean(INS_Switch_M, na.rm = TRUE),
                   INS_Switch_intern_M_HHs = mean(INS_Switch_intern_M, na.rm = TRUE),
                   INS_Switch_extern_M_HHs = mean(INS_Switch_extern_M, na.rm = TRUE),
                   INS_Switch_SD_HHs = sd(INS_Switch_M, na.rm = TRUE),
                   INS_Switch_intern_SD_HHs = sd(INS_Switch_intern_M, na.rm = TRUE),
                   INS_Switch_extern_SD_HHs = sd(INS_Switch_extern_M, na.rm = TRUE)) |> 
  mutate(INS_Switch_M_HHs = round(INS_Switch_M_HHs * 100, 1),
         INS_Switch_extern_M_HHs = round(INS_Switch_extern_M_HHs * 100, 1),
         INS_Switch_intern_M_HHs = round(INS_Switch_intern_M_HHs * 100, 1),
         INS_Switch_SD_HHs = round(INS_Switch_SD_HHs * 100, 1),
         INS_Switch_extern_SD_HHs = round(INS_Switch_extern_SD_HHs * 100, 1),
         INS_Switch_intern_SD_HHs = round(INS_Switch_intern_SD_HHs * 100, 1))

## Average switch for the entire THUAS (not grouped).
dfSwitch_HHs <- dfSwitch |>
  dplyr::summarise(INS_Switch_M_HHs = mean(INS_Switch, na.rm = TRUE),
                   INS_Switch_intern_M_HHs = mean(INS_Switch_intern, na.rm = TRUE),
                   INS_Switch_extern_M_HHs = mean(INS_Switch_extern, na.rm = TRUE)) |> 
  mutate(INS_Switch_M_HHs = round(INS_Switch_M_HHs * 100, 1),
         INS_Switch_extern_M_HHs = round(INS_Switch_extern_M_HHs * 100, 1),
         INS_Switch_intern_M_HHs = round(INS_Switch_intern_M_HHs * 100, 1))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.2 Dropout after 1 year ####

## By study programme (based on SUC_Uitval_na_1_jaar_tf)
dfUitval_per_opleiding_jr1 <- dfOpleidingen_alle_eerstejaars |>
  dplyr::group_by(INS_Opleiding,
                  INS_Opleidingsvorm) |>
  dplyr::summarise(INS_Uitval_jr1_M = mean(SUC_Uitval_na_1_jaar_tf, na.rm = TRUE)) |>
  dplyr::ungroup() |> 
  arrange(-INS_Uitval_jr1_M)

## HHs (based on SUC_Uitval_na_1_jaar_tf)
dfUitval_per_opleiding_jr1_HHs <- dfOpleidingen_alle_eerstejaars |>
  dplyr::summarise(INS_Uitval_jr1_M_HHs = mean(SUC_Uitval_na_1_jaar_tf, na.rm = TRUE)) 

## By study programme (based on LTA)
dfUitval_per_opleiding_X1 <- dfOpleidingen_alle_eerstejaars |>
  dplyr::filter(SUC_Uitval_aantal_jaar_cat_LTA == 'Na 1 jaar') |>
  mutate(SUC_Uitval_na_1_jaar_met_P_tf = ifelse(SUC_Uitval_na_1_jaar_tf == T & SUC_P_na_1_jaar_tf == T, T, F)) |>
  dplyr::group_by(INS_Opleiding,
                  INS_Opleidingsvorm) |>
  dplyr::summarise(INS_Uitval_jr1_met_P_M = mean(SUC_Uitval_na_1_jaar_met_P_tf, na.rm = TRUE)) |>
  dplyr::ungroup() |> 
  arrange(-INS_Uitval_jr1_met_P_M)

## HHs (based on LTA)
dfUitval_per_opleiding_HHs_X1 <- dfOpleidingen_alle_eerstejaars |>
  dplyr::filter(SUC_Uitval_aantal_jaar_cat_LTA == 'Na 1 jaar') |>
  mutate(SUC_Uitval_na_1_jaar_met_P_tf = ifelse(SUC_P_na_1_jaar_tf == T, T, F)) |>
  dplyr::summarise(INS_Uitval_jr1_met_P_M_HHs = mean(SUC_Uitval_na_1_jaar_met_P_tf, na.rm = TRUE)) |> 
  mutate(INS_Uitval_jr1_met_P_M_HHs = round(INS_Uitval_jr1_met_P_M_HHs * 100, 1))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.3 Dropout after 2 years ####

## By study programme (based on LTA)
dfUitval_per_opleiding_X2 <- dfOpleidingen_alle_eerstejaars |>
  mutate(SUC_Uitval_na_2_jaar_tf = ifelse(SUC_Uitval_aantal_jaar_cat_LTA == 'Na 1 jaar' | SUC_Uitval_aantal_jaar_cat_LTA == 'Na 2 jaar', 
                                          T, F)) |>
  dplyr::group_by(INS_Opleiding,
                  INS_Opleidingsvorm) |>
  dplyr::summarise(INS_Uitval_jr2_M = mean(SUC_Uitval_na_2_jaar_tf, na.rm = TRUE)) |>
  dplyr::ungroup() |> 
  arrange(-INS_Uitval_jr2_M)

## HHs (based on LTA)
dfUitval_HHs_X2 <- dfOpleidingen_alle_eerstejaars |>
  mutate(SUC_Uitval_na_2_jaar_tf = ifelse(SUC_Uitval_aantal_jaar_cat_LTA == 'Na 1 jaar' | SUC_Uitval_aantal_jaar_cat_LTA == 'Na 2 jaar', T, F)) |>
  dplyr::filter(SUC_Uitval_aantal_jaar_cat_LTA == 'Na 1 jaar' | SUC_Uitval_aantal_jaar_cat_LTA == 'Na 2 jaar' ) |>
  mutate(SUC_Uitval_na_2_jaar_met_P_tf = ifelse(SUC_P_na_2_jaar_tf == T, T, F)) |>
  dplyr::summarise(INS_Uitval_jr2_met_P_M = mean(SUC_Uitval_na_2_jaar_met_P_tf, na.rm = TRUE)) |> 
  mutate(INS_Uitval_jr2_met_P_M = round(INS_Uitval_jr2_met_P_M * 100, 1))

names(dfOpleidingen_alle_eerstejaars)

table(dfOpleidingen_alle_eerstejaars$BSA_Advies)
table(dfOpleidingen_alle_eerstejaars$BSA_Advies_huidig)
table(dfOpleidingen_alle_eerstejaars$BSA_Advies_collegejaar_huidig)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.4 Interim disenrolled ####

dfTussentijds_uitgeschreven <- dfOpleidingen_alle_eerstejaars |>
  dplyr::filter(SUC_Uitval_aantal_jaar_cat_LTA != 'Na 1 jaar') |>
  
  ## Als een student een staker is en niet uitvalt na 1 jaar, is het een tussentijdse uitschrijving
  mutate(SUC_Tussentijds_uitgeschreven_tf = ifelse(BSA_Advies_collegejaar_huidig == 'Staker', T, F)) |>
  
  ## Bereken aantallen
  dplyr::summarise(SUC_Tussentijds_uitgeschreven_M = mean(SUC_Tussentijds_uitgeschreven_tf, na.rm = TRUE)) |> 
  mutate(SUC_Tussentijds_uitgeschreven_M = round(SUC_Tussentijds_uitgeschreven_M * 100, 1))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.5 Retention after 1 year ####

dfRetentie_per_opleiding_jr1 <- dfOpleidingen_alle_eerstejaars |>
  dplyr::group_by(INS_Opleiding,
                  INS_Opleidingsvorm) |>
  
  ## Calculate retention after 1 year as 1 - average dropout after 1 year
  dplyr::summarise(INS_Retentie_jr1_M = 1 - mean(SUC_Uitval_na_1_jaar_tf, na.rm = TRUE)) |>
  dplyr::ungroup() |> 
  arrange(-INS_Retentie_jr1_M)

## Retention after 1 year as rds (needed for calculation of Lift)
saveRDS(dfRetentie_per_opleiding_jr1, "10. Output/all/dfRetentie_per_opleiding_jr1.rds")

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. CLEAN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(list = ls())
