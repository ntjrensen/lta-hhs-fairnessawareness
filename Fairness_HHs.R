## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Fairness_HHs.R ####
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
## 04-08-2024: TB: Aanmaak bestand
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. LAAD LTABASE PACKAGE ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.1 Standaard packages die direct nodig zijn ####

## Installeer here, cli en icecream indien nodig
for (i in c("here", "cli", "icecream")) {
  if(!requireNamespace(i, quietly = TRUE)) {
    install.packages(i)
  }
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.2 ltabase package in (installeer indien nodig) ####

source("99. Functies & Libraries/Inladen_ltabase.R")
source("99. Functies & Libraries/Fairness_functies.R")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.3 Default datasets: dfOpleidigen, sectors, studytypes, studyforms ####

## Laad de default datasets: dfOpleidigen, sectors, studytypes, studyforms
ltabase::load_lta_datasets(message = TRUE)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.4 Laad extra bibliotheken ####

## Laad extra bibliotheken
library(conflicted)   # om conflicten op te lossen
library(tidymodels)   # voor machine learning

library(vip)          # voor variable importance plots
library(forcats)      # om factor variabelen te bewerken
library(performance)  # voor performance metingen op lr modellen
#library(dlookr)      # om data te inspecteren > geeft conflicten vanwege showtext_auto()
library(gtsummary)    # voor beschrijvende summary tabellen
library(flextable)    # voor flextables
library(officer)      # voor opmaak in tabellen
library(gt)           # voor tabellen
library(gtExtras)     # voor sparklines
library(cli)          # voor cli teksten
library(glue)         # voor string interpolatie
library(probably)     # voor probabilistische modellen
library(discrim)      # discriminant analysis
library(klaR)         # voor classificatie en visualisatie
library(betacal)      # voor beta calibration

library(doParallel)   # voor parallel processing
library(DALEX)        # voor explainable AI
library(DALEXtra)     # voor explainable AI
library(lobstr)       # voor het meten van objecten
library(butcher)      # voor het verkleinen van modellen
library(iBreakDown)   # voor het uitleggen van modellen
library(ggtext)       # voor het maken van opmaak in titels

library(showtext)     # voor het instellen van lettertypes
library(ggplot2)      # voor het maken van plots
library(cvms)         # voor confusion matrices
library(ggimage)      # voor confusion matrices
library(rsvg)         # voor confusion matrices
library(ggnewscale)   # voor confusion matrices

library(ggpubr)       # voor het bewaren van plots
library(bbplot)       # voor het bewaren van plots
library(grid)         # voor het bewaren van plots

library(gridGraphics) # voor het bewaren van plots
library(extrafont)    # voor het bewaren van plots
library(sysfonts)     # voor fonts

library(fairmodels)   # voor fairness in modellen

library(fs)          # voor file system functies

library(quartostamp)  # voor extra quarto add-in functionaliteit

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.5 Fonts ####

extrafont::loadfonts(quiet = TRUE)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.6 Laad extra functies ####

source("99. Functies & Libraries/Rapport_functies.R")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.7 Kleuren ####

source("01. Includes/Include_Colors.R")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.8 Bepaal de voorkeur voor de thema's ####

Set_LTA_Theme()

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.9 Laad extra functies ####

source("99. Functies & Libraries/Rapport_functies.R")

## Bepaal de volgorde van een aantal levels
Get_Levels()

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.10 Conflicts ####

conflicts_prefer(dplyr::select)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.1 Netwerkpaden ####

# Als LTA_ROOT, LTA_DATA of LTA_BOARD niet bestaan, dan wordt de omgeving opnieuw ingesteld
ltabase::set_lta_sys_env()

## Bepaal de netwerkdirectory
Network_directory <- ltabase::get_lta_network_directory()



## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. LAAD BESTANDEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.1 Lijst van alle .rds bestanden in de directory ####

dir <- "10. Output"
lFiles <- list.files(path = dir, recursive = TRUE, pattern = "*.rds", full.names = TRUE)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.2 Behoud paden met 'fairness' ####

lFiles <- lFiles[grepl("fairness", lFiles)]

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.3 Laad en combineer de data ####

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.3.1 Basis ####

# Functie om de faculteit, opleiding, vorm en soort analyse te extraheren uit het pad
dfFairness_HHs <- bind_rows(lapply(lFiles, function(file) {
  df <- readRDS(file)
  df <- df |>
    mutate(
      FRN_Analyse = "Retentie na 1 jaar"
    )
  return(df)
}))

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. BEWERK BESTANDEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.1 Maak UUID en sub dataframes ####

dfVars <- tribble(
  ~FRN_Group, ~FRN_Subgroup,
  "Geslacht",      "M",
  "Geslacht",      "V",
  "Vooropleiding", "MBO",
  "Vooropleiding", "HAVO",
  "Vooropleiding", "VWO",
  "Vooropleiding", "BD",
  "Vooropleiding", "CD",
  "Vooropleiding", "HO",
  "Vooropleiding", "Overig",
  "Vooropleiding", "Onbekend",
  "Aansluiting",   "Direct",
  "Aansluiting",   "Tussenjaar",
  "Aansluiting",   "Switch extern",
  "Aansluiting",   "Switch intern",
  "Aansluiting",   "Na CD",
  "Aansluiting",   "2e Studie",
  "Aansluiting",   "Overig",
  "Aansluiting",   "Onbekend"
)

dfFairness_HHs <- dfFairness_HHs |> 
  mutate(FRN_UUID = paste(FRN_Faculteit, FRN_Opleidingstype, FRN_Opleiding, FRN_Opleidingsvorm, sep = "_")) |> 
  select(-c(FRN_Faculteit, FRN_Opleidingstype, FRN_Opleiding, FRN_Opleidingsvorm))

dfModel <- tibble(
  FRN_Model = unique(dfFairness_HHs$FRN_Model)
)

dfMetrics <- tibble(
  FRN_Metric = unique(dfFairness_HHs$FRN_Metric)
)

dfUUID <- tibble(
  FRN_UUID = unique(dfFairness_HHs$FRN_UUID)
)

dfSoortAnalyse <- tibble(
  FRN_Analyse = unique(dfFairness_HHs$FRN_Analyse)
)

dfVars_Metrics <- expand_grid(dfVars, dfModel, dfMetrics, dfUUID, dfSoortAnalyse)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.2 Join de dataframes ####

dfFairness_HHs.2 <- dfFairness_HHs |>
  full_join(
    dfVars_Metrics,
    by = join_by(
      FRN_Model,
      FRN_Group,
      FRN_Subgroup,
      FRN_Metric,
      FRN_UUID,
      FRN_Analyse
    )
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.3 Split FRN_UUID ####

## Splits de FRN_UUID in Faculteit, Opleidingstype, Opleiding en Opleidingsvorm
dfFairness_HHs.3 <- dfFairness_HHs.2 |>
  separate(
    FRN_UUID, into = c("FRN_Faculteit", 
                       "FRN_Opleidingstype", 
                       "FRN_Opleiding", 
                       "FRN_Opleidingsvorm"),
    sep = "_"
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.4 Voeg de naam van de opleiding toe ####

dfFairness_HHs.4 <- dfFairness_HHs.3 |>
  left_join(
    dfOpleidingen |> 
      select(INS_Opleiding, INS_Opleidingsnaam_huidig) |> 
      distinct(),
    by = c("FRN_Opleiding" = "INS_Opleiding")
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.5 Bepaal welke bias er is ####

dfFairness_HHs.5 <- dfFairness_HHs.4 |>
  mutate(
    FRN_Bias = case_when(
      is.na(FRN_Score) ~ NA,
      FRN_Score < 0.8 ~ "Negatieve Bias",
      FRN_Score > 1.25 ~ "Positieve Bias",
      .default = "Geen Bias"
    )
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.6 Bepaal of er enige bias is ####

dfFairness_HHs.6 <- dfFairness_HHs.5 |>
  select(-c(FRN_Model, FRN_Metric, FRN_Analyse, FRN_Fair, N)) |>
  group_by(FRN_Group, 
           FRN_Subgroup, 
           FRN_Bias,
           FRN_Faculteit,
           FRN_Opleidingsvorm,
           FRN_Opleiding,
           INS_Opleidingsnaam_huidig) |>
  
  ## Tel het aantal Bias per groep
  summarise(FRN_Bias_count = n(), 
            .groups = "drop") |> 
  select(FRN_Faculteit,
         INS_Opleidingsnaam_huidig,
         FRN_Opleiding,
         FRN_Opleidingsvorm,
         everything()) |> 

  ## Pivot de data
  pivot_wider(names_from = FRN_Bias, 
              values_from = c(FRN_Bias_count),
              values_fill = list(FRN_Bias_count = 0)) |> 
  
  ## Vervang NA door 0
  replace_na(list(`Geen Bias` = 0,
                  `Negatieve Bias` = 0,
                  `Positieve Bias` = 0)) |>
  
  ## Voeg Bias toe
  ## Pas de Bias aan
  mutate(Bias = case_when(
    `Negatieve Bias` > 1 | `Positieve Bias` > 1 ~ 'Ja',
    `Geen Bias` == 0 & `Negatieve Bias` == 0 & `Positieve Bias` == 0 ~ 'NTB',
    .default = "Nee")) |> 
  
  ## Hernoem de kolommen
  rename(Faculteit = FRN_Faculteit,
         Opleidingsnaam = INS_Opleidingsnaam_huidig,
         Opleiding = FRN_Opleiding,
         Opleidingsvorm = FRN_Opleidingsvorm,
         Variabele = FRN_Group,
         Groep = FRN_Subgroup) |>
  
  ## Sorteer de Variabele en Groep
  mutate(Variabele = factor(Variabele, 
                            levels = c("Geslacht", 
                                       "Vooropleiding", 
                                       "Aansluiting")),
         Groep = factor(Groep,
                        levels = c(lLevels_geslacht, 
                                   ## Maak lLevels_vop uniek om Overig en onbekend niet te herhalen
                                   setdiff(lLevels_vop, lLevels_aansluiting), 
                                   lLevels_aansluiting))
  ) |> 
  
  ## Selecteer de kolommen
  select(Faculteit, Opleidingsnaam, Opleiding, Opleidingsvorm,
         Variabele, Groep, Bias, `Geen Bias`, `Negatieve Bias`, `Positieve Bias`) |> 
  
  ## Sorteer de data
  arrange(Faculteit, Opleidingsnaam, Opleiding, Opleidingsvorm, 
          Variabele, Groep)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.7 Voeg tellingen toe

## Maak een dataframe voor alle opleidingen
tblOpleidingen <- dfFairness_HHs.6 |>
  select(Opleiding, Opleidingsvorm) |>
  distinct()

lInschrijvingen <- list()

## Loop over de opleidingen, haal de inschrijvingen op en voeg deze toe aan de lijst
for (i in 1:nrow(tblOpleidingen)) {
  
  ## Bepaal de opleiding en opleidingsvorm
  opleiding <- tblOpleidingen$Opleiding[i]
  opleidingsvorm <- tblOpleidingen$Opleidingsvorm[i]
  
  ## Maak de variabelen voor de huidige opleiding op basis van de opleidingsnaam en opleidingsvorm
  current_opleiding <- Get_Current_Opleiding(
    opleiding = opleiding,
    opleidingsvorm = toupper(opleidingsvorm)
  )
  
  ## Bepaal op basis hiervan afgeleide variabelen
  Set_Current_Opleiding_Vars(current_opleiding, debug = F)
  
  dfOpleiding_inschrijvingen_base <- get_lta_studyprogram_enrollments_pin(
    board = "HHs/Inschrijvingen",
    faculty = faculteit,
    studyprogram = opleidingsnaam_huidig,
    studytrack = opleiding,
    studyform = toupper(opleidingsvorm),
    range = "eerstejaars")
  
  lInschrijvingen[[i]] <- dfOpleiding_inschrijvingen_base
  
}

## Combineer de lijst met inschrijvingen
dfInschrijvingen <- bind_rows(lInschrijvingen)

## Maak een telling van de variabelen
dfTellingen <- dfInschrijvingen |>
  select(INS_Faculteit,
         INS_Opleiding,
         INS_Opleidingsvorm,
         DEM_Geslacht, 
         VOP_Toelaatgevende_vooropleiding_soort, 
         INS_Aansluiting_LTA) |>
  rename(Geslacht = DEM_Geslacht,
         Vooropleiding = VOP_Toelaatgevende_vooropleiding_soort,
         Aansluiting = INS_Aansluiting_LTA) |>
  pivot_longer(cols = c(Geslacht, Vooropleiding, Aansluiting)) |>
  group_by(INS_Faculteit, INS_Opleiding, INS_Opleidingsvorm, name, value) |>
  summarise(N = n()) |>
  ungroup()

## Voeg de tellingen toe aan de data
dfFairness_HHs.7 <- dfFairness_HHs.6 |>
  left_join(
    dfTellingen |> 
      mutate(INS_Opleidingsvorm = tolower(INS_Opleidingsvorm)),
    by = c("Faculteit" = "INS_Faculteit",
           "Opleiding" = "INS_Opleiding",
           "Opleidingsvorm" = "INS_Opleidingsvorm",
           "Variabele" = "name",
           "Groep" = "value")
  ) |>
  replace_na(list(N = 0))
  
## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. FAIRNESS FLEXTABLE ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4.1 Maak de tabel #### 

ft <- flextable(dfFairness_HHs.7 |> 
                  dplyr::filter(N > 0))

sColor_Bias_Positive <- "#9DBF9E"
sColor_Bias_Negative <- "#A84268"
sColor_Bias_Neutral  <- "#FCB97D"
sColor_Bias_None     <- "#E5E5E5"

# Voeg de kolom 'Variabele' samen voor visueel groeperen
# Pas voorwaardelijke opmaak toe
ft <- ft |>
  merge_v(j = c("Faculteit",
                "Opleidingsnaam",
                "Opleiding",
                "Opleidingsvorm",
                "Variabele")) |>
  fix_border_issues() |>
  theme_vanilla() |>
  set_header_labels(
    Variabele = "Variabele",
    Groep = "Groep",
    Bias = "Bias",
    `Geen Bias` = "Geen Bias",
    `Negatieve Bias` = "Negatieve Bias",
    `Positieve Bias` = "Positieve Bias"
  ) |>
  autofit() |> 
  italic(j = 5, italic = TRUE, part = "body") |> 
  color(i = ~ `Negatieve Bias` > 1,
        j = c("Groep", "Bias", "Negatieve Bias"),
        color = "white") |>
  color(i = ~ `Positieve Bias` > 1,
        j = c("Groep", "Bias", "Positieve Bias"),
        color = "white") |>
  bg(i = ~ `Negatieve Bias` > 1, 
     j = c("Groep", "Bias", "Negatieve Bias"), 
     bg = sColor_Bias_Negative) |>
  bg(i = ~ `Positieve Bias` > 1, 
     j = c("Groep", "Bias", "Positieve Bias"), 
     bg = sColor_Bias_Positive) |>
  bg(i = ~ `Negatieve Bias` > 1 & `Positieve Bias` > 1, 
     j = c("Groep", "Bias"), 
     bg = sColor_Bias_Neutral) |>
  bg(i = ~ N < 15 & (`Negatieve Bias` > 1 | `Positieve Bias` > 1), 
     j = c("Groep", "Bias"), 
     bg = sColor_Bias_Neutral) |>
  bg(i = ~ `Geen Bias` == 0 & `Positieve Bias` == 0 & `Negatieve Bias` == 0,
     j = 6:7,
     bg = sColor_Bias_None) |>
  bold(i = ~ `Negatieve Bias` > 1,
       j = c("Groep", "Bias", "Negatieve Bias")) |>
  bold(i = ~ `Positieve Bias` > 1,
       j = c("Groep", "Bias", "Positieve Bias")) |> 
  valign(j = 1:5, valign = "top", part = "all") |> 
  align_text_col(align = "left") |> 
  align_nottext_col(align = "center") 

ft
