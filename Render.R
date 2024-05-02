## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Render.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Lectoraat Learning Technology & Analytics De Haagse Hogeschool
## Copyright 2024 De HHs
## Web Page: http://www.hhs.nl
## Contact: Theo Bakker (t.c.bakker@hhs.nl)
## Verspreiding buiten De HHs: Nee
##
## Doel: Het renderen van de analyses voor meerdere opleidingen
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
## 01-05-2024: TB: Aanmaak bestand
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. LAAD LTABASE PACKAGE + DEFAULT DATABASES ####
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

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.3 Default datasets: dfOpleidigen, sectors, studytypes, studyforms ####

## Laad de default datasets: dfOpleidigen, sectors, studytypes, studyforms
ltabase::load_lta_datasets(message = TRUE)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.4 Laad extra bibliotheken

## Laad extra bibliotheken
library(tidymodels)  # voor machine learning
library(vip)         # voor variable importance plots
library(forcats)     # om factor variabelen te bewerken
library(performance) # voor performance metingen op lr modellen
library(dlookr)      # om data te inspecteren
library(gtsummary)   # voor beschrijvende summary tabellen
library(cli)         # voor cli teksten
library(quarto)      # voor quarto bestanden

## Geef de voorkeur bij conflicten aan het tidymodels package
tidymodels_prefer(quiet = TRUE)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.4 Default datasets: dfOpleidigen, sectors, studytypes, studyforms ####

## Laad de default datasets: dfOpleidigen, sectors, studytypes, studyforms
ltabase::load_lta_datasets(message = TRUE)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.5 Laad extra functies ####

source("99. Functies & Libraries/Rapport_functies.R")


## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. RENDER DE OUTPUT ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.0 Bepaal de selectie ####

dfRender <- dfOpleidingen |>
  mutate(INS_Opleiding_en_Opleidingsvorm = paste(INS_Opleiding, INS_Opleidingsvorm, sep = "_")) |>
  dplyr::filter(
    INS_Opleiding_en_Opleidingsvorm %in% c(
      # "HBO-V_VT", 
      # "HBO-V_DT",
      # "CMD_VT", 
      # "IPO_VT", 
      # "MT_VT", 
      # "SW_VT", 
      "ORM_DT",
      "ORM_VT"#,
      #"AC_VT"
      ) 
  ) |> 
  dplyr::select(INS_Opleiding_en_Opleidingsvorm, 
                INS_Faculteit,
                INS_Opleiding,
                INS_Opleidingsvorm,
                everything())

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.1 Loop over opleidingen en render de output ####

lUitval <-  c("Uitval na 1 jaar", "Alle uitval")

for(i in 1:nrow(dfRender)) {
  
  ## Bepaal de opleiding en opleidingsvorm
  .opleiding      <- dfRender$INS_Opleiding[i] 
  .opleidingsvorm <- dfRender$INS_Opleidingsvorm[i]
  
  cli::cli_alert_info(
    paste(
      "Render de output voor de opleiding:",
      .opleiding,
      "en opleidingsvorm:",
      .opleidingsvorm
    )
  )
  
  for (j in lUitval) {
  
    ## Maak de variabelen voor de huidige opleiding op basis van de opleidingsnaam en opleidingsvorm
    current_render_opleiding <- Get_Current_opleiding(opleiding = .opleiding,
                                                      opleidingsvorm = .opleidingsvorm)

    ## Bepaal de output file:
    ## faculteit-opleiding-opleidingsvorm-uitval.html
    .uitval <- janitor::make_clean_names(j)
    .output_file <- paste(
      paste(
        current_render_opleiding$INS_Faculteit,
        current_render_opleiding$INS_Opleidingstype_LTA,
        current_render_opleiding$INS_Opleiding,
        current_render_opleiding$INS_Opleidingsvorm,
        sep = "-"
      ),
      "_",
      .uitval,
      ".html",
      sep = ""
    )
    
    ## Bepaal de output directory
    .output_dir <- file.path("10. Output",
                             tolower(current_render_opleiding$INS_Faculteit))
    
    ## Bepaal de parameters voor de quarto file
    .execute_params <- list(
      uitval                   = j,
      faculteit                = current_render_opleiding$INS_Faculteit,
      opleidingsnaam           = current_render_opleiding$INS_Opleidingsnaam_huidig,
      opleiding                = current_render_opleiding$INS_Opleiding,
      opleidingsvorm           = Get_Opleidingsvorm_lang(current_render_opleiding$INS_Opleidingsvorm),
      opleidingsvorm_afkorting = current_render_opleiding$INS_Opleidingsvorm
    )
    
    ## Render de quarto file en verplaats deze naar de output directory
    Quarto_Render_Move(input = "Index.qmd",
                       output_file = .output_file,
                       output_dir = .output_dir,
                       execute_params = .execute_params,
                       output_format = 'html')
    
    ## Collect garbage
    invisible(gc())
  
  }
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.2 Verplaats de output naar de output directory van de repo buiten dit project ####

bRemove_originals <- FALSE

## Kopieer de gerenderde bestanden naar de output directory van de repo buiten dit project
## Verwijder de originele bestanden indien bRemove_originals = TRUE
Copy_Reports(remove_orgials = bRemove_originals)

