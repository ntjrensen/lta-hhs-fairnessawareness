## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Render_book.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Lectoraat Learning Technology & Analytics De Haagse Hogeschool
## Copyright 2024 De HHs
## Web Page: http://www.hhs.nl
## Contact: Theo Bakker (t.c.bakker@hhs.nl)
## Verspreiding buiten De HHs: Nee
##
## Doel: Het renderen van de analyses voor meerdere opleidingen in boekvorm
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

source("99_Functies_Libraries/Inladen_ltabase.R")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.3 Default datasets: dfOpleidigen, sectors, studytypes, studyforms ####

## Laad de default datasets: dfOpleidigen, sectors, studytypes, studyforms
ltabase::load_lta_datasets(message = TRUE)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.4 Laad extra bibliotheken

## Laad extra bibliotheken
library(cli)         # voor cli teksten
library(quarto)      # voor quarto bestanden
library(tidyverse)   # voor make_clean_names
library(conflicted)  # om conflicten op te lossen
library(janitor)     # voor make_clean_names
library(fs)          # voor file system functies

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.4 Default datasets: dfOpleidigen, sectors, studytypes, studyforms ####

## Laad de default datasets: dfOpleidigen, sectors, studytypes, studyforms
ltabase::load_lta_datasets(message = TRUE)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.5 Laad extra functies ####

source("99_Functies_Libraries/report.helpers.R")

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. RENDER DE OUTPUT ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.1 Bepaal de selectie ####

dfRender <- dfOpleidingen |>
  
  ## Combineer de opleiding en opleidingsvorm tot een nieuwe variabele
  mutate(INS_Opleiding_en_Opleidingsvorm = paste(INS_Opleiding, INS_Opleidingsvorm, sep = "_")) |>
  # dplyr::filter(
  #   INS_Opleiding_en_Opleidingsvorm %in% c(
  #     # "HBO-V_VT",
  #     # "BO_DU",
  #     # "ICT_VT"
  #     # "HBO-V_DT",
  #     # "CMD_VT", 
  #     # "IPO_VT",
  #     # "E_VT",
  #     # "B_VT",
  #     # "W_VT",
  #     # "MT_VT", 
  #     # "SW_VT", 
  #     # "ORM_DT",
  #     # "ORM_VT",
  #     # "AC_VT",
  #     "CMD_VT"
  #     ) 
  # ) |>
  # dplyr::filter(INS_Faculteit %in% c("GVS","MO","SWE","TIS"),
  #               INS_Opleiding %in% c("HDT")) |>
  
  ## Beperk de selectie tot opleidingen die in 2022 nog bestaan 
  ## en meer dan 1 jaar hebben aan data
  dplyr::filter(INS_Collegejaar_max == 2022,
                INS_Collegejaar_min < 2022) |>
  dplyr::select(INS_Opleiding_en_Opleidingsvorm, 
                INS_Faculteit,
                INS_Opleiding,
                INS_Opleidingsvorm,
                everything())

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.2 Bepaal of een refresh nodig is ####

bRefresh_books <- FALSE

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.3 Bepaal synthetische data gebruikt wordt ####

bUse_Synthetic_data <- FALSE

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.4 Loop over opleidingen en render de output ####

## Bepaal de succes-variabelen
lSucces <-  c("Retentie na 1 jaar")

## Loop over de opleidingen
for(i in 1:nrow(dfRender)) {
  
  ## Bepaal de opleiding en opleidingsvorm
  .opleiding      <- dfRender$INS_Opleiding[i] 
  .opleidingsvorm <- dfRender$INS_Opleidingsvorm[i]
  
  ## Loop over de succes variabelen
  for (j in lSucces) {
    
    ## Meld de actie
    cli::cli_h1(
      paste(
        .opleiding,
        .opleidingsvorm,
        j
      )
    )
    cli::cli_alert_info(
      paste(
        "Render de output voor de opleiding:",
        .opleiding,
        "en opleidingsvorm:",
        .opleidingsvorm
      )
    )
    
    ## Als deze combinatie al bestaat, skip
    ## Maak de variabelen voor de huidige opleiding op basis van de opleidingsnaam en opleidingsvorm
    current_render_opleiding <- Get_Current_Opleiding(opleiding = .opleiding,
                                                      opleidingsvorm = .opleidingsvorm)

    ## Bepaal de output dir:
    ## faculteit/opleiding-opleidingsvorm/retentie-na-1-jaar/index.html
    
    .output_dir <- tolower(file.path(
      current_render_opleiding$INS_Faculteit,
      paste(
        current_render_opleiding$INS_Opleidingstype_LTA,
        current_render_opleiding$INS_Opleiding,
        current_render_opleiding$INS_Opleidingsvorm,
        sep = "-"
      ),
      .succes <- janitor::make_clean_names(j)
    ))
    
    if(bUse_Synthetic_data) {
      .output_dir <- paste0(.output_dir, "-synth")
    }
    
    ## Bepaal de parameters voor de quarto file
    .execute_params <- list(
      succes                   = j,
      faculteit                = current_render_opleiding$INS_Faculteit,
      opleidingsnaam           = current_render_opleiding$INS_Opleidingsnaam_huidig,
      opleiding                = current_render_opleiding$INS_Opleiding,
      opleidingsvorm           = Get_Opleidingsvorm_Lang(current_render_opleiding$INS_Opleidingsvorm),
      opleidingsvorm_afkorting = current_render_opleiding$INS_Opleidingsvorm,
      selectie                 = ifelse(current_render_opleiding$INS_Opleiding == "HDT", 
                                        TRUE, 
                                        FALSE),
      use_synthetic_data       = bUse_Synthetic_data
    )
    
    ## Als de output directory van dit book al bestaat, skip
    bBook_exists <- file.exists(Get_Output_Dir_Repo(.output_dir))
    if(bBook_exists & !bRefresh_books) {
      
      cli::cli_alert_info(
        paste(
          "De output voor de opleiding:",
          .opleiding,
          "en opleidingsvorm:",
          .opleidingsvorm,
          "en succes:",
          j,
          "bestaat al in de directory:",
          .output_dir
        )
      )
      
      next
      } else {
    
        ## Meld de actie
        cli::cli_alert_info(
          paste(
            "Render de output voor de opleiding:",
            .opleiding,
            "en opleidingsvorm:",
            .opleidingsvorm,
            "en succes:",
            j,
            "\n naar de directory:",
            .output_dir
          )
        ) 
        
        }

    ## Render de quarto files en verplaats deze naar de asset repo
    for(k in c("index.qmd",
               "H1_Index_basis.qmd",
               "H2_Index_verdieping_factoren.qmd",
               "H3_Index_verdieping_kansengelijkheid.qmd")) {

      # Render het qmd input-bestand naar de input_dir
      quarto::quarto_render(input = k,
                            execute_params = .execute_params,
                            output_format = 'html')
    }

    ## Kopieer de _book directory naar de output directory
    Copy_Book_To_Reports(output_dir = .output_dir)
    
    ## Collect garbage
    invisible(gc())
  
  }
  
}

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. EINDE ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cli::cli_alert_success("Einde van het script")

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


