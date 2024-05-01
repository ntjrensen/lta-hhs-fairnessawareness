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
## 1.5 Extra functies

## Functie om de lange naam van de opleidingsvorm te bepalen
Get_Opleidingsvorm_lang <- function(opleidingsvorm) {
  if (opleidingsvorm == "VT") {
    return("voltijd")
  } else if (opleidingsvorm == "DT") {
    return("deeltijd")
  } else if (opleidingsvorm == "DU") {
    return("duaal")
  } else {
    return("onbekend")
  }
}

## Functie om een quarto bestand te renderen en te verplaatsen
Quarto_Render_Move <- function(input,
                               output_file = NULL,
                               output_dir = NULL,
                               ...) {
  
  # Haal alle informatie op over de output van de quarto file
  x <- quarto::quarto_inspect(input)
  output_format <- names(x$formats)
  output <- x$formats[[output_format]]$pandoc$`output-file`
  if (is.null(output_file)) {
    output_file <- output
  }
  input_dir <- dirname(input)
  if (is.null(output_dir)) {
    output_dir <- input_dir
  }
  output_path_from <- file.path(input_dir, output)
  output_path_to <- file.path(output_dir, output_file)
  
  # Render het qmd input-bestand naar de input_dir
  quarto::quarto_render(input = input, ... = ...)
  
  # Als de output_dir verschilt van de input_dir, kopieer het gerenderde bestand
  ## daarheen en verwijder het originele bestand
  if (input_dir != output_dir) {
    # Try to make the folder if it doesn't yet exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir)
    }
    
    # Verplaats nu de uitvoer naar de output_dir en verwijder de originele uitvoer
    file.copy(from = output_path_from,
              to = output_path_to,
              overwrite = TRUE)
    file.remove(output_path_from)
    
    # Als de output_dir hetzelfde is als de input_dir, maar het gerenderde bestand
    # een andere naam heeft dan het invoerbestand, hernoem het dan
  } else if (output_file != output) {
    file.rename(from = output_path_from, to = output_path_to)
  }
  
}

## Functie om bestanden te kopieren naar de output directory van de repo buiten dit project
Copy_Reports <- function(remove_orgials = F, debug = F) {
  
  ## Bepaal de output directory van de repo buiten dit project
  sOutput_directory <- file.path(Get_Rootdirectory(),
                                 "00 LTA Git/Git HHs/LTA_Reports/lta-hhs-studiesucces-models")
  
  ## Maak een filelist van de .html bestanden in de output directory
  file_list <- list.files(
    "10. Output",
    pattern = "*.html",
    full.names = TRUE,
    recursive = TRUE
  )
  
  if(debug) {
    cli::cli_h1("File list:")
    cli::cli_bullets(file_list)
  }
  
  ## Kopieer de .html bestanden van de output directory naar de output directory van de repo buiten dit project
  if(!dir.exists(sOutput_directory)) {
    dir.create(sOutput_directory)
  }
  
  ## Als er bestanden zijn, kopieer deze dan
  if(length(file_list) > 0) {
    
    ## Kopieer de .html bestanden van de output directory 
    ## naar de output directory van de repo buiten dit project
    for(f in file_list) {
      
      ## Maak een output directory voor de bestanden: repo + faculteit
      .output_dir <- file.path(sOutput_directory,
                               file.path(dirname(f)) |> basename())
      
      if(!dir.exists(.output_dir)) {
        dir.create(.output_dir)
      }
      
      file.copy(
        from = f,
        to = .output_dir,
        overwrite = TRUE,
        recursive = FALSE,
        copy.mode = TRUE
      )
    }
    
    cli::cli_alert("De bestanden zijn gekopieerd")
    
  } else {
    cli::cli_alert("Er zijn geen bestanden om te kopiëren")
  }
  
  if(remove_orgials) {
    file.remove(file_list)
    cli::cli_alert("De originele bestanden zijn verwijderd")
  }
}

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
  dplyr::filter(INS_Opleiding %in% c("HBO-V", "SW"),
                INS_Opleidingsvorm %in% c("VT"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.1 Loop over opleidingen en render de output ####

lUitval <-  c("Uitval na 1 jaar", "Alle uitval")

for(i in dfRender$INS_Opleiding) {
  
  for (j in lUitval) {
  
    ## Bepaal de opleidingsvorm
    .opleidingsvorm <- dfRender$INS_Opleidingsvorm[dfRender$INS_Opleiding == i]
    
    ## Maak de variabelen voor de huidige opleiding op basis van de opleidingsnaam en opleidingsvorm
    current_render_opleiding <- Get_Current_opleiding(opleiding = i,
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
Copy_Reports(remove_orgials = bRemove_originals)

