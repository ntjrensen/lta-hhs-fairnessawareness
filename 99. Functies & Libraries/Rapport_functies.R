## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Rapport functies.R ####
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
## TODO:
## 1) ___.
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Geschiedenis:
## 30-04-2024: TB: Aanmaak bestand
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. OPLEIDINGSSPECIFIEKE FUNCTIES ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Functie om de huidige opleiding te bepalen
Get_Current_opleiding <- function(opleiding, opleidingsvorm) {
  
  if(!exists("dfOpleidingen")){
    cli::cli_abort("dfOpleidingen is niet gedefinieerd")
  }
  
  ## Test of deze opleiding wel voorkomt
  if(!any(dfOpleidingen$INS_Opleiding == opleiding &
          dfOpleidingen$INS_Opleidingsvorm == opleidingsvorm)) {
    cli::cli_abort(paste0("De opleiding ", opleiding, " met opleidingsvorm ", opleidingsvorm, " bestaat niet. \n",
                          "Controleer of de opleidingsnaam en opleidingsvorm correct zijn."))
  }
  
  current_opleiding <- dfOpleidingen |>
    dplyr::filter(INS_Opleiding == opleiding,
                  INS_Opleidingsvorm == opleidingsvorm) |>
    select(
      INS_Opleiding,
      INS_Opleidingsvorm,
      INS_Opleidingsnaam,
      INS_Opleidingsnaam_huidig,
      INS_Opleidingstype_LTA,
      INS_Collegejaar_min,
      INS_Collegejaar_max,
      INS_Faculteit,
      INS_Sector_VH_LTA,
      INS_Sector_VH_lang,
      INS_Vestiging_HHs_LTA
    ) |>
    group_by(INS_Opleiding, INS_Opleidingsvorm) |>
    dplyr::filter(max(INS_Collegejaar_max) == INS_Collegejaar_max) |>
    ungroup() |>
    distinct() |>
    as.list()
  
  return(current_opleiding)
}

## Fuctie om de variabelen van de huidige opleiding in te stellen
Set_Current_opleiding_vars <- function(current_opleiding, debug = F){
  
  Cli_Subheader("Instelling van de huidige opleiding")
  
  opleiding               <<- current_opleiding$INS_Opleiding
  faculteit               <<- current_opleiding$INS_Faculteit
  opleidingstype          <<- current_opleiding$INS_Opleidingstype_LTA
  opleidingsnaam_huidig   <<- current_opleiding$INS_Opleidingsnaam_huidig
  opleidingsvorm          <<- tolower(current_opleiding$INS_Opleidingsvorm)
  hhs_locatie             <<- tolower(current_opleiding$INS_Vestiging_HHs_LTA)
  vh_sector               <<- current_opleiding$INS_Sector_VH_LTA
  vh_sector_long          <<- current_opleiding$INS_Sector_VH_lang
  opleidingcleanname      <<- Get_Opleiding_directory(faculteit,
                                                      opleidingsnaam_huidig,
                                                      opleiding,
                                                      opleidingsvorm)
  
  if(debug) {
    Show_Current_opleiding_vars()
  }
  
}

## Functie om de variabelen van de huidige opleiding te tonen
Show_Current_opleiding_vars <- function(){
  
  if(!exists("current_opleiding")){
    cli::cli_abort("current_opleiding is niet gedefinieerd")
  }
  
  ## Print alle waarden van de huidige opleiding
  cli::cli_h1(paste0("Huidige opleiding:"))
  
  cli_bullets(c(
    "*" = paste0("Faculteit: ",            faculteit),
    "*" = paste0("Opleidingstype: ",       opleidingstype),
    "*" = paste0("Opleiding: ",            opleiding),
    "*" = paste0("Opleidingsnaam: ",       opleidingsnaam_huidig),
    "*" = paste0("Studievorm: ",           opleidingsvorm),
    "*" = paste0("HHS Locatie: ",          hhs_locatie),
    "*" = paste0("VH Sector lowercase: ",  vh_sector),
    "*" = paste0("VH Sector lange naam: ", vh_sector_long),
    "*" = paste0("Opleiding clean name: ", opleidingcleanname)
  ))
}

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. PAD FUNCTIES ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Functie om het pad naar de flextables te bepalen
Get_Opleiding_directory <- function(faculteit,
                                    opleidingsnaam_huidig,
                                    opleidingsnaam = NULL,
                                    opleidingsvorm) {
  
  if(faculteit == "Xtern") {
    path <- janitor::make_clean_names(paste(
      faculteit,
      opleidingsnaam_huidig,
      opleidingsvorm,
      sep = "_"
    ))
  } else {
    path <- janitor::make_clean_names(paste(
      faculteit,
      opleidingsnaam_huidig,
      opleidingsnaam,
      opleidingsvorm,
      sep = "_"
    ))
  }
  
  return(path)
}

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. DATASET FUNCTIES ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Functie om de Uitval variabele te maken
Mutate_Uitval <- function(df, model = "Uitval na 1 jaar") {
  
  if (model == "Uitval na 1 jaar") {
    return(
      df |>
        mutate(
          SUC_Uitval = ifelse(SUC_Uitval_aantal_jaar_LTA == 1, T, F),
          SUC_Uitval = coalesce(SUC_Uitval, F)
        ) 
    )
  } else if (model == "Uitval na 2 jaar") {
    return(
      df |>
        mutate(
          SUC_Uitval = ifelse(SUC_Uitval_aantal_jaar_LTA == 1:2, T, F),
          SUC_Uitval = coalesce(SUC_Uitval, F)
        ) 
    )
  } else if (model == "Uitval na 3 jaar") {
    return(
      df |>
        mutate(
          SUC_Uitval = ifelse(SUC_Uitval_aantal_jaar_LTA == 1:3, T, F),
          SUC_Uitval = coalesce(SUC_Uitval, F)
        ) 
    )
  } else if (model == "Alle uitval"){
    return(
      df |>
        mutate(
          SUC_Uitval = ifelse(SUC_Uitval_aantal_jaar_LTA > 0, T, F),
          SUC_Uitval = coalesce(SUC_Uitval, F)
        ) 
    )
  }
  
}

## Functie om de volgorde van een aantal levels te bepalen
Get_Levels <- function() {
  
  ## Bepaal de volgorde van de levels in de studiekeuzeprofielen:
  lLevels_skp <<- c(
    "EM",
    "CM",
    "EM&CM",
    "NT",
    "NG",
    "NT&NG",
    "NG&NT",
    "NG&CM",
    "NG&EM",
    "NT&CM",
    "NT&EM",
    "OS",
    "CERT",
    "AHO",
    "ALG",
    "BI",
    "EA",
    "HO",
    "HB",
    "ICT",
    "MedV",
    "MobV",
    "TP",
    "TR",
    "TSL",
    "UV",
    "VS",
    "VNL",
    "ZW"
  )
  
  lLevels_aansluiting <<- c(
    "Direct",
    "Tussenjaar",
    "2e Studie",
    "Switch intern",
    "Switch extern",
    "Na CD",
    "Overig",
    "Onbekend"
  )
  
  lLevels_vop <<- c(
    "MBO",
    "HAVO",
    "VWO",
    "BD",
    "HO",
    "CD",
    "Overig",
    "Onbekend"
  )
  
}

## Pas levels aan, zodat deze goed gesorteerd worden
Sort_Levels <- function(levels, df, var) {
  lLevels <- intersect(levels, 
                       unique(df[[var]])
                       )
  return(lLevels)
}

## Functie om de levels van een variabele te bepalen
Set_Levels <- function(df = dfOpleiding_inschrijvingen_base) {
  
  lLevels_skp <<-
    Sort_Levels(
      lLevels_skp,
      df,
      "VOP_Studiekeuzeprofiel_LTA_afkorting"
    )
  
  lLevels_aansluiting <<-
    Sort_Levels(lLevels_aansluiting,
                df,
                "INS_Aansluiting_LTA")
  
  lLevels_vop <<-
    Sort_Levels(
      lLevels_vop,
      df,
      "VOP_Toelaatgevende_vooropleiding_soort"
    )
}

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. RENDER FUNCTIES ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5. CLI FUNCTIES ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Functie voor het printen van een subheader
Cli_Subheader <- function(sText) {
  
  cli::cli_div(theme = list(span.neutral = list(color = "orange"),
                            span.success = list(color = "darkgreen"),
                            span.failure = list(color = "red"),
                            span.null    = list(color = "darkgray"),
                            span.topic   = list(color = "darkblue")))
  
  cli::cat_rule()
  cli::cat_line(sText)
  cli::cat_rule()
  
}
