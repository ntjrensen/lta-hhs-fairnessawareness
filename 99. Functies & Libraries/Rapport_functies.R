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

## Functie om output directory te bepalen van de huidige opleiding
Get_Current_opleiding_output_dir <- function(current_opleiding,
                                             mode) {
  
  .fac_opl_vorm <- paste0(
    current_opleiding$INS_Faculteit,
    "/",
    current_opleiding$INS_Opleidingstype_LTA,
    "-",
    current_opleiding$INS_Opleiding,
    "-",
    current_opleiding$INS_Opleidingsvorm
  ) |> tolower()
  
  if(mode == "last-fits" | mode == "modelresults") {
    .output_dir <- file.path("10. Output",
                             .fac_opl_vorm,
                             "modelresults")
  } else if(mode == "data") {
    .output_dir <- file.path("10. Output",
                             .fac_opl_vorm,
                             "data")
  } else if(mode == "html") {
    .output_dir <- file.path("10. Output",
                             .fac_opl_vorm)
  } else if(mode == "plot") {
    .output_dir <- file.path("10. Output",
                             .fac_opl_vorm,
                             "plots")
  } else {
    cli::cli_alert("De mode is niet correct")
  }
  
  return(.output_dir)
  
}

## Functie om de output directory te bepalen van de huidige opleiding
Get_Current_opleiding_output_file <- function(df, mode, analyse = NULL) {
  
  ## Bepaal de beschrijving van de analyse
  if(is.null(analyse)) {
    .analyse <- Get_Huidige_analyse()
  } else {
    .analyse <- analyse
  }
  
  if(mode == "last-fits") {
    .suffix <- "_last-fits.rds"
  } else if(mode == "html") {
    .suffix <- ".html"
  } else if(mode == "modelresults") {
    .suffix <- "_modelresults.rds"
  } else if(mode == "data") {
    .suffix <- ".rds"
  } else if(mode == "plot") {
    .suffix <- ".png"
  } else {
    cli::cli_alert("De mode is niet correct")
  }
  
  ## Bepaal de output file:
  ## faculteit-opleiding-opleidingsvorm + analyse + suffix
  .output_file <- paste0(
    paste(
      df$INS_Faculteit,
      df$INS_Opleidingstype_LTA,
      df$INS_Opleiding,
      df$INS_Opleidingsvorm,
      sep = "-"
    ),
    "_",
    .analyse,
    .suffix
  )
  
}

## Functie om de output path te bepalen voor de fitted models
Get_Model_outputpath <- function(mode) {
  
  ## Bepaal de output file
  .output_file <- Get_Current_opleiding_output_file(current_opleiding, mode)
  
  ## Bepaal de output directory
  .output_dir <- Get_Current_opleiding_output_dir(current_opleiding, mode)
  
  ## Maak de directory indien deze nog niet bestaat
  if (!dir.exists(.output_dir)) {
    dir.create(.output_dir, recursive = TRUE)
  }
  
  ## Geef het volledige outputpath terug
  return(file.path(.output_dir, .output_file))
  
}

## Functie om de output path te bepalen voor de plots
Get_Plot_outputpath <- function(plotname, mode = "plot") {
  
  ## Bepaal de output file
  .output_file <- paste0(plotname, ".png")
  
  ## Bepaal de output directory
  .output_dir <- Get_Current_opleiding_output_dir(current_opleiding, mode)
  
  ## Maak de directory indien deze nog niet bestaat
  if (!dir.exists(.output_dir)) {
    dir.create(.output_dir, recursive = TRUE)
  }
  
  ## Geef het volledige outputpath terug
  return(file.path(.output_dir, .output_file))
  
}

## Functie om de output path te bepalen voor de breakdown plots
Get_Breakdown_plotpath <- function(student_groep, student_categorie) {
  
  ## Vervang spaties door - in de student_groep en student_categorie
  .student_groep     <- gsub(" ", "-", student_groep)
  .student_categorie <- gsub(" ", "-", student_categorie)
  
  file.path(Get_Plot_outputpath(
    plotname = tolower(glue("lf_break_down_{(.student_groep)}_{(.student_categorie)}")))
  )
}

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. DATASET FUNCTIES ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Functie om de Uitval variabele te maken
Mutate_Uitval <- function(df, model = "Uitval na 1 jaar") {
  
  ## Vul de missende waarden in met 0
  df <- df |>
    mutate(
      SUC_Uitval_aantal_jaar_LTA = coalesce(SUC_Uitval_aantal_jaar_LTA, 0)
    )
  
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
          SUC_Uitval = ifelse(SUC_Uitval_aantal_jaar_LTA %in% c(1, 2), T, F),
          SUC_Uitval = coalesce(SUC_Uitval, F)
        ) 
    )
  } else if (model == "Uitval na 3 jaar") {
    return(
      df |>
        mutate(
          SUC_Uitval = ifelse(SUC_Uitval_aantal_jaar_LTA %in% c(1, 2, 3), T, F),
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

## Functie om Uitval te verbijzonderen (met en zonder propedeusediploma)
Filter_Propedeusediploma <- function(df, propedeusediploma = "Nvt") {
  
  if (propedeusediploma == "Nvt" || propedeusediploma == "" || is.na(propedeusediploma)) {
    
    ## Neem alle uitval mee
    return(df)
    
  } else if (propedeusediploma == "Met P") {
    
    ## Verwijder de studenten die uitvallen met een propedeusediploma
    return(
      df |>
        filter(SUC_Studiestatus_cat != "Uitval zonder propedeusediploma")
    )
    
  } else if (propedeusediploma == "Zonder P") {
    
    ## Verwijder de studenten die uitvallen met een propedeusediploma
    return(
        df |>
        filter(SUC_Studiestatus_cat != "Uitval met propedeusediploma")
        ) 
    
  } else {
    cli::cli_alert("De propedeusediploma variabele is niet correct")
  }
  
}

## Functie om de Succes variabele te maken
Mutate_Succes <- function(df, model = "Uitval na 1 jaar") {
  
  if (model == "Uitval na 1 jaar") {
    return(
      df |>
        mutate(
          SUC_Succes = ifelse(SUC_Uitval_aantal_jaar_LTA == 1, T, F),
          SUC_Succes = coalesce(SUC_Succes, F)
        ) 
    )
  } else if (model == "Uitval na 2 jaar") {
    return(
      df |>
        mutate(
          SUC_Succes = ifelse(SUC_Uitval_aantal_jaar_LTA == 1:2, T, F),
          SUC_Succes = coalesce(SUC_Succes, F)
        ) 
    )
  } else if (model == "Uitval na 3 jaar") {
    return(
      df |>
        mutate(
          SUC_Succes = ifelse(SUC_Uitval_aantal_jaar_LTA == 1:3, T, F),
          SUC_Succes = coalesce(SUC_Succes, F)
        ) 
    )
  } else if (model == "Alle uitval"){
    return(
      df |>
        mutate(
          SUC_Succes = ifelse(SUC_Uitval_aantal_jaar_LTA > 0, T, F),
          SUC_Succes = coalesce(SUC_Succes, F)
        ) 
    )
  } 
  
}

## Mutate missing cijfers VO
Mutate_Cijfers_VO <- function(df) {
  
  df <- df |>
    mutate(
      Cijfer_SE_VO_missing          = ifelse(is.na(Cijfer_SE_VO),          "Ja", "Nee"),          
      Cijfer_CE_VO_missing          = ifelse(is.na(Cijfer_CE_VO),          "Ja", "Nee"),        
      Cijfer_CE_Nederlands_missing  = ifelse(is.na(Cijfer_CE_Nederlands),  "Ja", "Nee"),
      Cijfer_CE_Engels_missing      = ifelse(is.na(Cijfer_CE_Engels),      "Ja", "Nee"),
      Cijfer_CE_Wiskunde_missing    = ifelse(is.na(Cijfer_CE_Wiskunde),    "Ja", "Nee"),
      Cijfer_CE_Natuurkunde_missing = ifelse(is.na(Cijfer_CE_Natuurkunde), "Ja", "Nee")
    )
  
  return(df)
  
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
    "Switch intern",
    "Switch extern",
    "Na CD",
    "2e Studie",
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

## Functie om de levels van een variabele te muteren
Mutate_Levels <- function(df, vars, levels) {
  for (i in seq_along(vars)) {
    df <- df |>
      mutate(
        !!rlang::sym(vars[i]) := fct_expand(!!rlang::sym(vars[i]), levels[[i]]),
        !!rlang::sym(vars[i]) := fct_relevel(!!rlang::sym(vars[i]), levels[[i]]),
        !!rlang::sym(vars[i]) := fct_drop(!!rlang::sym(vars[i]))
      )
  }
  return(df)
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

## Functie om de tekst te bepalen voor het uitval model (in de titel)
Get_Uitval_model_text <- function(propedeusediploma, uitval_model) {
  
  if (propedeusediploma == "Zonder P") {
    uitval_model_text <- paste(uitval_model, "bij studenten zonder propedeusediploma")
  } else if (propedeusediploma == "Met P") {
    uitval_model_text <- paste(uitval_model, "bij studenten met propedeusediploma")
  } else {
    uitval_model_text <- uitval_model
  }
  
  return(uitval_model_text)
  
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

## Functie om de huidige analyse te bepalen
Get_Huidige_analyse <- function() {
  
  ## Bepaal de beschrijving van de analyse
  .uitval     <- janitor::make_clean_names(params$uitval)
  .propedeuse <- janitor::make_clean_names(params$propedeusediploma)
  
  .analyse <- paste(
    .uitval,
    .propedeuse,
    sep = "_"
  )
  
  return(.analyse)
  
}

## Functie om een header 3 te knitten
Knit_Header <- function(x, rep = 1) {
  
  .header <- rep("#", rep) |> paste(collapse = "")
  
  Knit_print_rule(glue("{(.header)} {x}"))
}

## Functie om een regel te knitten
Knit_print_rule <- function(x) {
  
  knit_print(glue("\n\n\n{x}\n\n"))
  
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

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 6. QUERY FUNCTIES ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Functie om de meest voorkomende categorie te bepalen
Get_Mostcommon_Category <- function(x) {
  
  ## Test of x categorisch is
  if(!is.factor(x) && !is.character(x)) {
    stop("De variabele is niet categorisch")
  }
  
  ## Bepaal de meest voorkomende categorie
  x <- names(which.max(table(x)))
  
  return(x)
  
}

## Functie om de mediaan te bepalen (afgerond en zonder NA's)
Get_Median_Rounded <- function(x) {
  
  ## Test of x numeriek is
  if(!is.numeric(x)) {
    stop("De variabele is niet numeriek")
  }
  
  ## Bepaal de mediaan
  x <- round(median(x, na.rm = TRUE), 1)
  
  return(x)
  
}

# Functie om een persona te maken van de studenten van een opleiding
Get_dfPersona <- function(group = NULL) {
  
  lSelect_categorical <- c(
    "Aansluiting",
    "APCG",
    "Cijfer_CE_Engels_missing",
    "Cijfer_CE_Natuurkunde_missing",
    "Cijfer_CE_Nederlands_missing",
    "Cijfer_CE_VO_missing",
    "Cijfer_CE_Wiskunde_missing",
    "Cijfer_SE_VO_missing",
    "Dubbele_studie",
    "Geslacht",
    "Studiekeuzeprofiel",
    "Vooropleiding"
  ) 
  
  if (!is.null(group)) {
    .group <- as.name(group)
    # Verwijder de groep variabele uit deze lijst
    lSelect_categorical <- setdiff(lSelect_categorical, group)
  }
  
  lSelect_numerical <- c(
    "Aanmelding",
    "Cijfer_CE_Engels",
    "Cijfer_CE_Natuurkunde",
    "Cijfer_CE_Nederlands",
    "Cijfer_CE_VO",
    "Cijfer_CE_Wiskunde",
    "Cijfer_SE_VO",
    "Leeftijd",
    "Reistijd",
    "SES_Arbeid",
    "SES_Welvaart",
    "SES_Totaal",
    "Uitval"
  )
  
  # Bereken het totaal voor deze opleiding
  .totaal <- dfOpleiding_inschrijvingen |> 
    count() |> 
    pull(n)
  
  if (!is.null(group)) {
    
    # Maak personas aan op basis van de opgegeven groep
    dfPersona <- dfOpleiding_inschrijvingen |>
      
      # Split de opleidingen op basis van de groep
      group_by(!!.group) |>
      
      # Maak een persona aan op basis van de overige variabelen: 
      # kies de meest voorkomende waarden per variabele bij categorieën en de mediaan bij numerieke variabelen
      summarise(
        
        # Categorische variabelen
        across(
          all_of(lSelect_categorical), 
          Get_Mostcommon_Category,
          .names = "{col}"
        ),
        
        # Numerieke variabelen
        across(
          all_of(lSelect_numerical),
          Get_Median_Rounded,
          .names = "{col}"
        ), 
        
        # Overige variabelen
        Collegejaar = median(Collegejaar, na.rm = TRUE),
        ID = NA,
        
        # Subtotaal aantal studenten
        Subtotaal = n(),
        
        .groups = "drop") |> 
      
      # Tel het aantal studenten per groep
      mutate(Totaal = .totaal,
             Percentage = round(Subtotaal/Totaal, 3)) |>
      
      # Voeg de groep variabele toe en bepaal de categorie binnen de groep
      mutate(Groep = group,
             Categorie = !!.group) |>
      
      # Herorden
      select(Groep, Categorie, Totaal, Subtotaal, Percentage, everything())
    
  } else {
    
    # Maak persona voor alle studenten zonder groepering
    dfPersona <- dfOpleiding_inschrijvingen |>
      
      # Maak een persona aan op basis van de overige variabelen: 
      # kies de meest voorkomende waarden per variabele bij categorieën 
      # en de mediaan bij numerieke variabelen
      summarise(
        
        # Categorische variabelen
        across(
          all_of(lSelect_categorical), 
          Get_Mostcommon_Category,
          .names = "{col}"
        ),
        
        # Numerieke variabelen
        across(
          all_of(lSelect_numerical),
          Get_Median_Rounded,
          .names = "{col}"
        ), 
        
        # Overige variabelen
        Collegejaar = median(Collegejaar, na.rm = TRUE),
        ID = NA,
        
        # Subtotaal aantal studenten
        Subtotaal = n(),
        
        .groups = "drop") |> 
      
      # Tel het aantal studenten per groep
      mutate(Totaal = .totaal,
             Percentage = round(Subtotaal/Totaal, 3)) |>
      
      # Voeg de groep variabele toe en bepaal de categorie binnen de groep
      mutate(Groep = "Alle",
             Categorie = "Alle studenten") |>
      
      # Herorden
      select(Groep, Categorie, Totaal, Subtotaal, Percentage, everything())
  }
  
  return(dfPersona)
}

# Convert to data frame for ggplot
Get_dfBreakdown_lm <- function(bd_lm) {
  
  dfBreakdown_lm <- as.data.frame(bd_lm) |>
    
    ## Sorteer op basis van de position
    arrange(desc(position)) |>
    
    ## Hernoem variabelen (intercept en prediction)
    mutate(
      variable = case_when(
        variable == "intercept" ~ "Intercept",
        variable == "prediction" ~ "Voorspelling",
        TRUE ~ variable
      )
    ) |>
    
    ## Maak het label aan (in percentages)
    mutate(label = paste0(as.character(round(contribution, 3) * 100), "%")) |>
    mutate(
      label = case_when(
        variable %in% c("Intercept", "Voorspelling") ~ label,
        sign == 1  ~ paste0("+", label),
        sign == -1 ~ paste0(label),
        sign == 0  ~ paste0("+", label),
        .default = label
      )
    ) |>
    mutate(label = case_when(label == "0%" ~ "+0%", .default = label)) |>
    
    ## Verwijder variabelen met een contribution van 0
    filter(sign != 0) |>
    
    ## Maak start en end variabelen aan
    mutate(start = dplyr::lag(cumulative, default = min(cumulative)),
           end = cumulative) 
  
  ## Voeg een rij toe voor "Overige variabelen"
  dfBreakdown_lm <- dfBreakdown_lm |>
    add_row(
      variable = "+ Overige variabelen",
      contribution = 0,
      variable_name = NA,
      variable_value = NA,
      cumulative = dfBreakdown_lm$cumulative[dfBreakdown_lm$position == 1],
      sign = "0",
      position = 2,
      label = "+0%",
      start = dfBreakdown_lm$cumulative[dfBreakdown_lm$position == 1],
      end = dfBreakdown_lm$cumulative[dfBreakdown_lm$position == 1]
    ) |>
    
    ## Pas de positie aan
    arrange(position) |>
    mutate(position = row_number()) |>
    arrange(desc(position)) |>
    
    ## Bepaal de volgende start en positie
    mutate(
      next_start = lead(start, default = NA),
      next_position = lead(position, default = NA)
    ) |>
    mutate(start = case_when(
      variable == "Intercept" ~ 0,
      variable == "Voorspelling" ~ 0,
      .default = start
    )) |>
    
    ## Pas de sign aan
    mutate(sign = case_when(variable == "Intercept" ~ "X", .default = sign)) |>
    
    ## Bepaal de kleur van de labels
    mutate(label_color = case_when(
      sign == "1" ~ lColors_default[["sNegative_color"]], 
      sign == "-1" ~ lColors_default[["sPositive_color"]], 
      .default = "black")) |>
    
    ## Bepaal de positie van de labels
    rowwise() |>
    mutate(label_position = max(start, end)) |>
    ungroup()
  
  ## Pas de sign aan naar een factor
  dfBreakdown_lm$sign <- factor(
    dfBreakdown_lm$sign,
    levels = c("1", "-1", "0", "X"),
    labels = c("Positief", "Negatief", "Geen", "X")
  ) 
  
  return(dfBreakdown_lm)
  
}

## Functie om de Shapley waarden te bepalen
Get_dfShapley <- function(shapley_object) {
  
  ## Maak een dataframe van de shapley waarden
  dfShapley <- shapley_object |> 
    
    ## Verwijder variabelen zonder contribution
    filter(contribution != 0) |>
    
    ## Bereken de gemiddelde contribution per variabele
    group_by(variable) |>
    mutate(mean_val = mean(contribution)) |>
    ungroup() |>
    
    ## Sorteer de variabelen op basis van de gemiddelde contribution
    mutate(variable = fct_reorder(variable, abs(mean_val))) 
  
  return(dfShapley)
  
}

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 7. PLOT FUNCTIES ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

lColors_default <- c(
  
  ## Kleuren van title, onderwerp, ondertitel, caption, background
  sTitle_color            = "black",
  sSubject_color          = "#808080",
  sSubtitle_color         = "black",
  sSubtitle_prefix_color  = "#808080",
  sSubtitle_warning_color = "#C8133B",
  sCaption_color          = "darkgray",
  sBackground_color       = "white",
  
  ## Kleur van tekst
  sText_color            = "black",
  sText_inside_color     = "white",
  
  ## Intercept (0) en gridlines
  sBaseline_color        = "black",
  sGridline_color        = "#CBCBCB",
  sDeadline_color        = "black",
  sBaseline_color_ses    = "darkgray",
  
  ## Vulkleur
  sFill_color            = "lightgray",
  
  ## Lijnkleur
  sAverage_line_color    = "#CBCBCB",
  
  ## Tekstkleur
  sAverage_text_color    = "darkgray",
  
  ## Kleur van annotaties
  sAnnotation_text_color = "black",
  sArrow_color           = "darkgray",
  
  ## Kleur van jitter
  sJitter_color          = "darkgray",
  
  ## Kleur van de errorband
  sSE_color              = "#CBCBCB",
  
  ## Kleur van de band
  sBand_color            = "grey95",
  
  ## Positief en negatief
  sPositive_color        = "#466F9D",
  sNegative_color        = "#C8133B"
)

## Bepaal het basisthema
Set_LTA_Theme <- function(title.font = c("sans"), type = "plot") {
  theme_set(theme_minimal())
  theme_update(
    ## Margins
    ## plot.margin = margin(c(10, 10, 10, 10), "points"),
    
    ## Titel en caption
    plot.title = element_textbox_simple(
      size = 16,
      lineheight = 1,
      color = lColors_default["sTitle_color"],
      face = "bold",
      padding = margin(0, 0, 0, 0),
      margin = margin(5, 0, 5, 0) ,
      family = title.font
    ),
    plot.subtitle = element_textbox_simple(
      size = 12,
      lineheight = 1,
      color = lColors_default["sSubtitle_color"],
      padding = margin(0, 0, 0, 0),
      margin = margin(5, 0, 15, 0)
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(size = 8,
                                          color = lColors_default["sCaption_color"],
                                          padding = margin(0, 0, 0, 0),
                                          margin = margin(15, 0, 0, 0)),
    
    ## Assen
    axis.title.x = element_text(face = "bold",
                                vjust = 5),
    axis.title.y = element_text(face = "bold",
                                margin = margin(
                                  t = 0,
                                  r = 10,
                                  b = 0,
                                  l = 0
                                )),
    axis.text.x  = element_text(size = 11),
    axis.text.y  = element_text(size = 11),

    ## Lijnen
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),

    ## Legenda
    legend.key.size = unit(.5, "cm"),
    legend.text = element_text(size = 10),

    ## Achtergrond wit en border niet zichtbaar
    plot.background = element_rect(fill = lColors_default["sBackground_color"],
                                   color = NA) +

    ## Maak van de title van x en y een markdown element
    theme(
      axis.title.x = element_markdown(),
      axis.title.y = element_markdown()
    ) 
      
  )
  
}

## Functie om LTA thema elementen toe te voegen
Add_LTA_theme_elements <- function() {
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_markdown(),
    axis.text.y = element_text(size = 10),
    plot.caption = element_textbox_simple(
      size = 8,
      color = lColors_default["sCaption_color"],
      padding = margin(0, 0, 0, 0),
      margin = margin(15, 0, 0, 0)
    )
  )
}

## Functie om de caption te bepalen
Get_sCaption <- function() {
  
  sCaption <- paste0(
    paste(
      lMetadata[["sDataset"]],
      lResearch_settings[["sResearch_path"]],
      sep = ", "
    ),
    ". \U00A9 ",
    lMetadata[["sAnalyse"]],
    ", ",
    format(Sys.Date(), "%Y")
  )
  
  return(sCaption)
  
}

## Functie om een ROC plot te maken
Get_ROC_plot <- function(models, position = NULL) {
  
  lColors <- c("#fc7d0b", "#1170aa",
               "#c85200", "#a3cce9")
  
  ## Combineer eventueel meerdere modellen
  if(is.list(models)) {
    models <- bind_rows(models)
  }
  
  if(!is.null(position)) {
    lColors <- lColors[position]
  }
  
  plot <- models |>
    ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) + 
    geom_path(lwd = 1.5, alpha = 0.8) +
    geom_abline(lty = 3) + 
    coord_equal() + 
    scale_color_manual(values = lColors) +
    labs(x = "1 - specificiteit", y = "sensitiviteit", color = "Model") +
    theme(
      axis.title.x = element_text(margin = margin(t = 20))
    )
  
  return(plot)
  
}

## Functie om de titels te bepalen
Get_Breakdown_Titles <- function(bd, df, j, 
                                 student_groep, student_categorie, 
                                 mode = "group",
                                 debug = FALSE) {
  
  ## Bepaal de uitvalskans, totalen en de titel/ondertitel
  nUitval     <- Number_to_readable(as.numeric(bd$cumulative[bd$variable == 'prediction']) * 100, digits = 1)
  nSubtotaal  <- Number_to_readable(as.numeric(df[j, 'Subtotaal']))
  nTotaal     <- Number_to_readable(as.numeric(df[j, 'Totaal']))
  nPercentage <- Number_to_readable(as.numeric(df[j, 'Percentage']) * 100, digits = 1)
  
  if(debug) {
    cli::cli_alert_info(c("nUitval: ",     nUitval))
    cli::cli_alert_info(c("nSubtotaal: ",  nSubtotaal))
    cli::cli_alert_info(c("nTotaal: ",     nTotaal))
    cli::cli_alert_info(c("nPercentage: ", nPercentage))
  }
  
  # Bouw de titel
  if(mode == "all") {
    student_current_title <- glue(
      "Opbouw van de kans op uitval ({tolower(student_groep)})"
    )
  } else if(mode == "group") {
    student_current_title <- glue(
      "Opbouw van de kans op uitval naar {tolower(student_groep)}"
    )
  }  
  
  if(debug) {
    cli::cli_alert_info(student_current_title)
  }
  
  student_current_subtitle <- glue(
    " | kans op uitval: {nUitval}%"
  )
  
  if(mode == "all") {
    student_current_subtitle <- glue(
      "**{student_categorie}**",
      student_current_subtitle,
      " | _N_ = {nSubtotaal}"
    )
  } else if(mode == "group") {
    student_current_subtitle <- glue(
      "{student_groep}: **{student_categorie}**",
      student_current_subtitle,
      " | _N_ = {nSubtotaal} van {nTotaal} ({nPercentage}%)"
    )
  }
  
  if(debug) {
    cli::cli_alert_info(student_current_subtitle)
  }
  
  return(list(student_current_title, 
              student_current_subtitle))
}

## Functie om een watervalplot te maken
Get_Waterfall_plot <- function(df, titles) {
   
  # Bepaal de breaks voor de x-as
  y_breaks <- seq(0, 1, by = 0.2)
  y_labels <- paste0(seq(0, 100, by = 20), "%")
  
  ## Maak een watervalplot
  plot <- ggplot(df) +
    
    # Voeg horizontale lijnen toe om de 0.2
    geom_hline(
      yintercept = y_breaks,
      color = "#CBCBCB",
      linetype = "solid",
      linewidth = 0.5
    ) +
    
    # Voeg een horizontale lijn toe op de laagste waarde
    geom_hline(
      yintercept = df$cumulative[df$position == 1],
      color = "black",
      linetype = "dotted"
    ) +
    
    # Voeg de waterval bands toe
    geom_rect(aes(
      xmin = position - 0.4,
      xmax = position + 0.4,
      ymin = start,
      ymax = end,
      fill = sign
    )) +
    
    # Voeg de waterval lijnen toe
    geom_segment(aes(
      x = next_position - 0.4,
      xend = position + 0.4,
      y = end,
      yend = end
    ),
    color = "darkgray") +
    
    # Flip de plot
    coord_flip() +
    
    # Bepaal de title en ondertitel
    labs(
      title = titles[[1]],
      subtitle = titles[[2]],
      caption = sCaption,
      x = NULL,
      y = NULL
    ) +
    
    # Vul de kleuren in
    scale_fill_manual(values = c(
      "Positief" = lColors_default[["sNegative_color"]],
      "Negatief" = lColors_default[["sPositive_color"]],
      "X" = lColors_default[["sNegative_color"]]
    )) +
    
    # Voeg tekstlabels toe voor de variabelen
    geom_text(
      aes(x = position, y = label_position, label = label),
      hjust = -0.1,
      size = 4,
      color = "black"
    ) +
    
    # Pas het thema aan om de y-as labels weer te geven
    scale_x_continuous(breaks = df$position, labels = df$variable) +
    scale_y_continuous(breaks = y_breaks,
                       labels = y_labels,
                       limits = c(0, 1)) +
    
    theme(legend.position = "none") +
    theme(
      axis.text.y = element_text(size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
  
    ## Maak de labels van Intercept en voorspelling vetgedrukt
    ## suppressWarnings, omdat er een warning komt dat vectorized input in de toekomst niet mogelijk is
    suppressWarnings(theme(axis.text.y = element_text(
      face = ifelse(
        df$variable %in% c("Intercept", "Voorspelling"),
        "bold",
        "plain"
      )
    )))
  
  return(plot)
  
}

## Print breakdownplots
Get_Breakdown_Plots <- function(groep) {
  
  ## Bepaal het pad
  plotdir <- Get_Current_opleiding_output_dir(current_opleiding, mode = "plot")
  
  ## Laad de dataset voor de huidige persona
  dfPersona <- lDfPersona[[groep]]
  
  ## Bepaal de groep
  .groep <- tolower(groep)
  
  Knit_Header("Naar {.groep}", 3)
  
  ## Bepaal of er in de dataset een groep is met minder dan 21 studenten;
  ## Zo ja, laat die buiten beschouwing en meld dit
  if (any(dfPersona$Subtotaal < 21)) {
    
    lCategorie_te_laag <- dfPersona |>
      filter(Subtotaal < 21) |>
      pull(Categorie) |>
      paste(collapse = ", ")
    
    Knit_print_rule(glue("Subtotaal voor {.groep}: {lCategorie_te_laag} is te laag voor een betrouwbare analyse."))
  }
  
  ## Open een panel-tabset
  Knit_print_rule(glue("::: {.panel-tabset}", 
                       .open = "{{", 
                       .close = "}}"))
  
  for (j in 1:nrow(dfPersona)) {
    
    ## Bepaal de huidige student
    student_current   <- dfPersona[j, ]
    student_groep     <- student_current$Groep
    student_categorie <- levels(student_current[[student_groep]])[j]
    
    sPlot <- glue("![]({plotdir}/lf_break_down_{tolower(student_groep)}_{tolower(student_categorie)}.png")
    Knit_print_rule(sPlot)
    
    ## ![](Index_verdieping_factoren_files/figure-html/lf_model_parts-1.png){width=672}
    ## ![](10. Output/gvs/b-hbo-v-vt/plots/lf_break_down_geslacht_m.png
    ## ![](10. Output/gvs/b-hbo-v-vt/plots/lf_break_down_distribution_all.png){width=1333}
    
  }
  
    #knit_print(glue("\n\n\n### Naar {.categorie}\n\n"))
    # lPlots <<- list.files(
    #   path = plotdir,
    #   pattern = glue("^lf_break_down_{.categorie}_(.*).png$"),
    #   full.names = TRUE
    # )
  
  ##knitr::include_graphics(as.character(lPlots), error = TRUE)
  
  ## Sluit de panel-tabset
  Knit_print_rule(":::")
  
}

## Functie om een Shapley plot te maken
Get_Shapley_plot <- function(data) {
  
  p <- data |> 
    ggplot(aes(contribution, variable, fill = mean_val > 0)) +
    geom_col(data = ~distinct(., variable, mean_val), 
             aes(mean_val, variable), 
             alpha = 0.5) +
    geom_boxplot(width = 0.5) +
    theme(legend.position = "none") +
    scale_fill_manual(values = c("TRUE" = lColors_default[["sNegative_color"]], 
                                 "FALSE" = lColors_default[["sPositive_color"]])) +
    
    # Bepaal de title en ondertitel
    labs(
      title = "Shapley values",
      subtitle = "Bijdrage per variabele voor de meest voorkomende student",
      caption = sCaption,
      x = NULL,
      y = NULL
    )
  
  return(p)
  
}

## Op basis van het bbplot package gebouwd (vandaar de namen in lowercase,
## zodat deze functies die van het bbplot package overschrijven)

## Left align
left_align <- function (plot_name, pieces) {
  grob <- ggplot2::ggplotGrob(plot_name)
  n <- length(pieces)
  grob$layout$l[grob$layout$name %in% pieces] <- 2
  return(grob)
}

## Save plot
save_plot <- function (plot_grid, width, height, save_filepath) {
  
  ggplot2::ggsave(
    filename = save_filepath,
    plot = plot_grid,
    width = (width / 72),
    height = (height / 72),
    bg = "white",
    device = ragg::agg_png,
    res = 300,
    create.dir = TRUE
  )
}

## Bewaar een afbeelding
Finalize_Plot <-
  function (plot_name,
            source_name,
            save_filepath = file.path(Sys.getenv("TMPDIR"),
                                      "tmp-nc.png"),
            width_pixels = nPlotWidth,
            height_pixels = nPlotHeight,
            show_plot = FALSE) {
    
    ## Print de plot
    plot_grid <- ggpubr::ggarrange(
      plot_name,
      ncol = 1,
      nrow = 2,
      # heights = c(1, 0.045 / (height_pixels / 450)) ## Correctie op BBC template (marge)
      heights = c(1, 0 / (height_pixels / 450))
    )
    save_plot(plot_grid, width_pixels, height_pixels, save_filepath)
    
    if(show_plot) {
      invisible(plot_grid)
    }
  }

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 8. HULP FUNCTIES ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Functie nummers om te zetten naar een leesbare notatie
Number_to_readable <- function(x, digits = 0) {
  return(formatC(round(x, digits), format = "f", digits = digits, big.mark = ".", decimal.mark = ","))
}

## Haal de dataset (versie) op
Get_sDataset <- function(df) {
  unique(df$LTA_Dataset)
}

## Functie om de metadata van de analyse te bepalen
Get_Metadata <- function() {
  
  lMetadata <- list(
    "sBron_label"    = "Bron",
    "sDataset_label" = "Dataset",
    "sPlot_label"    = "Plot",
    "sAnalyse_label" = "Analyse",
    "sInstelling"    = "De HHs",
    "sBron"          = "De HHs, IR & Analytics",
    "sDataset"       = lResearch_settings[["sDataset"]],
    "sAnalyse"       = "De HHs, Lectoraat Learning Technology & Analytics"
  )
  
  return(lMetadata)
}

