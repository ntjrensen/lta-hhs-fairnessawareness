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

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INSPECTEER dfOpleiding_inschrijvingen ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.1 Namen en inhoud ####

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

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.2 Functies ####

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

## Functie om een persona te maken van de studenten van een opleiding
Get_dfPersona_1 <- function(group) {
  
  .group <- as.name(group)
  
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
  
  ## Verwijder de groep variabele uit deze lijst
  lSelect_categorical <- setdiff(lSelect_categorical, group)
  
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
  
  ## Bereken het totaal voor deze opleiding
  .totaal <- dfOpleiding_inschrijvingen |> 
    count() |> 
    pull(n)
  
  ## Maak personas aan op basis van de opleiding
  dfPersona <- dfOpleiding_inschrijvingen |>
    
    ## Split de opleidingen op basis van de Vooropleiding
    group_by(!!.group) |>
    
    ## Maak een persona aan op basis van de overige variabelen: 
    ## kies de meest voorkomende waarden per variabele bij categorieën en de mediaan bij numerieke variabelen
    summarise(
      
      ## Categorische variabelen
      across(
        all_of(lSelect_categorical), 
        Get_Mostcommon_Category,
        .names = "{col}"
      ),
      
      ## Numerieke variabelen
      across(
        all_of(lSelect_numerical),
        Get_Median_Rounded,
        .names = "{col}"
      ), 
      
      ## Overige variabelen
      Collegejaar                   = median(Collegejaar),
      ID                            = NA,
      
      ## Subtotaal aantal studenten
      Subtotaal = n(),
      
      .groups = "drop") |> 
    
    ## Tel het aantal studenten per groep
    mutate(Totaal = .totaal,
           Percentage = round(Subtotaal/Totaal, 3)) |>
    
    ## Voeg de groep variabele toe en bepaal de categorie binnen de groep
    mutate(Groep = group,
           Categorie = !!.group) |>
    
    ## Herorden
    select(Groep, Categorie, Totaal, Subtotaal, Percentage, everything())
  
  return(dfPersona)
  
}

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BOUW PERSONAS ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Maak een lijst van dfPersonas
lDfPersona <- list()

## Loop over de variabelen
lDfPersona <- map(c("Geslacht", "Vooropleiding", "Aansluiting"), ~ Get_dfPersona(.x)) |> 
  set_names(c("Geslacht", "Vooropleiding", "Aansluiting"))

## Voeg de personas samen
## dfPersona <- bind_rows(lDfPersona)

