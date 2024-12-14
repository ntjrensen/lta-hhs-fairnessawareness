# Functie om een persona te maken van de studenten van een opleiding
Get_dfPersona <- function(group = NULL) {
  
  ## Bepaal de categorische variabelen die gebruikt worden
  lSelect_categorical <- c(
    "Geslacht",
    "Vooropleiding",
    "Aansluiting",
    "Studiekeuzeprofiel",
    "APCG",
    "Cijfer_CE_VO_missing",
    "Cijfer_SE_VO_missing",
    "Cijfer_CE_Nederlands_missing",
    "Cijfer_CE_Engels_missing",
    "Cijfer_CE_Wiskunde_missing",
    "Cijfer_CE_Natuurkunde_missing",
    "Dubbele_studie"
  ) 
  
  ## Verwijder de huidige groep variabele uit deze lijst
  if (!is.null(group)) {
    .group <- as.name(group)
    # Verwijder de groep variabele uit deze lijst
    lSelect_categorical <- setdiff(lSelect_categorical, group)
  }
  
  ## Verwijder variabelen die niet voorkomen in deze opleiding
  lSelect_categorical <- intersect(lSelect_categorical, 
                                   colnames(dfOpleiding_inschrijvingen))
  
  ## Bepaal de numerieke variabelen die gebruikt worden
  lSelect_numerical <- c(
    "Leeftijd",
    "Aanmelding",
    "Reistijd",
    "Cijfer_CE_VO",
    "Cijfer_SE_VO",
    "Cijfer_CE_Nederlands",
    "Cijfer_CE_Wiskunde",
    "Cijfer_CE_Engels",
    "Cijfer_CE_Natuurkunde",
    "SES_Totaal",
    "SES_Welvaart",
    "SES_Arbeid"
  )
  
  ## Als de opleiding gelijk is aan HDT, voeg dan Rangnummer toe
  if(current_opleiding$INS_Opleiding == "HDT") {
    lSelect_numerical <- c(lSelect_numerical, "Rangnummer")
  }
  
  ## Verwijder variabelen die niet voorkomen in deze opleiding
  lSelect_numerical <- intersect(lSelect_numerical, 
                                 colnames(dfOpleiding_inschrijvingen))
  
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
      mutate(Groep = group,
             Categorie = !!.group) |>
      
      # Mutate leeftijd naar integer
      mutate(Leeftijd = as.integer(round(Leeftijd, 0))) |>
      
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
      
      # Mutate leeftijd naar integer
      mutate(Leeftijd = as.integer(round(Leeftijd, 0))) |>
      
      # Herorden
      select(Groep, Categorie, Totaal, Subtotaal, Percentage, everything())
  }
  
  return(dfPersona)
}
