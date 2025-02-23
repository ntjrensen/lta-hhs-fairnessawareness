## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Test recursive dfPersona ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Lectoraat Learning Technology & Analytics De Haagse Hogeschool
## Copyright 2025 De HHs
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
## 28-01-2025: TB: Aanmaak bestand
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Get_dfPersona_Recursive <- function(variable_list = NULL) {
  
  # Initialize the result dataframe
  dfResults <- NULL
  
  # Initialize the working dataframe
  dfWorking <- dfOpleiding_inschrijvingen
  
  # Calculate the total number of students
  .totaal <- dfWorking |> count() |> pull(n)
  
  # Define categorical and numerical variables present in the dataframe
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
  ) |> intersect(colnames(dfWorking))
  
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
  ) |> intersect(colnames(dfWorking))
  
  # Add "Rangnummer" if the study programme is HDT
  if (current_opleiding$INS_Opleiding == "HDT") {
    lSelect_numerical <- c(lSelect_numerical, "Rangnummer") |> 
      intersect(colnames(dfWorking))
  }
  
  if (is.null(variable_list)) {
    # If no variable list is provided, calculate for the entire dataset
    dfResults <- dfWorking |>
      summarise(
        # Categorical variables
        across(
          all_of(lSelect_categorical), 
          Get_Mostcommon_Category,
          .names = "{col}"
        ),
        # Numerical variables
        across(
          all_of(lSelect_numerical),
          Get_Median_Rounded,
          .names = "{col}"
        ),
        # Other variables
        Collegejaar = median(Collegejaar, na.rm = TRUE),
        ID = NA,
        Subtotaal = n(),
        .groups = "drop"
      ) |>
      mutate(
        Totaal = .totaal,
        Percentage = round(Subtotaal / Totaal, 3),
        Groep = "Alle",
        Categorie = "Alle studenten"
      ) |>
      mutate(Leeftijd = as.integer(round(Leeftijd, 0))) |>
      select(Groep, Categorie, Totaal, Subtotaal, Percentage, everything())
    
  } else {
    
    # Loop through each variable in the list
    for (variable in variable_list) {
      
      # Convert variable to symbol for dynamic grouping
      .variable <- as.name(variable)
      
      # Exclude the grouping variable from the categorical variables
      lSelect_categorical <- setdiff(lSelect_categorical, variable)
      
      # Check if the current variable exists in the dataframe
      if (!(variable %in% colnames(dfWorking))) {
        warning(paste("Variable", variable, "not found in the dataset. Skipping."))
        next
      }
      
      # Summarise data for the current variable
      dfPersona <- dfWorking |>
        group_by(!!.variable) |>
        summarise(
          # Categorical variables
          across(
            all_of(lSelect_categorical), 
            Get_Mostcommon_Category,
            .names = "{col}"
          ),
          # Numerical variables
          across(
            all_of(lSelect_numerical),
            Get_Median_Rounded,
            .names = "{col}"
          ),
          # Other variables
          Collegejaar = median(Collegejaar, na.rm = TRUE),
          ID = NA,
          Subtotaal = n(),
          .groups = "drop"
        ) |>
        mutate(
          Totaal = .totaal,
          Percentage = round(Subtotaal / Totaal, 3),
          Groep = variable,
          Categorie = !!.variable
        ) |>
        mutate(Leeftijd = as.integer(round(Leeftijd, 0))) |>
        select(Groep, Categorie, Totaal, Subtotaal, Percentage, everything())
      
      # Append to the result dataframe
      dfResults <- bind_rows(dfResults, dfPersona)
      
      # Update the working dataframe for the next iteration
      dfWorking <- dfWorking |> filter(!!.variable == Get_Mostcommon_Category(dfWorking[[variable]]))
    }
  }
  
  return(dfResults)
}

# Example usage
# For the entire dataset
dfPersona_All <- Get_dfPersona_Recursive()

# For a specific set of variables
variable_list <- c("Geslacht", "Vooropleiding", "Aansluiting")
dfPersona_Grouped <- Get_dfPersona_Recursive(variable_list)



