## Create a list of dfPersonas
lDfPersona <- list()

## Walk over the variables
lDfPersona <- map(lSensitive_labels,
                  ~ Get_dfPersona_Recursive(.x)) |>
  set_names(lSensitive_labels)

dfPersona_per_group <- bind_rows(lDfPersona) 

## Save this file as an Excel spreadsheet
sOutputPath <- file.path("R/data", "dfPersona_per_group.xlsx")
writexl::write_xlsx(dfPersona_per_group, sOutputPath)

## Load the personas
dfPersona_all <- Get_dfPersona_Recursive()
