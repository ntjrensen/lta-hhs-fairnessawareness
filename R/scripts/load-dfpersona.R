## Create a list of dfPersonas
lDfPersona <- list()

## Walk over the variables
lDfPersona <- map(sensitive_labels,
                  ~ get_df_persona_recursive(.x)) |>
  set_names(sensitive_labels)

dfPersona_per_group <- bind_rows(lDfPersona) 

## Save this file as an Excel spreadsheet
sOutputPath <- file.path("R/data", "dfPersona_per_group.xlsx")
writexl::write_xlsx(dfPersona_per_group, sOutputPath)

## Load the personas
df_persona_all <- get_df_persona_recursive()
