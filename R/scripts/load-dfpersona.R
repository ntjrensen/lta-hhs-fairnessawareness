## Create a list of dfPersonas
df_persona_list <- list()

## Walk over the variables
df_persona_list <- map(sensitive_labels,
                       ~ get_df_persona_recursive(.x)) |>
  set_names(sensitive_labels)

df_persona_per_group <- bind_rows(df_persona_list) 

## Save this file as an Excel spreadsheet
output_path <- file.path("R/data", "df_persona_per_group.xlsx")
writexl::write_xlsx(df_persona_per_group, output_path)

## Load the personas
df_persona_all <- get_df_persona_recursive()
