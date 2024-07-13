# Laad bibliotheken
library(dplyr)

# Functie om missende waarden in factorvariabelen te behandelen
Process_Factor_Variables <- function(df) {
  df <- df %>%
    mutate(across(where(is.factor), ~ {
      # Controleer of de factor 'Weet niet' bevat en verder uitsluitend numerieke waarden of NA's
      factor_levels <- levels(.)
      if (any(factor_levels == "Weet niet") && all(factor_levels %in% c("Weet niet", as.character(0:99999), NA))) {
        # Converteer factor naar karakter
        char_col <- as.character(.)
        # Vervang 'Weet niet' door 99999
        char_col[char_col == "Weet niet"] <- "99999"
        # Converteer terug naar numeriek (NA's worden automatisch geïntroduceerd voor niet-numerieke waarden)
        as.numeric(char_col)
      } else {
        # Als het geen specifieke factor is, behoud de originele factor kolom
        .
      }
    })) 
  
  df <- df %>%
    mutate(across(where(is.numeric), list(
      ori = ~ .,
      na = ~ ifelse(. == 99999, NA, .),
      imputed = ~ ifelse(. == 99999, median(.[. != 99999], na.rm = TRUE), .)
    ), .names = "{col}_{fn}"))
  
  # Verplaats de nieuwe kolommen achter de oorspronkelijke kolom
  col_names <- names(df)
  num_cols <- col_names[sapply(df, is.numeric)]
  for (col in num_cols) {
    df <- df %>%
      relocate(starts_with(paste0(col, "_")), .after = all_of(col))
  }
  
  return(df)
}

# Voorbeeld dataframe
dfOriginal <- data.frame(
  Maanden = factor(c(12, 24, "Weet niet", 36, 48, 60, "Weet niet")),
  AndereVariabele = factor(c(5, 10, 15, 20, 25, "Weet niet", 35)),
  Categorie = factor(c("A", "B", "C", "A", "B", "C", "A"))
)

# Pas de functie toe
dfProcessed <- Process_Factor_Variables(dfOriginal)

# Toon het resultaat
print(dfProcessed)
