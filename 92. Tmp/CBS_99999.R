# Laad bibliotheken
library(dplyr)

# Functie om missende waarden in CBS te behandelen
Process_Missing_Numeric <- function(df) {
  
  ## Vervang 99999 door NA en impute de missende waarden met de mediaan
  df <- df |>
    mutate(across(
      where(is.numeric),
      list(
        ori = ~ .,
        na = ~ ifelse(. == 99999, NA, .),
        imputed = ~ ifelse(. == 99999, median(.[. != 99999], na.rm = TRUE), .)
      ),
      .names = "{col}_{fn}"
    ))
  
  return(df)
}

# Functie om te testen of 99999 nog voorkomt
Test_Missing <- function(df) {
  any(sapply(df, function(x) any(x == 99999, na.rm = TRUE)))
}

# Voorbeeld dataframe
dfOriginal <- data.frame(
  Maanden = c(12, 24, 99999, 36, 48, 60, 99999),
  AndereVariabele = c(5, 10, 15, 20, 25, 99999, 35)
)

# Pas de functie toe
dfProcessed <- Process_Missing_Numeric(dfOriginal)

# Test of 99999 nog voorkomt in de verwerkte dataframe
contains_99999 <- Test_Missing(dfProcessed)

# Toon het resultaat
print(dfProcessed)
cat("Bevat de verwerkte dataset nog waarden van 99999? ", contains_99999, "\n")
