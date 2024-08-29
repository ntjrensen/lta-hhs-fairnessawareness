## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Fairness_HHs.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Lectoraat Learning Technology & Analytics De Haagse Hogeschool
## Copyright 2024 De HHs
## Web Page: http://www.hhs.nl
## Contact: Theo Bakker (t.c.bakker@hhs.nl)
## Verspreiding buiten De HHs: Nee
##
## Doel: Een totaal overzicht van bias per opleiding
##
## Afhankelijkheden: ltabase package
##
## Datasets: Geen
##
## Opmerkingen:
## 1) Geen.
## 2) ___
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Geschiedenis:
## 04-08-2024: TB: Aanmaak bestand
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. INCLUDE VOORBEREIDINGEN  ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("91. Verdiepende analyses/Include_Voorbereidingen_Verdieping.R")

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. LAAD BESTANDEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.1 Lijst van alle .rds bestanden in de directory ####

dir <- "10. Output"
lFiles <- list.files(path = dir, recursive = TRUE, pattern = "*.rds", full.names = TRUE)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.2 Behoud paden met 'fairness' ####

lFiles <- lFiles[grepl("fairness", lFiles)]

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.3 Laad en combineer de data: dfFairness_HHs ####

# Functie om de faculteit, opleiding, vorm en soort analyse te extraheren uit het pad
dfFairness_HHs <- bind_rows(lapply(lFiles, function(file) {
  df <- readRDS(file)
  df <- df |>
    mutate(
      FRN_Analyse = "Retentie na 1 jaar"
    )
  return(df)
}))

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. BEWERK BESTANDEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.1 Maak UUID en sub dataframes ####

## Variabelen en hun waarden
dfVars <- tribble(
  ~FRN_Group, ~FRN_Subgroup,
  "Geslacht",      "M",
  "Geslacht",      "V",
  "Vooropleiding", "MBO",
  "Vooropleiding", "HAVO",
  "Vooropleiding", "VWO",
  "Vooropleiding", "BD",
  "Vooropleiding", "CD",
  "Vooropleiding", "HO",
  "Vooropleiding", "Overig",
  "Vooropleiding", "Onbekend",
  "Aansluiting",   "Direct",
  "Aansluiting",   "Tussenjaar",
  "Aansluiting",   "Switch extern",
  "Aansluiting",   "Switch intern",
  "Aansluiting",   "Na CD",
  "Aansluiting",   "2e Studie",
  "Aansluiting",   "Overig",
  "Aansluiting",   "Onbekend"
)

## Maak een FRN_UUID
dfFairness_HHs <- dfFairness_HHs |> 
  mutate(FRN_UUID = paste(FRN_Faculteit, FRN_Opleidingstype, FRN_Opleiding, FRN_Opleidingsvorm, sep = "_")) |> 
  select(-c(FRN_Faculteit, FRN_Opleidingstype, FRN_Opleiding, FRN_Opleidingsvorm))

## Model
dfModel <- tibble(
  FRN_Model = unique(dfFairness_HHs$FRN_Model)
)

## Metrics
dfMetrics <- tibble(
  FRN_Metric = unique(dfFairness_HHs$FRN_Metric)
)

## UUID
dfUUID <- tibble(
  FRN_UUID = unique(dfFairness_HHs$FRN_UUID)
)

## Soorten analyes
dfSoortAnalyse <- tibble(
  FRN_Analyse = unique(dfFairness_HHs$FRN_Analyse)
)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.2 Join de dataframes ####

## Maak een df met alle mogelijke combinaties
dfVars_Metrics <- expand_grid(dfVars, dfModel, dfMetrics, dfUUID, dfSoortAnalyse)

## Koppeld deze aan dfFairness_HHs
dfFairness_HHs.2 <- dfFairness_HHs |>
  full_join(
    dfVars_Metrics,
    by = join_by(
      FRN_Model,
      FRN_Group,
      FRN_Subgroup,
      FRN_Metric,
      FRN_UUID,
      FRN_Analyse
    )
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.3 Split FRN_UUID ####

## Splits de FRN_UUID waar naar Faculteit, Opleidingstype, Opleiding en Opleidingsvorm
dfFairness_HHs.3 <- dfFairness_HHs.2 |>
  separate(
    FRN_UUID, into = c("FRN_Faculteit", 
                       "FRN_Opleidingstype", 
                       "FRN_Opleiding", 
                       "FRN_Opleidingsvorm"),
    sep = "_"
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.4 Voeg de naam van de opleiding toe ####

dfFairness_HHs.4 <- dfFairness_HHs.3 |>
  left_join(
    dfOpleidingen |> 
      select(INS_Opleiding, INS_Opleidingsnaam_huidig) |> 
      distinct(),
    by = c("FRN_Opleiding" = "INS_Opleiding")
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.5 Bepaal welke bias er is ####

dfFairness_HHs.5 <- dfFairness_HHs.4 |>
  mutate(
    FRN_Bias = case_when(
      is.na(FRN_Score) ~ NA,
      FRN_Score < 0.8 ~ "Negatieve Bias",
      FRN_Score > 1.25 ~ "Positieve Bias",
      .default = "Geen Bias"
    )
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.6 Bepaal of er enige bias is ####

dfFairness_HHs.6 <- dfFairness_HHs.5 |>
  select(-c(FRN_Model, FRN_Metric, FRN_Analyse, FRN_Fair, N)) |>
  group_by(FRN_Group, 
           FRN_Subgroup, 
           FRN_Bias,
           FRN_Faculteit,
           FRN_Opleidingsvorm,
           FRN_Opleiding,
           INS_Opleidingsnaam_huidig) |>
  
  ## Tel het aantal Bias per groep
  summarise(FRN_Bias_count = n(), 
            .groups = "drop") |> 
  
  ## Bepaal de juist volgorde van de variabelen
  select(FRN_Faculteit,
         INS_Opleidingsnaam_huidig,
         FRN_Opleiding,
         FRN_Opleidingsvorm,
         everything()) |> 

  ## Pivot de data
  pivot_wider(names_from = FRN_Bias, 
              values_from = c(FRN_Bias_count),
              values_fill = list(FRN_Bias_count = 0)) |> 
  
  ## Vervang NA door 0
  replace_na(list(`Geen Bias` = 0,
                  `Negatieve Bias` = 0,
                  `Positieve Bias` = 0)) |>
  
  ## Voeg Bias toe
  ## Pas de Bias aan
  mutate(Bias = case_when(
    `Negatieve Bias` > 1 | `Positieve Bias` > 1 ~ 'Ja',
    `Geen Bias` == 0 & `Negatieve Bias` == 0 & `Positieve Bias` == 0 ~ 'NTB',
    .default = "Nee")) |> 
  
  ## Hernoem de kolommen zodat deze in de tabel beter leesbaar zijn
  rename(Faculteit = FRN_Faculteit,
         Opleidingsnaam = INS_Opleidingsnaam_huidig,
         Opleiding = FRN_Opleiding,
         Opleidingsvorm = FRN_Opleidingsvorm,
         Variabele = FRN_Group,
         Groep = FRN_Subgroup) |>
  
  ## Sorteer de Variabele en Groep
  mutate(Variabele = factor(Variabele, 
                            levels = c("Geslacht", 
                                       "Vooropleiding", 
                                       "Aansluiting")),
         Groep = factor(Groep,
                        levels = c(lLevels_geslacht, 
                                   ## Maak lLevels_vop uniek om Overig en onbekend niet te herhalen
                                   setdiff(lLevels_vop, lLevels_aansluiting), 
                                   lLevels_aansluiting))
  ) |> 
  
  ## Selecteer de nodige kolommen
  select(Faculteit, Opleidingsnaam, Opleiding, Opleidingsvorm,
         Variabele, Groep, Bias, `Geen Bias`, `Negatieve Bias`, `Positieve Bias`) |> 
  
  ## Sorteer de data
  arrange(Faculteit, Opleidingsnaam, Opleiding, Opleidingsvorm, 
          Variabele, Groep)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.7 Bepaal tellingen ####

## Maak een dataframe voor alle opleidingen
tblOpleidingen <- dfFairness_HHs.6 |>
  select(Opleiding, Opleidingsvorm) |>
  distinct()

## Maak een lijst van Inschrijvingen
lInschrijvingen <- list()

## Loop over de opleidingen, haal de inschrijvingen op en voeg deze toe aan de lijst
for (i in 1:nrow(tblOpleidingen)) {
  
  ## Bepaal de opleiding en opleidingsvorm
  opleiding <- tblOpleidingen$Opleiding[i]
  opleidingsvorm <- tblOpleidingen$Opleidingsvorm[i]
  
  ## Maak de variabelen voor de huidige opleiding op basis van de opleidingsnaam en opleidingsvorm
  current_opleiding <- Get_Current_Opleiding(
    opleiding = opleiding,
    opleidingsvorm = toupper(opleidingsvorm)
  )
  
  ## Bepaal op basis hiervan afgeleide variabelen
  Set_Current_Opleiding_Vars(current_opleiding, debug = F)
  
  dfOpleiding_inschrijvingen_base <- get_lta_studyprogram_enrollments_pin(
    board = "HHs/Inschrijvingen",
    faculty = faculteit,
    studyprogram = opleidingsnaam_huidig,
    studytrack = opleiding,
    studyform = toupper(opleidingsvorm),
    range = "eerstejaars")
  
  lInschrijvingen[[i]] <- dfOpleiding_inschrijvingen_base
  
}

## Combineer de lijst met inschrijvingen
dfInschrijvingen <- bind_rows(lInschrijvingen)

## Maak een df met tellingen van de variabelen
dfTellingen <- dfInschrijvingen |>
  select(INS_Faculteit,
         INS_Opleiding,
         INS_Opleidingsvorm,
         DEM_Geslacht, 
         VOP_Toelaatgevende_vooropleiding_soort, 
         INS_Aansluiting_LTA) |>
  rename(Geslacht = DEM_Geslacht,
         Vooropleiding = VOP_Toelaatgevende_vooropleiding_soort,
         Aansluiting = INS_Aansluiting_LTA) |>
  pivot_longer(cols = c(Geslacht, Vooropleiding, Aansluiting)) |>
  group_by(INS_Faculteit, INS_Opleiding, INS_Opleidingsvorm, name, value) |>
  summarise(N = n()) |>
  ungroup()

## Voeg de tellingen toe aan de data
dfFairness_HHs.7 <- dfFairness_HHs.6 |>
  left_join(
    dfTellingen |> 
      mutate(INS_Opleidingsvorm = tolower(INS_Opleidingsvorm)),
    by = c("Faculteit" = "INS_Faculteit",
           "Opleiding" = "INS_Opleiding",
           "Opleidingsvorm" = "INS_Opleidingsvorm",
           "Variabele" = "name",
           "Groep" = "value")
  ) |>
  replace_na(list(N = 0))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.8 Maak een korte versie van de opleidingen ####

## Maak van de opleiding een korte versie voor de tabel
dfFairness_HHs.8 <- dfFairness_HHs.7 |>
  mutate(Opleidingsvorm = toupper(Opleidingsvorm),
         Opleiding = paste0(Opleidingsnaam, " ", 
                            Opleidingsvorm, 
                            " (", Opleiding, ")")) |> 
  select(-Opleidingsnaam, -Opleidingsvorm)
  
## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. FAIRNESS FLEXTABLE ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4.1 Bepaal de kleuren #### 

sColor_Bias_Positive      <- "#9DBF9E"
sColor_Bias_Negative      <- "#A84268"
sColor_Bias_Neutral       <- "#FCB97D"
sColor_Bias_None          <- "#E5E5E5"
sColor_Bias_Undetermined  <- "#FFFFFF"

sColor_Bias_Positive      <- "#174D7C"
sColor_Bias_Positive      <- "#4C7D8E"
sColor_Bias_Negative      <- "#FD6D4E"

sColor_Bias_Positive      <- "#1170AA"
sColor_Bias_Negative      <- "#FC7D0B"

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4.2 Maak de tabel (versie 1 - long) #### 

ftFairness.1 <- flextable(dfFairness_HHs.8 |> 
                            dplyr::filter(N > 0))

# Voeg de kolom 'Variabele' samen voor visueel groeperen
# Pas voorwaardelijke opmaak toe
ftFairness.1 <- ftFairness.1 |>
    merge_v(j = c("Faculteit",
                  "Opleiding",
                  "Variabele")) |>
    fix_border_issues() |>
    theme_vanilla() |>
    set_header_labels(
      Variabele = "Variabele",
      Groep = "Groep",
      Bias = "Bias",
      `Geen Bias` = "Geen Bias",
      `Negatieve Bias` = "Negatieve Bias",
      `Positieve Bias` = "Positieve Bias"
    ) |>
    autofit() |> 
    italic(j = 3, italic = TRUE, part = "body") |> 
    color(i = ~ `Negatieve Bias` > 1,
          j = c("Groep", "Bias", "Negatieve Bias"),
          color = "white") |>
    color(i = ~ `Positieve Bias` > 1,
          j = c("Groep", "Bias", "Positieve Bias"),
          color = "white") |>
    bg(i = ~ `Negatieve Bias` > 1, 
       j = c("Groep", "Bias", "Negatieve Bias"), 
       bg = sColor_Bias_Negative) |>
    bg(i = ~ `Positieve Bias` > 1, 
       j = c("Groep", "Bias", "Positieve Bias"), 
       bg = sColor_Bias_Positive) |>
    bg(i = ~ `Negatieve Bias` > 1 & `Positieve Bias` > 1, 
       j = c("Groep", "Bias"), 
       bg = sColor_Bias_Neutral) |>
    bg(i = ~ N < 15 & (`Negatieve Bias` > 1 | `Positieve Bias` > 1), 
       j = c("Groep", "Bias"), 
       bg = sColor_Bias_Neutral) |>
    bg(i = ~ `Geen Bias` == 0 & `Positieve Bias` == 0 & `Negatieve Bias` == 0,
       j = 4:5,
       bg = sColor_Bias_None) |>
    bold(i = ~ `Negatieve Bias` > 1,
         j = c("Groep", "Bias", "Negatieve Bias")) |>
    bold(i = ~ `Positieve Bias` > 1,
         j = c("Groep", "Bias", "Positieve Bias")) |> 
    valign(j = 1:3, valign = "top", part = "all") |> 
    valign(i = 1, valign = "middle", part = "header") |> 
    align_text_col(align = "left") |> 
    align_nottext_col(align = "center") 
  
## Toon de flextable
ftFairness.1

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4.3 Maak de tabel (versie 2 - wide) #### 

dfFairness_HHs.9 <- dfFairness_HHs.8 |> 
  mutate(Bias = case_when(
    N == 0 ~ 'Geen data',
    N <= 15 ~ 'Onbeslist',
    `Negatieve Bias` > 1 & `Positieve Bias` > 1 ~ 'Onbeslist',
    `Negatieve Bias` > 1 ~ 'Negatief',
    `Positieve Bias` > 1 ~ 'Positief',
    `Geen Bias` == 0 & `Negatieve Bias` == 0 & `Positieve Bias` == 0 ~ 'NTB',
    .default = "Nee")) |>
  select(-c(`Negatieve Bias`, `Positieve Bias`, `Geen Bias`, N)) |> 
  pivot_wider(names_from = c(Variabele, Groep), 
              values_from = Bias) |> 
  
  ## Verwijder de kolommen met 'Onbekend' en 'Overig'
  select(-c(Vooropleiding_Onbekend, 
            Aansluiting_Onbekend,
            Vooropleiding_Overig, 
            Aansluiting_Overig)) |> 
  
  ## Hernoem de kolommen: bewaar alleen wat er achter een _ staat als er een _ voorkomt
  rename_with(~ gsub(".*_", "", .x)) 

## Pas de waarden bij M/V aan op basis van afleiding bij V/M
dfFairness_HHs.10 <- dfFairness_HHs.9 |> 
  rowwise() |>
  mutate(
    M = case_when(
      V == "Negatief" ~ "Positief",
      V == "Positief" ~ "Negatief",
      M == "NTB" ~ "Nee",
      .default = M
    ),
    V = case_when(
      M == "Negatief" ~ "Positief",
      M == "Positief" ~ "Negatief",
      V == "NTB" ~ "Nee",
      .default = V
    )
  )

## Maak een basis flextable op basis van dfFairness_HHs.9
ftFairness.2 <- flextable(dfFairness_HHs.10)

## Pas de basis layout aan
ftFairness.2 <- ftFairness.2 |>
  add_header_row(values = c("", "Geslacht", "Vooropleiding", "Aansluiting"), 
                 colwidths = c(2, 2, 6, 6)) |>
  merge_v(j = c("Faculteit")) |>
  fix_border_issues() |>
  theme_vanilla() |>
  autofit() |> 
  valign(i = 1:2, valign = "middle", part = "header") |> 
  valign(j = 1:2, valign = "top", part = "body") |> 
  align(j = 1:2, align = "left") |> 
  align(j = 3:16, align = "center", part = "all")

# Functie om de kleur van de achtergrond te bepalen
Get_Fairness_Bg <- function(value) {
  case_when(
    value == "Negatief"  ~ sColor_Bias_Negative,
    value == "Positief"  ~ sColor_Bias_Positive,
    value == "Onbeslist" ~ sColor_Bias_Undetermined,
    value == "Geen data" ~ "white",
    value == "NTB"       ~ sColor_Bias_None,
    TRUE ~ "white"
  )
}

# Functie om de kleur van de tekst te bepalen
Get_Fairness_Color <- function(value) {
  case_when(
    value == "Negatief"  ~ "white",
    value == "Positief"  ~ "white",
    value == "Onbeslist" ~ "black",
    value == "Geen data" ~ "black",
    value == "NTB"       ~ "black",
    TRUE ~ "black"
  )
}

# Pas de kleuren toe per kolom
for (col in colnames(dfFairness_HHs.10)[-1]) {
  
  ## Pas de achtergrond aan
  colors <- map_chr(dfFairness_HHs.10[[col]], Get_Fairness_Bg)
  ftFairness.2 <- ftFairness.2 |>
    bg(j = col, bg = colors)
  
  ## Pas de kleur aan
  colors <- map_chr(dfFairness_HHs.10[[col]], Get_Fairness_Color)
  ftFairness.2 <- ftFairness.2 |>
    color(j = col, color = colors)
}

# Voeg verticale lijnen toe tussen de gewenste kolommen
border_vline <- fp_border(color = "black", width = 1)

# Pas de borderlines aan
ftFairness.2 <- ftFairness.2 |>
  vline(j = c(2, 4, 10), border = border_vline)

## Bewaar flextable 2
ftFairness.2 |> 
  save_as_image(path = "10. Output/all/Fairness_HHs_2.png")

## Toon de flextable
ftFairness.2

## Corrigeer beoordeling in de flextable voor Geslacht:
## Als M = Negatief, dan V = Positief
## Als V = Negatief, dan M = Positief
## Als M = Positief, dan V = Negatief
## Als V = Positief, dan M = Negatief

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5. OPRUIMEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(list = ls())
