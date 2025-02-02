# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 01.fairness.thuas.R ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R code of the Learning Technology & Analytics Research Group of THUAS
# Copyright 2025 THUAS
# Web Page: http://www.thuas.com
# Contact: Theo Bakker (t.c.bakker@hhs.nl)
# Distribution outside THUAS: No
#
# Purpose: Een totaal overzicht van bias per opleiding
#
# Dependencies: ltabase package
#
# Datasets: Geen
#
# Remarks:
# 1) None.
# 2) ___
#
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Version history:
# 04-08-2024: TB: File creation
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. INCLUDE PREPARATIONS  ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("R/scripts/preparations.explorations.R")

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. LOAD FILE ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2.1 List of all .rds files in the directory ####

dir <- "10. Output"
lFiles <- list.files(path = dir, recursive = TRUE, pattern = "*.rds", full.names = TRUE)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2.2 Preserve trails with 'fairness' ####

lFiles <- lFiles[grepl("fairness", lFiles)]

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2.3 Load and combine the data: dfFairness_HHs ####

# Function to extract faculty, study programmes, form and type of analysis from the path
dfFairness_HHs <- bind_rows(lapply(lFiles, function(file) {
  df <- readRDS(file)
  df <- df |>
    mutate(
      FRN_Analyse = "Retentie na 1 jaar"
    )
  return(df)
}))

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. EDIT FILES ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3.1 Create UUID and sub data frames ####

# Variables and their values
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

# Create a FRN_UUID
dfFairness_HHs <- dfFairness_HHs |> 
  mutate(FRN_UUID = paste(FRN_Faculteit, FRN_Opleidingstype, FRN_Opleiding, FRN_Opleidingsvorm, sep = "_")) |> 
  select(-c(FRN_Faculteit, FRN_Opleidingstype, FRN_Opleiding, FRN_Opleidingsvorm))

# Model
dfModel <- tibble(
  FRN_Model = unique(dfFairness_HHs$FRN_Model)
)

# Metrics
dfMetrics <- tibble(
  FRN_Metric = unique(dfFairness_HHs$FRN_Metric)
)

# UUID
dfUUID <- tibble(
  FRN_UUID = unique(dfFairness_HHs$FRN_UUID)
)

# Types of analyzes
dfSoortAnalyse <- tibble(
  FRN_Analyse = unique(dfFairness_HHs$FRN_Analyse)
)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3.2 Join de dataframes ####

# Create a df with all possible combinations
dfVars_Metrics <- expand_grid(dfVars, dfModel, dfMetrics, dfUUID, dfSoortAnalyse)

# Link this to dfFairness_ THUAS
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

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3.3 Split FRN_UUID ####

# Split the FRN_UUID into Faculteit, Opleidingstype, Opleiding and Opleidingsvorm
dfFairness_HHs.3 <- dfFairness_HHs.2 |>
  separate(
    FRN_UUID, into = c("FRN_Faculteit", 
                       "FRN_Opleidingstype", 
                       "FRN_Opleiding", 
                       "FRN_Opleidingsvorm"),
    sep = "_"
  )

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3.4 Add the name of the study programme ####

dfFairness_HHs.4 <- dfFairness_HHs.3 |>
  left_join(
    dfOpleidingen |> 
      select(INS_Opleiding, INS_Opleidingsnaam_huidig) |> 
      distinct(),
    by = c("FRN_Opleiding" = "INS_Opleiding")
  )

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3.5 Determine what bias there is ####

dfFairness_HHs.5 <- dfFairness_HHs.4 |>
  mutate(
    FRN_Bias = case_when(
      is.na(FRN_Score) ~ NA,
      FRN_Score < 0.8 ~ "Negatieve Bias",
      FRN_Score > 1.25 ~ "Positieve Bias",
      .default = "Geen Bias"
    )
  )

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3.6 Determine if there is any bias ####

dfFairness_HHs.6 <- dfFairness_HHs.5 |>
  select(-c(FRN_Model, FRN_Metric, FRN_Analyse, FRN_Fair, N)) |>
  group_by(FRN_Group, 
           FRN_Subgroup, 
           FRN_Bias,
           FRN_Faculteit,
           FRN_Opleidingsvorm,
           FRN_Opleiding,
           INS_Opleidingsnaam_huidig) |>
  
  # Count the number of Bias per group
  summarise(FRN_Bias_count = n(), 
            .groups = "drop") |> 
  
  # Determine the correct order of the variables
  select(FRN_Faculteit,
         INS_Opleidingsnaam_huidig,
         FRN_Opleiding,
         FRN_Opleidingsvorm,
         everything()) |> 

  # Pivot the data
  pivot_wider(names_from = FRN_Bias, 
              values_from = c(FRN_Bias_count),
              values_fill = list(FRN_Bias_count = 0)) |> 
  
  # Replace NA by 0
  replace_na(list(`Geen Bias` = 0,
                  `Negatieve Bias` = 0,
                  `Positieve Bias` = 0)) |>
  
  # Add Bias/Adjust the Bias
  mutate(Bias = case_when(
    `Negatieve Bias` > 1 | `Positieve Bias` > 1 ~ 'Ja',
    `Geen Bias` == 0 & `Negatieve Bias` == 0 & `Positieve Bias` == 0 ~ 'NTB',
    .default = "Nee")) |> 
  
  # Rename the columns to make them more readable in the table
  rename(Faculteit = FRN_Faculteit,
         Opleidingsnaam = INS_Opleidingsnaam_huidig,
         Opleiding = FRN_Opleiding,
         Opleidingsvorm = FRN_Opleidingsvorm,
         Variabele = FRN_Group,
         Groep = FRN_Subgroup) |>
  
  # Sort the Variable and Group
  mutate(Variabele = factor(Variabele, 
                            levels = lSensitive_labels),
         Groep = factor(Groep,
                        levels = c(lLevels[["Geslacht"]], 
                                   # Maak lLevels[["Vooropleiding"]] uniek om Overig en onbekend niet te herhalen
                                   setdiff(lLevels[["Vooropleiding"]], lLevels[["Aansluiting"]]), 
                                   lLevels[["Aansluiting"]]))
  ) |> 
  
  # Select the necessary columns
  select(Faculteit, Opleidingsnaam, Opleiding, Opleidingsvorm,
         Variabele, Groep, Bias, `Geen Bias`, `Negatieve Bias`, `Positieve Bias`) |> 
  
  # Sort the data
  arrange(Faculteit, Opleidingsnaam, Opleiding, Opleidingsvorm, 
          Variabele, Groep)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3.7 Determine counts ####

# Create a data frame for all study programmes
tblOpleidingen <- dfFairness_HHs.6 |>
  select(Opleiding, Opleidingsvorm) |>
  distinct()

# Create a list of Enrollments
lInschrijvingen <- list()

# Walk over the study programmes, collect enrollments and add to the list
for (i in 1:nrow(tblOpleidingen)) {
  
  # Determine study programme and type of education
  opleiding <- tblOpleidingen$Opleiding[i]
  opleidingsvorm <- tblOpleidingen$Opleidingsvorm[i]
  
  # Create the variables for the current study programme based on the programme name and type of education
  current_opleiding <- Get_Current_Opleiding(
    opleiding = opleiding,
    opleidingsvorm = toupper(opleidingsvorm)
  )
  
  # Based on this, determine derived variables
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

# Combine enrollment list
dfInschrijvingen <- bind_rows(lInschrijvingen)

# Create a df with counts of the variables
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

# Add the counts to the data
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

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3.8 Create a short version of the study programmes ####

# Make the study programme short for the table
dfFairness_HHs.8 <- dfFairness_HHs.7 |>
  mutate(Opleidingsvorm = toupper(Opleidingsvorm),
         Opleiding = paste0(Opleidingsnaam, " ", 
                            Opleidingsvorm, 
                            " (", Opleiding, ")")) |> 
  select(-Opleidingsnaam, -Opleidingsvorm)
  
# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4. FAIRNESS FLEXTABLE ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4.1 Determine the colors ### 

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

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4.2 Create the table (version 1 - long) ### 

ftFairness.1 <- flextable(dfFairness_HHs.8 |> 
                            dplyr::filter(N > 0))

# Merge Variable column for visual grouping
# Apply conditional formatting
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
  
# Show the flextable
ftFairness.1

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4.3 Create the table (version 2 - wide) ### 

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
  
  # Remove the columns with 'Unknown' and 'Other'
  select(-c(Vooropleiding_Onbekend, 
            Aansluiting_Onbekend,
            Vooropleiding_Overig, 
            Aansluiting_Overig)) |> 
  
  # Rename columns: keep only what is after a _ if a _ occurs
  rename_with(~ gsub(".*_", "", .x)) 

# Adjust values at M/V based on derivation at V/M
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

# Create a basic flextable based on dfFairness_HHs.9
ftFairness.2 <- flextable(dfFairness_HHs.10)

# Customize the basic layout
ftFairness.2 <- ftFairness.2 |>
  add_header_row(values = c("", lSensitive_labels), 
                 colwidths = c(2, 2, 6, 6)) |>
  merge_v(j = c("Faculteit")) |>
  fix_border_issues() |>
  theme_vanilla() |>
  autofit() |> 
  valign(i = 1:2, valign = "middle", part = "header") |> 
  valign(j = 1:2, valign = "top", part = "body") |> 
  align(j = 1:2, align = "left") |> 
  align(j = 3:16, align = "center", part = "all")

# Function to determine the color of the background
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

# Function to define the color of the text
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

# Apply colors by column
for (col in colnames(dfFairness_HHs.10)[-1]) {
  
  # Adjust the background
  colors <- map_chr(dfFairness_HHs.10[[col]], Get_Fairness_Bg)
  ftFairness.2 <- ftFairness.2 |>
    bg(j = col, bg = colors)
  
  # Customize the color
  colors <- map_chr(dfFairness_HHs.10[[col]], Get_Fairness_Color)
  ftFairness.2 <- ftFairness.2 |>
    color(j = col, color = colors)
}

# Add vertical lines between desired columns
border_vline <- fp_border(color = "black", width = 1)

# Adjust borderlines
ftFairness.2 <- ftFairness.2 |>
  vline(j = c(2, 4, 10), border = border_vline)

# Save flextable 2
ftFairness.2 |> 
  save_as_image(path = "10. Output/all/Fairness_HHs_2.png")

# Show the flextable
ftFairness.2

# Correct rating in the flextable for Geslacht:
# If M = Negative, then V = Positive
# If V = Negative, then M = Positive
# If M = Positive, then V = Negative
# If V = Positive, then M = Negative

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5. CLEAN ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(list = ls())
