## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Include_Colors.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Lectoraat Learning Technology & Analytics De Haagse Hogeschool
## Copyright 2024 De HHs
## Web Page: http://www.hhs.nl
## Contact: Theo Bakker (t.c.bakker@hhs.nl)
## Verspreiding buiten De HHs: Nee
##
## Doel: Het insluiten van de color settings
##
## Afhankelijkheden: Geen
##
## Datasets: Nvt
##
## Opmerkingen:
## 1) Geen.
## 2) ___
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Geschiedenis:
## 23-06-2024: TB: Aanmaak bestand
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. COLOR LIJSTEN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.1 Standaard kleuren ####

lColors_default <- c(
  
  ## Kleuren van title, onderwerp, ondertitel, caption, background
  sTitle_color            = "black",
  sSubject_color          = "#808080",
  sSubtitle_color         = "black",
  sSubtitle_prefix_color  = "#808080",
  sSubtitle_warning_color = "#C8133B",
  sCaption_color          = "darkgray",
  sBackground_color       = "white",
  
  ## Kleur van tekst
  sText_color            = "black",
  sText_inside_color     = "white",
  
  ## Intercept (0) en gridlines
  sBaseline_color        = "black",
  sGridline_color        = "#CBCBCB",
  sDeadline_color        = "black",
  sBaseline_color_ses    = "darkgray",
  
  ## Vulkleur
  sFill_color            = "lightgray",
  
  ## Lijnkleur
  sAverage_line_color    = "#CBCBCB",
  
  ## Tekstkleur
  sAverage_text_color    = "darkgray",
  
  ## Kleur van annotaties
  sAnnotation_text_color = "black",
  sArrow_color           = "darkgray",
  
  ## Kleur van jitter
  sJitter_color          = "darkgray",
  
  ## Kleur van de errorband
  sSE_color              = "#CBCBCB",
  
  ## Kleur van de band
  sBand_color            = "grey95",
  
  ## Positief en negatief
  sPositive_color        = "#466F9D",
  sNegative_color        = "#C8133B",
  
  ## Metrics
  sMetrics_green         = "#287233",
  sMetrics_red           = "#C8133B",
  sMetrics_yellow        = "#FFD966",
  sMetrics_blue          = "#5FA2CE"
)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.2 Specifieke kleuren ####

lColors_geslacht <- c(
  "M" = "#1170AA",
  "V" = "#FC7D0B")

lColors_toelaatgevende_vooropleiding <- c(
  "MBO"      = "#1170AA",
  "HAVO"     = "#FC7D0B",
  "VWO"      = "#F1CE63",
  "BD"       = "#A3CCE9",
  "CD"       = "#57606C",
  "HO"       = "#9467BD",
  "Overig"   = "#A3ACB9",
  "Onbekend" = "#C8D0D9"
)

lColors_aansluiting <- c(
  "Direct"            = "#FC7D0B",
  "Tussenjaar"        = "#1170AA",
  "Switch intern"     = "#5FA2CE",
  "Switch extern"     = "#A3CCE9",
  "2e Studie"         = "#F1CE63",
  "Na CD"             = "#57606C",
  "Overig"            = "#A3ACB9",
  "Onbekend"          = "#C8D0D9"
)

lColors_hhs <- c(
  "hhs-color-green" = "#9EA700",
  "hhs-color-blue"  = "#00B2CD"
)
