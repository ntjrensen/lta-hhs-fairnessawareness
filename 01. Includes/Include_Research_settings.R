## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Include_Researchsettings.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Lectoraat Learning Technology & Analytics De Haagse Hogeschool
## Copyright 2024 De HHs
## Web Page: http://www.hhs.nl
## Contact: Theo Bakker (t.c.bakker@hhs.nl)
## Verspreiding buiten De HHs: Nee
##
## Doel: Het inladen van de research settings
##
## Afhankelijkheden: Geen
##
## Datasets: Geen
##
## Opmerkingen:
## 1) Geen.
## 2) ___
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## TODO:
## 1) ___.
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Geschiedenis:
## 15-01-2024: TB: Aanmaak bestand
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. BFM ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.1 AC - Acccountancy ####

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("AC"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.2 CE - Commerciële economie (DT) ####

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("CE"),
#                           lOpleidingsvormen = c("DT"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.3 FC - Finance & Control ####

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("FC"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.4 FC - Finance & Control (Ad) ####

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("FC-AD"),
#                           lOpleidingsvormen = c("DT"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.5 FC - Finance & Control (DT) ####

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("FC"),
#                           lOpleidingsvormen = c("DT"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.6 IB-ES - Ondernemerschap & Retail Management (DT) ####

# lResearch_settings <-
#   Get_Research_Settings(lOpleidingstracks = c("IB-ES"),
#                         lOpleidingsvormen = c("VT"),
#                         lVooropleidingen = c("MBO", "HAVO", "VWO", "BD"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1.y ORM - Ondernemerschap & Retail Management (DT) ####

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("ORM"),
#                           lOpleidingsvormen = c("DT"))

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. BRV ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.1 BO - Bestuurskunde/Overheidsmanagement ####

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("BO"),
#                           lOpleidingsvormen = c("DU"),
#                           lVooropleidingen = c("MBO", "HAVO", "VWO"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2.2 HBO-R - HBO-Rechten ####

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("HBO-R"),
#                           lOpleidingsvormen = c("DT"),
#                           lVooropleidingen = c("MBO", "HAVO", "VWO"))

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("HBO-R"),
#                           lOpleidingsvormen = c("DU"),
#                           lVooropleidingen = c("MBO", "HAVO", "VWO"))


## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. GVS ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.1.a BT/MT - Bewegingstechnologie/Mens en Techniek ####

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("BT","MT"),
#                           lOpleidingsvormen = c("VT"),
#                           lVooropleidingen = c("MBO", "HAVO", "VWO"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.1.b MT - Mens en Techniek ####

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("MT"),
#                           lVooropleidingen = c("MBO", "HAVO", "VWO"))
#
# lResearch_settings <-
#   Get_Research_Settings(
#     lOpleidingstracks = c("BT", "MT"),
#     lVooropleidingen = c("MBO", "HAVO", "VWO", "BD")
#   )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.2 HBO-V - Opleiding tot Verpleegkundige ####

lResearch_settings <-
    Get_Research_Settings(lOpleidingstracks = c("HBO-V"),
                          lOpleidingsvormen = c("VT"),
                          lVooropleidingen = c("MBO", "HAVO", "VWO"))

# lResearch_settings <-
#   Get_Research_Settings(lOpleidingstracks = c("HBO-V"),
#                         lOpleidingsvormen = c("DT"),
#                         lVooropleidingen = c("MBO", "HAVO", "VWO")

# lResearch_settings <-
#   Get_Research_Settings(lOpleidingstracks = c("HBO-V"),
#                         lOpleidingsvormen = c("DU"),
#                         lVooropleidingen = c("MBO", "HAVO", "VWO")

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("HBO-V"),
#                           lOpleidingsvormen = c("DT","DU"),
#                           lVooropleidingen = c("MBO","HAVO","VWO"))

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("HBO-V"),
#                           lOpleidingsvormen = c("VT"))

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("HBO-V"),
#                           lOpleidingsvormen = c("DT"))

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("HBO-V"),
#                           lOpleidingsvormen = c("DU"))
## Zij-instroom

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("HBO-V"),
#                           lOpleidingsvormen = c("DT"),
#                           lVooropleidingen = c("HAVO"))

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("HBO-V"),
#                           lOpleidingsvormen = c("VT"),
#                           lVooropleidingen = c("MBO", "HAVO"))

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("HDT"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.3 HDT - Huidtherapie ####

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("HDT"),
#                           lOpleidingsvormen = c("VT"),
#                           lVooropleidingen = c("MBO", "HAVO", "VWO"))
#
# lResearch_settings <-
#   Get_Research_Settings(lOpleidingstracks = c("HDT"),
#                         lOpleidingsvormen = c("DT"),
#                         lVooropleidingen = c("MBO", "HAVO", "VWO"))
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3.4 VD - Voeding en Dietetiek ####

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("VD"),
#                           lOpleidingsvormen = c("VT"),
#                           lVooropleidingen = c("MBO", "HAVO", "VWO"))
#
# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("VD"),
#                           lOpleidingsvormen = c("VT"),
#                           lVooropleidingen = c("MBO", "HAVO", "VWO", "BD"))

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("VD"),
#                           lOpleidingsvormen = c("DT"),
#                           lVooropleidingen = c("MBO", "HAVO", "VWO"))

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. ITD ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4.1 CMD - Communication and Multimedia Design ####

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("CMD"),
#                           lOpleidingsvormen = c("VT"),
#                           lVooropleidingen = c("MBO", "HAVO", "VWO"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4.2 CMD-ES-3 - Communication and Multimedia Design - English Stream (3 yr.) ####

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("CMD-ES-3"),
#                           lOpleidingsvormen = c("VT"),
#                           lVooropleidingen = c("MBO", "HAVO", "VWO", "BD"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4.3 CMD, CMD-ES-3 - Communication and Multimedia Design - 4 jaar + English Stream (3 yr.) ####
#
# lResearch_settings <-
#   Get_Research_Settings(lOpleidingstracks = c("CMD","CMD-ES-3"),
#                         lOpleidingsvormen = c("VT"),
#                         lVooropleidingen = c("MBO", "HAVO", "VWO", "BD"))

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5. MO ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5.1 CO - Communicatie ####

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("CO"))

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 6. SWE ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 6.1 PABO - Opleiding tot Leraar Basisonderwijs ####

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("PABO"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 6.2 PED - Pedagogiek ####

# lResearch_settings <-
#     Get_Research_Settings(lOpleidingstracks = c("PED"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 6.3 SWE - Social Work ####

# lResearch_settings <-
#   Get_Research_Settings(
#     lOpleidingstracks = c("SW","SPH","MWD","CMV"),
#     lOpleidingsvormen = c("VT"),
#     lVooropleidingen = c("MBO", "HAVO", "VWO", "BD")
#   )

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 7. TIS ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 7.0 TIS totaal ####

# lResearch_settings <-
#   Get_Research_Settings(
#     lFaculteiten = c("TIS"),
#     lOpleidingsvormen = c("VT"),
#     lVooropleidingen = c("MBO", "HAVO", "VWO", "BD"),
#     sRapportniveau = "Faculteit"
#   )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 7.1 B - Bouwkunde ####

# lResearch_settings <-
#   Get_Research_Settings(lOpleidingstracks = c("B"),
#                         lOpleidingsvormen = c("VT"),
#                         lVooropleidingen = c("MBO", "HAVO", "VWO"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 7.2 CV - Werktuigbouwkunde ####

# lResearch_settings <-
#   Get_Research_Settings(lOpleidingstracks = c("CV"),
#                         lOpleidingsvormen = c("VT"),
#                         lVooropleidingen = c("MBO", "HAVO", "VWO", "BD"))
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 7.3 E - Elektrotechniek ####

# lResearch_settings <-
#   Get_Research_Settings(lOpleidingstracks = c("E"),
#                         lOpleidingsvormen = c("VT"),
#                         lVooropleidingen = c("MBO", "HAVO", "VWO", "BD"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 7.4 IPO - Industrieel Product Ontwerpen ####

# lResearch_settings <-
#   Get_Research_Settings(lOpleidingstracks = c("IPO"),
#                         lOpleidingsvormen = c("VT"),
#                         lVooropleidingen = c("MBO", "HAVO", "VWO", "BD"))

# lResearch_settings <-
#   Get_Research_Settings(lOpleidingstracks = c("IPO","IPO-ES","IPO-ES-3"),
#                         lOpleidingsvormen = c("VT"),
#                         lVooropleidingen = c("MBO", "HAVO", "VWO", "BD"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 7.5 W - Werktuigbouwkunde ####

# lResearch_settings <-
#   Get_Research_Settings(lOpleidingstracks = c("W"),
#                         lOpleidingsvormen = c("VT"),
#                         lVooropleidingen = c("MBO", "HAVO", "VWO", "BD"))


if(!exists("lResearch_settings")) {
  cli::cli_alert_danger("Geen research settings ingesteld")
}



