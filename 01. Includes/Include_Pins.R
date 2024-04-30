## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Include_Pins.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code voor Lectoraat Learning Technology & Analytics De Haagse Hogeschool
## Copyright 2024 De HHs
## Web Page: http://www.hhs.nl
## Contact: Theo Bakker (t.c.bakker@hhs.nl)
## Verspreiding buiten De HHs: Nee
##
## Doel: Het inladen van pins voor deze analyse
##
## Afhankelijkheden: pinboard LTA
##
## Datasets: pins
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
## 22-01-2024: TB: Aanmaak bestand
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 0. Voorbereidingen ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (ltabase::we_are_on_ice() ) {
    cli::cli_h1("Research_Settings voor Include_Pins")
    Print_Research_Settings()
}

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. Bepaal de waarden voor de pins ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Faculteiten, sectoren, opleidingen, opleidingstracks en studievormen
lFaculties     <- Expand_String(lResearch_settings$sFaculteit)
lSectors       <- Expand_String(lResearch_settings$sSector)
lStudyprograms <- Expand_String(lResearch_settings$sOpleiding)
lStudytracks   <- Expand_String(lResearch_settings$lOpleidingstracks)
lStudyforms    <- Expand_String(lResearch_settings$lOpleidingsvormen)

## Variabelen die nodig zijn (basis)
lVariabelen_base <- c(
  "BUI_Buitenlandse_student_tf",
  "BUI_Internationale_student_tf",
  "CBS_APCG_tf",
  "CBS_Gemeentecode",
  "CBS_Gemeentenaam",
  "CBS_Wijkcode",
  "CBS_Wijknaam",
  "DEM_Geslacht",
  "DEM_Leeftijd_1_oktober",
  "GIS_Tijd_fiets_OV",
  "INS_Aanmelddatum",
  "INS_Aansluiting",
  "INS_Aansluiting_LTA",
  "INS_Aantal_inschrijvingen",
  "INS_BRIN4_hoogste_VO_binnen_HO",
  "INS_BRIN4_hoogste_VO_binnen_HO_Instellingsnaam",
  "INS_BRIN4_hoogste_VO_binnen_HO_soort_HO",
  "INS_BRIN6_hoogste_VO_binnen_HO",
  "INS_BRIN6_voor_het_HO",
  "INS_Collegejaar",
  "INS_Collegejaar_EOI_tf",
  "INS_Dagen_tussen_aanmelding_en_1_september",
  "INS_Dagen_tussen_aanmelding_en_1_september_cat",
  "INS_Dubbele_studie_tf",
  "INS_Eerste_jaar_aan_deze_opleiding_en_instelling",
  "INS_Eerstejaars_tf",
  "INS_Faculteit",
  "INS_ID_Student_Croho_Collegejaar_Type_Vorm",
  "INS_Instromer_hoofdfase_tf",
  "INS_Naam_vestiging_VO_voor_HO_LTA",
  "INS_Navitas_tf",
  "INS_Opleiding",
  "INS_Opleidingsnaam_huidig",
  "INS_Opleidingsnaam_huidig_clean",
  "INS_Opleidingstype_LTA",
  "INS_Opleidingsvorm",
  "INS_Opleidingsvorm_wissel_aantal",
  "INS_Opleidingsvorm_wissel_tf",
  "INS_Ouderejaars_tf",
  "INS_Sector_VH_lang",
  "INS_Sector_VH_LTA",
  "INS_Student_ID_LTA",
  "INS_Student_UUID_opleiding",
  "INS_Student_UUID_opleiding_vorm",
  "INS_Studiejaar_opleiding",
  "INS_Studiejaar_opleiding_vorm",
  "INS_Switch_extern_soort",
  "INS_Switch_intern_soort",
  "INS_Switcher_intern_naar_opleiding",
  "INS_Switcher_intern_naar_vorm",
  "INS_Switcher_intern_tf",
  "INS_Switcher_intern_van_naar",
  "INS_Switcher_intern_van_opleiding",
  "INS_Switcher_intern_van_vorm",
  "INS_Vestiging_HHs_LTA",
  "LTA_Dataset",
  "RNK_Datum_selectieronde",
  "RNK_Rangnummer",
  "SES_Deelscore_arbeid",
  "SES_Deelscore_welvaart",
  "SES_Interval_bovengrens",
  "SES_Interval_ondergrens",
  "SES_Spreiding_arbeid",
  "SES_Spreiding_totaalscore",
  "SES_Spreiding_welvaart",
  "SES_Totaalscore",
  "VOP_Cijfer_CE1_wiskunde_A",
  "VOP_Gemiddeld_eindcijfer_VO_van_de_hoogste_vooropleiding_voor_het_HO",
  "VOP_Studiekeuzeprofiel_LTA",
  "VOP_Studiekeuzeprofiel_LTA_afkorting",
  "VOP_Toelaatgevende_vooropleiding_soort",
  "VOP_Toelaatgevende_vooropleiding_soort_LTA"
)

## Succes variabelen
lSucces_variabelen <- c(
  "BSA_Advies",
  "BSA_Advies_collegejaar",
  "BSA_Advies_collegejaar_huidig",
  "BSA_Advies_huidig",
  "BSA_Datum",
  "SUC_Afstudeer_nominaliteit_cat",
  "SUC_Diploma_aantal_jaar",
  "SUC_Diploma_aantal_jaar_cat",
  "SUC_Diploma_na_4_jaar_tf",
  "SUC_Diploma_na_5_jaar_tf",
  "SUC_Diploma_na_6_jaar_tf",
  "SUC_Diplomadatum",
  "SUC_P_aantal_jaar",
  "SUC_P_aantal_jaar_cat",
  "SUC_P_na_1_jaar_tf",
  "SUC_P_na_2_jaar_tf",
  "SUC_P_na_3_jaar_tf",
  "SUC_Propedeusedatum",
  "SUC_Studiestatus_cat",
  "SUC_Uitval_aantal_jaar",
  "SUC_Uitval_aantal_jaar_cat",
  "SUC_Uitval_aantal_jaar_LTA",
  "SUC_Uitval_aantal_jaar_cat_LTA",
  "SUC_Uitval_datum",
  "SUC_Uitval_na_1_jaar_tf",
  "SUC_Uitval_na_2_jaar_tf",
  "SUC_Uitval_na_3_jaar_tf"
)

lVariabelen <- c(lVariabelen_base, lSucces_variabelen) |>
    unique() |>
    sort()

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. De pins voor de opleiding ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dfOpleiding_inschrijvingen <- get_lta_studyprogram_enrollments_pin(
    board = "HHs/Inschrijvingen",
    faculty = lFaculties,
    studyprogram = lStudyprograms,
    studytrack = lStudytracks,
    studyform = lStudyforms,
    range = "eerstejaars") |>
    remove_rownames() |>

    ## Beperk de selectie tot de variabelen die nodig zijn
    select_at(lVariabelen) |>

    ## Pas de namen en codes van Gemeenten aan waar nodig
    Mutate_CBS_Gemeenten() |>

    ## Voeg bij switch van en naar toe
    Mutate_Interne_switch()

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. De pins voor de faculteit ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dfFaculteit_inschrijvingen <-
  get_lta_studyprogram_enrollments_pin(
    board = "HHs/Inschrijvingen",
    faculty = lFaculties,
    studytype = "B",
    studyform = lStudyforms,
    range = "eerstejaars"
  ) |>
  remove_rownames() |>

  ## Beperk de selectie tot de variabelen die nodig zijn
  select_at(lVariabelen) |>

  ## Pas de namen en codes van Gemeenten aan waar nodig
  Mutate_CBS_Gemeenten()|>

  ## Voeg bij switch van en naar toe
  Mutate_Interne_switch()

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. De pins voor een domein ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

dfSector_inschrijvingen <- get_lta_studyprogram_enrollments_pin(
  board = "HHs/Inschrijvingen",
  sector = lSectors,
  studytype = "B",
  studyform = lStudyforms,
  range = "eerstejaars"
) |>
  remove_rownames() |>

  ## Beperk de selectie tot de variabelen die nodig zijn
  select_at(lVariabelen) |>

  ## Relevel de INS_Opleidingsnaam_huidig variabele
  mutate(INS_Opleidingsnaam_huidig = fct_relevel(INS_Opleidingsnaam_huidig,
                                                 lStudyprograms)) |>

  ## Pas voor INS_Aansluiting de waarden aan (kort ze in)
  mutate(
    INS_Aansluiting = case_when(
      INS_Aansluiting == "Direct na vooropleiding" ~ "Direct",
      .default = INS_Aansluiting
    )
  ) |>

  ## Pas de namen en codes van Gemeenten aan waar nodig
  Mutate_CBS_Gemeenten() |>

  ## Voeg bij switch van en naar toe
  Mutate_Interne_switch() |>

  ## Sorteer de data
  ltabase::sort_distinct()

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 5. Vul lResearch_settings aan ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

lResearch_settings["sDataset"] <- Get_sDataset(dfOpleiding_inschrijvingen)

## . ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 6. Toon het eindresultaat ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (ltabase::we_are_on_ice() ) {
    cli::cli_h1("Research_Settings na Include_Pins")
    Print_Research_Settings()
}


