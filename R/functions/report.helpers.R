# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# report.helpers.R ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R code of the Learning Technology & Analytics Research Group of THUAS
# Copyright 2025 THUAS
# Web Page: http://www.thuas.com
# Contact: Theo Bakker (t.c.bakker@hhs.nl)
# Distribution outside THUAS: Yes
#
# Purpose: Doel
#
# Dependencies: Afhankelijkheid
#
# Datasets: Datasets
#
# Remarks:
# 1) None.
# 2) ___
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. STUDY PROGRAMME-SPECIFIC FUNCTIONS ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Get_Subtitle <- function() {
  
  subtitle <- format(Sys.Date(), '%d-%m-%Y')
  return(subtitle)
}

# Function to determine current study programme
Get_Current_Opleiding <- function(opleiding, opleidingsvorm) {
  
  if(!exists("dfOpleidingen")){
    cli::cli_abort("dfOpleidingen is niet gedefinieerd")
  }
  
  # Test if this study programme does appear
  if(!any(dfOpleidingen$INS_Opleiding == opleiding &
          dfOpleidingen$INS_Opleidingsvorm == opleidingsvorm)) {
    cli::cli_abort(paste0("De opleiding ", opleiding, " met opleidingsvorm ", opleidingsvorm, " bestaat niet. \n",
                          "Controleer of de opleidingsnaam en opleidingsvorm correct zijn."))
  }
  
  current_opleiding <- dfOpleidingen |>
    dplyr::filter(INS_Opleiding == opleiding,
                  INS_Opleidingsvorm == opleidingsvorm) |>
    select(
      INS_Opleiding,
      INS_Opleidingsvorm,
      INS_Opleidingsnaam,
      INS_Opleidingsnaam_huidig,
      INS_Opleidingstype_LTA,
      INS_Collegejaar_min,
      INS_Collegejaar_max,
      INS_Faculteit,
      INS_Sector_VH_LTA,
      INS_Sector_VH_lang,
      INS_Vestiging_HHs_LTA
    ) |>
    group_by(INS_Opleiding, INS_Opleidingsvorm) |>
    dplyr::filter(max(INS_Collegejaar_max) == INS_Collegejaar_max) |>
    ungroup() |>
    distinct() |>
    as.list()
  
  return(current_opleiding)
}

# Function to set the variables of the current study programme
Set_Current_Opleiding_Vars <- function(current_opleiding, debug = F){
  
  opleiding               <<- current_opleiding$INS_Opleiding
  faculteit               <<- current_opleiding$INS_Faculteit
  opleidingstype          <<- current_opleiding$INS_Opleidingstype_LTA
  opleidingsnaam_huidig   <<- current_opleiding$INS_Opleidingsnaam_huidig
  opleidingsvorm          <<- tolower(current_opleiding$INS_Opleidingsvorm)
  hhs_locatie             <<- tolower(current_opleiding$INS_Vestiging_HHs_LTA)
  vh_sector               <<- current_opleiding$INS_Sector_VH_LTA
  vh_sector_long          <<- current_opleiding$INS_Sector_VH_lang
  opleidingcleanname      <<- Get_Opleiding_Directory(faculteit,
                                                      opleidingsnaam_huidig,
                                                      opleiding,
                                                      opleidingsvorm)
  
  if(debug) {
    #Cli_Subheader("Instelling van de huidige opleiding")
  
    Show_Current_Opleiding_Vars()
  }
  
}

# Function to show the variables of the current study programme
Show_Current_Opleiding_Vars <- function(){
  
  if(!exists("current_opleiding")){
    cli::cli_abort("current_opleiding is niet gedefinieerd")
  }
  
  # Print all values of the current study programme
  cli::cli_h1(paste0("Huidige opleiding:"))
  
  cli_bullets(c(
    "*" = paste0("Faculteit: ",            faculteit),
    "*" = paste0("Opleidingstype: ",       opleidingstype),
    "*" = paste0("Opleiding: ",            opleiding),
    "*" = paste0("Opleidingsnaam: ",       opleidingsnaam_huidig),
    "*" = paste0("Studievorm: ",           opleidingsvorm),
    "*" = paste0("HHS Locatie: ",          hhs_locatie),
    "*" = paste0("VH Sector lowercase: ",  vh_sector),
    "*" = paste0("VH Sector lange naam: ", vh_sector_long),
    "*" = paste0("Opleiding clean name: ", opleidingcleanname)
  ))
}

# Function to determine the current training name
Get_sOpleiding <- function() {
  
  if(!exists("current_opleiding")){
    cli::cli_abort("current_opleiding is niet gedefinieerd")
  }
  
  return(paste0(Get_Opleidingsnaam_Synth(current_opleiding$INS_Opleidingsnaam_huidig), 
                " (", 
                current_opleiding$INS_Opleiding, ") ", 
                current_opleiding$INS_Opleidingsvorm))
  
  # return(paste0(current_opleiding$INS_Opleidingsnaam_huidig, 
  #               " (", 
  #               current_opleiding$INS_Opleiding, ") ", 
  #               current_opleiding$INS_Opleidingsvorm))
  
}

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. PATH FUNCTIONS ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to determine the path to flextables
Get_Opleiding_Directory <- function(faculteit,
                                    opleidingsnaam_huidig,
                                    opleidingsnaam = NULL,
                                    opleidingsvorm) {
  
  if(faculteit == "Xtern") {
    path <- janitor::make_clean_names(paste(
      faculteit,
      opleidingsnaam_huidig,
      opleidingsvorm,
      sep = "_"
    ))
  } else {
    path <- janitor::make_clean_names(paste(
      faculteit,
      opleidingsnaam_huidig,
      opleidingsnaam,
      opleidingsvorm,
      sep = "_"
    ))
  }
  
  return(path)
}

# Function to determine output directory of current study programme
Get_Current_Opleiding_Output_Dir <- function(current_opleiding,
                                             mode) {
  
  .fac_opl_vorm <- paste0(
    current_opleiding$INS_Faculteit,
    "/",
    current_opleiding$INS_Opleidingstype_LTA,
    "-",
    current_opleiding$INS_Opleiding,
    "-",
    current_opleiding$INS_Opleidingsvorm
  ) |> tolower()
  
  # If synthetic data is used, add -synth to the directory
  if(params$use_synthetic_data == T){
    .fac_opl_vorm <- paste0(.fac_opl_vorm, "-synth")
  }
  
  if(mode == "last-fits" | mode == "modelresults") {
    .output_dir <- file.path("_output",
                             .fac_opl_vorm,
                             "modelresults")
  } else if(mode == "data") {
    .output_dir <- file.path("_output",
                             .fac_opl_vorm,
                             "data")
  } else if(mode == "fairness") {
    .output_dir <- file.path("_output",
                             .fac_opl_vorm,
                             "fairness")
  } else if(mode == "html") {
    .output_dir <- file.path("_output",
                             .fac_opl_vorm)
  } else if(mode == "plot") {
    .output_dir <- file.path("_output",
                             .fac_opl_vorm,
                             "plots")
  } else {
    cli::cli_alert("The mode is not correct")
  }
  
  return(.output_dir)
  
}

# Function to determine the output directory of the current study programme
Get_Current_Opleiding_Output_File <- function(df, mode, group = NULL, analyse = NULL) {
  
  # Define the description of the analysis
  if(is.null(analyse)) {
    .analyse <- Get_Current_Analysis()
  } else {
    .analyse <- analyse
  }
  
  if(mode == "last-fits") {
    .suffix <- "_last-fits.rds"
  } else if(mode == "fairness") {
    .suffix <- paste0("_fairness_", tolower(group), ".rds")
  } else if(mode == "html") {
    .suffix <- ".html"
  } else if(mode == "modelresults") {
    .suffix <- "_modelresults.rds"
  } else if(mode == "data") {
    .suffix <- ".rds"
  } else if(mode == "plot") {
    .suffix <- ".png"
  } else {
    cli::cli_alert("The mode is not correct")
  }
  
  # Determine the output file:
  # faculteit-opleiding-opleidingsvorm + analyse + suffix
  .output_file <- paste0(
    paste(
      df$INS_Faculteit,
      df$INS_Opleidingstype_LTA,
      df$INS_Opleiding,
      df$INS_Opleidingsvorm,
      sep = "-"
    ),
    "_",
    .analyse,
    .suffix
  )
  
}

# Function to determine the output path for fitted models
Get_Model_Outputpath <- function(mode, group = NULL) {
  
  # Define the output file
  .output_file <- Get_Current_Opleiding_Output_File(current_opleiding, mode, group)
  
  # Define the output directory
  .output_dir <- Get_Current_Opleiding_Output_Dir(current_opleiding, mode)
  
  # Create the directory if it does not already exist
  if (!dir.exists(.output_dir)) {
    dir.create(.output_dir, recursive = TRUE)
  }
  
  # Return the full output path
  return(file.path(.output_dir, .output_file))
  
}

# Function to determine the output path for the plots
Get_Plot_Outputpath <- function(plotname, mode = "plot", bestmodel = NA) {
  
  # Best model is not NA, make the name clean
  if(!is.na(bestmodel)) {
    
    .bestmodel <- janitor::make_clean_names(bestmodel)
    
    # Define the output file
    .output_file <- glue("{plotname}_{.bestmodel}.png") 
    
  } else {
    
    # Define the output file
    .output_file <- glue("{plotname}.png") 
    
    }
  
  # Define the output directory
  .output_dir <- Get_Current_Opleiding_Output_Dir(current_opleiding, mode)
  
  # Create the directory if it does not already exist
  if (!dir.exists(.output_dir)) {
    dir.create(.output_dir, recursive = TRUE)
  }
  
  # Return the full output path
  return(file.path(.output_dir, .output_file))
  
}

# Function to determine the output path for the breakdown plots
Get_Breakdown_Plotpath <- function(student_groep, student_categorie, bestmodel) {
  
  # Replace spaces with - in the student_group and student_category
  .student_groep     <- gsub(" ", "-", student_groep)
  .student_categorie <- gsub(" ", "-", student_categorie)
  
  file.path(Get_Plot_Outputpath(
    plotname = tolower(glue("lf_break_down_{(.student_groep)}_{(.student_categorie)}")),
    bestmodel = bestmodel)
  )
}

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. DATASET FUNCTIONS ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to read the data dictionary.
Get_Data_Dictionary <- function() {
  
  sInput_path <- "R/vars"
  
  df <- rio::import(file.path(sInput_path, "data-dictionary.xlsx")) 
  
  return(df)
}

# Function to create a table from the data dictionary
Get_tblData_Dictionary <- function(df) {
  
  tbl <- 
    df |> 
    flextable() |> 
    theme_vanilla() |>  
    set_table_properties(width = 1, layout = "autofit") |>  
    bg(i = ~ seq_len(nrow(dfData_dictionary)) %% 2 == 0, bg = "gray95") |>  
    align(align = "left", part = "all") |>  
    border_remove() |>  
    border_inner_h(border = fp_border(color = "gray", width = 0.5)) |>  
    border_inner_v(border = fp_border(color = "gray", width = 0.5)) |>  
    bold(part = "header") |>  
    fontsize(size = 11, part = "all") |>  
    color(part = "header", color = "black") |>  
    set_caption(caption = "Overzicht van variabelen")  
  
  return(tbl)
}

# Function to retrieve synthetic data
Get_Studyprogram_Enrollments_Synthetic <- function(studytrack,
                                                       studyform) {
  
  sInput_path <- "R/data/syn"
  
  df <- readRDS(file.path(sInput_path, "dfOpleiding_inschrijvingen_syn.rds")) |> 
    filter(INS_Opleiding == studytrack,
           INS_Opleidingsvorm == studyform)
  
  return(df)
}

# Function to make the Failure variable
Mutate_Uitval <- function(df, model = "Uitval na 1 jaar") {
  
  # Fill in the missing values with 0
  df <- df |>
    mutate(
      SUC_Uitval_aantal_jaar_LTA = coalesce(SUC_Uitval_aantal_jaar_LTA, 0)
    )
  
  if (model == "Uitval na 1 jaar") {
    return(
      df |>
        mutate(
          SUC_Uitval = ifelse(SUC_Uitval_aantal_jaar_LTA == 1, T, F),
          SUC_Uitval = coalesce(SUC_Uitval, F)
        ) 
    )
  } else if (model == "Uitval na 2 jaar") {
    return(
      df |>
        mutate(
          SUC_Uitval = ifelse(SUC_Uitval_aantal_jaar_LTA %in% c(1, 2), T, F),
          SUC_Uitval = coalesce(SUC_Uitval, F)
        ) 
    )
  } else if (model == "Uitval na 3 jaar") {
    return(
      df |>
        mutate(
          SUC_Uitval = ifelse(SUC_Uitval_aantal_jaar_LTA %in% c(1, 2, 3), T, F),
          SUC_Uitval = coalesce(SUC_Uitval, F)
        ) 
    )
  } else if (model == "Alle uitval"){
    return(
      df |>
        mutate(
          SUC_Uitval = ifelse(SUC_Uitval_aantal_jaar_LTA > 0, T, F),
          SUC_Uitval = coalesce(SUC_Uitval, F)
        ) 
    )
  } 
  
}

# Function to differentiate Dropout (with and without a propaedeutic degree)
Filter_Propedeusediploma_Uitval <- function(df, propedeusediploma = "Nvt") {
  
  if (propedeusediploma == "Nvt" || propedeusediploma == "" || is.na(propedeusediploma)) {
    
    # Include all dropout
    return(df)
    
  } else if (propedeusediploma == "Met P") {
    
    # Remove students who drop out with a propaedeutic degree
    return(
      df |>
        filter(SUC_Studiestatus_cat != "Uitval zonder propedeusediploma")
    )
    
  } else if (propedeusediploma == "Zonder P") {
    
    # Remove students who drop out with a propaedeutic degree
    return(
        df |>
        filter(SUC_Studiestatus_cat != "Uitval met propedeusediploma")
        ) 
    
  } else {
    cli::cli_alert("The propaedeutic degree variable is incorrect")
  }
  
}

# Function to create the Retention variable
Mutate_Retentie <- function(df, model = "Retentie na 1 jaar") {
  
  if (model == "Retentie na 1 jaar") {
    return(
      df |>
        mutate(
          SUC_Retentie = ifelse(SUC_Uitval_aantal_jaar_LTA == 1, F, T),
          SUC_Retentie = coalesce(SUC_Retentie, T)
        ) 
    )
  } else if (model == "Retentie na 2 jaar") {
    return(
      df |>
        mutate(
          SUC_Retentie = ifelse(SUC_Uitval_aantal_jaar_LTA == 1:2, F, T),
          SUC_Retentie = coalesce(SUC_Retentie, T)
        ) 
    )
  } else if (model == "Retentie na 3 jaar") {
    return(
      df |>
        mutate(
          SUC_Retentie = ifelse(SUC_Uitval_aantal_jaar_LTA == 1:3, F, T),
          SUC_Retentie = coalesce(SUC_Retentie, T)
        ) 
    )
  } else if (model == "Alle retentie"){
    return(
      df |>
        mutate(
          SUC_Retentie = ifelse(SUC_Uitval_aantal_jaar_LTA > 0, F, T),
          SUC_Retentie = coalesce(SUC_Retentie, T)
        ) 
    )
  } 
  
}

# Mutate Dubbele studies
Mutate_Dubbele_studie <- function(df) {
  
  df <- df |>
    mutate(INS_Dubbele_studie = ifelse(INS_Aantal_inschrijvingen > 1, "Ja", "Nee"))
  
  return(df)
  
}

# Mutate APCG
Mutate_APCG <- function(df) {
  
  df <- df |>
    mutate(APCG = case_when(APCG == TRUE ~ "Ja", 
                            APCG == FALSE ~ "Nee", 
                            .default = "Onbekend"))
  
  return(df)
  
}

# Mutate missing figures VO
Mutate_Cijfers_VO <- function(df) {
  
  df <- df |>
    mutate(
      Cijfer_SE_VO_missing          = ifelse(is.na(Cijfer_SE_VO),          "Ja", "Nee"),          
      Cijfer_CE_VO_missing          = ifelse(is.na(Cijfer_CE_VO),          "Ja", "Nee"),        
      Cijfer_CE_Nederlands_missing  = ifelse(is.na(Cijfer_CE_Nederlands),  "Ja", "Nee"),
      Cijfer_CE_Engels_missing      = ifelse(is.na(Cijfer_CE_Engels),      "Ja", "Nee"),
      Cijfer_CE_Wiskunde_missing    = ifelse(is.na(Cijfer_CE_Wiskunde),    "Ja", "Nee"),
      Cijfer_CE_Natuurkunde_missing = ifelse(is.na(Cijfer_CE_Natuurkunde), "Ja", "Nee")
    )
  
  return(df)
  
}

# Function to get the variables
Get_dfVariables <- function() {
  
  dfVariables <- rio::import(file.path("R/data/", 
                                       "dfVariables.xlsx"), 
                             sheet = "Variables")
  
  return(dfVariables)
}

# Function to get the sensitive variables
Get_lSelect <- function(df, var) {
  
  lSelect <- df |> 
    filter(VAR_Select_tf == TRUE) |> 
    select(all_of(var)) |> 
    pull()
  
  return(lSelect)
}

# Function to get the levels of the variables
Get_dfLevels <- function() {
  
  dfLevels <- rio::import(file.path("R/data/", 
                                    "dfVariables.xlsx"), 
                          sheet = "Levels") |> 
    group_by(VAR_Formal_variable) |>
    arrange(VAR_Level_order, 
            .by_group = TRUE) |>
    ungroup()
  
  return(dfLevels)
}

# Function to get the sensitive variables
Get_lSensitive <- function(df, var) {
  
  lSensitive <- df |> 
    filter(VAR_Sensitive_tf == TRUE) |> 
    select(all_of(var)) |> 
    pull()
  
  return(lSensitive)
}

# Function to determine the order of a number of levels
Get_lLevels <- function(df, formal = FALSE) {
  
  ## Set lLevels
  lLevels <- list()
  
  if(formal) {
    for (i in df$VAR_Formal_variable) {
      lLevels[[i]] <- df$VAR_Level_NL[df$VAR_Formal_variable == i]
    }
  } else {
    for (i in df$VAR_Simple_variable) {
      lLevels[[i]] <- df$VAR_Level_NL[df$VAR_Simple_variable == i]
    }
  }
  
  return(lLevels)
  
}
# Function to determine the levels of sensitive variables for breakdown plots
Get_lSensitive_Levels_Breakdown <- function(df, list) {
  
  lSenstive_levels_breakdown <- list()
  for(i in list) {
    lSenstive_levels_breakdown[[i]] <- df |> 
      filter(VAR_Formal_variable == i,
             VAR_Breakdown_tf == TRUE) |>
      select(VAR_Level_NL) |> 
      pull()
  }
  
  return(lSenstive_levels_breakdown)
}

# Function to determine the levels of a variable
Set_Levels <- function(df = dfOpleiding_inschrijvingen_base, lLevels) {

  lNew_levels <- list()

  lNew_levels[["Studiekeuzeprofiel"]] <-
    Sort_Levels(
      lLevels[["Studiekeuzeprofiel"]],
      df,
      "VOP_Studiekeuzeprofiel_LTA_afkorting"
    )

  # Loop over the sensitive attributes
  for (sensitive_attribute in lSensitive_labels) {

    formal_variable <- unique(dfSensitive$VAR_Formal_variable[dfSensitive$VAR_Simple_variable == sensitive_attribute])

    lNew_levels[[sensitive_attribute]] <- Sort_Levels(lLevels[[sensitive_attribute]],
                                                      df,
                                                      formal_variable)

  }

  return(lNew_levels)

}

# Adjust levels so that they are sorted properly
Sort_Levels <- function(levels, df, var) {
  levels_sorted <- intersect(levels,
                       unique(df[[var]])
  )
  return(levels_sorted)
}

# Function to mutate the levels of a variable
Mutate_Levels <- function(df, vars, levels) {
  
  walk(seq_along(vars), function(i) {
    df <- df |>
      mutate(
        !!rlang::sym(vars[i]) := fct_expand(!!rlang::sym(vars[i]), levels[[i]]),
        !!rlang::sym(vars[i]) := fct_relevel(!!rlang::sym(vars[i]), levels[[i]]),
        !!rlang::sym(vars[i]) := fct_drop(!!rlang::sym(vars[i]))
      )
  })
  
  return(df)
}

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4. RENDER FUNCTIONS ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to retrieve training name with or without synth
Get_Opleidingsnaam_Synth <- function(opleidingsnaam) {
  
  if(params$use_synthetic_data == T){
    sOpleidingsnaam <- paste(opleidingsnaam, "(Synth)")
  } else {
    sOpleidingsnaam <- opleidingsnaam
  }
  
  return(sOpleidingsnaam)
  
}

# Function to determine the long name of the type of education
Get_Opleidingsvorm_Lang <- function(opleidingsvorm) {
  
  return(switch(
    opleidingsvorm,
    "VT" = "voltijd",
    "DT" = "deeltijd",
    "DU" = "duaal",
    "onbekend"
  ))
  
}

# Function to determine the long name of the type of education
Get_Faculteitsnaam_Lang <- function(faculteit) {
  
  return(switch(
    faculteit,
    "BFM" = "Business, Finance & Marketing",
    "BRV" = "Bestuur, Recht & Veiligheid",
    "GVS" = "Gezondheid, Voeding & Sport",
    "ITD" = "IT & Design",
    "MO"  = "Management & Organisatie",
    "SWE" = "Sociaal Werk & Educatie",
    "TIS" = "Technologie, Innovatie & Samenleving",
    "onbekend"
  ))
  
}

# Function to create a summary table
Get_tblSummary <- function(df) {
  
  dfSummary <- df |> 
    
    tbl_summary(
      by = Retentie,
      statistic = list(
        all_continuous() ~ "{mean} ({sd})",
        all_categorical() ~ "{n} ({p}%)"
      ),
      digits = all_continuous() ~ 2, 
      missing = "no",
      percent = "row"
    ) |> 
    
    # Organize the design of the table
    modify_header(all_stat_cols() ~ "**{level}**, N={n} ({style_percent(p)}%)") |>
    modify_spanning_header(c("stat_1", "stat_2") ~ "**Retentie**") |>
    modify_header(label = "**Variabele**") |>
    bold_labels() |>
    modify_caption("**Studentkenmerken versus Retentie**") |>
    add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2),
          test.args = list(
            all_tests("fisher.test") ~ list(simulate.p.value = TRUE),
            all_tests("wilcox.test") ~ list(exact = FALSE)
          )) |>
    add_q(method = "bonferroni",
          pvalue_fun = ~ style_pvalue(.x, digits = 2)) |>
    add_significance_stars(
      hide_p = FALSE,
      pattern = "{q.value}{stars}"
    ) |>
    add_overall(last = TRUE, col_label = "**Totaal**, N = {N}") |>
    as_flex_table() |>
    flextable::border(border.top = fp_border(color = "grey")) |>
    set_table_properties(width = 0.8, layout = "autofit")
  
  return(dfSummary)
  
}

# Function to define the text for the model (in the title)
Get_Succes_Model_Text <- function(propedeusediploma, succes_model) {
  
  if (propedeusediploma == "Zonder P") {
    succes_model_text <- paste(succes_model, "bij studenten zonder propedeusediploma")
  } else if (propedeusediploma == "Met P") {
    succes_model_text <- paste(succes_model, "bij studenten met propedeusediploma")
  } else {
    succes_model_text <- succes_model
  }
  
  return(succes_model_text)
  
}

# Function to render and move a quarto file
Quarto_Render_Move <- function(input,
                               output_file = NULL,
                               output_dir = NULL,
                               ...) {
  
  # Retrieve all information about the output of the quarto file
  x <- quarto::quarto_inspect(input)
  output_format <- names(x$formats)
  output <- x$formats[[output_format]]$pandoc$`output-file`
  if (is.null(output_file)) {
    output_file <- output
  }
  input_dir <- dirname(input)
  if (is.null(output_dir)) {
    output_dir <- input_dir
  }
  output_path_from <- file.path(input_dir, output)
  output_path_to <- file.path(output_dir, output_file)
  
  # Render the qmd input file to the input_dir
  quarto::quarto_render(input = input, ... = ...)
  
  # If the output_dir is different from the input_dir, copy the rendered file
  # there and delete the original file
  if (input_dir != output_dir) {
    # Try to make the folder if it doesn't yet exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir)
    }
    
    # Now move the output to the output_dir and delete the original output
    file.copy(from = output_path_from,
              to = output_path_to,
              overwrite = TRUE)
    file.remove(output_path_from)
    
    # If the output_dir is the same as the input_dir, but the rendered file
    # has a different name than the input file, rename it
  } else if (output_file != output) {
    file.rename(from = output_path_from, to = output_path_to)
  }
  
}

# Function to retrieve the asset repo
Get_Asset_Repo <- function() {
  
  # Determine the asset repo
  .asset_repo <- file.path(Sys.getenv("ONEDRIVE"), 
                           "HHs_NFWA/lta-hhs-tidymodels-studiesucces-reports")
  
  return(.asset_repo)

}

# Function to copy the _book directory 
# to the output directory of the repo outside this project
Copy_Book_To_Reports <- function(output_dir, debug = FALSE) {
  
  # Determine the asset repo
  .asset_repo <- Get_Asset_Repo()
  
  # Define the output directory of the repo outside this project
  output_dir_repo <- Get_Output_Dir_Repo(output_dir)
  
  # Define the input directory
  input_dir_book <- file.path("_book")
  
  # If the output directory of the repo outside this project does not exist, 
  # create it first
  if (!dir.exists(output_dir_repo)) {
    dir.create(output_dir_repo, recursive = TRUE)
  }
  
  # Copy the _book directory to the output directory 
  # from the repo outside this project
  fs::dir_copy(input_dir_book, output_dir_repo, overwrite = TRUE)
  
  # Test if the copy action was successful
  if (dir.exists(output_dir_repo)) {
    cli::cli_alert_success(
      c(
        "The _book directory is copied to {.var {(.asset_repo)}}: \n",
        "{.file {output_dir_repo}}"
      )
    )
  } else {
    cli::cli_alert_error(
      c(
        "The _book directory is NOT copied to {.var {(.asset_repo)}}: \n",
        "{.file {output_dir_repo}}"
      )
    )
  }
  
}

# Function to copy the _book directory
Get_Output_Dir_Repo <- function(output_dir) {
  
  # Determine the asset repo
  .asset_repo <- Get_Asset_Repo()
  
  # Determine the output directory of the repo outside this project
  output_dir_repo <- file.path(.asset_repo, output_dir)
  
  return(output_dir_repo)
  
}

# Function to copy files to the output directory of the repo outside this project
Copy_Reports <- function(remove_orgials = F, debug = F) {
  
  # Determine the output directory of the repo outside this project
  sOutput_directory <- file.path(Get_Rootdirectory(),
                                 "00 LTA Git/Git HHs/LTA_Reports/lta-hhs-studiesucces-models")
  
  # Create a filelist of the .html files in the output directory
  file_list <- list.files(
    "_output",
    pattern = "*.html",
    full.names = TRUE,
    recursive = TRUE
  )
  
  if(debug) {
    cli::cli_h1("File list:")
    cli::cli_bullets(file_list)
  }
  
  # Copy the .html files from the output directory to the output directory 
  # of the repo outside this project
  if(!dir.exists(sOutput_directory)) {
    dir.create(sOutput_directory)
  }
  
  # If there are files, copy them
  if(length(file_list) > 0) {
    
    # Copy the .html files from the output directory 
    # to the output directory of the repo outside this project
    walk(file_list, function(f) {
      
      # Create an output directory for the files: repo + faculty
      .output_dir <- file.path(sOutput_directory,
                               file.path(dirname(f)) |> basename())
      
      if(!dir.exists(.output_dir)) {
        dir.create(.output_dir)
      }
      
      file.copy(
        from = f,
        to = .output_dir,
        overwrite = TRUE,
        recursive = FALSE,
        copy.mode = TRUE
      )
    })
    
    cli::cli_alert("The files have been copied")
    
  } else {
    cli::cli_alert("There are no files to copy")
  }
  
  if(remove_orgials) {
    file.remove(file_list)
    cli::cli_alert("The original files have been deleted")
  }
}

# Function to determine the current analysis
Get_Current_Analysis <- function() {
  
  # Define the description of the analysis
  .succes     <- janitor::make_clean_names(params$succes)
  .propedeuse <- janitor::make_clean_names(params$propedeusediploma)
  
  .analyse <- paste(
    .succes,
    .propedeuse,
    sep = "_"
  )
  
  return(.analyse)
  
}

# Function to knit a header
Knit_Header <- function(x, rep = 1) {
  
  .header <- rep("#", rep) |> paste(collapse = "")
  
  Knit_Print_Rule(glue("{(.header)} {x}"))
}

# Function to knit a line
Knit_Print_Rule <- function(x) {
  
  knitr::knit_print(glue("\n\n\n{x}\n\n"))
  
}

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5. CLI FEATURES ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function for printing a subheader
Cli_Subheader <- function(sText) {
  
  cli::cli_div(theme = list(span.neutral = list(color = "orange"),
                            span.success = list(color = "darkgreen"),
                            span.failure = list(color = "red"),
                            span.null    = list(color = "darkgray"),
                            span.topic   = list(color = "darkblue")))
  
  cli::cat_rule()
  cli::cat_line(sText)
  cli::cat_rule()
  
}

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 6. QUERY FEATURES ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to find the most common value
get_mode <- function(x) {
  x |>
    table() |>
    which.max() |>
    names()
}

# Function to determine the most common category
Get_Mostcommon_Category <- function(x) {
  
  # Test of x categorisch is
  if(!is.factor(x) && !is.character(x)) {
    stop("De variabele is niet categorisch")
  }
  
  # Bepaal de meest voorkomende categorie
  x <- names(which.max(table(x)))
  
  return(x)
  
}

# Function to determine median (rounded and without NAs)
Get_Median_Rounded <- function(x) {
  
  # Test of x numeriek is
  if(!is.numeric(x)) {
    stop("De variabele is niet numeriek")
  }
  
  # Bepaal de mediaan
  x <- round(median(x, na.rm = TRUE), 1)
  
  return(x)
  
}

# Helper function to find most common value
Get_Mostcommon_Value <- function(x) {
  if (is.numeric(x)) {
    return(median(x, na.rm = TRUE))
  } else {
    return(names(sort(table(x), decreasing = TRUE))[1])
  }
}

# Function to create a persona of a study programme's students (OLD VERSION)
Get_dfPersona <- function(group = NULL) {
  
  # Determine the categorical variables used
  lSelect_categorical <- c(
    lSensitive_labels,
    "Studiekeuzeprofiel",
    "APCG",
    "Cijfer_CE_VO_missing",
    "Cijfer_SE_VO_missing",
    "Cijfer_CE_Nederlands_missing",
    "Cijfer_CE_Engels_missing",
    "Cijfer_CE_Wiskunde_missing",
    "Cijfer_CE_Natuurkunde_missing",
    "Dubbele_studie"
  ) 
  
  # Remove the current group variable from this list
  if (!is.null(group)) {
    .group <- as.name(group)
    # Verwijder de groep variabele uit deze lijst
    lSelect_categorical <- setdiff(lSelect_categorical, group)
  }
  
  # Remove variables not present in this study programme
  lSelect_categorical <- intersect(lSelect_categorical, 
                                   colnames(dfOpleiding_inschrijvingen))
  
  # Define the numeric variables used
  lSelect_numerical <- c(
    "Leeftijd",
    "Aanmelding",
    "Reistijd",
    "Cijfer_CE_VO",
    "Cijfer_SE_VO",
    "Cijfer_CE_Nederlands",
    "Cijfer_CE_Wiskunde",
    "Cijfer_CE_Engels",
    "Cijfer_CE_Natuurkunde",
    "SES_Totaal",
    "SES_Welvaart",
    "SES_Arbeid"
  )
  
  # If study programme is similar to HDT, add Rangnummer
  if(current_opleiding$INS_Opleiding == "HDT") {
    lSelect_numerical <- c(lSelect_numerical, "Rangnummer")
  }
  
  # Remove variables not present in this study programme
  lSelect_numerical <- intersect(lSelect_numerical, 
                                 colnames(dfOpleiding_inschrijvingen))
  
  # Calculate the total for this study programme
  .totaal <- dfOpleiding_inschrijvingen |> 
    count() |> 
    pull(n)
  
  if (!is.null(group)) {
    
    # Create personas based on the specified group
    dfPersona <- dfOpleiding_inschrijvingen |>
      
      # Split study programmes by group
      group_by(!!.group) |>
      
      # Create a persona based on the remaining variables: 
      # choose the most common values per variable in the case of categories 
      # and the median for numeric variables
      summarise(
        
        # Categorical variables
        across(
          all_of(lSelect_categorical), 
          Get_Mostcommon_Category,
          .names = "{col}"
        ),
        
        # Numerical variables
        across(
          all_of(lSelect_numerical),
          Get_Median_Rounded,
          .names = "{col}"
        ), 
        
        # Other variables
        Collegejaar = median(Collegejaar, na.rm = TRUE),
        ID = NA,
        
        # Subtotal number of students
        Subtotaal = n(),
        
        .groups = "drop") |> 
      
      # Count the number of students per group
      mutate(Totaal = .totaal,
             Percentage = round(Subtotaal/Totaal, 3)) |>
      
      # Add the group variable and define the category within the group
      mutate(Groep = group,
             Categorie = !!.group) |>
      
      # Mutate age to integer
      mutate(Leeftijd = as.integer(round(Leeftijd, 0))) |>
      
      # Reorder
      select(Groep, Categorie, Totaal, Subtotaal, Percentage, everything())
    
  } else {
    
    # Create persona for all students without grouping
    dfPersona <- dfOpleiding_inschrijvingen |>
      
      # Create a persona based on the remaining variables: 
      # choose the most common values per variable in the case of categories 
      # and the median for numeric variables
      summarise(
        
        # Categorical variables
        across(
          all_of(lSelect_categorical), 
          Get_Mostcommon_Category,
          .names = "{col}"
        ),
        
        # Numerical variables
        across(
          all_of(lSelect_numerical),
          Get_Median_Rounded,
          .names = "{col}"
        ), 
        
        # Other variables
        Collegejaar = median(Collegejaar, na.rm = TRUE),
        ID = NA,
        
        # Subtotal number of students
        Subtotaal = n(),
        
        .groups = "drop") |> 
      
      # Count the number of students per group
      mutate(Totaal = .totaal,
             Percentage = round(Subtotaal/Totaal, 3)) |>
      
      # Add the group variable and define the category within the group
      mutate(Groep = "Alle",
             Categorie = "Alle studenten") |>
      
      # Mutate age to integer
      mutate(Leeftijd = as.integer(round(Leeftijd, 0))) |>
      
      # Reorder
      select(Groep, Categorie, Totaal, Subtotaal, Percentage, everything())
  }
  
  return(dfPersona)
}

Get_dfPersona_Recursive <- function(variable_list = NULL) {
  
  # Initialize the result dataframe
  dfResults <- NULL
  
  # Initialize the working dataframe
  dfWorking <- dfOpleiding_inschrijvingen
  
  # Calculate the total number of students
  .totaal <- dfWorking |> count() |> pull(n)
  
  # Define categorical and numerical variables present in the dataframe
  lSelect_categorical <- c(
    lSensitive_labels,
    "Studiekeuzeprofiel",
    "APCG",
    "Cijfer_CE_VO_missing",
    "Cijfer_SE_VO_missing",
    "Cijfer_CE_Nederlands_missing",
    "Cijfer_CE_Engels_missing",
    "Cijfer_CE_Wiskunde_missing",
    "Cijfer_CE_Natuurkunde_missing",
    "Dubbele_studie"
  ) |> intersect(colnames(dfWorking))
  
  lSelect_numerical <- c(
    "Leeftijd",
    "Aanmelding",
    "Reistijd",
    "Cijfer_CE_VO",
    "Cijfer_SE_VO",
    "Cijfer_CE_Nederlands",
    "Cijfer_CE_Wiskunde",
    "Cijfer_CE_Engels",
    "Cijfer_CE_Natuurkunde",
    "SES_Totaal",
    "SES_Welvaart",
    "SES_Arbeid"
  ) |> intersect(colnames(dfWorking))
  
  # Add "Rangnummer" if the study programme is HDT
  if (current_opleiding$INS_Opleiding == "HDT") {
    lSelect_numerical <- c(lSelect_numerical, "Rangnummer") |> 
      intersect(colnames(dfWorking))
  }
  
  if (is.null(variable_list)) {
    # If no variable list is provided, calculate for the entire dataset
    dfResults <- dfWorking |>
      summarise(
        # Categorical variables
        across(
          all_of(lSelect_categorical), 
          Get_Mostcommon_Category,
          .names = "{col}"
        ),
        # Numerical variables
        across(
          all_of(lSelect_numerical),
          Get_Median_Rounded,
          .names = "{col}"
        ),
        # Other variables
        Collegejaar = median(Collegejaar, na.rm = TRUE),
        ID = NA,
        Subtotaal = n(),
        .groups = "drop"
      ) |>
      mutate(
        Totaal = .totaal,
        Percentage = round(Subtotaal / Totaal, 3),
        Groep = "Alle",
        Categorie = "Alle studenten"
      ) |>
      mutate(Leeftijd = as.integer(round(Leeftijd, 0))) |>
      select(Groep, Categorie, Totaal, Subtotaal, Percentage, everything())
    
  } else {
    
    # Loop through each variable in the list
    for (variable in variable_list) {
      
      # Convert variable to symbol for dynamic grouping
      .variable <- as.name(variable)
      
      # Exclude the grouping variable from the categorical variables
      lSelect_categorical <- setdiff(lSelect_categorical, variable)
      
      # Check if the current variable exists in the dataframe
      if (!(variable %in% colnames(dfWorking))) {
        warning(paste("Variable", variable, "not found in the dataset. Skipping."))
        next
      }
      
      # Summarise data for the current variable
      dfPersona <- dfWorking |>
        group_by(!!.variable) |>
        summarise(
          # Categorical variables
          across(
            all_of(lSelect_categorical), 
            Get_Mostcommon_Category,
            .names = "{col}"
          ),
          # Numerical variables
          across(
            all_of(lSelect_numerical),
            Get_Median_Rounded,
            .names = "{col}"
          ),
          # Other variables
          Collegejaar = median(Collegejaar, na.rm = TRUE),
          ID = NA,
          Subtotaal = n(),
          .groups = "drop"
        ) |>
        mutate(
          Totaal = .totaal,
          Percentage = round(Subtotaal / Totaal, 3),
          Groep = variable,
          Categorie = !!.variable
        ) |>
        mutate(Leeftijd = as.integer(round(Leeftijd, 0))) |>
        select(Groep, Categorie, Totaal, Subtotaal, Percentage, everything())
      
      # Append to the result dataframe
      dfResults <- bind_rows(dfResults, dfPersona)
      
      # Update the working dataframe for the next iteration
      dfWorking <- dfWorking |> filter(!!.variable == Get_Mostcommon_Category(dfWorking[[variable]]))
    }
  }
  
  return(dfResults)
}

# Function to create a breakdown plot
Get_dfBreakdown_Lf <- function(bd_lf) {
  
  dfBreakdown_lf <- as.data.frame(bd_lf) |>
    
    # Sort by the position of the variablese (descending)
    arrange(desc(position)) |>
    
    # Rename variables (intercept and prediction)
    mutate(
      variable = case_when(
        variable == "intercept" ~ "Intercept",
        variable == "prediction" ~ "Voorspelling",
        TRUE ~ variable
      )
    ) |>
    
    # Create the label (in percentages)
    mutate(label = paste0(as.character(round(contribution, 3) * 100), "%")) |>
    mutate(
      label = case_when(
        variable %in% c("Intercept", "Voorspelling") ~ label,
        sign == 1  ~ paste0("+", label),
        sign == -1 ~ paste0(label),
        sign == 0  ~ paste0("+", label),
        .default = label
      )
    ) |>
    mutate(label = case_when(label == "0%" ~ "+0%", .default = label)) |>
    
    # Remove variables with a contribution of 0
    filter(sign != 0) |>
    
    # Create start and end variables
    mutate(start = dplyr::lag(cumulative, default = min(cumulative)),
           end = cumulative) 
  
  # Add a row for “Other variables”
  dfBreakdown_lf <- dfBreakdown_lf |>
    add_row(
      variable = "+ Overige variabelen",
      contribution = 0,
      variable_name = NA,
      variable_value = NA,
      cumulative = dfBreakdown_lf$cumulative[dfBreakdown_lf$position == 1],
      sign = "0",
      position = 2,
      label = "+0%",
      start = dfBreakdown_lf$cumulative[dfBreakdown_lf$position == 1],
      end = dfBreakdown_lf$cumulative[dfBreakdown_lf$position == 1]
    ) |>
    
    # Adjust the position
    arrange(position) |>
    mutate(position = row_number()) |>
    arrange(desc(position)) |>
    
    # Determine the next start and position
    mutate(
      next_start = lead(start, default = NA),
      next_position = lead(position, default = NA)
    ) |>
    mutate(start = case_when(
      variable == "Intercept" ~ 0,
      variable == "Voorspelling" ~ 0,
      .default = start
    )) |>
    
    # Customize the sign
    mutate(sign = case_when(variable == "Intercept" ~ "X", .default = sign)) |>
    
    # Determine the color of the labels
    mutate(label_color = case_when(
      sign == "1" ~ lColors_default[["sNegative_color"]], 
      sign == "-1" ~ lColors_default[["sPositive_color"]], 
      .default = lColors_default[["sText_color"]])) |>
    
    # Determine the position of the labels
    rowwise() |>
    mutate(label_position = max(start, end)) |>
    ungroup()
  
  # Adjust the sign to a factor
  dfBreakdown_lf$sign <- factor(
    dfBreakdown_lf$sign,
    levels = c("1", "-1", "0", "X"),
    labels = c("Positief", "Negatief", "Geen", "X")
  ) 
  
  return(dfBreakdown_lf)
  
}

# Function to determine the Shapley values
Get_dfShapley <- function(shapley_object) {
  
  # Create a dataframe of the shapley values
  dfShapley <- shapley_object |> 
    
    # Remove variables without contribution
    filter(contribution != 0) |>
    
    # Calculate the average contribution per variable
    group_by(variable) |>
    mutate(mean_val = mean(contribution)) |>
    ungroup() |>
    
    # Sort the variables by mean contribution
    mutate(variable = fct_reorder(variable, abs(mean_val))) 
  
  return(dfShapley)
  
}

# Function to create a fairness object
Get_objFairness <- function(explainer,
                            protected_var, 
                            privileged, 
                            verbose = FALSE) {
  
  # Define the protected variable
  .protected <- dfOpleiding_inschrijvingen |> 
    select(-Retentie) |>
    select(all_of({{protected_var}})) |>
    pull() 
  
  # Create a fairness object
  fobject <- fairness_check(explainer,
                            protected = .protected,
                            privileged = privileged,
                            cutoff = 0.5,
                            verbose = verbose,
                            colorize = TRUE)
  
  # Return the fairness object
  return(fobject)
}

# Function to make the privileged (majority)
Get_Privileged <- function(df, group) {
  
  # Calculate the frequencies of each subgroup
  dfTally <- table(df[[group]])
  
  # Determine the most common subgroup(s).
  max_frequency <- max(dfTally)
  most_common_subgroups <- names(dfTally[dfTally == max_frequency])
  
  # If there are several, choose the first one (or determine another logic)
  sPrivileged <- most_common_subgroups[1]
  
  return(sPrivileged)
}

# Function to create the fairness table
Get_dfFairness_Total <- function(fobject) {
  
  # Create a table from the fairness analysis
  dfFairness <<- fobject[["fairness_check_data"]] |>
    as.data.frame() |> 
    filter(!is.na(score))
  
  # Calculate for each metric whether the score is outside the cutoff
  dfFairness_metric <<- dfFairness |>
    
    # For each group, calculate whether the score is outside the cutoff
    mutate(Categorie_buiten_grenzen = ifelse(score < 0.8 |
                                              score > 1.2, "Ja", "Nee")) |> 
    # For each group, calculate whether there is > 1 Ja
    group_by(metric) |>
    summarise(Metric_buiten_grenzen = ifelse(sum(Categorie_buiten_grenzen == "Ja") > 1, 
                                             "Ja", 
                                             "Nee"))
  
  # Enrich the table with variable Metric_outside_limits
  dfFairness_totaal <- dfFairness |>
    
    # For each group, calculate whether the score is outside the cutoff
    mutate(Categorie_buiten_grenzen = ifelse(score < 0.8 |
                                              score > 1.2, "Ja", "Nee")) |> 
    
    # Link to the metric
    left_join(dfFairness_metric, by = "metric") |> 
    select(-model) |> 
    
    # Rename the columns
    rename(Metric = metric,
           `Metric buiten grenzen` = Metric_buiten_grenzen,
           Score = score,
           Categorie = subgroup,
           `Categorie buiten grenzen` = Categorie_buiten_grenzen) |> 
    select(Metric, 
           `Metric buiten grenzen`, 
           Categorie, 
           `Categorie buiten grenzen`) #|> 
    
    # Remove missing values
    #drop_na()
  
  return(dfFairness_totaal)
  
}

# Function to convert fairness analysis df to a wide df
Get_dfFairness_Wide <- function(lDf) {
  
  ## Create a dataframe with the variables based on lSensitive_variables
  dfVars <- do.call(rbind, lapply(names(lLevels), function(group) {
    data.frame(
      FRN_Group = group,
      FRN_Subgroup = lLevels[[group]],
      stringsAsFactors = FALSE
    )
  })) |>
    
    ## Filter on lSensitive_labels
    filter(FRN_Group %in% lSensitive_labels) |>
    
    ## Order by the order in lSensitive_labels
    mutate(FRN_Group = factor(FRN_Group, levels = lSensitive_labels)) |>
    arrange(FRN_Group)
  
  dfBias <- tibble(
    FRN_Bias = c("Geen Bias", "Negatieve Bias", "Positieve Bias")
  )
  
  # Combine dfVars and dfBias
  dfVars_Bias <- dfVars |> 
    crossing(dfBias)
  
  # Total size of the data set
  total_rows <- nrow(bind_rows(lDf))
  
  df <- bind_rows(lDf) |> 
    group_by(FRN_Group, FRN_Subgroup, FRN_Bias) |>
    summarise(
      FRN_Bias_count = n(), 
      .groups = "drop"
    ) |> 
    full_join(dfVars_Bias,
              by = c("FRN_Group" = "FRN_Group", 
                     "FRN_Subgroup" = "FRN_Subgroup",
                     "FRN_Bias" = "FRN_Bias")) |>
    pivot_wider(names_from = FRN_Bias, 
                values_from = FRN_Bias_count,
                values_fill = list(FRN_Bias_count = 0)) |> 
    replace_na(list(`Geen Bias` = 0, `Negatieve Bias` = 0, `Positieve Bias` = 0)) |>
    rename(Variabele = FRN_Group,
           Groep = FRN_Subgroup) |>
    select(Variabele, Groep, `Geen Bias`, `Negatieve Bias`, `Positieve Bias`) 
  
  dfTellingen <- dfOpleiding_inschrijvingen |>
    select(all_of(lSensitive_labels)) |>
    pivot_longer(cols = all_of(lSensitive_labels)) |>
    count(name, value, name = "N") |> 
    group_by(name) |>
    mutate(
      Perc = round(N / sum(N) * 100, 1) 
    ) |> 
    ungroup()
  
  # Make the df wide
  dfWide <- df |>
    
    # Adjust the Bias
    mutate(Bias = case_when(
      `Negatieve Bias` > 1 | `Positieve Bias` > 1 ~ 'Ja',
      `Geen Bias` == 0 & `Negatieve Bias` == 0 & `Positieve Bias` == 0 ~ 'NTB',
      .default = "Nee")) |> 
    
    # Sort the Variable and Group
    # Make levels unique based on the first occurence (to avoid conflicts for repeating levels)
    mutate(Variabele = factor(Variabele, 
                              levels = lSensitive_labels),
           Groep = factor(Groep,
                          levels = unique(dfVars$FRN_Subgroup, fromLast = FALSE))
    ) |> 
    select(Variabele, Groep, Bias, `Geen Bias`, `Negatieve Bias`, `Positieve Bias`) |> 
    arrange(Variabele, Groep)
  
  # Add numbers and percentages
  dfWide2 <- dfWide |> 
    left_join(dfTellingen, by = c("Variabele" = "name", "Groep" = "value")) |>
    select(Variabele, Groep, N, everything()) |> 
    mutate(
      N = replace_na(N, 0), 
      Perc = replace_na(Perc, 0) 
    ) |> 
    mutate(Perc = format(Perc, decimal.mark = ",", nsmall = 1)) |> 
    filter(N > 0) |> 
    select(Variabele, Groep, N, Perc, Bias, `Geen Bias`, `Negatieve Bias`, `Positieve Bias`) 
  
  # Add labels and text to the groups based on dfLevels
  dfWide3 <- dfWide2 %>%
    left_join(dfLevels |> 
                filter(!is.na(VAR_Level_label_NL_description)) |> 
                select(VAR_Level_NL, VAR_Level_label_NL_description) |> 
                distinct(), by = c("Groep" = "VAR_Level_NL")) |> 
    mutate(
      Groep_label = if_else(!is.na(VAR_Level_label_NL_description), VAR_Level_label_NL_description, Groep),
      Text = glue("{Groep_label} ({Groep}: N = {N}, {Perc}%)")
    ) |> 
    select(-VAR_Level_label_NL_description) |> 
    select(Variabele, Groep, Groep_label, everything(), Text)
  
  return(dfWide3)
}


# Function to create the flextable for fairness analysis
Get_ftFairness <- function(ft) {
  
  sColor_Bias_Positive <- lColors_default[["sColor_Bias_Positive"]] # "#9DBF9E"
  sColor_Bias_Negative <- lColors_default[["sColor_Bias_Negative"]] # "#A84268"
  sColor_Bias_Neutral  <- lColors_default[["sColor_Bias_Neutral"]]  # "#FCB97D"
  sColor_Bias_None     <- lColors_default[["sColor_Bias_None"]]     # "#E5E5E5"
  
  # Merge the 'Variable' column for visual grouping
  # Apply conditional formatting
  ft <- ft |>
    merge_v(j = ~ Variabele) |>
    fix_border_issues() |>
    theme_vanilla() |>
    set_header_labels(
      Variabele = "Variabele",
      Groep = "Groep",
      N = "N",
      Perc = "%",
      Bias = "Bias",
      `Geen Bias` = "Geen Bias",
      `Negatieve Bias` = "Negatieve Bias",
      `Positieve Bias` = "Positieve Bias"
    ) |>
    autofit() |> 
    italic(j = 1, italic = TRUE, part = "body") |> 
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
       j = 2:8,
       bg = sColor_Bias_None) |>
    bold(i = ~ `Negatieve Bias` > 1,
         j = c("Groep", "Bias", "Negatieve Bias")) |>
    bold(i = ~ `Positieve Bias` > 1,
         j = c("Groep", "Bias", "Positieve Bias")) |> 
    # italic(i = ~ `Geen Bias` == 0 & `Positieve Bias` == 0 & `Negatieve Bias` == 0,
    #        j = NULL) |>
    valign(j = 1, valign = "top", part = "all") |> 
    align_text_col(align = "left") |> 
    align_nottext_col(align = "center") |> 
    # Align % and Bias column
    align(j = 4:5, align = "center", part = "header") |> 
    align(j = 4:5, align = "center")
    
  
  return(ft)
}

# Function to determine fairness inferences
Get_Fairness_Conclusies <- function(df, variabele, succes = "Retentie na 1 jaar") {
  
  sText <- ""
  
  # Define the groups
  dfVariabele <- df |>
    filter(Variabele == variabele,
           N > 14) 
  
  if(any(dfVariabele$Bias == "Ja")) {
    sConclusie <- glue("Er is sprake van bias in {succes} op basis van {tolower(variabele)}.")
  } else {
    sConclusie <- glue("Er is geen sprake van bias in {succes} op basis van {tolower(variabele)}.")
    return(sConclusie)
  }
  
  # Determine the groups with negative bias
  if(any(dfVariabele$`Negatieve Bias` > 1)) {
    lNegatieve_Bias <- dfVariabele |>
      filter(`Negatieve Bias` > 1) |> 
      pull(Text) |>
      paste(collapse = ", ")
    
    # Replace the final comma by 'en'
    lNegatieve_Bias <- Concatenate_List(lNegatieve_Bias)
    sNegatieve_Bias <- glue("Er is een negatieve bias voor {lNegatieve_Bias}.")
  } else {
    sNegatieve_Bias <- ""
  }
  
  # Determine the groups with positive bias
  if(any(dfVariabele$`Positieve Bias` > 1)) {
    lPositieve_Bias <- dfVariabele |>
      filter(`Positieve Bias` > 1) |> 
      pull(Text) |>
      paste(collapse = ", ")
    
    # Replace the final comma by 'en'
    lPositieve_Bias <- Concatenate_List(lPositieve_Bias)
    sPositieve_Bias <- glue("Er is een positieve bias voor {lPositieve_Bias}.")
  } else {
    sPositieve_Bias <- ""
  }
  
  sText <- glue("{sConclusie} {sNegatieve_Bias} {sPositieve_Bias}")
  
  return(sText)
  
}

# Function to create a data frame from the fairness check data
Get_dfFairness_Check_Data <- function(fobject, group) {
  df <- fobject |>
    dplyr::mutate(
      Fair_TF = ifelse(score < 0.8 | score > 1.25, FALSE, TRUE),
      FRN_Metric = case_when(
        grepl("Accuracy equality", metric)       ~ "Accuracy Equality",
        grepl("Predictive parity ratio", metric) ~ "Predictive Parity",
        grepl("Predictive equality", metric)     ~ "Predictive Equality",
        grepl("Equal opportunity", metric)       ~ "Equal Opportunity",
        grepl("Statistical parity", metric)      ~ "Statistical Parity"
      ),
      FRN_Group = group
    ) |>
    rename(
      FRN_Score = score,
      FRN_Subgroup = subgroup,
      FRN_Fair = Fair_TF,
      FRN_Model = model
    ) |>
    select(FRN_Model,
           FRN_Group,
           FRN_Subgroup,
           FRN_Metric,
           FRN_Score,
           FRN_Fair)
  
  # Create a dataframe of the fairness check data
  dfTellingen <- dfOpleiding_inschrijvingen |>
    select(!!group) |>
    pivot_longer(cols = c(!!group)) |>
    count(name, value, name = "N")
  
  # Combine with numbers
  df <- df |>
    left_join(dfTellingen, by = c("FRN_Group" = "name", "FRN_Subgroup" = "value")) |>
    replace_na(list(N = 0)) |> 
    mutate(FRN_Faculteit = faculteit,
           FRN_Opleiding = opleiding,
           FRN_Opleidingstype = opleidingstype,
           FRN_Opleidingsvorm = opleidingsvorm) 
    
  return(df)
}

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 7. PLOT FEATURES ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Determine the basic theme
Set_Theme <- function(title.font = c("sans"), type = "plot") {
  theme_set(theme_minimal())
  theme_update(
    
    # Title and caption
    plot.title = element_textbox_simple(
      size = 16,
      lineheight = 1,
      color = lColors_default["sTitle_color"],
      face = "bold",
      padding = margin(0, 0, 0, 0),
      margin = margin(5, 0, 5, 0) ,
      family = title.font
    ),
    plot.subtitle = element_textbox_simple(
      size = 12,
      lineheight = 1,
      color = lColors_default["sSubtitle_color"],
      padding = margin(0, 0, 0, 0),
      margin = margin(5, 0, 15, 0)
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(size = 8,
                                          color = lColors_default["sCaption_color"],
                                          padding = margin(0, 0, 0, 0),
                                          margin = margin(15, 0, 0, 0)),
    
    # Assen
    axis.title.x = element_text(face = "bold",
                                vjust = 5),
    axis.title.y = element_text(face = "bold",
                                margin = margin(
                                  t = 0,
                                  r = 10,
                                  b = 0,
                                  l = 0
                                )),
    axis.text.x  = element_text(size = 11),
    axis.text.y  = element_text(size = 11),

    # Lines
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),

    # Legend
    legend.key.size = unit(.5, "cm"),
    legend.text = element_text(size = 10),

    # Background white and border not visible
    plot.background = element_rect(fill = lColors_default["sBackground_color"],
                                   color = NA) +

    # Make the title of x and y a markdown element
    theme(
      axis.title.x = element_markdown(),
      axis.title.y = element_markdown()
    ) 
      
  )
  
}

# Function to add theme elements
Add_Theme_Elements <- function(p,
                               title_subtitle = TRUE,
                               extended = FALSE) {
  
  # Customize theme with or without title and subtitle
  if(title_subtitle) {
    p <- p + theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_markdown(),
        axis.text.y = element_text(size = 10),
        plot.caption = element_textbox_simple(
          size = 8,
          color = lColors_default["sCaption_color"],
          padding = margin(0, 0, 0, 0),
          margin = margin(15, 0, 0, 0)
        )
      ) 
    } else {
      p <- p + theme(
          axis.text.y = element_text(size = 10),
          plot.caption = element_textbox_simple(
            size = 8,
            color = lColors_default["sCaption_color"],
            padding = margin(0, 0, 0, 0),
            margin = margin(15, 0, 0, 0)
          )
        )
    }
  
  # If the theme needs to be expanded, add additional elements
  if(extended) {
    
    p <- p + 
      
      # Customize the theme further
      theme(
        axis.title.x = element_text(margin = margin(t = 20))
      ) +
      
      # Adjust the position of the legend and hide the title
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
      
      # Make the grid a little quieter
      theme(panel.grid.minor = element_blank()) +
      
      # Make the cups of the facets larger
      theme(strip.text = element_text(size = 12))
  }
  
  return(p)
  
}

# Function to set the y-axis
Set_XY_Axis <- function(axis, breaks = 4) {
  
  if(axis == "x") {
    lX_Axis <- list()
    lX_Axis[["x_breaks"]] <- seq(0, 1, by = (1/breaks))
    lX_Axis[["x_labels"]] <- paste0(seq(0, 100, by = (100/breaks)), "%")
    
    return(lX_Axis)
  }
  
  if(axis == "y") {
    lY_Axis <- list()
    lY_Axis[["y_breaks"]] <- seq(0, 1, by = 0.25)
    lY_Axis[["y_labels"]] <- paste0(seq(0, 100, by = 25), "%")
    
    return(lY_Axis)
  }
  
}

# Function to define the caption
Get_sCaption <- function() {
  
  sCaption <- paste0(
    paste(
      lResearch_settings[["sDataset"]],
      lResearch_settings[["sResearch_path"]],
      lResearch_settings[["sOpleiding"]],
      sep = ", "
    ),
    ". \U00A9 ",
    lMetadata[["sAnalyse"]],
    ", ",
    format(Sys.Date(), "%Y")
  )
  
  return(sCaption)
  
}

# Function to determine color values and labels
Get_Color_Values_and_Labels <- function(group, cp_lf_all) {
  
  colors_list <- lColors[[group]]
  
  unique_values <- unique(cp_lf_all[[group]])
  .values <- unname(colors_list[unique_values])
  .labels <- names(colors_list[unique_values])
  
  return(list(values = .values, labels = .labels))
}

# Function to create an ROC plot
Get_ROC_Plot <- function(models, position = NULL) {
  
  # Combine multiple models if necessary
  if(is.list(models)) {
    models <- bind_rows(models)
  }
  
  if(!is.null(position)) {
    lColors <- lColors[["ROC_plots"]][position]
  } else {
    lColors <- lColors[["ROC_plots"]]
  }
  
  # Create an ROC plot
  roc_plot <- models |>
    ggplot(aes(x = 1 - specificity, 
               y = sensitivity, 
               col = model)) + 
    geom_path(lwd = 1.5, alpha = 0.8) +
    geom_abline(lty = 3) + 
    coord_equal() + 
    
    # Add multiple colors to multiple models
    scale_color_manual(values = lColors) +
    
    # Make the labs
    labs(x = "1 - specificiteit", 
         y = "sensitiviteit", 
         color = "Model",
         caption = sCaption) +
    theme(
      axis.title.x = element_text(margin = margin(t = 20))
    )
    
    # Add elements
    roc_plot <- Add_Theme_Elements(roc_plot,
                                       title_subtitle = FALSE)
  
  return(roc_plot)
  
}

# Function to create a confusion plot
Get_Confusion_Plot <- function(dfConf_matrix) {
 
  confusion_plot <- plot_confusion_matrix(
    dfConf_matrix,
    target_col = "Werkelijkheid",
    prediction_col = "Prediction",
    counts_col = "n",
    palette = "Blues",
    add_sums = TRUE,
    theme_fn = ggplot2::theme_light,
    sums_settings = sum_tile_settings(
      palette = "Greens",
      label = "Totaal",
      tc_tile_border_color = "black"
    )) +
    
    # Customize the labels
    labs(
      title = "Confusion Matrix",
      x = "Werkelijke uitkost",
      y = "Voorspelde uitkomst",
      caption = sCaption
    ) +
    
    Set_Theme()
  
  # Add elements
  confusion_plot <- Add_Theme_Elements(confusion_plot, 
                                           title_subtitle = TRUE)
  
  return(confusion_plot)
   
}

# Function to create an RMSE plot
Get_RMSE_Plot <- function(mp_rmse) {
  
  # Create an RMSE plot
  mp_rmse_plot <- plot(mp_rmse) +
    
    # Themes
    Set_Theme() +
    
    # Title, subtitle and caption
    labs(
      title = "Meest voorspellende factoren",
      subtitle = "Root Mean Square Error (RMSE) na permutaties",
      caption = sCaption,
      x = NULL,
      y = NULL
    ) +
    
    # Hide the legend
    theme(
      legend.position = "none"
    )
  
  # Add elements
  mp_rmse_plot <- Add_Theme_Elements(mp_rmse_plot)
  
  return(mp_rmse_plot)

}

# Function to determine titles
Get_Breakdown_Titles <- function(bd, df, j, 
                                 student_groep, student_categorie, 
                                 mode = "group",
                                 debug = FALSE) {
  
  # Determine retention rate, totals and title/subtitle
  nRetentie   <- Change_Number_Marks(as.numeric(bd$cumulative[bd$variable == 'prediction']) * 100, 
                                     digits = 1)
  nSubtotaal  <- Change_Number_Marks(as.numeric(df[j, 'Subtotaal']))
  nTotaal     <- Change_Number_Marks(as.numeric(df[j, 'Totaal']))
  nPercentage <- Change_Number_Marks(as.numeric(df[j, 'Percentage']) * 100, digits = 1)
  
  if(debug) {
    cli::cli_alert_info(c("nRetentie: ",   nRetentie))
    cli::cli_alert_info(c("nSubtotaal: ",  nSubtotaal))
    cli::cli_alert_info(c("nTotaal: ",     nTotaal))
    cli::cli_alert_info(c("nPercentage: ", nPercentage))
  }
  
  # Build the title
  if(mode == "all") {
    student_current_title <- glue(
      "Opbouw van de {tolower(lResearch_settings[['sSucces_label']])} ({tolower(student_groep)})"
    )
  } else if(mode == "group") {
    student_current_title <- glue(
      "Opbouw van de {tolower(lResearch_settings[['sSucces_label']])} naar {tolower(student_groep)}"
    )
  }  
  
  if(debug) {
    cli::cli_alert_info(student_current_title)
  }
  
  # Define the subtitle
  student_current_subtitle <- glue(
    " | {tolower(lResearch_settings[['sSucces_label']])}: {nRetentie}%"
  )
  
  if(mode == "all") {
    student_current_subtitle <- glue(
      "**{student_categorie}**",
      student_current_subtitle,
      " | _N_ = {nSubtotaal}"
    )
  } else if(mode == "group") {
    student_current_subtitle <- glue(
      "{student_groep}: **{student_categorie}**",
      student_current_subtitle,
      " | _N_ = {nSubtotaal} van {nTotaal} ({nPercentage}%)"
    )
  }
  
  if(debug) {
    cli::cli_alert_info(student_current_subtitle)
  }
  
  return(list(student_current_title, 
              student_current_subtitle))
}

# Function to create a breakdown plot (all)
Get_Breakdown_Plot_All <- function(breakdown_lf_all, lTitles) {
  
  # Build the basic plot
  breakdown_plot <- suppressWarnings(plot(breakdown_lf_all, plot_distributions = TRUE)) 
  
  # Determine the y axis
  lY_Axis <- Set_XY_Axis(axis = "y")

  # Complete the plot based on the specific theme
  breakdown_plot <- breakdown_plot +

    # Themes
    Set_Theme() +

    # Define the title, subtitle and caption
    labs(
      title = lTitles[[1]],
      subtitle = lTitles[[2]],
      caption = sCaption,
      x = NULL,
      y = NULL
    )

  # Adjust the summary layer for the average probability (layer 3)
  breakdown_plot$layers[[3]] <- stat_summary(
    fun = mean,
    geom = "point",
    shape = 20,
    size = 3,
    color = lColors_default[["sNegative_color"]],
    fill = lColors_default[["sNegative_color"]]
  )

  # Complete the plot
  breakdown_plot <- breakdown_plot +

    # Adjust the y-axis labels
    scale_y_continuous(breaks = lY_Axis[["y_breaks"]],
                       labels = lY_Axis[["y_labels"]],
                       limits = c(0, 1)) +

    # Hide the legend
    theme(
      legend.position = "none"
    )

  # Add elements
  breakdown_plot <- Add_Theme_Elements(breakdown_plot)
  
  return(breakdown_plot)
  
}

# Function to create a waterfall plot
Get_Breakdown_Plot <- function(df, titles) {
   
  # Determine the breaks for the x-axis (y-axis, but is tilted)
  lY_Axis  <- Set_XY_Axis(axis = "y")
  
  # Create a waterfall plot
  breakdown_plot <- ggplot(df) +
    
    # Add horizontal lines every 0.2
    geom_hline(
      yintercept = lY_Axis[["y_breaks"]],
      color = lColors_default[["sGridline_color"]],
      linetype = "solid",
      linewidth = 0.5
    ) +
    
    # Add a horizontal line at the lowest value
    geom_hline(
      yintercept = df$cumulative[df$position == 1],
      color = lColors_default[["sBreakdown_intercept_color"]],
      linetype = "dotted"
    ) +
    
    # Add the waterfall bands
    geom_rect(aes(
      xmin = position - 0.4,
      xmax = position + 0.4,
      ymin = start,
      ymax = end,
      fill = sign
    )) +
    
    # Add the waterfall lines
    geom_segment(aes(
      x = next_position - 0.4,
      xend = position + 0.4,
      y = end,
      yend = end
    ),
    color = lColors_default[["sBreakdown_segment_color"]]) +
    
    # Flip the plot
    coord_flip() +
    
    # Define the title and subtitle
    labs(
      title = titles[[1]],
      subtitle = titles[[2]],
      caption = sCaption,
      x = NULL,
      y = NULL
    ) +
    
    # Fill in the colors
    scale_fill_manual(values = c(
      "Positief" = lColors_default[["sPositive_color"]],
      "Negatief" = lColors_default[["sNegative_color"]],
      "X" = lColors_default[["sPositive_color"]]
    )) +
    
    # Add text labels for the variables
    geom_text(
      aes(x = position, y = label_position, label = label),
      hjust = -0.1,
      size = 4,
      color = lColors_default[["sText_color"]]
    ) +
    
    # Adjust the theme to display the y-axis labels
    scale_x_continuous(breaks = df$position, labels = df$variable) +
    scale_y_continuous(breaks = lY_Axis[["y_breaks"]],
                       labels = lY_Axis[["y_labels"]],
                       limits = c(0, 1)) +
    
    # Remove the legend and make the plot quieter
    theme(legend.position = "none") +
    theme(
      axis.text.y = element_text(size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
  
    # Make the labels of Intercept and prediction bold.
    # suppressWarnings, because there will be a warning that vectorized input 
    # will not be possible in the future
    suppressWarnings(theme(axis.text.y = element_text(
      face = ifelse(
        df$variable %in% c("Intercept", "Voorspelling"),
        "bold",
        "plain"
      )
    )))
  
  return(breakdown_plot)
  
}

# Function to create a Shapley plot
Get_Shapley_Plot <- function(data) {
  
  shapley_plot <- data |> 
    
    # Build the plot and fill the color with positive and negative values
    ggplot(aes(contribution, variable, fill = mean_val > 0)) +
    
    # Create a barplot and add a boxplot
    geom_col(data = ~distinct(., variable, mean_val), 
             aes(mean_val, variable), 
             alpha = 0.5) +
    geom_boxplot(width = 0.5) +
    
    # Remove the legend
    theme(legend.position = "none") +
    
    # Determine the colors
    scale_fill_manual(values = c("TRUE" = lColors_default[["sPositive_color"]], 
                                 "FALSE" = lColors_default[["sNegative_color"]])) +
    
    # Define the title and subtitle
    labs(
      title = "Shapley values",
      subtitle = "Bijdrage per variabele voor de meest voorkomende student",
      caption = sCaption,
      x = NULL,
      y = NULL
    )
  
  return(shapley_plot)
  
}

# Function to create the ceteris paribus plot
Get_Ceteris_Paribus_Plot <- function(cp_lf_all, group) {
  
  # Determine the y axis
  lY_Axis <- Set_XY_Axis(axis = "y")
  
  # Plot the ceteris paribus analysis
  # Use color for _ids_
  cp_plot <- plot(
    cp_lf_all,
    color = "_ids_",  
    variables = c(
      "Leeftijd",
      "Cijfer_CE_VO",
      "Cijfer_CE_Wiskunde",
      "SES_Totaal",
      "Aanmelding"
    )
  )

  # Remove the existing color scale,
  # so there is no warning about the existing color scale
  cp_plot$scales$scales <- list()

  # Build the color scale based on the variable
  # Define the values and labels
  lColor_values_labels <- Get_Color_Values_and_Labels(group, cp_lf_all)
  
  # Now build the plot further
  cp_plot <- cp_plot +

    # Add a single bowl for the fill
    scale_color_manual(
      name = group,
      values = lColor_values_labels$values,
      labels = lColor_values_labels$labels,
    ) +

    # Adjust the y-axis scale
    scale_y_continuous(breaks = lY_Axis[["y_breaks"]],
                       labels = lY_Axis[["y_labels"]],
                       limits = c(0, 1)) +

    # Customize the labels
    labs(title = "Ceteris-paribus profiel",
         subtitle = glue("{lResearch_settings[['sSucces_label']]} voor de meest voorkomende studenten naar **{tolower(group)}**"),
         y = NULL,
         caption = sCaption) +

    # Apply the theme
    Set_Theme()
    
  # Add elements
  cp_plot <- Add_Theme_Elements(cp_plot, 
                                title_subtitle = TRUE, 
                                extended = TRUE) +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    guides(colour = guide_legend(nrow = 1))
  
  # Return the plot
  return(cp_plot)
  
}

# Function to create the ceteris paribus plot
Get_Partial_Dependence_Plot <- function(pdp_lf, 
                                        group = "all",
                                        show_profiles = TRUE) {
  
  # Determine the y axis
  lY_Axis <- Set_XY_Axis(axis = "y")
  
  # Define color scales for each variable
  if (group == "all") {
    .values = lColors_default[["sMetrics_blue"]]
  } else {
    .values = lColors[[group]]
  }
  
  # Build the subtitle
  if(group == "all") {
    .subtitle <- glue("{lResearch_settings[['sSucces_label']]}")
  } else {
    .subtitle <- glue("{lResearch_settings[['sSucces_label']]} naar **{tolower(group)}**")
  }
  
  # Remove from pdp_lf[[“agr_profiles”]][[“_label_”]] the name of the model
  # so that the labels match the category names in the variables
  .model <- explain_lf$label
  pdp_lf[["agr_profiles"]][["_label_"]] <- gsub(paste0(.model, "_"),
                                                "",
                                                pdp_lf[["agr_profiles"]][["_label_"]])
  
  # Analyseer de gedeeltelijke afhankelijkheid
  if(show_profiles) {
    pdp_plot <- plot(pdp_lf, geom = "profiles")
  } else {
    pdp_plot <- plot(pdp_lf)
  }

  # Remove the existing color scale, 
  # so there is no warning about the existing color scale
  pdp_plot$scales$scales <- list()
  
  # Now build the plot further
  pdp_plot <- pdp_plot +  
    
    # Add a single scale for the fill
    scale_color_manual(
      name = NULL,
      values = .values
    ) +

    # Adjust the y-axis scale
    scale_y_continuous(breaks = lY_Axis[["y_breaks"]],
                       labels = lY_Axis[["y_labels"]],
                       limits = c(0, 1)) +
    
    # Customize the labels
    labs(title = "Partial Dependence profielen",
         subtitle = .subtitle,
         y = NULL,
         caption = sCaption) +
    
    # Apply the theme
    Set_Theme()
    
  # Add elements
    pdp_plot <- Add_Theme_Elements(pdp_plot,
                                   title_subtitle = TRUE,
                                   extended = TRUE) +
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
      guides(colour = guide_legend(nrow = 1))
    
    # Return the plot
    return(pdp_plot)
  
}

# Function to create a density plot
Get_Density_Plot <- function(fairness_object, group) {
  
  # Determine the x axis
  lX_Axis <- Set_XY_Axis(axis = "x")
  
  # Define color scales for each variable
  if (group == "all") {
    .values = lColors_default[["sMetrics_blue"]]
  } else {
    .values = lColors[[group]]
  }
  
  # Create a density plot
  density_plot <- fairness_object |> 
    
    plot_density() +
    
    # Add title and subtitle
    labs(
      title = glue("Verdeling en dichtheid van {tolower(lResearch_settings[['sSucces_label']])}"),
      subtitle = glue("Naar **{group}**"),
      caption = sCaption,
      x = NULL,
      y = NULL)
    
  # Remove the existing color scale,
  # so there is no warning about the existing color scale
  density_plot$scales$scales <- list()
  
  # Define the color
  density_plot <- density_plot +
    
    # Add a single scale for the fill
    scale_fill_manual(
      name = NULL,
      values = .values
    ) +
    
    # Adjust the x-axis scale
    scale_x_continuous(breaks = lX_Axis[["x_breaks"]],
                       labels = lX_Axis[["x_labels"]],
                       limits = c(0, 1)) +
    
    # Add a line on the 50% labeled “50%”
    geom_vline(xintercept = 0.5,
               linetype = "dotted",
               color = lColors_default[["sPositive_color"]]) +
    
    # Add the label “50%”
    annotate(
      "text",
      x = 0.53,
      y = 0.5,
      label = "50%",
      vjust = -0.3,
      color = lColors_default[["sPositive_color"]]) +
    
    # Apply the theme
    Set_Theme()  +
    
    # Customize some theme elements
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "none",
      strip.text = element_blank()
    )
  
  # Add elements.
  density_plot <- Add_Theme_Elements(density_plot,
                                     title_subtitle = TRUE) +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    guides(fill = guide_legend(nrow = 1))

  
  return(density_plot)
  
}

# Function to create a fairness plot
Get_Fairness_Plot <- function(fairness_object, group, privileged) {
  
  # Determine the y axis
  y_breaks <- seq(-100, 100, by = 0.2)
  
  # Create a fairness plot
  fairness_plot <- fairness_object |> 
    plot() +
    theme_minimal() +
    Set_Theme() +
    
    # Add title and subtitle
    labs(
      title = "Fairness check",
      subtitle = glue("Fairness van het model voor **{group}** ",
                      "ten opzichte van **{privileged}**"),
      caption = sCaption,
      x = NULL,
      y = NULL)
    
  # Remove the existing color scale, 
  # so there is no warning about the existing color scale
  fairness_plot$scales$scales <- list()
  
  # Build the plot further
  fairness_plot <- fairness_plot +
    
    # Define the color
    scale_fill_manual(
      values = c(lColors_default[["sPositive_color"]])
    ) +
      
    # Adjust the y-axis scale
    scale_y_continuous(breaks = y_breaks)
    
    # Add elements.
    fairness_plot <- Add_Theme_Elements(fairness_plot,
                                        title_subtitle = TRUE) +
    
    # Customize some theme elements
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "none",
      strip.text = element_text(hjust = 0)
    )
  
  return(fairness_plot)
  
}

# Based on the bbplot package are built left_align ensave_plot
# (hence the names in lowercase, so that these functions override those of the bbplot package)

# Left align
left_align <- function (plot_name, pieces) {
  grob <- ggplot2::ggplotGrob(plot_name)
  n <- length(pieces)
  grob$layout$l[grob$layout$name %in% pieces] <- 2
  return(grob)
}

# Save plot
save_plot <- function (plot_grid, width, height, save_filepath) {
  
  ggplot2::ggsave(
    filename = save_filepath,
    plot = plot_grid,
    width = (width / 72),
    height = (height / 72),
    bg = lColors_default[["sBackground_color"]],
    device = ragg::agg_png,
    res = 300,
    create.dir = TRUE
  )
}

# Save a plot
Finalize_Plot <-
  function (plot_name,
            source_name,
            save_filepath = file.path(Sys.getenv("TMPDIR"),
                                      "tmp-nc.png"),
            width_pixels = nPlotWidth,
            height_pixels = nPlotHeight,
            show_plot = FALSE) {
    
    # Print de plot
    plot_grid <- ggpubr::ggarrange(
      plot_name,
      ncol = 1,
      nrow = 2,
      # heights = c(1, 0.045 / (height_pixels / 450)) # Correction to BBC template (margin)
      heights = c(1, 0 / (height_pixels / 450))
    )
    save_plot(plot_grid, width_pixels, height_pixels, save_filepath)
    
    # If the plot must be shown, show it
    if(show_plot) {
      invisible(plot_grid)
    }
  }

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 8. ADDITIONAL AID FUNCTIONS ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function convert numbers to readable notation
Change_Number_Marks <- function(x, digits = 0) {
  return(formatC(round(x, digits), 
                 format = "f", 
                 digits = digits, 
                 big.mark = ".", 
                 decimal.mark = ","))
}

# Retrieve the version name of the dataset
Get_sDataset <- function(df) {
  unique(df$LTA_Dataset)
}

# Function to determine the metadata of the analysis
Get_Metadata <- function() {
  
  lMetadata <- list(
    "sBron_label"    = "Bron",
    "sDataset_label" = "Dataset",
    "sPlot_label"    = "Plot",
    "sAnalyse_label" = "Analyse",
    "sInstelling"    = lResearch_settings[["sInstelling"]],
    "sBron"          = lResearch_settings[["sBron"]],
    "sDataset"       = lResearch_settings[["sDataset"]],
    "sOpleiding"     = lResearch_settings[["sOpleiding"]],
    "sAnalyse"       = lResearch_settings[["sAnalyse"]]
  )
  
  return(lMetadata)
}

# Function to concatenate a list of strings
Concatenate_List <- function(l, 
                             lang = "nl", 
                             extend = TRUE, 
                             tolower = FALSE, 
                             backtick = FALSE) {
  
  if(tolower){
    l <- tolower(l)
  }
  if(backtick) {
    l <- backtick(l)
  }
  if(lang == "en"){
    sLast <- ", and "
  } else if(lang == "nl") {
    sLast <- " en "
  } else {
    sLast <- ", "
  }
  
  if(extend){
      sCollapse <- glue_collapse(l, sep = ", ", last = sLast)
    } else {
      sCollapse <- glue_collapse(l, sep = ", ")
    }
  
  return(sCollapse)
  
}

# Function to add a colored square
Get_Colored_Square <- function(color, bordercolor = "darkgrey", size = 12) {
  sprintf(
    '<span style="display:inline-block; width:%dpx; height:%dpx; background-color:%s; border:1px solid %s;"></span>', 
    size, size, color, bordercolor
  )
}
