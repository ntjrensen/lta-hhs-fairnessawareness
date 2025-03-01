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

get_subtitle <- function() {
  
  subtitle <- format(Sys.Date(), "%d-%m-%Y")
  
  subtitle
}

# Function to determine current study programme
get_current_sp <- function(sp, sp_form) {
  
  if (!exists("df_studyprogrammes")) {
    cli::cli_abort("df_studyprogrammes is not defined")
  }
  
  # Test if this study programme does appear
  if (!any(
    df_studyprogrammes$INS_Opleiding == sp &
      df_studyprogrammes$INS_Opleidingsvorm == sp_form
  )) {
    cli::cli_abort(
      paste0(
        "The studyprogramme ",
        sp,
        " with studyprogramme form ",
        sp_form,
        " does not exist.\n",
        "Check if the studyprogramme name and studyprogramme form are correct."
      )
    )
  }
  
  current_sp <- df_studyprogrammes |>
    dplyr::filter(INS_Opleiding == sp,
                  INS_Opleidingsvorm == sp_form) |>
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
  
  current_sp
}

# Function to set the variables of the current study programme
set_current_sp_vars <- function(current_sp, debug = FALSE) {
  
  sp               <<- current_sp$INS_Opleiding
  faculty          <<- current_sp$INS_Faculteit
  sp_type          <<- current_sp$INS_Opleidingstype_LTA
  sp_name_current  <<- current_sp$INS_Opleidingsnaam_huidig
  sp_form          <<- tolower(current_sp$INS_Opleidingsvorm)
  hhs_location     <<- tolower(current_sp$INS_Vestiging_HHs_LTA)
  vh_sector        <<- current_sp$INS_Sector_VH_LTA
  vh_sector_long   <<- current_sp$INS_Sector_VH_lang
  sp_clean_name    <<- get_sp_dir(faculty, sp_name_current, sp, sp_form)
  
  if (debug) {
    show_current_sp_vars()
  }
  
}

# Function to show the variables of the current study programme
show_current_sp_vars <- function() {
  
  if (!exists("current_sp")) {
    cli::cli_abort("current_sp is not defined")
  }
  
  # Print all values of the current study programme
  cli::cli_h1(paste0("Current sp:"))
  
  cli_bullets(c(
    "*" = paste0("Faculty: ",                   faculty),
    "*" = paste0("Studyprogramme type: ",       sp_type),
    "*" = paste0("Studyprogramme: ",            sp),
    "*" = paste0("Studyprogramme name: ",       sp_name_current),
    "*" = paste0("Studyprogramme form: ",       sp_form),
    "*" = paste0("HHS location: ",              hhs_location),
    "*" = paste0("VH sector lowercase: ",       vh_sector),
    "*" = paste0("VH sector long name: ",       vh_sector_long),
    "*" = paste0("Studyprogramme clean name: ", sp_clean_name)
  ))
}

# Function to determine the current training name
get_sp <- function() {
  
  if (!exists("current_sp")) {
    cli::cli_abort("current_sp is not defined")
  }
  
  paste0(
    get_sp_name_syn(current_sp$INS_Opleidingsnaam_huidig),
    " (",
    current_sp$INS_Opleiding,
    ") ",
    current_sp$INS_Opleidingsvorm
  )
  
}

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. PATH FUNCTIONS ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to determine the path to flextables
get_sp_dir <- function(faculty,
                       sp_name_current,
                       sp_name = NULL,
                       sp_form) {
  
  if (faculty == "Xtern") {
    path <- janitor::make_clean_names(paste(
      faculty,
      sp_name_current,
      sp_form,
      sep = "_"
    ))
  } else {
    path <- janitor::make_clean_names(paste(
      faculty,
      sp_name_current,
      sp_name,
      sp_form,
      sep = "_"
    ))
  }
  
  path
}

# Function to determine output directory of current study programme
get_current_sp_output_dir <- function(current_sp,
                                      mode) {
  
  faculty_sp_form <- paste0(
    current_sp$INS_Faculteit,
    "/",
    current_sp$INS_Opleidingstype_LTA,
    "-",
    current_sp$INS_Opleiding,
    "-",
    current_sp$INS_Opleidingsvorm
  ) |> tolower()
  
  # If synthetic data is used, add -synth to the directory
  if (params$use_synthetic_data == TRUE) {
    faculty_sp_form <- paste0(faculty_sp_form, "-synth")
  }
  
  if (mode == "last-fits" ||
        mode == "modelresults") {
    this_output_dir <- file.path("_output", faculty_sp_form, "modelresults")
  } else if (mode == "data") {
    this_output_dir <- file.path("_output", faculty_sp_form, "data")
  } else if (mode == "fairness") {
    this_output_dir <- file.path("_output", faculty_sp_form, "fairness")
  } else if (mode == "html") {
    this_output_dir <- file.path("_output", faculty_sp_form)
  } else if (mode == "plot") {
    this_output_dir <- file.path("_output", faculty_sp_form, "plots")
  } else {
    cli::cli_alert("The mode is not correct")
  }
  
  this_output_dir
  
}

# Function to determine the output directory of the current study programme
get_current_sp_output_file <- function(df, mode, group = NULL, analysis = NULL) {
  
  # Define the description of the analysis
  if (is.null(analysis)) {
    this_analysis <- get_current_analysis()
  } else {
    this_analysis <- analysis
  }
  
  if (mode == "last-fits") {
    suffix <- "_last-fits.rds"
  } else if (mode == "fairness") {
    suffix <- paste0("_fairness_", tolower(group), ".rds")
  } else if (mode == "html") {
    suffix <- ".html"
  } else if (mode == "modelresults") {
    suffix <- "_modelresults.rds"
  } else if (mode == "data") {
    suffix <- ".rds"
  } else if (mode == "plot") {
    suffix <- ".png"
  } else {
    cli::cli_alert("The mode is not correct")
  }
  
  # Determine the output file: faculty-sp-sp_form + analysis + suffix
  this_output_file <- paste0(
    paste(
      df$INS_Faculteit,
      df$INS_Opleidingstype_LTA,
      df$INS_Opleiding,
      df$INS_Opleidingsvorm,
      sep = "-"
    ),
    "_",
    this_analysis,
    suffix
  )
  
}

# Function to determine the output path for fitted models
get_model_outputpath <- function(mode, group = NULL) {
  
  # Define the output file
  this_output_file <- get_current_sp_output_file(current_sp, mode, group)
  
  # Define the output directory
  this_output_dir <- get_current_sp_output_dir(current_sp, mode)
  
  # Create the directory if it does not already exist
  if (!dir.exists(this_output_dir)) {
    dir.create(this_output_dir, recursive = TRUE)
  }
  
  # Return the full output path
  file.path(this_output_dir, this_output_file)
  
}

# Function to determine the output path for the plots
get_plot_outputpath <- function(plot_name, mode = "plot", best_model = NA) {
  
  # Best model is not NA, make the name clean
  if (!is.na(best_model)) {
    
    this_best_model <- janitor::make_clean_names(best_model)
    
    # Define the output file
    this_output_file <- glue("{plot_name}_{this_best_model}.png") 
    
  } else {
    
    # Define the output file
    this_output_file <- glue("{plot_name}.png") 
    
  }
  
  # Define the output directory
  this_output_dir <- get_current_sp_output_dir(current_sp, mode)
  
  # Create the directory if it does not already exist
  if (!dir.exists(this_output_dir)) {
    dir.create(this_output_dir, recursive = TRUE)
  }
  
  # Return the full output path
  file.path(this_output_dir, this_output_file)
  
}

# Function to determine the output path for the breakdown plots
get_breakdown_plotpath <- function(student_group,
                                   student_category,
                                   best_model) {
  
  # Replace spaces with - in the student_group and student_category
  this_student_group     <- gsub(" ", "-", student_group)
  this_student_category  <- gsub(" ", "-", student_category)
  
  file.path(get_plot_outputpath(plot_name = tolower(
    glue(
      "lf_break_down_{(this_student_group)}_{(this_student_category)}"
    )
  ), best_model = best_model))
}

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. DATASET FUNCTIONS ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to read the data dictionary.
get_data_dictionary <- function() {
  
  input_path <- "R/vars"
  
  df <- rio::import(file.path(input_path, "data-dictionary.xlsx"))
  
  df
}

# Function to create a table from the data dictionary
get_tbl_data_dictionary <- function(df) {
  
  tbl <- 
    df |> 
    flextable() |> 
    theme_vanilla() |>  
    set_table_properties(width = 1, layout = "autofit") |>  
    bg(i = ~ seq_len(nrow(df_data_dictionary)) %% 2 == 0, bg = "gray95") |>  
    align(align = "left", part = "all") |>  
    border_remove() |>  
    border_inner_h(border = fp_border(color = "gray", width = 0.5)) |>  
    border_inner_v(border = fp_border(color = "gray", width = 0.5)) |>  
    bold(part = "header") |>  
    fontsize(size = 11, part = "all") |>  
    color(part = "header", color = "black") |>  
    set_caption(caption = "Overzicht van variabelen")  
  
  tbl
}

# Function to retrieve synthetic data
get_sp_enrollments_syn <- function(sp, sp_form) {
  
  input_path <- "R/data/syn"
  
  df <- readRDS(file.path(input_path, "studyprogrammes_enrollments_syn.rds")) |> 
    filter(INS_Opleiding == sp,
           INS_Opleidingsvorm == sp_form)
  
  df
}

# Function to make the Failure variable
mutate_dropout <- function(df, model = "Uitval na 1 jaar") {
  
  # Fill in the missing values with 0
  df <- df |>
    mutate(SUC_Uitval_aantal_jaar_LTA = coalesce(SUC_Uitval_aantal_jaar_LTA, 0))
  
  if (model == "Uitval na 1 jaar") {
    df |>
      mutate(
        SUC_Uitval = ifelse(SUC_Uitval_aantal_jaar_LTA == 1, TRUE, FALSE),
        SUC_Uitval = coalesce(SUC_Uitval, FALSE)
      )
  } else if (model == "Uitval na 2 jaar") {
    df |>
      mutate(
        SUC_Uitval = ifelse(SUC_Uitval_aantal_jaar_LTA %in% c(1, 2), TRUE, FALSE),
        SUC_Uitval = coalesce(SUC_Uitval, FALSE)
      )
  } else if (model == "Uitval na 3 jaar") {
    df |>
      mutate(
        SUC_Uitval = ifelse(SUC_Uitval_aantal_jaar_LTA %in% c(1, 2, 3), TRUE, FALSE),
        SUC_Uitval = coalesce(SUC_Uitval, FALSE)
      )
  } else if (model == "Alle uitval") {
    df |>
      mutate(
        SUC_Uitval = ifelse(SUC_Uitval_aantal_jaar_LTA > 0, TRUE, FALSE),
        SUC_Uitval = coalesce(SUC_Uitval, FALSE)
      )
  }
  
}

# Function to differentiate Dropout (with and without a propaedeutic degree)
filter_pd_dropout <- function(df, pd = "Nvt") {
  
  if (pd == "Nvt" ||
        pd == "" || is.na(pd)) {
    # Include all dropout
    df
    
  } else if (pd == "Met P") {
    # Remove students who drop out with a propaedeutic degree
    df |>
      filter(SUC_Studiestatus_cat != "Uitval zonder propedeutisch diploma")
    
  } else if (pd == "Zonder P") {
    # Remove students who drop out with a propaedeutic degree
    df |>
      filter(SUC_Studiestatus_cat != "Uitval met propedeutisch diploma")
    
  } else {
    cli::cli_alert("The propaedeutic degree variable is incorrect")
  }
  
}

# Function to create the Retention variable
mutate_retention <- function(df, model = "Retentie na 1 jaar") {
  
  if (model == "Retentie na 1 jaar") {
    df |>
      mutate(
        SUC_Retentie = ifelse(SUC_Uitval_aantal_jaar_LTA == 1, FALSE, TRUE),
        SUC_Retentie = coalesce(SUC_Retentie, TRUE)
      ) 
  } else if (model == "Retentie na 2 jaar") {
    df |>
      mutate(
        SUC_Retentie = ifelse(SUC_Uitval_aantal_jaar_LTA == 1:2, FALSE, TRUE),
        SUC_Retentie = coalesce(SUC_Retentie, TRUE)
      ) 
  } else if (model == "Retentie na 3 jaar") {
    df |>
      mutate(
        SUC_Retentie = ifelse(SUC_Uitval_aantal_jaar_LTA == 1:3, FALSE, TRUE),
        SUC_Retentie = coalesce(SUC_Retentie, TRUE)
      ) 
  } else if (model == "Alle retentie") {
    df |>
      mutate(
        SUC_Retentie = ifelse(SUC_Uitval_aantal_jaar_LTA > 0, FALSE, TRUE),
        SUC_Retentie = coalesce(SUC_Retentie, TRUE)
      )
  } 
  
}

# Mutate Dubbele studies
mutate_parallel_sp <- function(df) {
  
  df <- df |>
    mutate(INS_Dubbele_studie = ifelse(INS_Aantal_inschrijvingen > 1, "Ja", "Nee"))
  
  df
  
}

# Mutate APCG
mutate_apcg <- function(df) {
  
  df <- df |>
    mutate(APCG = case_when(APCG == TRUE ~ "Ja", 
                            APCG == FALSE ~ "Nee", 
                            .default = "Onbekend"))
  
  df
  
}

# Mutate missing figures VO
mutate_grade_preeducation <- function(df) {
  
  df <- df |>
    mutate(
      Cijfer_SE_VO_missing          = ifelse(is.na(Cijfer_SE_VO),          "Ja", "Nee"),          
      Cijfer_CE_VO_missing          = ifelse(is.na(Cijfer_CE_VO),          "Ja", "Nee"),        
      Cijfer_CE_Nederlands_missing  = ifelse(is.na(Cijfer_CE_Nederlands),  "Ja", "Nee"),
      Cijfer_CE_Engels_missing      = ifelse(is.na(Cijfer_CE_Engels),      "Ja", "Nee"),
      Cijfer_CE_Wiskunde_missing    = ifelse(is.na(Cijfer_CE_Wiskunde),    "Ja", "Nee"),
      Cijfer_CE_Natuurkunde_missing = ifelse(is.na(Cijfer_CE_Natuurkunde), "Ja", "Nee")
    )
  
  df
  
}

# Function to get the variables
get_df_variables <- function() {
  
  df_variables <- rio::import(file.path("R/data", "variables.xlsx"), sheet = "Variables")
  df_variables
  
}

# Function to get the sensitive variables
get_list_select <- function(df, var) {
  
  list_select <- df |> 
    filter(VAR_Select_tf == TRUE) |> 
    select(all_of(var)) |> 
    pull()
  
  list_select
}

# Function to get the levels of the variables
get_df_levels <- function() {
  
  df_levels <- rio::import(file.path("R/data", "variables.xlsx"), sheet = "Levels") |>
    group_by(VAR_Formal_variable) |>
    arrange(VAR_Level_order, .by_group = TRUE) |>
    ungroup()
  
  df_levels
}

# Function to get the sensitive variables
get_list_sensitive <- function(df, var) {
  
  list_sensitive <- df |> 
    filter(VAR_Sensitive_tf == TRUE) |> 
    select(all_of(var)) |> 
    pull()
  
  list_sensitive
}

# Function to determine the order of a number of levels
get_levels <- function(df, formal = FALSE) {
  
  ## Set levels
  levels <- list()
  
  if (formal) {
    for (i in df$VAR_Formal_variable) {
      levels[[i]] <- df$VAR_Level_NL[df$VAR_Formal_variable == i]
    }
  } else {
    for (i in df$VAR_Simple_variable) {
      levels[[i]] <- df$VAR_Level_NL[df$VAR_Simple_variable == i]
    }
  }
  
  levels
  
}
# Function to determine the levels of sensitive variables for breakdown plots
get_sensitive_levels_breakdown <- function(df, list) {
  
  sensitive_levels_breakdown <- list()
  for (i in list) {
    sensitive_levels_breakdown[[i]] <- df |> 
      filter(VAR_Formal_variable == i,
             VAR_Breakdown_tf == TRUE) |>
      select(VAR_Level_NL) |> 
      pull()
  }
  
  sensitive_levels_breakdown
}

# Function to determine the levels of a variable
set_levels <- function(df = df_sp_enrollments, levels) {

  new_levels <- list()

  new_levels[["Studiekeuzeprofiel"]] <-
    sort_levels(
      levels[["Studiekeuzeprofiel"]],
      df,
      "VOP_Studiekeuzeprofiel_LTA_afkorting"
    )

  # Loop over the sensitive attributes
  for (sensitive_attribute in sensitive_labels) {
    formal_variable <- unique(dfSensitive$VAR_Formal_variable[dfSensitive$VAR_Simple_variable == 
                                                                sensitive_attribute])
    
    new_levels[[sensitive_attribute]] <- sort_levels(levels[[sensitive_attribute]], 
                                                     df, 
                                                     formal_variable)
    
  }

  new_levels

}

# Adjust levels so that they are sorted properly
sort_levels <- function(levels, df, var) {
  levels_sorted <- intersect(levels, unique(df[[var]]))
  levels_sorted
}

# Function to mutate the levels of a variable
mutate_levels <- function(df, vars, levels) {
  
  walk(seq_along(vars), function(i) {
    df <- df |>
      mutate(
        !!rlang::sym(vars[i]) := fct_expand(!!rlang::sym(vars[i]), levels[[i]]),
        !!rlang::sym(vars[i]) := fct_relevel(!!rlang::sym(vars[i]), levels[[i]]),
        !!rlang::sym(vars[i]) := fct_drop(!!rlang::sym(vars[i]))
      )
  })
  
  df
}

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4. RENDER FUNCTIONS ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to retrieve training name with or without synth
get_sp_name_syn <- function(sp_name) {
  
  if (params$use_synthetic_data == TRUE) {
    sp_name <- paste(sp_name, "(Synth)")
  } else {
    sp_name <- sp_name
  }
  
  sp_name
  
}

# Function to determine the long name of the type of education
get_sp_form_long <- function(sp_form) {
  
  switch(
    sp_form,
    "VT" = "voltijd",
    "DT" = "deeltijd",
    "DU" = "duaal",
    "onbekend"
  )
  
}

# Function to determine the long name of the type of education
get_faculty_name_long <- function(faculty) {
  
  switch(
    faculty,
    "BFM" = "Business, Finance & Marketing",
    "BRV" = "Bestuur, Recht & Veiligheid",
    "GVS" = "Gezondheid, Voeding & Sport",
    "ITD" = "IT & Design",
    "MO"  = "Management & Organisatie",
    "SWE" = "Sociaal Werk & Educatie",
    "TIS" = "Technologie, Innovatie & Samenleving",
    "onbekend"
  )
  
}

# Function to create a summary table
get_tbl_summary <- function(df) {
  
  df_summary <- df |> 
    
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
  
  df_summary
  
}

# Function to define the text for the model (in the title)
get_succes_model_text <- function(pd, succes_model) {
  
  if (pd == "Zonder P") {
    succes_model_text <- paste(succes_model, "bij studenten zonder propedeutisch diploma")
  } else if (pd == "Met P") {
    succes_model_text <- paste(succes_model, "bij studenten met propedeutisch diploma")
  } else {
    succes_model_text <- succes_model
  }
  
  succes_model_text
  
}

# Function to render and move a quarto file
quarto_render_move <- function(input,
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
get_asset_repo <- function() {
  
  # Determine the asset repo
  this_asset_repo <- file.path(Sys.getenv("ONEDRIVE"),
                               "HHs_NFWA/lta-hhs-tidymodels-studiesucces-reports")
  
  this_asset_repo

}

# Function to copy the _book directory 
# to the output directory of the repo outside this project
copy_book_to_reports <- function(output_dir, debug = FALSE) {
  
  # Determine the asset repo
  this_asset_repo <- Get_Asset_Repo()
  
  # Define the output directory of the repo outside this project
  output_dir_repo <- get_output_dir_repo(output_dir)
  
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
        "The _book directory is copied to {.var {(this_asset_repo)}}: \n",
        "{.file {output_dir_repo}}"
      )
    )
  } else {
    cli::cli_alert_error(
      c(
        "The _book directory is NOT copied to {.var {(this_asset_repo)}}: \n",
        "{.file {output_dir_repo}}"
      )
    )
  }
  
}

# Function to copy the _book directory
get_output_dir_repo <- function(output_dir) {
  
  # Determine the asset repo
  this_asset_repo <- Get_Asset_Repo()
  
  # Determine the output directory of the repo outside this project
  output_dir_repo <- file.path(this_asset_repo, output_dir)
  
  output_dir_repo
  
}

# Function to copy files to the output directory of the repo outside this project
copy_reports <- function(remove_orgials = FALSE, debug = FALSE) {
  
  # Determine the output directory of the repo outside this project
  output_dir <- file.path(Get_Rootdirectory(),
                          "00 LTA Git/Git HHs/LTA_Reports/lta-hhs-studiesucces-models")
  
  # Create a filelist of the .html files in the output directory
  file_list <- list.files(
    "_output",
    pattern = "*.html",
    full.names = TRUE,
    recursive = TRUE
  )
  
  if (debug) {
    cli::cli_h1("File list:")
    cli::cli_bullets(file_list)
  }
  
  # Copy the .html files from the output directory to the output directory 
  # of the repo outside this project
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # If there are files, copy them
  if (length(file_list) > 0) {
    
    # Copy the .html files from the output directory 
    # to the output directory of the repo outside this project
    walk(file_list, function(f) {
      
      # Create an output directory for the files: repo + faculty
      this_output_dir <- file.path(output_dir, file.path(dirname(f)) |> basename())
      
      if (!dir.exists(this_output_dir)) {
        dir.create(this_output_dir)
      }
      
      file.copy(
        from = f,
        to = this_output_dir,
        overwrite = TRUE,
        recursive = FALSE,
        copy.mode = TRUE
      )
    })
    
    cli::cli_alert("The files have been copied")
    
  } else {
    cli::cli_alert("There are no files to copy")
  }
  
  if (remove_orgials) {
    file.remove(file_list)
    cli::cli_alert("The original files have been deleted")
  }
}

# Function to determine the current analysis
get_current_analysis <- function() {
  
  # Define the description of the analysis
  .succes     <- janitor::make_clean_names(params$succes)
  .propedeuse <- janitor::make_clean_names(params$pd)
  
  this_analysis <- paste(
    .succes,
    .propedeuse,
    sep = "_"
  )
  
  this_analysis
  
}

# Function to knit a header
knit_header <- function(x, rep = 1) {
  
  .header <- rep("#", rep) |> paste(collapse = "")
  
  knit_print_rule(glue("{(.header)} {x}"))
}

# Function to knit a line
knit_print_rule <- function(x) {
  
  knitr::knit_print(glue("\n\n\n{x}\n\n"))
  
}

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5. CLI FEATURES ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function for printing a subheader
cli_subheader <- function(text) {
  
  cli::cli_div(theme = list(span.neutral = list(color = "orange"),
                            span.success = list(color = "darkgreen"),
                            span.failure = list(color = "red"),
                            span.null    = list(color = "darkgray"),
                            span.topic   = list(color = "darkblue")))
  
  cli::cat_rule()
  cli::cat_line(text)
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
get_most_common_category <- function(x) {
  
  # Test whether x is categorical
  if (!is.factor(x) && !is.character(x)) {
    stop("The variable is not categorical")
  }
  
  # Determine the most common category
  x <- names(which.max(table(x)))
  
  x
  
}

# Function to determine median (rounded and without NAs)
get_median_rounded <- function(x) {
  
  # Test whether x is numeric
  if (!is.numeric(x)) {
    stop("The variable is not numeric")
  }
  
  # Determine the median
  x <- round(median(x, na.rm = TRUE), 1)
  
  x
  
}

# Helper function to find most common value
get_most_common_value <- function(x) {
  if (is.numeric(x)) {
    median(x, na.rm = TRUE)
  } else {
    names(sort(table(x), decreasing = TRUE))[1]
  }
}

# Function to create a persona of a study programme's students (OLD VERSION)
get_df_persona <- function(group = NULL) {
  
  # Determine the categorical variables used
  list_select_categorical <- c(
    sensitive_labels,
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
    # Remove the group variable from this list
    list_select_categorical <- setdiff(list_select_categorical, group)
  }
  
  # Remove variables not present in this study programme
  list_select_categorical <- intersect(list_select_categorical, 
                                       colnames(df_sp_enrollments))
  
  # Define the numeric variables used
  list_select_numerical <- c(
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
  if (current_sp$INS_Opleiding == "HDT") {
    list_select_numerical <- c(list_select_numerical, "Rangnummer")
  }
  
  # Remove variables not present in this study programme
  list_select_numerical <- intersect(list_select_numerical, 
                                     colnames(df_sp_enrollments))
  
  # Calculate the total for this study programme
  total <- df_sp_enrollments |> 
    count() |> 
    pull(n)
  
  if (!is.null(group)) {
    
    # Create personas based on the specified group
    df_persona <- df_sp_enrollments |>
      
      # Split study programmes by group
      group_by(!!.group) |>
      
      # Create a persona based on the remaining variables: 
      # choose the most common values per variable in the case of categories 
      # and the median for numeric variables
      summarise(
        
                # Categorical variables
                across(
                  all_of(list_select_categorical), 
                  get_most_common_category,
                  .names = "{col}"
                ),
            
                # Numerical variables
                across(
                  all_of(list_select_numerical),
                  get_median_rounded,
                  .names = "{col}"
                ), 
            
                # Other variables
                Collegejaar = median(Collegejaar, na.rm = TRUE),
                ID = NA,
                
                # Subtotal number of students
                Subtotaal = n(),
                
                .groups = "drop") |> 
      
      # Count the number of students per group
      mutate(Totaal = total,
             Percentage = round(Subtotaal / Totaal, 3)) |>
      
      # Add the group variable and define the category within the group
      mutate(Groep = group,
             Categorie = !!.group) |>
      
      # Mutate age to integer
      mutate(Leeftijd = as.integer(round(Leeftijd, 0))) |>
      
      # Reorder
      select(Groep, Categorie, Totaal, Subtotaal, Percentage, everything())
    
  } else {
    
    # Create persona for all students without grouping
    df_persona <- df_sp_enrollments |>
      
      # Create a persona based on the remaining variables: 
      # choose the most common values per variable in the case of categories 
      # and the median for numeric variables
      summarise(
        
                # Categorical variables
                across(
                  all_of(list_select_categorical), 
                  get_most_common_category,
                  .names = "{col}"
                ),
                
                # Numerical variables
                across(
                  all_of(list_select_numerical),
                  get_median_rounded,
                  .names = "{col}"
                ), 
                
                # Other variables
                Collegejaar = median(Collegejaar, na.rm = TRUE),
                ID = NA,
                
                # Subtotal number of students
                Subtotaal = n(),
                
                .groups = "drop") |> 
      
      # Count the number of students per group
      mutate(Totaal = total,
             Percentage = round(Subtotaal / Totaal, 3)) |>
      
      # Add the group variable and define the category within the group
      mutate(Groep = "Alle",
             Categorie = "Alle studenten") |>
      
      # Mutate age to integer
      mutate(Leeftijd = as.integer(round(Leeftijd, 0))) |>
      
      # Reorder
      select(Groep, Categorie, Totaal, Subtotaal, Percentage, everything())
  }
  
  df_persona
}

get_df_persona_recursive <- function(variable_list = NULL) {
  
  # Initialize the result dataframe
  df_results <- NULL
  
  # Initialize the working dataframe
  df_working <- df_sp_enrollments
  
  # Calculate the total number of students
  total <- df_working |> count() |> pull(n)
  
  # Define categorical and numerical variables present in the dataframe
  list_select_categorical <- c(
    sensitive_labels,
    "Studiekeuzeprofiel",
    "APCG",
    "Cijfer_CE_VO_missing",
    "Cijfer_SE_VO_missing",
    "Cijfer_CE_Nederlands_missing",
    "Cijfer_CE_Engels_missing",
    "Cijfer_CE_Wiskunde_missing",
    "Cijfer_CE_Natuurkunde_missing",
    "Dubbele_studie"
  ) |> 
    intersect(colnames(df_working))
  
  list_select_numerical <- c(
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
  ) |> 
    intersect(colnames(df_working))
  
  # Add "Rangnummer" if the study programme is HDT
  if (current_sp$INS_Opleiding == "HDT") {
    list_select_numerical <- c(list_select_numerical, "Rangnummer") |> 
      intersect(colnames(df_working))
  }
  
  if (is.null(variable_list)) {
    # If no variable list is provided, calculate for the entire dataset
    df_results <- df_working |>
      summarise(
        # Categorical variables
        across(
          all_of(list_select_categorical), 
          get_most_common_category,
          .names = "{col}"
        ),
        # Numerical variables
        across(
          all_of(list_select_numerical),
          get_median_rounded,
          .names = "{col}"
        ),
        # Other variables
        Collegejaar = median(Collegejaar, na.rm = TRUE),
        ID = NA,
        Subtotaal = n(),
        .groups = "drop"
      ) |>
      mutate(
        Totaal = total,
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
      list_select_categorical <- setdiff(list_select_categorical, variable)
      
      # Check if the current variable exists in the dataframe
      if (!(variable %in% colnames(df_working))) {
        warning(paste("Variable", variable, "not found in the dataset. Skipping."))
        next
      }
      
      # Summarise data for the current variable
      df_persona <- df_working |>
        group_by(!!.variable) |>
        summarise(
          # Categorical variables
          across(
            all_of(list_select_categorical), 
            get_most_common_category,
            .names = "{col}"
          ),
          # Numerical variables
          across(
            all_of(list_select_numerical),
            get_median_rounded,
            .names = "{col}"
          ),
          # Other variables
          Collegejaar = median(Collegejaar, na.rm = TRUE),
          ID = NA,
          Subtotaal = n(),
          .groups = "drop"
        ) |>
        mutate(
          Totaal = total,
          Percentage = round(Subtotaal / Totaal, 3),
          Groep = variable,
          Categorie = !!.variable
        ) |>
        mutate(Leeftijd = as.integer(round(Leeftijd, 0))) |>
        select(Groep, Categorie, Totaal, Subtotaal, Percentage, everything())
      
      # Append to the result dataframe
      df_results <- bind_rows(df_results, df_persona)
      
      # Update the working dataframe for the next iteration
      df_working <- df_working |> 
        filter(!!.variable == get_most_common_category(df_working[[variable]]))
    }
  }
  
  df_results
}

# Function to create a breakdown plot
get_df_breakdown_lf <- function(breakdown_lf) {
  
  df_breakdown_lf <- as.data.frame(breakdown_lf) |>
    
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
  df_breakdown_lf <- df_breakdown_lf |>
    add_row(
      variable = "+ Overige variabelen",
      contribution = 0,
      variable_name = NA,
      variable_value = NA,
      cumulative = df_breakdown_lf$cumulative[df_breakdown_lf$position == 1],
      sign = "0",
      position = 2,
      label = "+0%",
      start = df_breakdown_lf$cumulative[df_breakdown_lf$position == 1],
      end = df_breakdown_lf$cumulative[df_breakdown_lf$position == 1]
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
    mutate(
      label_color = case_when(
        sign == "1" ~ colors_default[["negative_color"]],
        sign == "-1" ~ colors_default[["positive_color"]],
        .default = colors_default[["text_color"]]
      )
    ) |> 
    
    # Determine the position of the labels
    rowwise() |>
    mutate(label_position = max(start, end)) |>
    ungroup()
  
  # Adjust the sign to a factor
  df_breakdown_lf$sign <- factor(
    df_breakdown_lf$sign,
    levels = c("1", "-1", "0", "X"),
    labels = c("Positief", "Negatief", "Geen", "X")
  ) 
  
  df_breakdown_lf
  
}

# Function to determine the Shapley values
get_df_shapley <- function(shapley_object) {
  
  # Create a dataframe of the shapley values
  df_shapley <- shapley_object |> 
    
    # Remove variables without contribution
    filter(contribution != 0) |>
    
    # Calculate the average contribution per variable
    group_by(variable) |>
    mutate(mean_val = mean(contribution)) |>
    ungroup() |>
    
    # Sort the variables by mean contribution
    mutate(variable = fct_reorder(variable, abs(mean_val))) 
  
  df_shapley
  
}

# Function to create a fairness object
get_obj_fairness <- function(explainer,
                             protected_var,
                             privileged,
                             verbose = FALSE) {
  
  # Define the protected variable
  protected <- df_sp_enrollments |> 
    select(-Retentie) |>
    select(all_of({{protected_var}})) |>
    pull() 
  
  # Create a fairness object
  fairness_object <- fairness_check(
    explainer,
    protected = protected,
    privileged = privileged,
    cutoff = 0.5,
    verbose = verbose,
    colorize = TRUE
  )
  
  # Return the fairness object
  fairness_object
}

# Function to make the privileged (majority)
get_privileged <- function(df, group) {
  
  # Calculate the frequencies of each subgroup
  df_tally <- table(df[[group]])
  
  # Determine the most common subgroup(s).
  max_frequency <- max(df_tally)
  most_common_subgroups <- names(df_tally[df_tally == max_frequency])
  
  # If there are several, choose the first one (or determine another logic)
  privileged <- most_common_subgroups[1]
  
  privileged
}

# Function to create the fairness table
get_df_fairness_total <- function(fairness_object) {
  
  # Create a table from the fairness analysis
  df_fairness <<- fairness_object[["fairness_check_data"]] |>
    as.data.frame() |> 
    filter(!is.na(score))
  
  # Calculate for each metric whether the score is outside the cutoff
  df_fairness_metric <<- df_fairness |>
    
    # For each group, calculate whether the score is outside the cutoff
    mutate(category_outside_borders = ifelse(score < 0.8 |
                                               score > 1.2, "Ja", "Nee")) |>
    
    # For each group, calculate whether there is > 1 Ja
    group_by(metric) |>
    summarise(metric_outside_borders = ifelse(sum(category_outside_borders == "Ja") > 1, 
                                              "Ja", "Nee"))
  
  # Enrich the table with variable Metric_outside_limits
  df_fairness_total <- df_fairness |>
    
    # For each group, calculate whether the score is outside the cutoff
    mutate(category_outside_borders = ifelse(score < 0.8 |
                                               score > 1.2, "Ja", "Nee")) |> 
    
    # Link to the metric
    left_join(df_fairness_metric, by = "metric") |> 
    select(-model) |> 
    
    # Rename the columns
    rename(Metric = metric,
           `Metric buiten grenzen` = metric_outside_borders,
           Score = score,
           Categorie = subgroup,
           `Categorie buiten grenzen` = category_outside_borders) |> 
    select(Metric, 
           `Metric buiten grenzen`, 
           Categorie, 
           `Categorie buiten grenzen`) 
    
  df_fairness_total
  
}

# Function to convert fairness analysis df to a wide df
get_df_fairness_wide <- function(df_list) {
  
  ## Create a dataframe with the variables based on sensitive_variables
  df_vars <- do.call(rbind, lapply(names(levels), function(group) {
    data.frame(
      FRN_Group = group,
      FRN_Subgroup = levels[[group]],
      stringsAsFactors = FALSE
    )
  })) |>
    
    ## Filter on sensitive_labels
    filter(FRN_Group %in% sensitive_labels) |>
    
    ## Order by the order in sensitive_labels
    mutate(FRN_Group = factor(FRN_Group, levels = sensitive_labels)) |>
    arrange(FRN_Group)
  
  df_bias <- tibble(
    FRN_Bias = c("Geen Bias", "Negatieve Bias", "Positieve Bias")
  )
  
  # Combine df_vars and df_bias
  df_vars_bias <- df_vars |> 
    crossing(df_bias)
  
  # Total size of the data set
  total_rows <- nrow(bind_rows(df_list))
  
  df <- bind_rows(df_list) |> 
    group_by(FRN_Group, FRN_Subgroup, FRN_Bias) |>
    summarise(
      FRN_Bias_count = n(), 
      .groups = "drop"
    ) |> 
    full_join(df_vars_bias,
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
  
  df_counts <- df_sp_enrollments |>
    select(all_of(sensitive_labels)) |>
    pivot_longer(cols = all_of(sensitive_labels)) |>
    count(name, value, name = "N") |> 
    group_by(name) |>
    mutate(
      Perc = round(N / sum(N) * 100, 1) 
    ) |> 
    ungroup()
  
  # Make the df wide
  df_wide <- df |>
    
    # Adjust the Bias
    mutate(
      Bias = case_when(
        `Negatieve Bias` > 1 | `Positieve Bias` > 1 ~ "Ja",
        `Geen Bias` == 0 &
          `Negatieve Bias` == 0 & `Positieve Bias` == 0 ~ "NTB",
        .default = "Nee"
      )
    ) |> 
    
    # Sort the Variable and Group
    # Make levels unique based on the first occurence (to avoid conflicts for repeating levels)
    mutate(
      Variabele = factor(Variabele, levels = sensitive_labels),
      Groep = factor(Groep, levels = unique(df_vars$FRN_Subgroup, fromLast = FALSE))
    ) |> 
    select(Variabele, Groep, Bias, `Geen Bias`, `Negatieve Bias`, `Positieve Bias`) |> 
    arrange(Variabele, Groep)
  
  # Add numbers and percentages
  df_wide_2 <- df_wide |> 
    left_join(df_counts, by = c("Variabele" = "name", "Groep" = "value")) |>
    select(Variabele, Groep, N, everything()) |> 
    mutate(
      N = replace_na(N, 0), 
      Perc = replace_na(Perc, 0) 
    ) |> 
    mutate(Perc = format(Perc, decimal.mark = ",", nsmall = 1)) |> 
    filter(N > 0) |> 
    select(Variabele, Groep, N, Perc, Bias, `Geen Bias`, `Negatieve Bias`, `Positieve Bias`) 
  
  # Add labels and text to the groups based on df_levels
  df_wide_3 <- df_wide_2 %>%
    left_join(df_levels |> 
                filter(!is.na(VAR_Level_label_NL_description)) |> 
                select(VAR_Level_NL, VAR_Level_label_NL_description) |> 
                distinct(), by = c("Groep" = "VAR_Level_NL")) |> 
    mutate(
      Groep_label = if_else(
        !is.na(VAR_Level_label_NL_description),
        VAR_Level_label_NL_description,
        Groep
      ),
      Text = glue("{Groep_label} ({Groep}: N = {N}, {Perc}%)")
    ) |> 
    select(-VAR_Level_label_NL_description) |> 
    select(Variabele, Groep, Groep_label, everything(), Text)
  
  df_wide_3
}


# Function to create the flextable for fairness analysis
get_ft_fairness <- function(ft) {
  
  color_bias_positive <- colors_default[["color_bias_positive"]] # "#9DBF9E"
  color_bias_negative <- colors_default[["color_bias_negative"]] # "#A84268"
  color_bias_neutral  <- colors_default[["color_bias_neutral"]]  # "#FCB97D"
  color_bias_none     <- colors_default[["color_bias_none"]]     # "#E5E5E5"
  
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
       bg = color_bias_negative) |>
    bg(i = ~ `Positieve Bias` > 1, 
       j = c("Groep", "Bias", "Positieve Bias"), 
       bg = color_bias_positive) |>
    bg(i = ~ `Negatieve Bias` > 1 & `Positieve Bias` > 1, 
       j = c("Groep", "Bias"), 
       bg = color_bias_neutral) |>
    bg(i = ~ N < 15 & (`Negatieve Bias` > 1 | `Positieve Bias` > 1), 
       j = c("Groep", "Bias"), 
       bg = color_bias_neutral) |>
    bg(i = ~ `Geen Bias` == 0 & `Positieve Bias` == 0 & `Negatieve Bias` == 0,
       j = 2:8,
       bg = color_bias_none) |>
    bold(i = ~ `Negatieve Bias` > 1,
         j = c("Groep", "Bias", "Negatieve Bias")) |>
    bold(i = ~ `Positieve Bias` > 1,
         j = c("Groep", "Bias", "Positieve Bias")) |> 
    valign(j = 1, valign = "top", part = "all") |> 
    align_text_col(align = "left") |> 
    align_nottext_col(align = "center") |> 
    
    # Align % and Bias column
    align(j = 4:5, align = "center", part = "header") |> 
    align(j = 4:5, align = "center")
    
  ft
}

# Function to determine fairness inferences
get_fairness_conclusions <- function(df, variabele, succes = "Retentie na 1 jaar") {
  
  text <- ""
  
  # Define the groups
  df_variables <- df |>
    filter(Variabele == variabele,
           N > 14) 
  
  if (any(df_variables$Bias == "Ja")) {
    conclusion <- glue("Er is sprake van bias in {succes} op basis van {tolower(variabele)}.")
  } else {
    conclusion <- glue("Er is geen sprake van bias in {succes} op basis van {tolower(variabele)}.")
    conclusion
  }
  
  # Determine the groups with negative bias
  if (any(df_variables$`Negatieve Bias` > 1)) {
    negative_bias_list <- df_variables |>
      filter(`Negatieve Bias` > 1) |> 
      pull(Text) |>
      paste(collapse = ", ")
    
    # Replace the final comma by 'en'
    negative_bias_list <- concatenate_list(negative_bias_list)
    negative_bias <- glue("Er is een negatieve bias voor {negative_bias_list}.")
  } else {
    negative_bias <- ""
  }
  
  # Determine the groups with positive bias
  if (any(df_variables$`Positieve Bias` > 1)) {
    positive_bias_list <- df_variables |>
      filter(`Positieve Bias` > 1) |> 
      pull(Text) |>
      paste(collapse = ", ")
    
    # Replace the final comma by 'en'
    positive_bias_list <- concatenate_list(positive_bias_list)
    positive_bias <- glue("Er is een positieve bias voor {positive_bias_list}.")
  } else {
    positive_bias <- ""
  }
  
  text <- glue("{conclusion} {negative_bias} {positive_bias}")
  
  text
  
}

# Function to create a data frame from the fairness check data
get_df_fairness_check_data <- function(fairness_object, group) {
  df <- fairness_object |>
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
  df_counts <- df_sp_enrollments |>
    select(!!group) |>
    pivot_longer(cols = c(!!group)) |>
    count(name, value, name = "N")
  
  # Combine with numbers
  df <- df |>
    left_join(df_counts, by = c("FRN_Group" = "name", "FRN_Subgroup" = "value")) |>
    replace_na(list(N = 0)) |> 
    mutate(FRN_Faculteit = faculty,
           FRN_Opleiding = sp,
           FRN_Opleidingstype = sp_type,
           FRN_Opleidingsvorm = sp_form) 
    
  df
}

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 7. PLOT FEATURES ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Determine the basic theme
set_theme <- function(title_font = c("sans"), type = "plot") {
  
  theme_set(theme_minimal())
  theme_update(
    
    # Title and caption
    plot.title = element_textbox_simple(
      size = 16,
      lineheight = 1,
      color = colors_default["title_color"],
      face = "bold",
      padding = margin(0, 0, 0, 0),
      margin = margin(5, 0, 5, 0),
      family = title_font
    ),
    plot.subtitle = element_textbox_simple(
      size = 12,
      lineheight = 1,
      color = colors_default["subtitle_color"],
      padding = margin(0, 0, 0, 0),
      margin = margin(5, 0, 15, 0)
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(size = 8,
                                          color = colors_default["caption_color"],
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
    plot.background = element_rect(fill = colors_default["background_color"],
                                   color = NA) +

      # Make the title of x and y a markdown element
      theme(axis.title.x = element_markdown(), 
            axis.title.y = element_markdown()) 
      
  )
  
}

# Function to add theme elements
add_theme_elements <- function(p,
                               title_subtitle = TRUE,
                               extended = FALSE) {
  
  # Customize theme with or without title and subtitle
  if (title_subtitle) {
    p <- p + theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_markdown(),
      axis.text.y = element_text(size = 10),
      plot.caption = element_textbox_simple(
        size = 8,
        color = colors_default["caption_color"],
        padding = margin(0, 0, 0, 0),
        margin = margin(15, 0, 0, 0)
      )
    ) 
  } else {
    p <- p + theme(
      axis.text.y = element_text(size = 10),
      plot.caption = element_textbox_simple(
        size = 8,
        color = colors_default["caption_color"],
        padding = margin(0, 0, 0, 0),
        margin = margin(15, 0, 0, 0)
      )
    )
  }
  
  # If the theme needs to be expanded, add additional elements
  if (extended) {
    
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
  
  p
  
}

# Function to set the y-axis
set_xy_axis <- function(axis, breaks = 4) {
  
  if (axis == "x") {
    x_axis_list <- list()
    x_axis_list[["x_breaks"]] <- seq(0, 1, by = (1 / breaks))
    x_axis_list[["x_labels"]] <- paste0(seq(0, 100, by = (100 / breaks)), "%")
    
    x_axis_list
  }
  
  if (axis == "y") {
    y_axis_list <- list()
    y_axis_list[["y_breaks"]] <- seq(0, 1, by = 0.25)
    y_axis_list[["y_labels"]] <- paste0(seq(0, 100, by = 25), "%")
    
    y_axis_list
  }
  
}

# Function to define the caption
get_caption <- function() {
  
  caption <- paste0(
    paste(
      research_settings[["dataset"]],
      research_settings[["research_path"]],
      research_settings[["sp"]],
      sep = ", "
    ),
    ". \U00A9 ",
    metadata[["analysis"]],
    ", ",
    format(Sys.Date(), "%Y")
  )
  
  caption
  
}

# Function to determine color values and labels
get_color_values_lables <- function(group, cp_lf_all) {
  
  colors_list <- color_list[[group]]
  
  unique_values <- unique(cp_lf_all[[group]])
  .values <- unname(colors_list[unique_values])
  .labels <- names(colors_list[unique_values])
  
  list(values = .values, labels = .labels)
}

# Function to create an ROC plot
get_roc_plot <- function(models, position = NULL) {
  
  # Combine multiple models if necessary
  if (is.list(models)) {
    models <- bind_rows(models)
  }
  
  if (!is.null(position)) {
    color_list <- color_list[["roc_plots"]][position]
  } else {
    color_list <- color_list[["roc_plots"]]
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
    scale_color_manual(values = color_list) +
    
    # Make the labs
    labs(x = "1 - specificiteit", 
         y = "sensitiviteit", 
         color = "Model",
         caption = caption) +
    theme(
      axis.title.x = element_text(margin = margin(t = 20))
    )
  
  # Add elements
  roc_plot <- add_theme_elements(roc_plot, title_subtitle = FALSE)
  
  roc_plot
  
}

# Function to create a confusion plot
get_confusion_plot <- function(df_confusion_matrix) {
  confusion_plot <- plot_confusion_matrix(
    df_confusion_matrix,
    target_col = "Werkelijkheid",
    prediction_col = "Predictie",
    counts_col = "n",
    palette = "Blues",
    add_sums = TRUE,
    theme_fn = ggplot2::theme_light,
    sums_settings = sum_tile_settings(
      palette = "Greens",
      label = "Totaal",
      tc_tile_border_color = "black"
    )
  ) +
    
    # Customize the labels
    labs(
      title = "Confusion Matrix",
      x = "Werkelijke uitkost",
      y = "Voorspelde uitkomst",
      caption = caption
    ) +
    
    set_theme()
  
  # Add elements
  confusion_plot <- add_theme_elements(confusion_plot, title_subtitle = TRUE)
  
  confusion_plot
  
}

# Function to create an RMSE plot
get_rmse_plot <- function(mp_rmse) {
  
  # Create an RMSE plot
  mp_rmse_plot <- plot(mp_rmse) +
    
    # Themes
    set_theme() +
    
    # Title, subtitle and caption
    labs(
      title = "Meest voorspellende factoren",
      subtitle = "Root Mean Square Error (RMSE) na permutaties",
      caption = caption,
      x = NULL,
      y = NULL
    ) +
    
    # Hide the legend
    theme(
      legend.position = "none"
    )
  
  # Add elements
  mp_rmse_plot <- add_theme_elements(mp_rmse_plot)
  
  mp_rmse_plot

}

# Function to determine titles
get_breakdown_titles <- function(bd, df, j, 
                                 student_group, student_category, 
                                 mode = "group",
                                 debug = FALSE) {
  
  # Determine retention rate, totals and title/subtitle
  retention  <- change_number_marks(as.numeric(bd$cumulative[bd$variable == "prediction"]) * 100, 
                                    digits = 1)
  subtotal   <- change_number_marks(as.numeric(df[j, "Subtotaal"]))
  total      <- change_number_marks(as.numeric(df[j, "Totaal"]))
  percentage <- change_number_marks(as.numeric(df[j, "Percentage"]) * 100, digits = 1)
  
  if (debug) {
    cli::cli_alert_info(c("retention: ",  retention))
    cli::cli_alert_info(c("subtotal: ",   subtotal))
    cli::cli_alert_info(c("total: ",      total))
    cli::cli_alert_info(c("percentage: ", percentage))
  }
  
  # Build the title
  if (mode == "all") {
    student_current_title <- glue(
      "Opbouw van de {tolower(research_settings[['succes_label']])} ({tolower(student_group)})"
    )
  } else if (mode == "group") {
    student_current_title <- glue(
      "Opbouw van de {tolower(research_settings[['succes_label']])} naar {tolower(student_group)}"
    )
  }  
  
  if (debug) {
    cli::cli_alert_info(student_current_title)
  }
  
  # Define the subtitle
  student_current_subtitle <- glue(
    " | {tolower(research_settings[['succes_label']])}: {retention}%"
  )
  
  if (mode == "all") {
    student_current_subtitle <- glue(
      "**{student_category}**",
      student_current_subtitle,
      " | _N_ = {subtotal}"
    )
  } else if (mode == "group") {
    student_current_subtitle <- glue(
      "{student_group}: **{student_category}**",
      student_current_subtitle,
      " | _N_ = {subtotal} van {total} ({percentage}%)"
    )
  }
  
  if (debug) {
    cli::cli_alert_info(student_current_subtitle)
  }
  
  list(student_current_title, student_current_subtitle)
}

# Function to create a breakdown plot (all)
get_breakdown_plot_all <- function(breakdown_lf_all, titles_list) {
  
  # Build the basic plot
  breakdown_plot <- suppressWarnings(plot(breakdown_lf_all, plot_distributions = TRUE)) 
  
  # Determine the y axis
  y_axis_list <- set_xy_axis(axis = "y")

  # Complete the plot based on the specific theme
  breakdown_plot <- breakdown_plot +

    # Themes
    set_theme() +

    # Define the title, subtitle and caption
    labs(
      title = titles_list[[1]],
      subtitle = titles_list[[2]],
      caption = caption,
      x = NULL,
      y = NULL
    )

  # Adjust the summary layer for the average probability (layer 3)
  breakdown_plot$layers[[3]] <- stat_summary(
    fun = mean,
    geom = "point",
    shape = 20,
    size = 3,
    color = colors_default[["negative_color"]],
    fill = colors_default[["negative_color"]]
  )

  # Complete the plot
  breakdown_plot <- breakdown_plot +

    # Adjust the y-axis labels
    scale_y_continuous(breaks = y_axis_list[["y_breaks"]],
                       labels = y_axis_list[["y_labels"]],
                       limits = c(0, 1)) +

    # Hide the legend
    theme(
      legend.position = "none"
    )

  # Add elements
  breakdown_plot <- add_theme_elements(breakdown_plot)
  
  breakdown_plot
  
}

# Function to create a waterfall plot
get_breakdown_plot <- function(df, titles) {
   
  # Determine the breaks for the x-axis (y-axis, but is tilted)
  y_axis_list  <- set_xy_axis(axis = "y")
  
  # Create a waterfall plot
  breakdown_plot <- ggplot(df) +
    
    # Add horizontal lines every 0.2
    geom_hline(
      yintercept = y_axis_list[["y_breaks"]],
      color = colors_default[["gridline_color"]],
      linetype = "solid",
      linewidth = 0.5
    ) +
    
    # Add a horizontal line at the lowest value
    geom_hline(
      yintercept = df$cumulative[df$position == 1],
      color = colors_default[["breakdown_intercept_color"]],
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
    color = colors_default[["breakdown_segment_color"]]) +
    
    # Flip the plot
    coord_flip() +
    
    # Define the title and subtitle
    labs(
      title = titles[[1]],
      subtitle = titles[[2]],
      caption = caption,
      x = NULL,
      y = NULL
    ) +
    
    # Fill in the colors
    scale_fill_manual(values = c(
      "Positief" = colors_default[["positive_color"]],
      "Negatief" = colors_default[["negative_color"]],
      "X" = colors_default[["positive_color"]]
    )) +
    
    # Add text labels for the variables
    geom_text(
      aes(x = position, y = label_position, label = label),
      hjust = -0.1,
      size = 4,
      color = colors_default[["text_color"]]
    ) +
    
    # Adjust the theme to display the y-axis labels
    scale_x_continuous(breaks = df$position, labels = df$variable) +
    scale_y_continuous(breaks = y_axis_list[["y_breaks"]],
                       labels = y_axis_list[["y_labels"]],
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
  
  breakdown_plot
  
}

# Function to create a Shapley plot
get_shapley_plot <- function(data) {
  
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
    scale_fill_manual(values = c("TRUE" = colors_default[["positive_color"]], 
                                 "FALSE" = colors_default[["negative_color"]])) +
    
    # Define the title and subtitle
    labs(
      title = "Shapley values",
      subtitle = "Bijdrage per variabele voor de meest voorkomende student",
      caption = caption,
      x = NULL,
      y = NULL
    )
  
  shapley_plot
  
}

# Function to create the ceteris paribus plot
get_ceteris_paribus_plot <- function(cp_lf_all, group) {
  
  # Determine the y axis
  y_axis_list <- set_xy_axis(axis = "y")
  
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
  color_values_lables <- get_color_values_lables(group, cp_lf_all)
  
  # Now build the plot further
  cp_plot <- cp_plot +

    # Add a single bowl for the fill
    scale_color_manual(
      name = group,
      values = color_values_lables$values,
      labels = color_values_lables$labels,
    ) +

    # Adjust the y-axis scale
    scale_y_continuous(breaks = y_axis_list[["y_breaks"]],
                       labels = y_axis_list[["y_labels"]],
                       limits = c(0, 1)) +

    # Customize the labels
    labs(title = "Ceteris-paribus profiel",
         subtitle = glue("{research_settings[['succes_label']]} ",
                         "voor de meest voorkomende studenten naar **{tolower(group)}**"),
         y = NULL,
         caption = caption) +

    # Apply the theme
    set_theme()
    
  # Add elements
  cp_plot <- add_theme_elements(cp_plot, 
                                title_subtitle = TRUE, 
                                extended = TRUE) +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    guides(colour = guide_legend(nrow = 1))
  
  # Return the plot
  cp_plot
  
}

# Function to create the ceteris paribus plot
get_partial_dependence_plot <- function(pdp_lf, 
                                        group = "all",
                                        show_profiles = TRUE) {
  
  # Determine the y axis
  y_axis_list <- set_xy_axis(axis = "y")
  
  # Define color scales for each variable
  if (group == "all") {
    .values <- colors_default[["metrics_blue"]]
  } else {
    .values <- color_list[[group]]
  }
  
  # Build the subtitle
  if (group == "all") {
    .subtitle <- glue("{research_settings[['succes_label']]}")
  } else {
    .subtitle <- glue("{research_settings[['succes_label']]} naar **{tolower(group)}**")
  }
  
  # Remove from pdp_lf[[“agr_profiles”]][[“_label_”]] the name of the model
  # so that the labels match the category names in the variables
  .model <- explain_lf$label
  pdp_lf[["agr_profiles"]][["_label_"]] <- gsub(paste0(.model, "_"),
                                                "",
                                                pdp_lf[["agr_profiles"]][["_label_"]])
  
  # Analyseer de gedeeltelijke afhankelijkheid
  if (show_profiles) {
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
    scale_y_continuous(breaks = y_axis_list[["y_breaks"]],
                       labels = y_axis_list[["y_labels"]],
                       limits = c(0, 1)) +
    
    # Customize the labels
    labs(title = "Partial Dependence profielen",
         subtitle = .subtitle,
         y = NULL,
         caption = caption) +
    
    # Apply the theme
    set_theme()
    
  # Add elements
  pdp_plot <- add_theme_elements(pdp_plot, title_subtitle = TRUE, extended = TRUE) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    guides(colour = guide_legend(nrow = 1))
  
  # Return the plot
  pdp_plot
  
}

# Function to create a density plot
get_density_plot <- function(fairness_object, group) {
  
  # Determine the x axis
  x_axis_list <- set_xy_axis(axis = "x")
  
  # Define color scales for each variable
  if (group == "all") {
    .values <- colors_default[["metrics_blue"]]
  } else {
    .values <- color_list[[group]]
  }
  
  # Create a density plot
  density_plot <- fairness_object |> 
    
    plot_density() +
    
    # Add title and subtitle
    labs(
      title = glue(
        "Verdeling en dichtheid van {tolower(research_settings[['succes_label']])}"
      ),
      subtitle = glue("Naar **{group}**"),
      caption = caption,
      x = NULL,
      y = NULL
    )
    
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
    scale_x_continuous(breaks = x_axis_list[["x_breaks"]],
                       labels = x_axis_list[["x_labels"]],
                       limits = c(0, 1)) +
    
    # Add a line on the 50% labeled “50%”
    geom_vline(xintercept = 0.5,
               linetype = "dotted",
               color = colors_default[["positive_color"]]) +
    
    # Add the label “50%”
    annotate(
      "text",
      x = 0.53,
      y = 0.5,
      label = "50%",
      vjust = -0.3,
      color = colors_default[["positive_color"]]
    ) +
    
    # Apply the theme
    set_theme()  +
    
    # Customize some theme elements
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "none",
      strip.text = element_blank()
    )
  
  # Add elements.
  density_plot <- add_theme_elements(density_plot,
                                     title_subtitle = TRUE) +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    guides(fill = guide_legend(nrow = 1))

  
  density_plot
  
}

# Function to create a fairness plot
get_fairness_plot <- function(fairness_object, group, privileged) {
  
  # Determine the y axis
  y_breaks <- seq(-100, 100, by = 0.2)
  
  # Create a fairness plot
  fairness_plot <- fairness_object |>
    plot() +
    theme_minimal() +
    set_theme() +
    
    # Add title and subtitle
    labs(
      title = "Fairness check",
      subtitle = glue(
        "Fairness van het model voor **{group}** ",
        "ten opzichte van **{privileged}**"
      ),
      caption = caption,
      x = NULL,
      y = NULL
    )
  
  # Remove the existing color scale,
  # so there is no warning about the existing color scale
  fairness_plot$scales$scales <- list()
  
  # Build the plot further
  fairness_plot <- fairness_plot +
    
    # Define the color
    scale_fill_manual(values = c(colors_default[["positive_color"]])) +
    
    # Adjust the y-axis scale
    scale_y_continuous(breaks = y_breaks)
  
  # Add elements.
  fairness_plot <- add_theme_elements(fairness_plot, title_subtitle = TRUE) +
    
    # Customize some theme elements
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "none",
      strip.text = element_text(hjust = 0)
    )
  
  fairness_plot
  
}

# Based on the bbplot package are built left_align ensave_plot
# (hence the names in lowercase, so that these functions override those of the bbplot package)

# Left align
left_align <- function(plot_name, pieces) {
  grob <- ggplot2::ggplotGrob(plot_name)
  n <- length(pieces)
  grob$layout$l[grob$layout$name %in% pieces] <- 2
  grob
}

# Save plot
save_plot <- function(plot_grid, width, height, save_filepath) {
  
  ggplot2::ggsave(
    filename = save_filepath,
    plot = plot_grid,
    width = (width / 72),
    height = (height / 72),
    bg = colors_default[["background_color"]],
    device = ragg::agg_png,
    res = 300,
    create.dir = TRUE
  )
}

# Save a plot
finalize_plot <- function(plot_name,
                          source_name,
                          save_filepath = file.path(Sys.getenv("TMPDIR"), "tmp-nc.png"),
                          width_pixels = plot_width,
                          height_pixels = plot_height,
                          show_plot = FALSE) {
  # Print de plot
  plot_grid <- ggpubr::ggarrange(
    plot_name,
    ncol = 1,
    nrow = 2,
    heights = c(1, 0 / (height_pixels / 450))
  )
  save_plot(plot_grid, width_pixels, height_pixels, save_filepath)
  
  # If the plot must be shown, show it
  if (show_plot) {
    invisible(plot_grid)
  }
}

# . ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 8. ADDITIONAL AID FUNCTIONS ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function convert numbers to readable notation
change_number_marks <- function(x, digits = 0) {
  formatC(
    round(x, digits),
    format = "f",
    digits = digits,
    big.mark = ".",
    decimal.mark = ","
  )
}

# Retrieve the version name of the dataset
get_dataset <- function(df) {
  unique(df$LTA_Dataset)
}

# Function to determine the metadata of the analysis
get_metadata <- function() {
  
  metadata <- list(
    "data_provider_label"    = "Bron",
    "dataset_label"          = "Dataset",
    "plot_label"             = "Plot",
    "analysis_label"         = "Analyse",
    "institution"            = research_settings[["institution"]],
    "data_provider"          = research_settings[["data_provider"]],
    "dataset"                = research_settings[["dataset"]],
    "sp"                     = research_settings[["sp"]],
    "analysis"               = research_settings[["analysis"]]
  )
  
  metadata
}

# Function to concatenate a list of strings
concatenate_list <- function(l, 
                             lang = "nl", 
                             extend = TRUE, 
                             tolower = FALSE, 
                             backtick = FALSE) {
  
  if (tolower) {
    l <- tolower(l)
  }
  if (backtick) {
    l <- backtick(l)
  }
  if (lang == "en") {
    last <- ", and "
  } else if (lang == "nl") {
    last <- " en "
  } else {
    last <- ", "
  }
  
  if (extend) {
    collapse <- glue_collapse(l, sep = ", ", last = last)
  } else {
    collapse <- glue_collapse(l, sep = ", ")
  }
  
  collapse
  
}

# Function to add a colored square
get_colored_square <- function(color, bordercolor = "darkgrey", size = 12) {
  sprintf(
    glue('<span style="display:inline-block; width:%dpx; height:%dpx; ',
         'background-color:%s; border:1px solid %s;"></span>'), 
    size, size, color, bordercolor
  )
}
