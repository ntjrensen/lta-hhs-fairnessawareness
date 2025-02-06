# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# fairness.helpers.R ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R code of the Learning Technology & Analytics Research Group of THUAS
# Copyright 2025 THUAS
# Web Page: http://www.thuas.com
# Contact: Theo Bakker (t.c.bakker@hhs.nl)
# Distribution outside THUAS: Yes
#
# Purpose: Custom functions from the fairness package, to better display in html.
#
# Dependencies: None
#
# Datasets: None
#
# Remarks:
# 1) None.
# 2) ___
#
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Version history:
# 01-05-2024: TB: File creation
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Enclose the original help functions from the fairness package
 
source("R/functions/fairness.dalex.helpers.R")

library(purrr)

# Function to display the outcomes of a fairness object
Print_Fairness_Object_LTA <- function(x,
                                      ...,
                                      colorize = TRUE,
                                      fairness_metrics = c("ACC", "TPR", "PPV", "FPR", "STP"),
                                      fair_level = NULL,
                                      border_width = 1,
                                      loss_aggregating_function = NULL,
                                      mode = "html",
                                      show_header = FALSE,
                                      show_warning = FALSE) {
  
  # Determine the color codes if colorize = FALSE
  if (!colorize) {
    color_codes <- list(
      yellow_start = "",
      yellow_end = "",
      red_start = "",
      red_end = "",
      green_start = "",
      green_end = ""
    )
  }
  
  # Adjust the color codes if the mode is html
  if (mode == "html") {
    color_codes <- list(
      yellow_start = "<span class = metrics-orange>",
      yellow_end = "</span>",
      red_start = "<span class = metrics-red>",
      red_end = "</span>",
      green_start = "<span class = metrics-green>",
      green_end = "</span>"
    )
  }
  
  if (is.null(fair_level))
    fair_level <- length((fairness_metrics))
  unfair_level <- fair_level - border_width - 1
  
  stopifnot(border_width >= 0)
  stopifnot(is.numeric(border_width))
  stopifnot(is.numeric(fair_level))
  stopifnot(fair_level >= border_width)
  stopifnot(length(fairness_metrics) >= fair_level)
  
  if (!is.null(loss_aggregating_function)) {
    stopifnot(is.function(loss_aggregating_function))
  }
  
  # Determine the data and metrics
  data <- x$fairness_check_data
  
  models <- unique(data$model)
  epsilon <- x$epsilon
  metrics <- unique(data$metric)
  
  # Filter the data by the fairness metrics
  filtered <- filter_fairness_check_metrics(data, metrics, fairness_metrics)
  
  data <- filtered$data
  metrics <- filtered$metrics
  
  # Check for NAs in the data
  if (any(is.na(data$score)) & show_warning) {
    warning(
      "NA's zijn weggelaten voor: ",
      paste(unique(data[is.na(data$score), "model"]), collapse = ", "),
      "\nInformatie over doorgegeven maatstaven kan onnauwkeurig zijn door aanwezige NA's; het is raadzaam om de metric_scores plot te controleren.\n"
    )
  }
  
  # Determine the loss aggregating function
  if (is.null(loss_aggregating_function)) {
    loss_aggregating_function <- function(x) {
      return(sum(abs(na.omit(x) - 1)))
    }
    function_name <- "<br/>**Totaal verlies**"
  } else {
    function_name <- "<br/>**Aangepast verlies**"
  }
  
  # Print the results
  if(mode == "html" & show_header) {
    cat("<br/>Fairness check voor:",
        paste(models, collapse = ", "),
        "<br/>")
  }
  else if(mode == "console" & show_header) {
    cat("\nFairness check voor:",
        paste(models, collapse = ", "),
        "\n")
  }
  
  # Use purrr::walk instead of for-loop
  purrr::walk(models, function(model) {
    model_data <- data[data$model == model, ]
    
    failed_metrics <- unique(model_data[na.omit(model_data$score) < epsilon |
                                          na.omit(model_data$score) > 1 / epsilon, "metric"])
    passed_metrics <- length(metrics[!metrics %in% failed_metrics])
    
    # Values at unfair
    if (passed_metrics <= unfair_level) {
      cat(
        "\n",
        color_codes$red_start,
        "Prognosemodel (", 
        model,
        ") niet geslaagd: ",
        passed_metrics,
        " van ",
        as.character(length(fairness_metrics)),
        " maatstaven\n",
        color_codes$red_end,
        sep = ""
      )
    }
    
    # Values between unfair and fair
    if (passed_metrics > unfair_level & passed_metrics < fair_level) {
      cat(
        "\n",
        color_codes$yellow_start,
        "Prognosemodel (", 
        model,
        ") deels geslaagd: ",
        passed_metrics,
        " van ",
        as.character(length(fairness_metrics)),
        " maatstaven\n",
        color_codes$yellow_end,
        sep = ""
      )
    }
    
    # Values at fair
    if (passed_metrics >= fair_level) {
      cat(
        "\n",
        color_codes$green_start,
        "Prognosemodel (", 
        model,
        ") geslaagd: ",
        passed_metrics,
        " van ",
        as.character(length(fairness_metrics)),
        " maatstaven\n",
        color_codes$green_end,
        sep = ""
      )
    }
    
    # De verlies-aggregatiefunctie afdrukken
    cat(function_name,
        ": ",
        round(loss_aggregating_function(data[data$model == model, "score"]), 2),
        "\n")
  })
  
  cat("\n")
  return(invisible(NULL))
}

# Function to display the outcomes of a fairness object
Print_Fairness_Object_LTA_oud <- function(x,
                                  ...,
                                  colorize = TRUE,
                                  fairness_metrics = c("ACC", "TPR", "PPV", "FPR", "STP"),
                                  fair_level = NULL,
                                  border_width = 1,
                                  loss_aggregating_function = NULL,
                                  mode = "html",
                                  show_header = FALSE,
                                  show_warning = FALSE) {
  
  # Determine the color codes if colorize = FALSE
  if (!colorize) {
    color_codes <- list(
      yellow_start = "",
      yellow_end = "",
      red_start = "",
      red_end = "",
      green_start = "",
      green_end = ""
    )
  }
  
  # Adjust the color codes if the mode is html
  if (mode == "html") {
    color_codes <- list(
      yellow_start = "<span class = metrics-orange>",
      yellow_end = "</span>",
      red_start = "<span class = metrics-red>",
      red_end = "</span>",
      green_start = "<span class = metrics-green>",
      green_end = "</span>"
    )
  }
  
  if (is.null(fair_level))
    fair_level <- length((fairness_metrics))
  unfair_level <- fair_level - border_width - 1
  
  stopifnot(border_width >= 0)
  stopifnot(is.numeric(border_width))
  stopifnot(is.numeric(fair_level))
  stopifnot(fair_level >= border_width)
  stopifnot(length(fairness_metrics) >= fair_level)
  
  if (!is.null(loss_aggregating_function)) {
    stopifnot(is.function(loss_aggregating_function))
  }
  
  # Determine the data and metrics
  data <- x$fairness_check_data
  
  models  <- unique(data$model)
  epsilon <- x$epsilon
  metrics <- unique(data$metric)
  
  # Filter the data by the fairness metrics
  filtered <- filter_fairness_check_metrics(data, metrics, fairness_metrics)
  
  data <- filtered$data
  metrics <- filtered$metrics
  
  # Check for NAs in the data
  if (any(is.na(data$score)) & show_warning) {
    warning(
      "NA's zijn weggelaten voor: ",
      paste(unique(data[is.na(data$score), "model"]), collapse = ", "),
      "\nInformatie over doorgegeven maatstaven kan onnauwkeurig zijn door aanwezige NA's; het is raadzaam om de metric_scores plot te controleren.\n"
    )
  }
  
  # Determine the loss aggregating function
  if (is.null(loss_aggregating_function)) {
    loss_aggregating_function <- function(x) {
      return(sum(abs(na.omit(x) - 1)))
    }
    function_name <- "<br/>**Totaal verlies**"
  } else {
    function_name <- "<br/>**Aangepast verlies**"
  }
  
  # Print the results
  if(mode == "html" & show_header) {
    cat("<br/>Fairness check voor:",
        paste(models, collapse = ", "),
        "<br/>")
  }
  else if(mode == "console" & show_header) {
    cat("\nFairness check voor:",
        paste(models, collapse = ", "),
        "\n")
  }
  
  for (model in models) {
    model_data <- data[data$model == model, ]
    
    failed_metrics <- unique(model_data[na.omit(model_data$score) < epsilon |
                                          na.omit(model_data$score) > 1 / epsilon, "metric"])
    passed_metrics <- length(metrics[!metrics %in% failed_metrics])
    
    # Values at unfair
    if (passed_metrics <= unfair_level) {
      cat(
        "\n",
        color_codes$red_start,
        "Prognosemodel (", 
        model,
        ") niet geslaagd: ",
        passed_metrics,
        " van ",
        as.character(length((
          fairness_metrics
        ))),
        " maatstaven\n",
        color_codes$red_end,
        sep = ""
      )
    }
    
    # Values between unfair and fair
    if (passed_metrics > unfair_level &
        passed_metrics < fair_level) {
      cat(
        "\n",
        color_codes$yellow_start,
        "Prognosemodel (", 
        model,
        ") deels geslaagd: ",
        passed_metrics,
        " van ",
        as.character(length((
          fairness_metrics
        ))),
        " maatstaven\n",
        color_codes$yellow_end,
        sep = ""
      )
    }
    
    # Values at fair
    if (passed_metrics >= fair_level) {
      cat(
        "\n",
        color_codes$green_start,
        "Prognosemodel (", 
        model,
        ") geslaagd: ",
        passed_metrics,
        " van ",
        as.character(length((
          fairness_metrics
        ))),
        " maatstaven\n",
        color_codes$green_end,
        sep = ""
      )
    }
    
    # Printing the loss aggregation function
    cat(function_name,
        ": ",
        round(loss_aggregating_function(data[data$model == model, "score"]), 2),
        "\n")
  }
  
  cat("\n")
  return(invisible(NULL))
}
