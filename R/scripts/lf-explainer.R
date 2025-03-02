# Extract the fitted model
fitted_model <- last_fit |>
  extract_fit_parsnip()

# Extract the workflow
workflow <- last_fit |>
  extract_workflow()

# Create an explainer
explain_lf <- DALEX::explain(model = workflow,
                             data = df_sp_enrollments |> select(-Retentie),
                             y = df_sp_enrollments$Retentie,
                             colorize = TRUE,
                             verbose = TRUE,
                             label = best_model)

if (is.null(explain_lf$y_hat) || is.null(explain_lf$residuals)) {
  cli::cli_alert_danger(glue::glue(
    "The explainer does not contain the correct results. ",
    "Check the installation of model packages: ",
    "{glue::glue_collapse(explain_lf$model_info$package, sep = ', ')}"
  ))
  stop("Solve this problem first")
}
