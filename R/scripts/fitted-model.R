# Select the best model
best_model <- df_model_results$model[df_model_results$best == TRUE]
last_fit    <- last_fits[[best_model]]

fitted_model <- last_fit |>
  extract_fit_parsnip()

# If the model is logistic regression, check that the coefficients of the model are numerical
if (best_model == "Logistic Regression") {
  
  coefs <- tidy(fitted_model)$estimate
  
  # Check that the coefficients are numerical
  if (!is.numeric(coefs)) {
    stop("De geëxtraheerde coëfficiënten zijn niet numeriek.")
  }
  
}
