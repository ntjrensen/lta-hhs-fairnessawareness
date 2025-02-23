# Select the best model
sBest_model <- dfModel_results$model[dfModel_results$best == TRUE]
last_fit    <- lLast_fits[[sBest_model]]

fitted_model <- last_fit |>
  extract_fit_parsnip()

# If the model is logistic regression, check that the coefficients of the model are numerical
if(sBest_model == "Logistic Regression") {
  
  coefs <- tidy(fitted_model)$estimate
  
  # Check that the coefficients are numerical
  if (!is.numeric(coefs)) {
    stop("De geëxtraheerde coëfficiënten zijn niet numeriek.")
  }
  
}
