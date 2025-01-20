<!-- MODEL 0: Bayes -->
  
  ::: {.content-hidden unless-meta="includes.model_bayes"}
# Model I: Bayes

## Maak het model

We bouwen eerst het model.

We bouwen eerst het model.

```{r}
#| label: bayes_mod
#| code-fold: false

## Bouw het model: logistische regressie
bayes_mod <- 
  naive_Bayes(Laplace = 1)

```

## Maak de recipe

```{r}
#| label: bayes_recipe
#| code-fold: false

## Bouw de recipe: bayes
bayes_recipe <- 
  recipe(Uitval ~ ., data = dfUitval_train) |>  
  update_role(ID, new_role = "ID") |>           ## Zet de student ID als ID variabele
  step_rm(ID, Collegejaar) |>                   ## Verwijder ID en collegejaar uit het model
  step_dummy(all_nominal_predictors()) |>       ## Converteer factoren naar dummy variabelen
  step_zv(all_predictors()) |>                  ## Verwijder zero values
  step_normalize(all_numeric_predictors())      ## Centreer en schaal numerieke variabelen

## Toon de recipe
tidy(bayes_recipe) |> 
  knitr::kable()
```

## Maak de workflow

We voegen het model en de recipe toe aan de workflow voor dit model.

```{r}
#| label: bayes_workflow
#| code-fold: false

## Maak de workflow: bayes
bayes_workflow <- 
  workflow() |> 
  add_model(bayes_mod) |>  ## Voeg het model toe
  add_recipe(bayes_recipe) 

cls_met <- metric_set(roc_auc, brier_class)

# We'll save the out-of-sample predictions to visualize them. 
ctrl <- control_resamples(save_pred = TRUE)

```

## Train het model

We trainen het model en evalueren de resultaten.

```{r}
#| label: bayes_res
#| code-fold: false

## Train en tune het model: logistische regressie
bayes_res <-
  bayes_workflow |> 
  fit_resamples(dfUitval_resamples, metrics = cls_met, control = ctrl)

collect_metrics(bayes_res)

```

## Calibreer het model

```{r}
#| label: bayes_plot
#| code-fold: false

## Plot de resultaten
bayes_res |> 
  collect_predictions()  |>
  ggplot(aes(.pred_FALSE)) +
  geom_histogram(col = "white", bins = 40) +
  facet_wrap(~ Uitval, ncol = 1) +
  geom_rug(col = "blue", alpha = 1 / 2) + 
  labs(x = "Probability Estimate van Uitval")

```

```{r}
#| label: bayes_cal_plot

cal_plot_breaks(bayes_res)
cal_plot_windowed(bayes_res, step_size = 0.025)
cal_plot_logistic(bayes_res)
```

```{r}
#| label: bayes_logit_val

logit_val <- cal_validate_logistic(bayes_res, metrics = cls_met, save_pred = TRUE)
collect_metrics(logit_val)

collect_predictions(logit_val) %>%
  filter(.type == "calibrated") %>%
  cal_plot_windowed(truth = Uitval, estimate = .pred_FALSE, step_size = 0.025) +
  ggtitle("Logistic calibration via GAM")

```

```{r}
#| label: bayes_iso_val

set.seed(2050)

iso_val <- cal_validate_isotonic_boot(bayes_res, metrics = cls_met, 
                                      save_pred = TRUE, times = 25)
collect_metrics(iso_val)

collect_predictions(iso_val) %>%
  filter(.type == "calibrated") %>%
  cal_plot_windowed(truth = Uitval, estimate = .pred_FALSE, step_size = 0.025) +
  ggtitle("Isotonic regression calibration")

```

```{r}
#| label: bayes_beta_val

set.seed(2050)

beta_val <- cal_validate_beta(bayes_res, metrics = cls_met, save_pred = TRUE)

collect_metrics(beta_val)

collect_predictions(beta_val) %>%
  filter(.type == "calibrated") %>%
  cal_plot_windowed(truth = Uitval, estimate = .pred_FALSE, step_size = 0.025) +
  ggtitle("Beta calibration")

```

```{r}
dfUitval_cal  <- cal_estimate_beta(bayes_res)
bayes_fit     <- bayes_workflow |>
  fit(data = dfUitval_train)

```
```{r}
#| label: bayes_test_pred

dfUitval_test_pred <- augment(bayes_fit, new_data = dfUitval_test)
dfUitval_test_pred  |>  
  cls_met(Uitval, .pred_FALSE)

dfUitval_test_cal_pred <-
  dfUitval_test_pred  |> 
  cal_apply(dfUitval_cal)
dfUitval_test_cal_pred |>  
  dplyr::select(Uitval, starts_with(".pred_"))

dfUitval_test_cal_pred |> 
  cls_met(Uitval, .pred_FALSE)

dfUitval_test_cal_pred %>%
  cal_plot_windowed(truth = Uitval, estimate = .pred_FALSE, step_size = 0.025)
```

