<!-- MODEL III: SVM -->
  
  ::: {.content-hidden unless-meta="includes.model_svm"}
# Model III: Support Vector Machine

-   Het derde model is een [Support Vector Machine](https://en.wikipedia.org/wiki/Support_vector_machine) (SVM). Het is een krachtig model dat goed om kan gaan met complexe data en veel variabelen. We gebruiken de `kernlab` engine voor het bouwen van het model.
-   We gebruiken de Area under the ROC Curve (AUC/ROC) als performance metric.

## Maak het model

We bouwen eerst het model.

```{r}
#| label: svm_mod
#| code-fold: false

## Bouw het model: svm
svm_mod <- 
  svm_poly(cost = tune(), degree = tune()) |>
  set_engine("kernlab") |>
  set_mode("classification")
```

## Maak de recipe

Vervolgens zetten we meerdere stappen in een 'recipe'. We verwijderen de student ID en het collegejaar uit de data, omdat deze niet moet worden gebruikt in het model. We converteren factoren naar dummy variabelen, verwijderen zero values en centreren en schalen numerieke variabelen.

```{r}
#| label: svm_recipe
#| code-fold: false

## Bouw de recipe: svm
svm_recipe <- 
  recipe(Uitval ~ ., data = dfUitval_train) |>  
  update_role(ID, new_role = "ID") |>           ## Zet de student ID als ID variabele
  step_rm(ID, Collegejaar) |>                   ## Verwijder ID en collegejaar uit het model
  step_dummy(all_nominal_predictors()) |>       ## Converteer factoren naar dummy variabelen
  step_zv(all_predictors()) |>                  ## Verwijder zero values
  step_normalize(all_numeric_predictors())      ## Centreer en schaal numerieke variabelen

## Toon de recipe
tidy(svm_recipe) |> 
  knitr::kable()
```

## Maak de workflow

Voor de uitvoering bouwen we een nieuwe workflow. Daaraan voegen we het model en de bewerkingen in de recipe toe.

```{r}
#| label: svm_workflow
#| code-fold: false

## Maak de workflow: svm
svm_workflow <- 
  workflow() |>         ## Maak een workflow
  add_model(svm_mod) |>  ## Voeg het model toe
  add_recipe(svm_recipe) ## Voeg de recipe toe

## Toon de workflow
svm_workflow
```

## Tune en train het model

Het model moet getuned worden. Dit houdt in dat we de beste parameters voor het model moeten vinden. We maken een grid met verschillende penalty waarden. Daarmee kunnen we vervolgens het beste model selecteren met de hoogste ROC/AUC. We plotten de resultaten van de tuning, zodat we hieruit het beste model kunnen kiezen.

```{r}
#| label: svm_reg_grid
#| code-fold: false

## Maak een grid: svm
svm_reg_grid <- grid_regular(cost(),
                             degree(range = c(1,3)),
                             levels = c(cost = 5, degree = 3))

head(svm_reg_grid, 5) |>
  knitr::kable()

## Maak een cluster vooor parallel processing
doParallel::registerDoParallel()
set.seed(2111)

## Train en tune het model: logistische regressie
svm_res <- 
  svm_workflow %>% 
  tune_grid(
    resamples = dfUitval_resamples,
    control = control_grid(save_pred = TRUE),
    grid = svm_reg_grid
  )

## Stop het cluster
stopCluster()

saveRDS(svm_res, file = "10_Output/modelresults/svm_initial_tune.rds")

autoplot(svm_res)
```

```{r}
#| label: svm_plot

## Plot de resultaten
svm_plot <- 
  svm_res |> 
  collect_metrics() |> 
  ggplot(aes(x = cost, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

svm_plot 
```

## Kies het beste model

We evalueren modellen met een zo hoog mogelijke Area under the ROC Curve (AUC/ROC) en een zo laag mogelijke cost. Zo kunnen we uit de resultaten het beste model kiezen. Tot slot maken we een ROC curve om de prestaties van het model te visualiseren.

```{r}
#| label: svm_top_models
#| code-fold: false

## Toon het beste model
top_models <-
  svm_res |> 
  show_best(metric = "roc_auc", n = 10) |> 
  mutate(mean = round(mean, 4)) |>
  arrange(cost) 

top_models|> 
  knitr::kable()

```

```{r}
#| label: svm_best
#| code-fold: false

## Selecteer het beste model: svm
svm_best <- 
  svm_res |> 
  collect_metrics() |> 
  arrange(cost) |> 
  slice(1) 

svm_best|> 
  mutate(mean = round(mean, 4)) |>
  knitr::kable()

```

```{r}
#| label: svm_auc
#| code-fold: false

## Verzamel de predicties en evalueer het model (AUC/ROC): svm
svm_auc <- 
  svm_res |> 
  collect_predictions(parameters = svm_best) |> 
  roc_curve(Uitval, .pred_FALSE) |> 
  mutate(model = "Logistisch Regressie")

autoplot(svm_auc) 

## Bepaal de AUC van het beste model
svm_auc_highest   <-
  svm_res |>
  collect_predictions(parameters = svm_best) |> 
  roc_auc(Uitval, .pred_FALSE)

## Voeg de naam van het model en de AUC toe dfModel_results
dfModel_results <- 
  dfModel_results |>
  add_row(model = "Support Vector Machin", auc = svm_auc_highest$.estimate)

```
:::
  
  
