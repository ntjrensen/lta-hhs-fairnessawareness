# Model I: Penalized Logistic Regression

-   Het eerste model is een [logistische regressie met penalized likelihood](https://wikistatistiek.amc.nl/Logistische_regressie); we gebruiken de `glmnet` engine voor het bouwen van het model. Penalized likelihood is een techniek die helpt bij het voorkomen van overfitting. [Glmnet](https://glmnet.stanford.edu/articles/glmnet.html) is een populair package voor het bouwen van logistische regressiemodellen.
-   We gebruiken de Area under the ROC Curve (AUC/ROC) als performance metric.

## Maak het model

We bouwen eerst het model.

```{r}
#| code-fold: false

## Bouw het model: logistische regressie
lr_mod <- 
  logistic_reg(penalty = tune(), mixture = 1) |> 
  set_engine("glmnet")
```

## Maak de recipe

Vervolgens zetten we meerdere stappen in een 'recipe'. We verwijderen de student ID en het collegejaar uit de data, omdat deze niet moet worden gebruikt in het model. We converteren factoren naar dummy variabelen, verwijderen zero values en centreren en schalen numerieke variabelen.

```{r}
#| code-fold: false

## Bouw de recipe: logistische regressie
lr_recipe <- 
  recipe(Uitval ~ ., data = dfUitval_train) |>  
  update_role(ID, new_role = "ID") |>           ## Zet de student ID als ID variabele
  step_rm(ID, Collegejaar) |>                   ## Verwijder ID en collegejaar uit het model
  step_dummy(all_nominal_predictors()) |>       ## Converteer factoren naar dummy variabelen
  step_zv(all_predictors()) |>                  ## Verwijder zero values
  step_normalize(all_numeric_predictors())      ## Centreer en schaal numerieke variabelen

## Toon de recipe
tidy(lr_recipe) |> 
  knitr::kable()
```

## Maak de workflow

Voor de uitvoering bouwen we een nieuwe workflow. Daaraan voegen we het model en de bewerkingen in de recipe toe.

```{r}
#| code-fold: false

## Maak de workflow: logistische regressie
lr_workflow <- 
  workflow() |>         ## Maak een workflow
  add_model(lr_mod) |>  ## Voeg het model toe
  add_recipe(lr_recipe) ## Voeg de recipe toe

## Toon de workflow
lr_workflow
```

## Tune en train het model

Het model moet getuned worden. Dit houdt in dat we de beste parameters voor het model moeten vinden. We maken een grid met verschillende penalty waarden. Daarmee kunnen we vervolgens het beste model selecteren met de hoogste ROC/AUC. We plotten de resultaten van de tuning, zodat we hieruit het beste model kunnen kiezen.

```{r}
#| code-fold: false

## Maak een grid: logistische regressie
lr_reg_grid <- tibble(penalty = 10 ^ seq(-4, -1, length.out = 30))

## Train en tune het model: logistische regressie
lr_res <- 
  lr_workflow |> 
  tune_grid(dfUitval_validation,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

```

```{r}

## Plot de resultaten
lr_plot <- 
  lr_res |> 
  collect_metrics() |> 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

lr_plot 
```

## Kies het beste model

We evalueren modellen met een zo hoog mogelijke Area under the ROC Curve (AUC/ROC) en een zo laag mogelijke penalty. Zo kunnen we uit de resultaten het beste model kiezen. Tot slot maken we een ROC curve om de prestaties van het model te visualiseren.

```{r}
#| code-fold: false

## Toon het beste model
top_models <-
  lr_res |> 
  show_best(metric = "roc_auc", n = 10) |> 
  mutate(mean = round(mean, 4)) |>
  arrange(penalty) 

top_models|> 
  knitr::kable()

```

```{r}
#| code-fold: false

## Selecteer het beste model: logistische regressie
lr_best <- 
  lr_res |> 
  collect_metrics() |> 
  arrange(penalty) |> 
  slice(1) ## Hier stond eerst slice(12)

lr_best|> 
  mutate(mean = round(mean, 4)) |>
  knitr::kable()

```

```{r}
#| code-fold: false

## Verzamel de predicties en evalueer het model (AUC/ROC): logistische regressie
lr_auc <- 
  lr_res |> 
  collect_predictions(parameters = lr_best) |> 
  roc_curve(Uitval, .pred_FALSE) |> 
  mutate(model = "Logistic Regression")

autoplot(lr_auc)
```

# Model II: Tree-based ensemble

-   Het tweede model is een [random forest](https://en.wikipedia.org/wiki/Random_forest): een ensemble van beslisbomen (decision trees). Het is een krachtig model dat goed om kan gaan met complexe data en veel variabelen.
-   We gebruiken de `ranger` engine voor het bouwen van het model.

## Bepaal het aantal PC-cores

Omdat een random forest model veel berekeningen vereist, willen we daarvoor alle computerkracht gebruiken die beschikbaar is. Het aantal CPU's (*cores*) van de computer bepaalt hoe snel het model getraind kan worden. Deze informatie gebruiken we bij het bouwen van het model.

```{r}

## Bepaal het aantal cores
cores <- parallel::detectCores()

```

## Maak het model

We bouwen eerst het model. We gebruiken de `rand_forest` functie om het model te bouwen. We tunen de `mtry` en `min_n` parameters. De `mtry` parameter bepaalt het aantal variabelen dat per boom wordt gebruikt. De `min_n` parameter bepaalt het minimum aantal observaties dat in een blad van de boom moet zitten. De functie `tune()` is hier nog een *placeholder* om de beste waarden voor deze parameters - die we later bepalen - daar in te stellen. We gebruiken 1.000 bomen c.q. versies van het model.

```{r}
#| code-fold: false

## Bouw het model: random forest

rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) |> 
  set_engine("ranger", num.threads = cores) |> 
  set_mode("classification")
```

## Maak de recipe

We maken een recipe voor het random forest model. We verwijderen de student ID en het collegejaar uit de data, omdat deze niet moet worden gebruikt in het model. Overige stappen zijn bij een random forest minder relevant in tegenstelling tot een regressiemodel.

```{r}
#| code-fold: false

## Maak de recipe: random forest
rf_recipe <- 
  recipe(Uitval ~ ., data = dfUitval_train) |> 
  step_rm(ID, Collegejaar)                      ## Verwijder ID en Collegejaar uit het model
  
## Toon de recipe
tidy(rf_recipe) |> 
  knitr::kable()
```

## Maak de workflow

We voegen het model en de recipe toe aan de workflow voor dit model.

```{r}
#| code-fold: false

## Maak de workflow: random forest
rf_workflow <- 
  workflow() |> 
  add_model(rf_mod) |> 
  add_recipe(rf_recipe)

## Toon de workflow
rf_workflow
```

## Tune en train het model

We trainen en tunen het model in de workflow. We maken een grid met verschillende waarden voor de parameters `mtry` en `min_n`. We gebruiken de Area under the ROC Curve (AUC/ROC) als performance metric. Met de resultaten van de tuning kiezen we het beste model.

```{r}
#| code-fold: false

## Toon de parameters die getuned kunnen worden
rf_mod

## Extraheer de parameters die getuned worden
extract_parameter_set_dials(rf_mod)

## Bepaal de seed
set.seed(2904)

## Bouw het grid: random forest
rf_res <- 
  rf_workflow |> 
  tune_grid(dfUitval_validation,
            grid = 25,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))
```

## Kies het beste model

We evalueren de beste modellen en maken een ROC curve om de performance van het model te visualiseren. Vervolgens vergelijken we de prestaties van de modellen en kiezen we het beste model.

```{r}
#| code-fold: false

## Toon de beste modellen
rf_res |> 
  show_best(metric = "roc_auc", n = 15) |> 
  mutate(mean = round(mean, 4)) |>
  knitr::kable()

## Plot de resultaten
autoplot(rf_res)

```

```{r}
#| code-fold: false

## Selecteer het beste model
rf_best <- 
  rf_res |> 
  select_best(metric = "roc_auc")

rf_best|> 
  knitr::kable()

```

```{r}
#| code-fold: true

## Verzamel de predicties
rf_res |> 
  collect_predictions() |> 
  head(10) |>
  knitr::kable()

## Bepaal de AUC/ROC curve
rf_auc <- 
  rf_res |> 
  collect_predictions(parameters = rf_best) |> 
  roc_curve(Uitval, .pred_FALSE) |> 
  mutate(model = "Random Forest")
```
