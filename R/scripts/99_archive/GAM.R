<!-- MODEL II: Generalized Additive Model -->
  
  ::: {.content-hidden unless-meta="includes.model_gam"}
# Model II: Generalized Additive Model

-   Het tweede models is een [Generalized Additive Model (GAM)](https://en.wikipedia.org/wiki/Generalized_additive_model). GAM is een generalisatie van de lineaire regressie, waarbij de afhankelijke variabele wordt gemodelleerd als een functie van de onafhankelijke variabelen. GAM is een flexibel model dat niet-lineaire relaties kan modelleren. Dit kan gunstig zijn voor variabelen die niet-lineair correleren met de uitkomstvariabele, zoals de leeftijd en de reistijd.
-   We gebruiken de Area under the ROC Curve (AUC/ROC) als performance metric.

## Maak het model

We bouwen eerst het model.

```{r}
#| label: gam_mod
#| code-fold: false

## Bouw het model: gam
gam_mod <- 
  gen_additive_mod(adjust_deg_free = numeric(1), select_features = logical(1)) %>% 
  set_engine("mgcv") %>% 
  set_mode("regression") 
```

## Maak de recipe

Vervolgens zetten we meerdere stappen in een 'recipe'. We verwijderen de student ID en het collegejaar uit de data, omdat deze niet moet worden gebruikt in het model. We converteren factoren naar dummy variabelen, verwijderen zero values en centreren en schalen numerieke variabelen.

```{r}
#| label: gam_recipe
#| code-fold: false

## Bouw de recipe: gam
gam_recipe <- 
  recipe(Uitval ~ ., data = dfUitval_train) |>  
  update_role(ID, new_role = "ID") |>           ## Zet de student ID als ID variabele
  step_rm(ID, Collegejaar) |>                   ## Verwijder ID en collegejaar uit het model
  step_dummy(all_nominal_predictors()) |>       ## Converteer factoren naar dummy variabelen
  step_zv(all_predictors()) |>                  ## Verwijder zero values
  step_normalize(all_numeric_predictors())      ## Centreer en schaal numerieke variabelen

## Toon de recipe
tidy(gam_recipe) |> 
  knitr::kable()
```

## Maak de workflow

Voor de uitvoering bouwen we een nieuwe workflow. Daaraan voegen we het model en de bewerkingen in de recipe toe.

```{r}
#| label: gam_workflow
#| code-fold: false

## Maak de workflow: gam
gam_workflow <- 
  workflow() |>             ## Maak een workflow
  add_model(gam_mod) |>     ## Voeg het model toe
  add_recipe(gam_recipe) |> ## Voeg de recipe toe
  ## Voeg de formule toe
  
  ## Toon de workflow
  gam_workflow
```

## Tune en train het model

Het model moet getuned worden. Dit houdt in dat we de beste parameters voor het model moeten vinden. We maken een grid met verschillende penalty waarden. Daarmee kunnen we vervolgens het beste model selecteren met de hoogste ROC/AUC. We plotten de resultaten van de tuning, zodat we hieruit het beste model kunnen kiezen.

```{r}
#| label: gam_reg_grid
#| code-fold: false

# ## Maak een grid: gam
# 
# # Maak een parameter object
# gam_params <- parameters(gam_mod)
# 
# # Maak een tuning grid
# gam_grid <- grid_max_entropy(gam_params, size = 10)
# 
# ## Train en tune het model: gam
# gam_res <- 
#   gam_workflow |> 
#   tune_grid(dfUitval_validation,
#             grid = gam_reg_grid,
#             control = control_grid(save_pred = TRUE),
#             metrics = metric_set(roc_auc))

gam_res <- gam_workflow

```

```{r}
#| label: gam_plot

## Plot de resultaten
lr_plot <- 
  lr_res |> 
  collect_metrics() |> 
  ggplot(aes(x = penalty, y = mean)) + 
  theme_minimal() +
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

lr_plot 
```

## Kies het beste model

We evalueren modellen met een zo hoog mogelijke Area under the ROC Curve (AUC/ROC) en een zo laag mogelijke penalty. Zo kunnen we uit de resultaten het beste model kiezen. Tot slot maken we een ROC curve om de prestaties van het model te visualiseren.

```{r}
#| label: gam_top_models
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
#| label: gam_best
#| code-fold: false

## Selecteer het beste model: logistische regressie
lr_best <- 
  lr_res |> 
  collect_metrics() |> 
  arrange(penalty) |> 
  slice(1) 

lr_best|> 
  mutate(mean = round(mean, 4)) |>
  knitr::kable()

```

```{r}
#| label: gam_auc
#| code-fold: false

## Verzamel de predicties en evalueer het model (AUC/ROC): logistische regressie
lr_auc <- 
  lr_res |> 
  collect_predictions(parameters = lr_best) |> 
  roc_curve(Uitval, .pred_FALSE) |> 
  mutate(model = "Logistisch Regressie")

autoplot(lr_auc) +
  theme_minimal()

## Bepaal de AUC van het beste model
lr_auc_highest   <-
  lr_res |>
  collect_predictions(parameters = lr_best) |> 
  roc_auc(Uitval, .pred_FALSE)

## Voeg de naam van het model en de AUC toe dfModel_results
dfModel_results <- 
  dfModel_results |>
  add_row(model = "Logistic Regression", auc = lr_auc_highest$.estimate)

```
