library(tune)
library(dials)
library(parsnip)
library(rsample)
library(recipes)
library(yardstick)
library(textrecipes)
library(janitor)
library(tidyverse)

## Read file
car_prices_tbl <- read_csv("data.csv") %>%
  clean_names() %>%
  select(msrp, everything())

## Split data
set.seed(123)
car_initial_split <- initial_split(car_prices_tbl, prop = 0.80)
car_initial_split

## Preprocessing Step
preprocessing_recipe <- recipe(msrp ~ ., data = training(car_initial_split)) %>%
  
  # Encode Categorical Data Types
  step_string2factor(all_nominal()) %>%
  step_mutate(
    engine_cylinders = as.factor(engine_cylinders),
    number_of_doors  = as.factor(number_of_doors)
  ) %>%
  
  # Feature Engineering - Market Category
  step_mutate(market_category = market_category %>% str_replace_all("_", "")) %>%
  step_tokenize(market_category) %>%
  step_tokenfilter(market_category, min_times = 0.05, max_times = 1, percentage = TRUE) %>%
  step_tf(market_category, weight_scheme = "binary") %>%
  
  # Combine low-frequency categories
  step_other(all_nominal(), threshold = 0.02, other = "other") %>%
  
  # Impute missing
  step_knnimpute(engine_fuel_type, engine_hp, engine_cylinders, number_of_doors,
                 neighbors = 5) %>%
  
  # Remove unnecessary columns
  step_rm(model) %>%
  prep()

car_training_preprocessed_tbl <- preprocessing_recipe %>% bake(training(car_initial_split))

## Cross Validation
set.seed(123)
car_cv_folds <- training(car_initial_split) %>% 
  bake(preprocessing_recipe, new_data = .) %>%
  vfold_cv(v = 5)

## GLM model
glmnet_model <- linear_reg(
  mode    = "regression", 
  penalty = tune(), 
  mixture = tune()
) %>%
  set_engine("glmnet")

## Params Set
glmnet_params <- parameters(penalty(), mixture())
glmnet_params

set.seed(123)
glmnet_grid <- grid_max_entropy(glmnet_params, size = 20)
glmnet_grid

## Tune Model
glmnet_stage_1_cv_results_tbl <- tune_grid(
  formula   = msrp ~ .,
  model     = glmnet_model,
  resamples = car_cv_folds,
  grid      = glmnet_grid,
  metrics   = metric_set(mae, mape, rmse, rsq),
  control   = control_grid(verbose = TRUE)
)
