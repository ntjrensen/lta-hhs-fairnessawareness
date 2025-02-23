library("DALEXtra")
library("tidymodels")
library("recipes")

data <- titanic_imputed
data$survived <- as.factor(data$survived)
rec <- recipe(survived ~ ., data = data) %>%
  step_normalize(fare)
model <- decision_tree(tree_depth = 25) %>%
  set_engine("rpart") %>%
  set_mode("classification")

wflow <- workflow() %>%
  add_recipe(rec) %>%
  add_model(model)


model_fitted <- wflow %>%
  fit(data = data)

explainer <- explain_tidymodels(model_fitted, 
                                data = titanic_imputed, 
                                y = titanic_imputed$survived)

fobject <- fairness_check(explainer,
                          protected = titanic_imputed$gender,
                          privileged = "male",
                          cutoff = 0.8,
                          verbose = FALSE,
                          colorize = TRUE)

plot(fobject)
