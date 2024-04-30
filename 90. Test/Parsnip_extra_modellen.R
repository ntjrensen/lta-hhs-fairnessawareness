gen_additive_mod_mgcv_spec <-
  gen_additive_mod(select_features = tune(), adjust_deg_free = tune()) %>%
  set_engine('mgcv') %>%
  set_mode('classification')

# Create a workflow
gen_additive_mod_mgcv_wf <-
  workflow() %>%
  add_recipe(gen_additive_mod_mgcv_recipe) %>%
  add_model(gen_additive_mod_mgcv_spec)

svm_linear_kernlab_spec <-
  svm_linear(cost = tune(), margin = tune()) %>%
  set_engine('kernlab') %>%
  set_mode('classification')

svm_linear_kernlab_wf <-
  workflow() %>%
  add_recipe(svm_linear_kernlab_recipe) %>%
  add_model(svm_linear_kernlab_spec)

boost_tree_xgboost_spec <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('classification')

boost_tree_xgboost_wf <-
  workflow() %>%
  add_recipe(boost_tree_xgboost_recipe) %>%
  add_model(boost_tree_xgboost_spec)

linear_reg_glmnet_spec <-
  linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine('glmnet')


