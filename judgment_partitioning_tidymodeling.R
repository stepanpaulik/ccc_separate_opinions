install.packages("tidyverse", "tidymodels", "kernlab", "xgboost", "doParallel", "vip")
library(tidyverse)
library(tidymodels)
library(doParallel)
library(vip)

doc2vec_df = readRDS("../data/doc2vec_df.rds")
saveRDS(doc2vec_df, "../data/doc2vec_df.rds")

# Random data filtering
# svm_df = svm_df %>% filter(tag != "dissent") %>% select(!dissenting_opinion)

# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
set.seed(222)

# Put 3/4 of the data into the training set 
data_split = initial_split(svm_df, prop = 3/4)

# Create data frames for the two sets:
train_data = training(data_split)
test_data  = testing(data_split)

# Tuning
# This creates a recipe object. A recipe object contains the formula for the model as well as additional information for example on the role of the columns in a dataframe (ID, predictor, outcome)
svm_rec = recipe(tag ~ ., data = train_data) %>%
  update_role(all_double(), new_role = "predictor") %>%
  update_role(doc_id, new_role = "ID") %>%
  update_role(tag, new_role = "outcome")
svm_rec

# This creates a model object, in which you set the model specifications including the parameters to be tuned or the engine of the model
svm_mod = svm_linear(cost = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")
svm_mod

# Bind the model and the recipe to a workflow
svm_wflow = workflow() %>%
  add_model(svm_mod) %>%
  add_recipe(svm_rec)

# A first fitting without any tuning or resampling
svm_fit = svm_wflow %>% 
  fit(data = train_data)

# Firstly we create a grid with the tuning parameters
svm_tune = grid_regular(cost(), levels = 5)

# We then tune the model with the tune_grid() function
svm_tune = svm_wflow %>% 
  tune_grid(resamples = folds, grid = svm_tune)

# Using various yardstick functions we can see which model parameters are the best and we save them with select_best() function
svm_tune %>% collect_metrics()
svm_tune %>% show_best("accuracy")
best_cost = svm_tune %>% select_best("accuracy")

# Now we input the best parameters into the final workflow with the finalize_workflow() function
svm_wflow_final = 
  svm_wflow %>% 
  finalize_workflow(best_cost)

# Lastly, the model is both trained and then fitted to the testing data with the last_fit() function; this function fits the finalized model on the full training data set and evaluates the finalized model on the testing data.
svm_fit_final = svm_wflow %>%
  last_fit(data_split)

svm_fit_final %>% collect_metrics()

svm_fit_final %>%
  collect_predictions() %>% 
  roc_curve(class, .pred_PS) %>% 
  autoplot()

# After having found the right model we can get to k-fold crossvalidation as well as the final model fitting
# This creates a recipe object. A recipe object contains the formula for the model as well as additional information for example on the role of the columns in a dataframe (ID, predictor, outcome)
svm_rec = recipe(tag ~ ., data = train_data) %>%
  update_role(all_double(), new_role = "predictor") %>%
  update_role(doc_id, new_role = "ID") %>%
  update_role(tag, new_role = "outcome")
svm_rec

# This creates a model object, in which you set the model specifications including the parameters with the tuned values or the engine of the model
svm_mod = svm_linear(cost = 0.01) %>%
  set_engine("kernlab") %>%
  set_mode("classification")
svm_mod

# Bind the model and the recipe to a workflow
svm_wflow = workflow() %>%
  add_model(svm_mod) %>%
  add_recipe(svm_rec)

# Instead of training on the whole train data, let's do cross validation
folds = vfold_cv(train_data, v = 6)

# This does a basic cross validation
svm_fit_cv = svm_wflow %>% 
  fit_resamples(folds)
collect_metrics(svm_fit_cv)

# A first fitting without any tuning or resampling
svm_fit = svm_wflow %>% 
  fit(data = train_data)

# Testing the model fit on test data
predict(svm_fit, test_data)

# Measuring the accuracy of the basic model with the augment function or predict, which requires further specifications and more actions
svm_aug = 
  augment(svm_fit, test_data) %>% 
  select(doc_id, tag, .pred_class) %>%
  mutate(
    tag = factor(tag),
    .pred_class = factor(.pred_class)
  )

svm_aug %>% 
  accuracy(truth = tag, .pred_class)

svm_aug %>%
  conf_mat(truth = tag, .pred_class)


# svm_pred = predict(svm_fit, test_data) %>%
#   bind_cols(predict(svm_fit, test_data, type = "prob")) %>%
#   bind_cols(test_data %>% select(tag)) %>%
#   mutate(
#     tag = factor(tag)
#   )

# svm_pred %>% 
#   accuracy(truth = tag, .pred_class)

## Boosted trees via xgboost
# Tuning
# This creates a recipe object. A recipe object contains the formula for the model as well as additional information for example on the role of the columns in a dataframe (ID, predictor, outcome)
xgboost_rec = recipe(tag ~ ., data = train_data) %>%
  update_role(all_double(), new_role = "predictor") %>%
  update_role(doc_id, new_role = "ID") %>%
  update_role(tag, new_role = "outcome")
xgboost_rec

xgboost_mod = boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(), mtry = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgboost_mod

xgboost_grid = grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train_data),
  learn_rate(),
  size = 30
)

xgboost_wflow = workflow() %>%
  add_model(xgboost_mod) %>%
  add_recipe(xgboost_rec)

folds = vfold_cv(train_data, v = 6)

# Parallelize the process of tuning
doParallel::registerDoParallel()

xgboost_res = tune_grid(
  xgboost_wflow,
  resamples = folds,
  grid = xgboost_grid,
  control = control_grid(save_pred = TRUE)
)

# Plot the tuned parameters
xgboost_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

show_best(xboost_res, "roc_auc")

# Select the best parameters for the final tuning
best_auc = select_best(xgboost_res, "roc_auc")

# Finalize the model fit
final_xgboost = finalize_workflow(
  xgboost_wflow,
  best_auc
)