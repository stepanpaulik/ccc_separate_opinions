xfun::pkg_attach2("tidyverse", "tidymodels", "themis", "xgboost", "doParallel", "parallel", "LiblineaR", "skimr", "broom")

source("../supporting_functions.R")
data = readRDS("../data/judgments_annotated_doc2vec.rds")
# data = readRDS("../data/judgments_annotated_dtm.rds")
metadata = readRDS("../data/US_metadata.rds")



# Data prep depending on the goal
# Create a binary variable for the presence of dissent in the decision, and include only the information on dissents
filter_dissenting_decisions = function(data, metadata = readRDS("../data/US_metadata.rds")){
    output = metadata %>% 
      select(doc_id, dissenting_opinion) %>% 
      left_join(data, ., by = "doc_id") %>% 
      mutate(dissenting_opinion = if_else(dissenting_opinion == "", 0, 1)) %>%
      filter(dissenting_opinion == 1) %>%
      select(-dissenting_opinion)
}

remove_dissents_recalculate = function(data){
  output = data %>%
    group_by(doc_id) %>%
    mutate(start = start/max(start),
           end = end/max(end),
           length = length/max(length))
  return(output)
}

# For dissent/not_dissent classification
data_dissent = filter_dissenting_decisions(data = data) %>%
  mutate(class = case_when(
    class != "dissent" ~ "not_dissent",
    .default = "dissent"
  )) %>%
  mutate(class = factor(class))

# For the remaining classes classification
data_structure = data %>% 
  filter(!class %in% c("dissent", "costs")) %>%
  remove_dissents_recalculate() %>%
  mutate(class = case_when(
    class == "comments" ~ "procedure history",
    .default = class
  ),
  class = factor(class))

# Explore the data
data_structure %>%
  group_by(class) %>%
  summarise(N = n(),
            avg_length = mean(length)) %>%
  mutate(freq = N/sum(N))

# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
set.seed(222)

# Put 3/4 of the data into the training set 
data_split = data_dissent %>%
  initial_split(prop = 3/4)

data_split = data_structure %>%
  initial_split(prop = 3/4)

# Create data frames for the two sets:
train_data = training(data_split)
test_data  = testing(data_split)

# Instead of training on the whole train data, let's do cross validation
folds = vfold_cv(train_data, v = 6)

# Tuning
# This creates a recipe object. A recipe object contains the formula for the model as well as additional information for example on the role of the columns in a dataframe (ID, predictor, outcome)
svm_rec = recipe(class ~ ., data = train_data) %>%
  update_role(all_double(), new_role = "predictor") %>%
  update_role(doc_id, new_role = "ID") %>%
  update_role(class, new_role = "outcome") %>%
  step_smote(class)
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
svm_fit_final = svm_wflow_final %>%
  last_fit(data_split)

# Final fit
svm_fit_final = svm_wflow_final %>%
  fit(train_data)

svm_fit_final %>% collect_metrics()

svm_fit_final %>%
  collect_predictions() %>% 
  roc_curve(class, .pred_dissent) %>% 
  autoplot()

# After having found the right model we can get to k-fold crossvalidation as well as the final model fitting
# This creates a recipe object. A recipe object contains the formula for the model as well as additional information for example on the role of the columns in a dataframe (ID, predictor, outcome)
svm_rec = recipe(class ~ ., data = train_data) %>%
  update_role(all_double(), new_role = "predictor") %>%
  update_role(doc_id, new_role = "ID") %>%
  update_role(class, new_role = "outcome") %>%
  themis::step_smote(class)
svm_rec

# This creates a model object, in which you set the model specifications including the parameters with the tuned values or the engine of the model
svm_mod = svm_linear(cost = 0.01313901) %>%
  set_engine("kernlab") %>%
  set_mode("classification")
svm_mod

# Bind the model and the recipe to a workflow
svm_wflow = workflow() %>%
  add_model(svm_mod) %>%
  add_recipe(svm_rec)


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
svm_aug = augment(svm_fit, test_data) %>% 
  select(doc_id, class, .pred_class)

svm_aug %>% 
  accuracy(truth = class, .pred_class)

svm_aug %>%
  conf_mat(truth = class, .pred_class)

svm_aug %>%
  recall(truth = class, .pred_class)

svm_aug %>%
  f_meas(truth = class, .pred_class)

svm_aug %>%
  precision(truth = class, .pred_class)


# svm_pred = predict(svm_fit, test_data) %>%
#   bind_cols(predict(svm_fit, test_data, type = "prob")) %>%
#   bind_cols(test_data %>% select(class)) %>%
#   mutate(
#     class = factor(class)
#   )

# svm_pred %>% 
#   accuracy(truth = class, .pred_class)

## Boosted trees via xgboost
# Tuning
# This creates a recipe object. A recipe object contains the formula for the model as well as additional information for example on the role of the columns in a dataframe (ID, predictor, outcome)
xgboost_rec = recipe(class ~ ., data = train_data) %>%
  update_role(all_double(), new_role = "predictor") %>%
  update_role(doc_id, new_role = "ID") %>%
  update_role(class, new_role = "outcome") %>%
  step_smote(class)
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

# folds = vfold_cv(train_data, v = 6)

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

show_best(xgboost_res, "roc_auc")

# Select the best parameters for the final tuning
best_auc = select_best(xgboost_res, "roc_auc")

# Finalize the model workflow with inputting the tuned parameters to the workflow
final_xgboost_wflow = finalize_workflow(
  xgboost_wflow,
  best_auc
)

# The final fit using the finalized wflow from previous lines
final_xgboost_fit = final_xgboost_wflow %>% 
  last_fit(data_split)

final_xgboost_fit %>% collect_metrics()

xgboost_aug = 
  augment(final_xgboost_fit, test_data) %>% 
  select(doc_id, class, .pred_class)

xgboost_aug %>% 
  accuracy(truth = class, .pred_class)

xgboost_aug %>%
  conf_mat(truth = class, .pred_class)

# What are the most important parameters for variable importance?
final_xgboost_wflow %>%
  fit(data = train_data) %>%
  extract_fit_parsnip() %>%
  vip::vip(geom = "point")


# Final tuned fit
xgboost_rec = recipe(class ~ ., data = train_data) %>%
  update_role(all_double(), new_role = "predictor") %>%
  update_role(doc_id, new_role = "ID") %>%
  update_role(class, new_role = "outcome") %>%
  step_smote(class)
xgboost_rec

xgboost_mod = boost_tree(
  trees = 1000,
  mtry = 17,
  min_n = 16,
  tree_depth = 13,
  learn_rate = 0.00540844661792405,
  loss_reduction = 3.41145754172432e-08,
  sample_size = 0.545441143696662
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgboost_wflow = workflow() %>%
  add_model(xgboost_mod) %>%
  add_recipe(xgboost_rec)

# 6-fold crossvalidation
metrics = metric_set(accuracy, precision, roc_auc)
xgboost_fit_cv = xgboost_wflow %>%
  fit_resamples(folds, metrics = metrics)
xgboost_fit_cv %>% collect_metrics()



# Fit with real data
xgboost_fit = xgboost_wflow %>% 
  fit(data = data_structure)

xgboost_aug = 
  augment(xgboost_fit, test_data) %>% 
  select(doc_id, class, .pred_class)

xgboost_aug %>% 
  accuracy(truth = class, .pred_class)

xgboost_aug %>% 
  precision(truth = class, .pred_class)

eexgboost_aug %>% 
  recall(truth = class, .pred_class)

xgboost_aug %>%
  conf_mat(truth = class, .pred_class)

save.image(file = "../data/tuned_model_data.RData")

# Workflow set
rec = recipe(class ~ ., data = train_data) %>%
  update_role(all_double(), new_role = "predictor") %>%
  update_role(doc_id, new_role = "ID") %>%
  update_role(class, new_role = "outcome") %>%
  step_smote(class)

# Models
svm_mod = svm_linear(cost = 0.01313901) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

xgboost_mod = boost_tree(
  trees = 1000,
  mtry = 17,
  min_n = 16,
  tree_depth = 13,
  learn_rate = 0.00540844661792405,
  loss_reduction = 3.41145754172432e-08,
  sample_size = 0.545441143696662
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# The workflow set
wflow = workflow_set(
  preproc = list(rec),
  models = list(svm = svm_mod, xgboost = xgboost_mod)
)

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE, verbose = TRUE)

result = wflow %>%
  workflow_map("fit_resamples", verbose = TRUE, resamples = folds, control = keep_pred)

collect_metrics(result)




  
  



