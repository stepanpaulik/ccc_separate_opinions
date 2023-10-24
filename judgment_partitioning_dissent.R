library(tidymodels)
library(themis)
library(xgboost)
library(kernlab)
library(LiblineaR)
library(broom)
library(skimr)
library(finetune)
library(ranger)
tidymodels_prefer()

library(doMC)
registerDoMC(cores = parallel::detectCores() - 2)

source("../supporting_functions.R")
data = readRDS("../data/judgments_annotated_doc2vec.rds")
# data = readRDS("../data/judgments_annotated_dtm.rds")
metadata = readRDS("../data/US_metadata.rds")

# For dissent/not_dissent classification
data_dissent = filter_dissenting_decisions(data = data) %>%
  mutate(class = case_when(
    class != "dissent" ~ "not_dissent",
    .default = "dissent"
  ))

# Explore the data
data_dissent %>%
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

# Create data frames for the two sets:
train_data = training(data_split)
test_data  = testing(data_split)

# Instead of training on the whole train data, let's do cross validation
folds = vfold_cv(train_data, repeats = 5)

# Tuning
# This creates a recipe object. A recipe object contains the formula for the model as well as additional information for example on the role of the columns in a dataframe (ID, predictor, outcome)
dissents_rec = recipe(class ~ ., data = train_data) %>%
  update_role(all_double(), new_role = "predictor") %>%
  update_role(doc_id, new_role = "ID") %>%
  update_role(class, new_role = "outcome") %>%
  themis::step_smote(class)

# This creates a model objects, in which you set the model specifications including the parameters to be tuned or the engine of the model
svm_mod = svm_linear(cost = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")
svm_mod

xgboost_mod = boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(), mtry = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

rand_forest_mod = rand_forest(mtry = tune(),
                              trees = tune(),
                              min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("classification")
rand_forest_mod

# Bind the model and the recipe to a workflow
wflow_set = workflow_set(
  preproc = list(recipe = dissents_rec),
  models = list(svm_lin = svm_mod, rand_forest = rand_forest_mod, xgboost = xgboost_mod)
)
wflow_set

# Create a control race
race_ctrl =
  control_race(
    save_pred = TRUE,
    parallel_over = "everything"
  )

classification_metrics = metric_set(accuracy, recall, f_meas, precision, mcc, roc_auc)

# We then tune the model with the tune_grid() function
wflow_set_tune =
  wflow_set %>%
  workflow_map(
    "tune_race_anova",
    seed = 1503,
    resamples = folds,
    grid = 25,
    control = race_ctrl,
    metrics = classification_metrics,
    verbose = TRUE
  )

# Some basic comparison of models
wflow_set_tune %>% 
  rank_results() %>% 
  filter(.metric == "roc_auc") %>% 
  select(model, .config, roc_auc = mean, rank)

autoplot(
  wflow_set_tune,
  rank_metric = "roc_auc",  # = how to order models
  metric = "roc_auc",       # = which metric to visualize
  select_best = TRUE     # = one point per workflow
) +
  geom_text(aes(y = mean - 1/15, label = wflow_id), angle = 90, hjust = 1) +
  lims(y = c(0,1)) +
  theme(legend.position = "none")

best_results = 
  wflow_set_tune %>% 
  extract_workflow_set_result("recipe_xgboost") %>% 
  select_best(metric = "roc_auc")
best_results

wflow_set_tune %>% 
  rank_results() %>% 
  filter(.metric == "roc_auc") %>% 
  select(model, .config, roc_auc = mean, rank)

# Finalizing the fit
final_wflow = wflow_set_tune %>%
  extract_workflow("recipe_xgboost") %>%
  finalize_workflow(best_results) %>%
  last_fit(split = data_split, metrics = classification_metrics)

collect_metrics(final_wflow)

# Concrete metrics
final_wflow_aug = wflow_set_tune %>%
  extract_workflow("recipe_xgboost") %>%
  finalize_workflow(best_results) %>%
  fit(train_data) %>%
  augment(., test_data) %>% 
  select(doc_id, class, .pred_class) %>%
  mutate(across(contains("class"), ~as.factor(.x)))

final_wflow_aug %>%
  conf_mat(truth = class, .pred_class)

# Final model used for predicting on new data
model_dissents = wflow_set_tune %>%
  extract_workflow("recipe_xgboost") %>%
  finalize_workflow(best_results) %>%
  fit(data_dissent)

predicted = augment(model_dissents, data_dissent)


# save.image("models/partinitioning_dissents.RData")
# readr::write_rds(model_dissents, file = "models/model_dissents.rds")
load("models/partinitioning_dissents.RData")

# DATA for appendix
appendix_conf_mat_dissent = final_wflow_aug %>%
  conf_mat(truth = class, .pred_class)  %>% 
  autoplot(type = "mosaic")

appendix_conf_mat_dissent

# Some basic comparison of models RESULT OF cross-validation
appendix_tuning_comp_dissent = wflow_set_tune %>% 
  rank_results() %>% 
  filter(.metric == c("accuracy")) %>% 
  select(model, .config, accuracy = mean, rank)

appendix_tuning_comp_dissent

# Final fit
appendix_final_fit = collect_metrics(final_wflow)
appendix_final_fit

to_remove = ls()
rm(list=to_remove[!grepl("^appendix_", to_remove)])
save.image(file = "models/appendix_classification_dissent.RData")
  



