xfun::pkg_attach2("tidyverse", "tidymodels", "themis", "xgboost", "doParallel", "parallel", "LiblineaR", "skimr", "broom")

source("../supporting_functions.R")
data = readRDS("../data/judgments_annotated_doc2vec.rds")

# PREDICTION TO NEW DATA
# DISSENT VS NOT_DISSENT
# Predict the dissent/not_dissent classes for newdata
newdata = readRDS("../data/data_paragraphs_doc2vec.rds")

# Create the newdata_dissent with a column of predicted classes
newdata_dissent = newdata %>%
  filter_dissenting_decisions() %>% 
  merge_id() %>%
  select(-text) %>%
  bind_cols(., predict(object = xgboost_fit, new_data = .)) %>%
  select(doc_id, .pred_class) %>%
  rename("class" = .pred_class) %>%
  split_id()

# saveRDS(newdata_dissent, "../data/dissent_classified.rds")
newdata_dissent = readRDS("../data/dissent_classified.rds")

# STRUCTURE
newdata_structure = newdata_dissent %>% 
  filter(class == "dissent") %>%
  anti_join(newdata, ., by = join_by(doc_id, paragraph_id)) %>%
  remove_dissents_recalculate() %>%
  merge_id() %>%
  select(-text) %>%
  bind_cols(., predict(object = xgboost_fit, new_data = .)) %>%
  select(doc_id, .pred_class) %>%
  rename("class" = .pred_class) %>%
  split_id()

# COMBINE AND SAVE
paragraphs_classified = newdata_dissent %>% 
  filter(class == "dissent") %>%
  bind_rows(., newdata_structure)

saveRDS(paragraphs_classified, file = "../data/data_paragraphs_classified.rds")

