library(tidyverse)
library(tidymodels)

source("../supporting_functions.R")
model_dissents = readr::read_rds("models/model_dissents.rds")
model_structure = readr::read_rds("models/model_structure.rds")

# PREDICTION TO NEW DATA
# DISSENT VS NOT_DISSENT
# Predict the dissent/not_dissent classes for newdata
newdata = readr::read_rds("../data/data_paragraphs_doc2vec.rds")

# Create the newdata_dissent with a column of predicted classes
newdata_dissent = newdata %>%
  filter_dissenting_decisions() %>% 
  merge_id() %>%
  select(-text) %>%
  augment(model_dissents, .) %>%
  select(doc_id, .pred_class) %>%
  rename("class" = .pred_class) %>%
  split_id()

# STRUCTURE
newdata_structure = newdata_dissent %>% 
  filter(class == "dissent") %>%
  anti_join(newdata, ., by = join_by(doc_id, paragraph_id)) %>%
  remove_dissents_recalculate() %>%
  merge_id() %>%
  select(-text) %>%
  bind_cols(., predict(object = model_structure, new_data = .)) %>%
  select(doc_id, .pred_class) %>%
  rename("class" = .pred_class) %>%
  split_id()

remove(newdata)

# COMBINE AND SAVE
paragraphs_classified = newdata_dissent %>% 
  filter(class == "dissent") %>%
  bind_rows(., newdata_structure) %>% 
  right_join(., readr::read_rds("../data/data_paragraphs_doc2vec.rds"), by = join_by(doc_id, paragraph_id)) %>%
  select(doc_id, paragraph_id, text, class)

readr::write_rds(paragraphs_classified, file = "../data/data_paragraphs_classified.rds")

