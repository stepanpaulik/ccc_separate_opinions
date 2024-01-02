library(tidyverse)
library(tidymodels)
library(parsnip)


# data wrangling ----------------------------------------------------------
data_metadata = read_rds("../data/US_metadata.rds") %>% 
  mutate(presence_dissent = if_else(is.na(as.character(dissenting_opinion)), "None", "At least 1"),
         merits_admissibility = case_when(
           str_detect(as.character(type_verdict), "vyhověno|zamítnuto") ~ "merits",
           str_detect(as.character(type_verdict), "procesní") & !str_detect(as.character(type_verdict), "vyhověno|zamítnuto|odmítnutno") ~ "procedural",
           .default = "admissibility")) %>%
  filter(year(date_decision) < 2023) %>%
  filter(merits_admissibility == "merits" | formation == "Plenum")

data_judges = read_rds("../data/US_judges.rds") %>%
  unnest(term) %>%
  mutate(judge_term_end = case_when(is.na(judge_term_end) ~ judge_term_start %m+% years(10),
                                    .default = judge_term_end))
data_dissents = read_rds("../data/US_dissents.rds") %>%
  filter(doc_id %in% data_metadata$doc_id)
data_compositions = read_rds("../data/US_compositions.rds") %>%
  filter(doc_id %in% data_metadata$doc_id)

data = left_join(data_compositions, data_dissents, by = join_by(doc_id, judge_id == dissenting_judge_id)) %>%
  mutate(dissenting_opinion = if_else(condition = is.na(dissenting_opinion), true = 0, false = 1)) %>%
  left_join(., data_metadata %>% select(doc_id, date_decision, field_register, formation, type_decision, type_proceedings, concerned_acts, concerned_constitutional_acts)) %>%
  mutate(n_concerned_acts = lengths(field_register)) %>% # REWRITE WHEN THE DATA IS UPDATED
  left_join(., data_judges %>% select(judge_id, judge_profession, judge_term_court, judge_term_start), by = join_by(judge_id)) %>%
  mutate(across(where(is.character), ~as.factor(.))) %>%
  mutate(dissenting_opinion = as.factor(dissenting_opinion),
         time_in_office = interval(judge_term_start, date_decision) %/% months(1))

field_register = data_metadata %>% 
  select(field_register) %>%
  unnest(field_register) %>%
  pluck(1) %>%
  unique()


# model -------------------------------------------------------------------
logistic_reg() %>%
  set_engine("glm") %>%
  fit(dissenting_opinion ~ n_concerned_acts + judge_term_court + judge_profession + time_in_office + judge_profession:time_in_office,
      data = data) %>%
  extract_fit_engine()
