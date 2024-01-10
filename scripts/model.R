library(tidyverse)
library(tidymodels)
library(parsnip)

controversial_topics = c("diskriminace", "spotřebitel", "vyvlastnění", "restituční nárok", "restituce", "církevní majetek", "sexuální orientace", "základní práva a svobody/rovnost v základních právech a svobodách a zákaz diskriminace", "základní práva a svobody/rovnost v právech a důstojnosti a zákaz diskriminace", "hospodářská, sociální a kulturní práva/právo na ochranu zdraví", "základní práva a svobody/právo vlastnit a pokojně užívat majetek/restituce", "základní práva a svobody/svoboda projevu a právo na informace/svoboda projevu", "základní ústavní principy/zákaz vázání státu na ideologii nebo náboženství (laický stát)")

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
  mutate(judge_term_end = case_when(is.na(judge_term_end) ~ judge_term_start %m+% years(10),
                                    .default = judge_term_end))
data_dissents = read_rds("../data/US_dissents.rds") %>%
  filter(doc_id %in% data_metadata$doc_id)
data_compositions = read_rds("../data/US_compositions.rds") %>%
  filter(doc_id %in% data_metadata$doc_id)

data = left_join(data_compositions, data_dissents, by = join_by(doc_id, judge_id == dissenting_judge_id)) %>%
  mutate(dissenting_opinion = as.factor(if_else(condition = is.na(dissenting_opinion), true = 0, false = 1))) %>%
  left_join(., data_metadata %>% select(doc_id, date_decision, subject_proceedings, field_register, formation, type_decision, type_proceedings, concerned_acts, concerned_constitutional_acts, type_verdict)) %>%
  mutate(n_concerned_acts = lengths(concerned_acts),
         n_concerned_constitutional_acts = lengths(concerned_constitutional_acts)) %>%  # REWRITE WHEN THE DATA IS UPDATED
  rename(dissenting_judge = judge_name) %>%
  mutate(separate_opinion = pmap(., function(doc_id, dissenting_judge, date_decision, ...) data_judges %>%
                                   filter(date_decision >= judge_term_start & date_decision <= judge_term_end & dissenting_judge == judge_name) %>%
                                   select(judge_gender, judge_uni, judge_degree, judge_profession, judge_term_start, judge_term_end, judge_term_court))) %>%
  unnest(separate_opinion) %>%
  mutate(across(where(is.character), ~as.factor(.)),
         time_in_office = interval(judge_term_start, date_decision) %/% months(1)) %>%
  rowwise() %>%
  mutate(controversial = if_else(any((subject_proceedings %>% pluck(1)) %in% controversial_topics) | any((field_register %>% pluck(1)) %in% controversial_topics), 1, 0)) %>%
  ungroup()

data_metadata %>% 
  select(field_register) %>%
  unnest(field_register) %>%
  pluck(1) %>%
  unique()

data_metadata %>% 
  select(subject_proceedings) %>%
  unnest(subject_proceedings) %>%
  pluck(1) %>%
  unique()

# model -------------------------------------------------------------------
model = logistic_reg() %>%
  set_engine("glm") %>%
  fit(dissenting_opinion ~ n_concerned_acts + n_concerned_constitutional_acts + judge_profession + time_in_office + judge_profession:time_in_office + judge_gender + controversial,
      data = data) %>%
  extract_fit_engine()

write_rds(model, "report/model_results.rds")
