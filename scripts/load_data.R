library(tidyverse)
library(furrr)
plan(multisession, workers = parallel::detectCores() - 2)

# PLACEBO -----------------------------------------------------------------
controversial_topics = c("diskriminace", "spotřebitel", "vyvlastnění", "restituční nárok", "restituce", "církevní majetek", "sexuální orientace", "základní práva a svobody/rovnost v základních právech a svobodách a zákaz diskriminace", "základní práva a svobody/rovnost v právech a důstojnosti a zákaz diskriminace", "hospodářská, sociální a kulturní práva/právo na ochranu zdraví", "základní práva a svobody/právo vlastnit a pokojně užívat majetek/restituce", "základní práva a svobody/svoboda projevu a právo na informace/svoboda projevu", "základní ústavní principy/zákaz vázání státu na ideologii nebo náboženství (laický stát)")

set.seed(2222)

subject_matter =  read_rds("../data/ccc_dataset/rds/ccc_subject_matter.rds") %>%
  select(subject_matter) %>%
  distinct(subject_matter)

subject_matter = replicate(n = 10, subject_matter %>% slice_sample(n = length(controversial_topics)), simplify=F) %>%
  bind_rows(.id="placebo")


# data wrangling ----------------------------------------------------------
data_metadata = read_rds("../data/ccc_dataset/rds/ccc_metadata.rds") %>% 
  mutate(presence_dissent = if_else(is.na(as.character(separate_opinion)), "None", "At least 1")) %>%
  filter(between(year(date_decision), 2004, 2022)) %>%
  filter(grounds == "merits" | formation == "Plenum") %>%
  filter(grounds != "procedural")

data_judges = read_rds("../data/ccc_dataset/rds/ccc_judges.rds")

data_compositions = data_metadata %>%
  select(doc_id, composition) %>%
  unnest(composition)

data_dissents = read_rds("../data/ccc_dataset/rds/ccc_separate_opinions.rds") %>%
  filter(doc_id %in% data_metadata$doc_id)

data_metadata_temp = read_rds("../data/ccc_dataset/rds/ccc_metadata.rds") %>% rename(date_decision_meta = date_decision,
                                                                    date_submission_meta = date_submission) %>%
  select(judge_rapporteur_name, date_decision_meta, date_submission_meta)

data = left_join(data_compositions, data_dissents, by = join_by(doc_id, judge_id == dissenting_judge_id)) %>%
  mutate(dissenting_judge_name = if_else(condition = is.na(dissenting_judge_name), true = 0, false = 1)) %>%
  rename(separate_opinion = dissenting_judge_name) %>%
  left_join(., data_metadata %>% select(doc_id, date_decision, subject_proceedings, grounds, subject_register, formation, type_decision, type_proceedings, concerned_acts, concerned_constitutional_acts, type_verdict, citations)) %>%
  mutate(n_concerned_acts = if_else(is.na(concerned_acts), 0, lengths(concerned_acts)),
         n_concerned_constitutional_acts = str_count(string = as.character(concerned_constitutional_acts), pattern = "čl")) %>%
  rowwise() %>%
  mutate(n_citations = length(unique(unlist(citations)))) %>%
  ungroup() %>%
  mutate(separate_opinion_nested = future_pmap(., function(doc_id, judge_name, date_decision, ...) data_judges %>%
                                          rename(judge_name.y = judge_name) %>%
                                          filter(date_decision >= judge_term_start & date_decision <= judge_term_end & judge_name == judge_name.y) %>%
                                          select(judge_gender, judge_uni, judge_degree, judge_profession, judge_term_start, judge_term_end, judge_term_court)),
         workload = future_pmap(., function(doc_id, judge_name, date_decision, ...) data_metadata_temp %>%
                                  filter(date_decision_meta > date_decision & 
                                           date_submission_meta < date_decision & 
                                           judge_rapporteur_name == judge_name) %>%
                                  nrow())) %>%
  unnest(c(separate_opinion_nested, workload)) %>%
  mutate(across(where(is.character), ~as_factor(.)),
         time_in_office = interval(judge_term_start, date_decision) %/% months(1)) %>%
  rowwise() %>%
  mutate(controversial = if_else(any((subject_proceedings %>% pluck(1)) %in% controversial_topics) | any((subject_register %>% pluck(1)) %in% controversial_topics), 1, 0) %>% as_factor()) %>%
  mutate(placebo1 = if_else(any((subject_proceedings %>% pluck(1)) %in% subject_matter$subject_matter[subject_matter$placebo == 1]) | any((subject_register %>% pluck(1)) %in% subject_matter$subject_matter[subject_matter$placebo == 1]), 1, 0) %>% as_factor(),
         placebo2 = if_else(any((subject_proceedings %>% pluck(1)) %in% subject_matter$subject_matter[subject_matter$placebo == 2]) | any((subject_register %>% pluck(1)) %in% subject_matter$subject_matter[subject_matter$placebo == 2]), 1, 0) %>% as_factor(),
         placebo3 = if_else(any((subject_proceedings %>% pluck(1)) %in% subject_matter$subject_matter[subject_matter$placebo == 3]) | any((subject_register %>% pluck(1)) %in% subject_matter$subject_matter[subject_matter$placebo == 3]), 1, 0) %>% as_factor(),
         placebo4 = if_else(any((subject_proceedings %>% pluck(1)) %in% subject_matter$subject_matter[subject_matter$placebo == 4]) | any((subject_register %>% pluck(1)) %in% subject_matter$subject_matter[subject_matter$placebo == 4]), 1, 0) %>% as_factor(),
         placebo5 = if_else(any((subject_proceedings %>% pluck(1)) %in% subject_matter$subject_matter[subject_matter$placebo == 5]) | any((subject_register %>% pluck(1)) %in% subject_matter$subject_matter[subject_matter$placebo == 5]), 1, 0) %>% as_factor(),
         placebo6 = if_else(any((subject_proceedings %>% pluck(1)) %in% subject_matter$subject_matter[subject_matter$placebo == 5]) | any((subject_register %>% pluck(1)) %in% subject_matter$subject_matter[subject_matter$placebo == 6]), 1, 0) %>% as_factor(),
         placebo7 = if_else(any((subject_proceedings %>% pluck(1)) %in% subject_matter$subject_matter[subject_matter$placebo == 5]) | any((subject_register %>% pluck(1)) %in% subject_matter$subject_matter[subject_matter$placebo == 7]), 1, 0) %>% as_factor(),
         placebo8 = if_else(any((subject_proceedings %>% pluck(1)) %in% subject_matter$subject_matter[subject_matter$placebo == 5]) | any((subject_register %>% pluck(1)) %in% subject_matter$subject_matter[subject_matter$placebo == 8]), 1, 0) %>% as_factor(),
         placebo9 = if_else(any((subject_proceedings %>% pluck(1)) %in% subject_matter$subject_matter[subject_matter$placebo == 5]) | any((subject_register %>% pluck(1)) %in% subject_matter$subject_matter[subject_matter$placebo == 9]), 1, 0) %>% as_factor(),
         placebo10 = if_else(any((subject_proceedings %>% pluck(1)) %in% subject_matter$subject_matter[subject_matter$placebo == 5]) | any((subject_register %>% pluck(1)) %in% subject_matter$subject_matter[subject_matter$placebo == 10]), 1, 0) %>% as_factor()) %>%
  ungroup() %>%
  group_by(doc_id) %>%
  filter(n() %in% c(3,9,10,11,12,13,14,15)) %>%
  filter(!any(judge_term_court %in% c("1st"))) %>%
  arrange(judge_name) %>%
  mutate(formation = if_else(formation == "Plenum", true = "Plenum", false = paste0(judge_name, collapse = ";")),
         formation = as_factor(formation)) %>%
  ungroup() %>%
  select(-where(is.list)) %>%
  select(-dissenting_group)

data_term_temp = data %>% 
  group_by(doc_id) %>%
  count(judge_term_court) %>%
  pivot_wider(names_from = judge_term_court, values_from = n) %>%
  mutate(across(everything(), ~replace_na(., replace = 0))) %>%
  summarise(panel_term = if_else(`3rd` > `2nd`, "3rd", "2nd") %>%
              as_factor())

data = data %>%
  left_join(., data_term_temp) %>%
  left_join(., read_rds("../data/ccc_dataset/rds/ccc_texts.rds") %>%
              mutate(text = unlist(text)) %>%
              group_by(doc_id) %>%
              summarise(length_decision = str_length(text)))

# COALITIONS --------------------------------------------------------------
# coalition_one = c("Kateřina Šimáčková", "Vojtěch Šimíček", "Ludvík David", "Jaromír Jirsa", "David Uhlíř", "Jiří Zemánek", "Tomáš Lichovník", "Jan Filip", "Milada Tomková", "Pavel Šámal")
# coalition_two = c("Radovan Suchánek","Vladimír Sládeček","Josef Fiala","Jan Musil","Jaroslav Fenyk","Pavel Rychetský")
# 
# data_coalition = data %>%
#   filter(formation != "Plenum") %>%
#   group_by(doc_id) %>%
#   filter(all(judge_name %in% c(coalition_one, coalition_two))) %>%
#   ungroup() %>%
#   mutate(coalition = if_else(judge_name %in% coalition_one, 1, 0)) %>%
#   group_by(doc_id) %>%
#   mutate(
#     coalition = sum(coalition),
#     coalition = case_when(coalition == 3 ~ "full_coal_1",
#                           coalition == 0 ~ "full_coal_2",
#                           coalition == 1 | 2 ~ "mixed_coal") %>% as_factor(),
#     separate_opinion = as_factor(separate_opinion)) %>%
#   ungroup()

rm(data_metadata_temp)
rm(data_term_temp)
 
