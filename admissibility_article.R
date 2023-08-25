library(tidyverse)

metadata = readRDS(file = "../data/US_metadata.rds")
texts = readRDS(file = "../data/US_texts.rds")

avg_length_judge_rapporteur = metadata %>%
  filter(type_decision == "Usnesení" & grepl(x = type_verdict, pattern = "odmítnuto pro zjevnou neopodstatněnost") & formation != "Plenum") %>% 
  select(doc_id, case_id, date_decision, date_submission, judge_rapporteur_name, year_decision) %>%
  left_join(., texts) %>%
  mutate(length_decision = nchar(text)/1800) %>%
  group_by(judge_rapporteur_name) %>%
  summarise(mean_length = median(length_decision),
            caseload = n())

avg_length_judge_rapporteur %>%
  ggplot(mapping = aes(x = caseload, mean_length)) +
  geom_smooth() +
  geom_point()
  
longest_decisions = metadata %>%
  filter(type_decision == "Usnesení" & grepl(x = type_verdict, pattern = "odmítnuto pro zjevnou neopodstatněnost") & formation != "Plenum")  %>% 
  select(doc_id, case_id, date_decision, date_submission, judge_rapporteur_name, year_decision) %>%
  left_join(., texts) %>%
  mutate(length_decision = nchar(text)/1800) %>%
  arrange(desc(length_decision)) %>%
  slice_head(n = 20)
