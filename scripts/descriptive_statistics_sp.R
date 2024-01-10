library(tidyverse); theme_set(theme_minimal())
library(patchwork)
library(geomtextpath)


# case-level variables ----------------------------------------------------
data_metadata = read_rds("../data/US_metadata.rds") %>% 
  mutate(presence_dissent = if_else(is.na(as.character(dissenting_opinion)), "None", "At least 1"),
         merits_admissibility = case_when(
           str_detect(as.character(type_verdict), "vyhověno|zamítnuto") ~ "merits",
           str_detect(as.character(type_verdict), "procesní") & !str_detect(as.character(type_verdict), "vyhověno|zamítnuto|odmítnutno") ~ "procedural",
           .default = "admissibility")) %>%
  filter(year(date_decision) < 2023) %>%
  filter(merits_admissibility == "merits" | formation == "Plenum") %>%
  filter(merits_admissibility != "procedural")
  
data_judges = read_rds("../data/US_judges.rds") %>%
  mutate(judge_term_end = case_when(is.na(judge_term_end) ~ judge_term_start %m+% years(10),
                                    .default = judge_term_end))
data_dissents = read_rds("../data/US_dissents.rds") %>%
  filter(doc_id %in% data_metadata$doc_id)
data_compositions = read_rds("../data/US_compositions.rds") %>%
  filter(doc_id %in% data_metadata$doc_id)

professions = data_judges %>% 
  filter(judge_term_court != "4th") %>%
  ggplot(aes(x = forcats::fct_infreq(judge_profession))) +
  geom_bar() +
  labs(x = NULL, y = NULL) +
  facet_wrap(~judge_term_court)
professions

age_distribution = data_judges %>% 
  filter(judge_term_court != "4th") %>%
  mutate(age_term_start = year(judge_term_start) - judge_yob,
         age_term_end = year(judge_term_end) - judge_yob) %>%
  pivot_longer(cols = c(age_term_start, age_term_end), names_to = "age_term", values_to = "age_term_value") %>%
  mutate(age_term = if_else(age_term == "age_term_start", "at the beginning", "at the end")) %>%
  ggplot(aes(x = age_term_value, colour = age_term, label = age_term)) +
  geom_density() +
  labs(x = "The age of a justice", y = "Density", colour = NULL)
age_distribution


data_metadata %>%
  filter(merits_admissibility %in% c("merits", "admissibility")) %>%
  group_by(year(date_submission)) %>%
  summarise(caseload = n(),
            avg_length = mean(length_proceeding)) %>%
  filter(avg_length < 400) %>%
  ggplot(aes(y = caseload, x = avg_length)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "black") +
  labs(x = "Average length of proceedings before the CCC", y = "Yearly caseload of the CCC")


caseload = data_metadata %>%
  ggplot(aes(x = year(date_submission), fill = merits_admissibility)) +
  geom_bar(position = position_stack(reverse = TRUE))  +
  scale_x_continuous(breaks = seq(1991, 2023, 2)) +
  scale_fill_brewer(palette="Pastel1") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = NULL, y = NULL, fill = "Type of verdict")

dissents_distribution_judges = data_dissents %>%
  group_by(doc_id) %>%
  count() %>%
  ungroup() %>%
  ggplot(aes(x = n)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(1, 9, 1))

dissents_distribution_opinions = data_dissents %>%
  group_by(doc_id) %>%
  summarise(n = length(unique(dissenting_group))) %>%
  ungroup() %>%
  ggplot(aes(x = n)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(1, 9, 1)) +
  labs(x = "Number of dissenting opinions", y = "Count")

dissents_prevalence = data_metadata %>%
  filter(merits_admissibility == "merits" | formation == "Plenum") %>%
  ggplot(aes(x = forcats::fct_infreq(presence_dissent))) +
  geom_bar() +
  labs(x = NULL, y = NULL)

overall_table = data_metadata %>%
  mutate(formation = if_else(formation == "Plenum", "Plenum", "Chamber"),
         presence_dissent = if_else(is.na(as.character(dissenting_opinion)), 0, 1)) %>%
  group_by(formation, merits_admissibility) %>%
  summarise(count = n(),
            ratio_total = scales::percent(x = n()/nrow(.), accuracy = 2),
            ratio_dissent = scales::percent(x = sum(presence_dissent)/n(), accuracy = 2))


save.image("report/descriptive_statistics.RData")
