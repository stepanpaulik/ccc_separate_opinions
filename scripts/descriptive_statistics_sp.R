library(tidyverse); theme_set(theme_minimal())
library(patchwork)
library(geomtextpath)
source("scripts/load_data.R")


# EXPLORATORY ANALYSIS ---------------------------------------------------
data %>%
  ggplot(aes(x = controversial, y = dissenting_opinion, color = judge_gender)) +
  facet_wrap(~judge_profession) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  theme_set(theme_bw(base_size = 10)) +
  theme(legend.position = "top") +
  labs(x = "", y = "Observed Probabilty of a separate opinion")
  
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


caseload = read_rds("../data/US_metadata.rds") %>%
  ggplot(aes(x = year(date_decision), fill = merits_admissibility)) +
  geom_bar(position = position_stack(reverse = TRUE))  +
  scale_x_continuous(breaks = seq(1991, 2023, 2)) +
  scale_fill_brewer(palette="Pastel1") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = NULL, y = NULL, fill = "Type of verdict")
caseload

separate_opinion_ratio = read_rds("../data/US_metadata.rds") %>% 
  filter(!merits_admissibility %in% c("procedural")) %>%
  mutate(presence_dissent = if_else(is.na(as.character(dissenting_opinion)), 0, 1)) %>%
  group_by(year(date_decision), merits_admissibility) %>%
  summarise(sum(presence_dissent)/n()) %>%
  rename(Year = `year(date_decision)`,
           `Dissent` = `sum(presence_dissent)/n()`) %>%
  ggplot(aes(x = Year, y = Dissent, group = merits_admissibility, color = merits_admissibility)) + 
  geom_point() + 
  geom_line() +
  labs(y = "Percent of Decisions with at least one Separate Opinion", color = NULL) +
  scale_y_continuous(labels = scales::label_percent())
separate_opinion_ratio

dissents_distribution_judges = data_dissents %>%
  group_by(doc_id) %>%
  count() %>%
  ungroup() %>%
  ggplot(aes(x = n)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(1, 9, 1))
dissents_distribution_judges

dissents_distribution_opinions = data_dissents %>%
  group_by(doc_id) %>%
  summarise(n = length(unique(dissenting_group))) %>%
  ungroup() %>%
  ggplot(aes(x = n)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(1, 9, 1)) +
  labs(x = "Number of dissenting opinions", y = "Count")
dissents_distribution_opinions

dissents_prevalence = data_metadata %>%
  filter(merits_admissibility == "merits" | formation == "Plenum") %>%
  ggplot(aes(x = forcats::fct_infreq(presence_dissent))) +
  geom_bar() +
  labs(x = NULL, y = NULL)
dissents_prevalence

descr_workload = data %>%
  select(workload) %>%
  skimr::skim() %>%
  select(skim_variable, numeric.mean, numeric.sd, numeric.p0, numeric.p25, numeric.p50, numeric.p75, numeric.p100)

plot_workload = data %>%
  ggplot(aes(x = workload)) +
  geom_density() +
  facet_wrap(~judge_name)
plot_workload 

overall_table = data_metadata %>%
  mutate(formation = if_else(formation == "Plenum", "Plenum", "Chamber"),
         presence_dissent = if_else(is.na(as.character(dissenting_opinion)), 0, 1)) %>%
  group_by(formation, merits_admissibility) %>%
  summarise(count = n(),
            ratio_total = scales::percent(x = n()/nrow(.), accuracy = 0.1),
            ratio_dissent = scales::percent(x = sum(presence_dissent)/n(), accuracy = 0.1)) %>%
  ungroup()
overall_table

descriptive_dependent = data %>%
  mutate(formation = if_else(formation == "Plenum", "Plenum", "Chamber"),
         presence_dissent = if_else(is.na(as.character(dissenting_opinion)), 0, 1)) %>%
  group_by(formation, merits_admissibility) %>%
  summarise(n_dissents = mean(dissenting_opinion))
descriptive_dependent

dependent_variables_overview = data %>%
  select(dissenting_opinion, judge_profession, n_concerned_acts, n_concerned_constitutional_acts, n_citations, controversial) %>%
  group_by(judge_profession) %>%
  select(where(is.numeric)) %>%
  skimr::skim() %>%
  as_tibble() %>%
  select(c(skim_variable, judge_profession, numeric.mean, numeric.sd))

save.image("report/descriptive_statistics.RData")
