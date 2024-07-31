library(tidyverse); theme_set(theme_minimal())
library(patchwork)
library(geomtextpath)
source("scripts/load_data.R")


# EXPLORATORY ANALYSIS ---------------------------------------------------
data_fair_trial = data |>
  left_join(read_rds("../data/ccc_database/rds/ccc_metadata.rds") |> 
              mutate(fair_trial = if_else(str_detect(string = as.character(concerned_constitutional_acts), pattern = "2/1993 Sb./Sb.m.s., čl. 36"), "Fair Trial", "Rest")) |>
              select(doc_id, fair_trial)) |>
  group_by(fair_trial) |>
  summarise(mean_n_concerned_constitutional_acts = mean(n_concerned_constitutional_acts),
            mean_caselaw = mean(n_citations))
data %>%
  ggplot(aes(x = controversial, y = separate_opinion, color = judge_gender)) +
  facet_wrap(~judge_profession) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  theme_set(theme_bw(base_size = 10)) +
  theme(legend.position = "top") +
  labs(x = "", y = "Observed Probabilty of a separate opinion")


model_selection = data %>%
  group_by(formation) %>%
  count() %>%
  ungroup() %>%
  summarise(
    units = n(),
    mean_observations = median(n)
  )
model_selection

grounds_ratio = read_rds("../data/ccc_database/rds/ccc_metadata.rds") %>%
  group_by(year_decision, grounds) %>%
  count() %>%
  pivot_wider(names_from = grounds, values_from = n) %>%
  group_by(year_decision) %>%
  summarise(count = sum(admissibility, merits),
            ratio_grounds = scales::percent(merits/admissibility, accuracy = 0.01))
grounds_ratio

read_rds("../data/ccc_database/rds/ccc_metadata.rds") %>%
  filter(grounds != "procedural") %>%
  group_by(year_decision, grounds) %>%
  count() %>%
  ggplot(aes(x = year_decision, y = n, fill = grounds)) +
  geom_col()

caseload = read_rds("../data/ccc_database/rds/ccc_metadata.rds") %>%
  ggplot(aes(x = year(date_decision), fill = grounds)) +
  geom_bar(position = position_stack(reverse = TRUE))  +
  scale_x_continuous(breaks = seq(1991, 2023, 2)) +
  scale_fill_brewer(palette="Pastel1") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = NULL, y = NULL, fill = "Type of verdict") +
  scale_fill_grey()
caseload

separate_opinion_ratio = read_rds("../data/ccc_database/rds/ccc_metadata.rds") %>% 
  filter(!grounds %in% c("procedural")) %>%
  mutate(presence_dissent = if_else(is.na(as.character(separate_opinion)), 0, 1)) %>%
  group_by(year(date_decision), grounds) %>%
  summarise(sum(presence_dissent)/n()) %>%
  rename(Year = `year(date_decision)`,
           `Dissent` = `sum(presence_dissent)/n()`) %>%
  ggplot(aes(x = Year, y = Dissent, group = grounds, color = grounds)) + 
  geom_point() + 
  geom_line() +
  labs(y = "Percent of Decisions with at least one Separate Opinion", color = NULL) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_colour_grey()
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
  filter(grounds == "merits" | formation == "Plenum") %>%
  ggplot(aes(x = forcats::fct_infreq(presence_dissent))) +
  geom_bar() +
  labs(x = NULL, y = NULL)
dissents_prevalence

descr_workload = data %>%
  select(workload) %>%
  skimr::skim() %>%
  select(skim_variable, numeric.mean, numeric.sd, numeric.p0, numeric.p25, numeric.p50, numeric.p75, numeric.p100)
descr_workload

plot_workload = data %>%
  ggplot(aes(x = workload)) +
  geom_density() +
  facet_wrap(~judge_name)
plot_workload


# Descriptive Statistics --------------------------------------------------
overall_table = data_metadata %>%
  mutate(formation = if_else(formation == "Plenum", "Plenum", "Chamber"),
         presence_dissent = if_else(is.na(as.character(separate_opinion)), 0, 1)) %>%
  group_by(formation, grounds) %>%
  summarise(count = n(),
            ratio_total = scales::percent(x = n()/nrow(.), accuracy = 0.1),
            ratio_dissent = scales::percent(x = sum(presence_dissent)/n(), accuracy = 0.1)) %>%
  ungroup()
overall_table

professions = data_judges %>% 
  filter(judge_term_court != "4th") %>%
  ggplot(aes(x = forcats::fct_infreq(judge_profession))) +
  geom_bar() +
  labs(x = NULL, y = NULL) +
  facet_wrap(~judge_term_court)
professions

age_distribution = data_judges %>% 
  filter(judge_term_court != "4th") %>%
  mutate(age_term_end = year(judge_term_end) - judge_yob) %>%
  ggplot(aes(x = age_term_end)) +
  geom_density() +
  labs(x = "The age of a justice at the end of their term", y = "Density", colour = NULL)
age_distribution

# descriptive_dependent = data %>%
#   mutate(formation = if_else(formation == "Plenum", "Plenum", "Chamber"),
#          presence_dissent = if_else(is.na(as.character(separate_opinion)), 0, 1)) %>%
#   group_by(formation, grounds) %>%
#   summarise(n_dissents = mean(separate_opinion))
# descriptive_dependent

dependent_variables_overview = data %>%
  select(separate_opinion, judge_profession, n_citations, controversial) %>%
  group_by(judge_profession) %>%
  select(where(is.numeric)) %>%
  skimr::skim() %>%
  as_tibble() %>%
  select(c(skim_variable, judge_profession, numeric.mean, numeric.sd))
dependent_variables_overview

# save.image("report/descriptive_statistics.RData")

