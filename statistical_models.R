# Load packages
library(tidyverse)
library(rstanarm)
library(rstan)
library(bayesplot)
library(bayesrules)
library(broom)
library(broom.mixed)
library(tidybayes)
library(lubridate)

# Load data
load(file = "report/data_dissents_article.RData")
data_citations = readRDS("../data/US_citations.rds")
data_metadata = readRDS("../data/US_metadata.rds")
data_dissents = readRDS("../data/US_dissents.rds")
data_judges = readRDS("../data/US_judges.rds")

remove(data_citations, data_metadata, data_dissents, data_judges)
# Set the number of chains & cores
chains = 4L

# # SAVE
save.image(file = "report/data_dissents_article.RData")
# saveRDS(model_hierarchical_absolute, "models/model_hierarchical_absolute.rds")
# saveRDS(model_pooled_absolute, "models/model_pooled_absolute.rds")
# saveRDS(model_pooled_length, "models/model_pooled_length.rds")

# LENGTH X DISSENT
# data_length = readRDS("../data/data_paragraphs_doc2vec.rds") %>% 
#   left_join(., readRDS(file = "../data/data_paragraphs_classified.rds"), by = join_by(doc_id, paragraph_id)) %>%
#   select(doc_id, paragraph_id, text, class)
# data_dissents = readRDS("../data/US_dissents.rds")
# 
# data_dissents = data_dissents %>%
#   group_by(doc_id) %>%
#   summarise(count_dissents = n())
# 
# data_length = data_length %>%
#   group_by(doc_id) %>%
#   summarise(argument_length = sum(str_count(text, '\\w+'))) %>%
#   left_join(., data_metadata %>% select(doc_id, formation, date_decision, year_decision, type_decision)) %>%
#   left_join(., data_dissents) %>%
#   mutate(formation = case_when(
#     formation == "Plenum" ~ "Plenum",
#     .default = "Chamber"
#   ),
#   formation = factor(formation),
#   count_dissents = replace_na(count_dissents,0),
#   one_dissent = if_else(count_dissents == 1, 1, 0),
#   more_dissent = if_else(count_dissents > 1, 1, 0),
#   type_decision = factor(type_decision)) %>%
#   select(-count_dissents)

data_length %>%
  ggplot(aes(x = argument_length)) +
  geom_density()




# Completely pooled model with all variables stored in absolute terms
model_pooled_length = stan_glm(
  argument_length ~ one_dissent + more_dissent + formation + type_decision,
  data = data_length, family = neg_binomial_2, 
  prior_intercept = normal(5, 1),
  prior = normal(1, 1), 
  prior_aux = exponential(1, autoscale = TRUE),
  chains = chains, iter = 5000*2, seed = 84735, cores = chains)

model_pooled_length_poisson = stan_glm(
  argument_length ~ one_dissent + more_dissent + formation + type_decision,
  data = data_length, family = poisson, 
  prior_intercept = normal(5, 1),
  prior = normal(1, 1), 
  chains = chains, iter = 5000*2, seed = 84735, cores = chains)
 

 pp_check(model_pooled_length_poisson)
  
  

# MCMC Diagnosis
mcmc_trace(model_pooled_length)
mcmc_acf(model_pooled_length, pars = c("one_dissent","more_dissent"))

# Posterior diagnosis
pp_check_length_negbinom = pp_check(model_pooled_length) +
  xlim(0,20000) +
  labs(x = "words of the majority opinion argumentation",
       title = "Posterior predictive check of the Negative Binomial model",
       subtitle = "Dependence of length of majority opinion argumentation on presence of dissenting opinion")

data_length %>% 
  add_epred_draws(object = model_pooled_length, ndraws = 4) %>%
  ggplot(aes(x = one_dissent, y = argument_length)) +
  geom_line(aes(y = .epred))

prediction_summary(model = model_pooled_length, data = data_length)

# K-fold crossvalidation - In this case, the results are similar, suggesting that our model is not overfit to our sample data.
prediction_summary_cv_length = prediction_summary_cv(model = model_pooled_length, data = data_length, k = 6)

# Parameters
mcmc_dens_overlay(model_pooled_length, pars = c("one_dissent","more_dissent"))
mcmc_areas_length = mcmc_areas(model_pooled_length, pars = c("one_dissent","more_dissent")) +
  labs(title = "Density plot of estimates of parameters of one or two and more dissents ",
       subtitle = "The inner area is for 95 % confidence interval, the outer for 50 %")
mcmc_dens(model_pooled_length, pars = c("one_dissent","more_dissent"))

mcmc_intervals_length = mcmc_intervals(model_pooled_length, pars = c("one_dissent","more_dissent")) +
  labs(title = "Plot of uncertainty intervals of estimates of parameters of one or two and more dissents ",
       subtitle = "The inner whisker is for 95 % confidence interval, the outer for 50 %")

# Save the output parameters of the model
output_workload = tidy(model_pooled_length, 
              conf.int = TRUE, 
              conf.level = 0.80)


# DISSENT RATE X WORKLOAD
# PLOTS
negbin_distribution = data_length %>%
  ggplot(aes(x = argument_length)) +
  geom_density() +
  xlim(0,10000) +
  labs(x = "words of the majority opinion argumentation",
       title = "Distribution of number of words of argumentation of the majority")
# Create the dataset
data_dissents = data_dissents %>%
  left_join(., data_metadata %>% select(doc_id, year_decision, formation)) %>%
  group_by(dissenting_judge, year_decision) %>%
  summarise(count_dissents = n())

data_caseload = data_metadata %>%
  group_by(judge_rapporteur, year_decision) %>%
  summarise(caseload = n())

data = right_join(data_dissents, data_caseload, by = join_by(dissenting_judge==judge_rapporteur, year_decision)) %>%
  replace(is.na(.), 0) %>%
  mutate(year_decision = factor(year_decision),
         dissenting_judge = factor(dissenting_judge))


data = data %>% mutate(dissenting_judge = factor(dissenting_judge))
# See the development of caseload overtime
  data %>%
    group_by(year_decision) %>%
    summarise(caseload = sum(caseload)) %>%
    mutate(year_decision = as.numeric(year_decision)) %>%
    ungroup() %>%
  ggplot(aes(x = year_decision, y = caseload)) +
  geom_point() +
  geom_smooth()
  
# See the development of number of dissents overtime
  data %>%
    group_by(year_decision) %>%
    summarise(count_dissents = sum(count_dissents)) %>%
    mutate(year_decision = as.numeric(year_decision)) %>%
    ungroup() %>%
    ggplot(aes(x = year_decision, y = count_dissents)) +
    geom_point() +
    geom_smooth()
  
  data %>%
    group_by(year_decision) %>%
    summarise(ratio = sum(count_dissents)/sum(caseload)) %>%
    mutate(year_decision = as.numeric(year_decision)) %>%
    ggplot(aes(x = year_decision, y = ratio)) +
    geom_point() +
    geom_smooth(method = "lm")


# Test correlation between importance proxied by number of citations and by formation of the CC
citations_distribution = data_citations %>% 
  left_join(., y = data_metadata %>% 
              select(case_id, formation), by = join_by(matched_case_id == case_id)) %>%
  drop_na() %>%
  mutate(formation = case_when(
    formation == "Plenum" ~ "Plenary",
    .default = "Panel"
  )) %>%
  group_by(formation) %>%
  summarise(Count_cited = n()) %>%
  mutate(Percent_cited = paste(round(100*Count_cited/sum(Count_cited), 1), "%"))

cases_distributions = data_metadata %>%
  select(formation) %>% 
  drop_na() %>%
  mutate(formation = case_when(
    formation == "Plenum" ~ "Plenary",
    .default = "Panel"
  )) %>%
  group_by(formation) %>%
  summarise(Count_total = n()) %>%
  mutate(Percent_total = paste(round(100*Count_total/sum(Count_total), 1), "%"))

final_distributions = inner_join(cases_distributions, citations_distribution, by = join_by(formation))
final_distributions

# Model 1 Caseload x Dissent


# Data exploration - to me there doesn't seem to be good linear relationship between the two
ggplot(data, aes(x = caseload, y = count_dissents)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~dissenting_judge)

# Check the structure of your variables
ggplot(data = data, aes(x = caseload)) +
  geom_density()

ggplot(data = data, aes(x = count_dissents)) +
  geom_density()

ggplot(data = data, aes(x = rate_dissents)) +
  geom_density()

# Create rate variables
data %<>% 
  group_by(dissenting_judge) %>% 
  arrange(dissenting_judge, year_decision) %>% 
  mutate(roc_caseload = 100 * (caseload - lag(caseload))/lag(caseload),
         rate_dissents = count_dissents/caseload,
         log_roc_raseload = log(roc_caseload),
         log_rate_dissents = log(rate_dissents)) %>%
  ungroup()


# Completely pooled model with all variables stored in absolute terms
model_pooled_absolute = stan_glm(
  count_dissents ~ caseload + year_decision,
  data = data, family = poisson, 
  prior_intercept = normal(0, 0.5),
  prior = normal(0, 5, autoscale = TRUE), 
  chains = 4, iter = 10000*2, seed = 84735)

# Completely pooled model with the count_dissents transformed into a rate
model_pooled_rate = update(model_pooled_absolute, 
                               formula = count_dissents ~ roc_caseload + year_decision + offset(log(caseload)))

# Hierarchical model with all variables stored in absolute terms
model_hierarchical_absolute = stan_glmer(
  count_dissents ~ caseload + year_decision + (1 | dissenting_judge),
  data = data, family = poisson, 
  prior_intercept = normal(0, 0.5),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 10000*2, seed = 84735)

# Hierarchical model with all variables of interest stored as rates
model_hierarchical_rate = update(model_hierarchical_absolute, 
                                     formula = count_dissents ~ roc_caseload + year_decision + offset(log(caseload)))

# THE POOLED MODEL
# ABSOLUTE
# MCMC Diagnosis
mcmc_trace(model_pooled_absolute)
mcmc_acf(model_pooled_absolute, pars = "caseload")

# Posterior diagnosis
pp_check(model_pooled_absolute) +
  xlab("n_dissents")

data %>% 
  add_epred_draws(object = model_pooled_absolute, ndraws = 4) %>%
  ggplot(aes(x = caseload, y = count_dissents)) +
  geom_line(aes(y = .epred, group = paste(dissenting_judge, .draw)))

prediction_summary(model = model_pooled_absolute, data = data)

# K-fold crossvalidation - In this case, the results are similar, suggesting that our model is not overfit to our sample data.
prediction_summary_cv(model = model_pooled_absolute, data = data, k = 6)

# Parameters
mcmc_dens_overlay(model_pooled_absolute, pars = "roc_caseload")
mcmc_areas(model_pooled_absolute, pars = "roc_caseload")
mcmc_dens(model_pooled_absolute, pars = "roc_caseload")

mcmc_intervals(model_pooled_absolute, pars = "caseload")
tidy(model_pooled_absolute, 
     conf.int = TRUE, conf.level = 0.80)

# RATE
# MCMC Diagnosis
mcmc_trace(model_pooled_rate)
mcmc_acf(model_pooled_rate, pars = "caseload")

# Posterior diagnosis
pp_check(model_pooled_rate) +
  xlab("n_dissents")

data %>% 
  add_epred_draws(object = model_pooled_rate, ndraws = 4) %>%
  ggplot(aes(x = caseload, y = count_dissents)) +
  geom_line(aes(y = .epred, group = paste(dissenting_judge, .draw)))

prediction_summary(model = model_pooled_rate, data = data)

# K-fold crossvalidation - In this case, the results are similar, suggesting that our model is not overfit to our sample data.
prediction_summary_cv(model = model_pooled_rate, data = data, k = 6)

# Parameters
mcmc_dens_overlay(model_pooled_rate, pars = "roc_caseload")
mcmc_areas(model_pooled_rate, pars = "roc_caseload")
mcmc_dens(model_pooled_rate, pars = "roc_caseload")

mcmc_intervals(model_pooled_rate, pars = "caseload")
tidy(model_pooled_rate, 
     conf.int = TRUE, conf.level = 0.80)


# THE HIERARCHICAL MODEL
# ABSOLUTE
# MCMC Diagnosis
mcmc_trace(model_hierarchical_absolute)
mcmc_acf(model_hierarchical_absolute, pars = "caseload")

# Parameters
mcmc_dens_hierarchical_absolute = mcmc_dens_overlay(model_hierarchical_absolute, pars = "caseload")
mcmc_areas_hierarchical_absolute = mcmc_areas(model_hierarchical_absolute, pars = "caseload")  +
  labs(title = "Density plot of estimates of parameters of workload of a judge",
       subtitle = "The inner area is for 95 % confidence interval, the outer for 50 %")
mcmc_dens(model_hierarchical_absolute, pars = "caseload")

mcmc_intervals_hierarchical_absolute = mcmc_intervals(model_hierarchical_absolute, pars = "caseload") +
  labs(title = "Plot of uncertainty intervals of estimates of parameters oof workload of a judge",
       subtitle = "The inner whisker is for 95 % confidence interval, the outer for 50 %")

output_hierarchical_absolute = tidy(model_hierarchical_absolute, conf.int = TRUE, conf.level = 0.8, effects = "fixed") %>%
  filter(term %in% c("(Intercept)", "caseload"))

# Posterior diagnosis
pp_check_hierarchical_absolute = pp_check(model_hierarchical_absolute) +
  labs(x = "Dissent count",
       title = "Posterior predictive check of the absolute terms hierarchical model")

data %>% 
  add_epred_draws(object = model_hierarchical_absolute, ndraws = 4) %>%
  ggplot(aes(x = caseload, y = count_dissents)) +
  geom_line(aes(y = .epred, group = paste(dissenting_judge, .draw)))

# Accuracy of the model
prediction_summary(model = model_hierarchical_absolute, data = data)

# K-fold crossvalidation - In this case, the results are similar, suggesting that our model is not overfit to our sample data.
prediction_summary_cv_workload = prediction_summary_cv(model = model_hierarchical_absolute, data = data, group = "dissenting_judge", k = 6)

  # RATE
# MCMC Diagnosis
mcmc_trace(model_hierarchical_rate)
mcmc_acf(model_hierarchical_rate, pars = "caseload")

# Parameters
mcmc_dens_overlay(model_hierarchical_rate, pars = "caseload")
mcmc_areas(model_hierarchical_rate, pars = "caseload")
mcmc_dens(model_hierarchical_rate, pars = "caseload")

mcmc_intervals(model_hierarchical_rate, pars = "caseload")
tidy(model_hierarchical_rate, 
     conf.int = TRUE, conf.level = 0.80)

# Posterior diagnosis
pp_check(model_hierarchical_rate) +
  xlab("n_dissents")

data %>% 
  add_epred_draws(object = model_hierarchical_rate, ndraws = 4) %>%
  ggplot(aes(x = caseload, y = count_dissents)) +
  geom_line(aes(y = .epred, group = paste(dissenting_judge, .draw)))

# Accuracy of the model
prediction_summary(model = model_hierarchical_rate, data = data)

# K-fold crossvalidation - In this case, the results are similar, suggesting that our model is not overfit to our sample data.
prediction_summary_cv(model = model_hierarchical_rate, data = data, k = 6)

# caseload_submission = data_dissents %>%
#   # left_join(., data_metadata %>% select(doc_id, date_submission)) %>%
#   # mutate(year_submission = year(date_submission)) %>%
#   # group_by(dissenting_judge, year_submission) %>%
#   # summarise(count_dissents = n())


# END OF TERM X DISSENT BEHAVIOR
data_term = data_judges %>%
  mutate(end = case_when(
    is.na(end) & reelection == 1 ~ start %m+% years(20),
    is.na(end) & reelection == 0 ~ start %m+% years(10),
    .default = end
  )) %>%
  select(judge_name, yob, gender, start, end, reelection) %>%
  left_join(data, ., by = join_by("dissenting_judge" == "judge_name")) %>%
  mutate(start = year(start),
         end = year(end),
         end_term = if_else(end - as.numeric(as.character(year_decision)) < 3, 1, 0),
         start_term = if_else(end - as.numeric(as.character(year_decision)) > 7, 1, 0),
         dissenting_judge = factor(dissenting_judge))

# Model
model_term = stan_glmer(
  count_dissents ~ end_term + start_term + reelection + (1 | dissenting_judge),
  data = data_term, family = poisson, 
  prior_intercept = normal(0, 0.5),
  prior = normal(0, 5, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = chains, iter = 10000*2, seed = 84735, cores = chains)

# MCMC Diagnosis
mcmc_trace(model_term)
mcmc_acf(model_term)

# Parameters
mcmc_dens_term = mcmc_dens_overlay(model_term, pars = c("start_term", "end_term"))
mcmc_areas_term = mcmc_areas(model_term, pars = c("start_term", "end_term"))
mcmc_dens(model_term, pars = c("start_term", "end_term"))

mcmc_intervals_term = mcmc_intervals(model_term, pars = c("start_term", "end_term"))

output_term = tidy(model_term, conf.int = TRUE, conf.level = 0.8, effects = "fixed")

# Posterior diagnosis
pp_check_term = pp_check(model_term) +
  labs(x = "Dissent count",
       title = "Posterior predictive check of the absolute terms hierarchical model")

data_term %>% 
  add_epred_draws(object = model_term, ndraws = 4) %>%
  ggplot(aes(x = end_term, y = count_dissents)) +
  geom_line(aes(y = .epred, group = paste(dissenting_judge, .draw)))

# Accuracy of the model
prediction_summary(model = model_term, data = data_term)

# K-fold crossvalidation - In this case, the results are similar, suggesting that our model is not overfit to our sample data.
prediction_summary_cv_term = prediction_summary_cv(data = data_term, model = model_term, group = "dissenting_judge", k = 6)

folds = 6
kfold(x = model_term, K = folds, cores = folds)


