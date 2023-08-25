# Load packages
library(tidyverse)
library(tidymodels)
library(ggplot2); theme_set(theme_minimal())
library(rstanarm)
library(rstan)
library(bayesplot)
library(bayesrules)
library(broom)
library(broom.mixed)
library(tidybayes)
library(lubridate)
library(foreach)
library(parallel)
library(multilevelmod)
tidymodels_prefer()

#Load data
# load(file = "report/data_dissents_article.RData")
data_citations = readr::read_rds("../data/US_citations.rds")
data_metadata = readr::read_rds("../data/US_metadata.rds") %>%
  mutate(year_submission = year(date_submission),
         year_decision = year(date_decision))
data_dissents = readr::read_rds("../data/US_dissents.rds")
data_judges = readr::read_rds("../data/US_judges.rds")
data_compositions = readr::read_rds("../data/US_compositions.rds")

# remove(data_citations, data_metadata, data_dissents, data_judges)
# Set the number of chains & cores
chains = 4L

# # SAVE
# save.image(file = "report/data_dissents_article.RData")
# readr::write_rds(model_hierarchical_absolute, "models/model_hierarchical_absolute.rds")
# readr::write_rds(model_pooled_absolute, "models/model_pooled_absolute.rds")
# readr::write_rds(com_pooled_mod_length_neg2, "models/com_pooled_mod_length_neg2.rds")

# RQ1: LENGTH X DISSENT
data_length = readr::read_rds("../data/data_paragraphs_doc2vec.rds") %>%
  left_join(., readr::read_rds(file = "../data/data_paragraphs_classified.rds"), by = join_by(doc_id, paragraph_id)) %>%
  select(doc_id, paragraph_id, text, class) %>%
  group_by(doc_id) %>%
  summarise(argument_length = sum(str_count(text, '\\w+'))) %>%
  left_join(., data_metadata %>% select(doc_id, formation, date_decision, year_decision, type_decision)) %>%
  left_join(., data_dissents %>%
              group_by(doc_id) %>%
              summarise(count_dissents = n())) %>%
  mutate(formation = case_when(
    formation == "Plenum" ~ "Plenum",
    .default = "Chamber"
  ),
  formation = factor(formation),
  count_dissents = replace_na(count_dissents,0),
  one_dissent = if_else(count_dissents == 1, 1, 0),
  more_dissent = if_else(count_dissents > 1, 1, 0),
  type_decision = factor(type_decision)) %>%
  select(-count_dissents)

data_length %>%
  ggplot(aes(x = argument_length)) +
  geom_density()

# All models
com_pooled_mod_length_neg2 = linear_reg() %>%
  set_engine("stan",
             chains = chains,
             family = "neg_binomial_2",
             iter = 5000*2,
             seed = 84735,
             prior_intercept = normal(5, 1),
             prior = normal(1, 1), 
             prior_aux = exponential(1, autoscale = TRUE),
             cores = chains) %>%
  set_mode("regression") %>% 
  fit(argument_length ~ one_dissent + more_dissent + formation + type_decision, data = data_length) %>% 
  extract_fit_engine()

com_pooled_mod_length_poisson = linear_reg() %>%
  set_engine("stan",
             family = poisson, 
             prior_intercept = normal(5, 1),
             prior = normal(1, 1), 
             chains = chains, 
             iter = 5000*2, 
             seed = 84735, cores = chains) %>%
  set_mode("regression") %>% 
  fit(argument_length ~ one_dissent + more_dissent + formation + type_decision, data = data_length) %>% 
  extract_fit_engine() 

# MCMC Diagnosis
mcmc_trace(com_pooled_mod_length_neg2)
mcmc_acf(com_pooled_mod_length_neg2, pars = c("one_dissent","more_dissent"))

# Posterior diagnosis
pp_check_length_negbinom = pp_check(com_pooled_mod_length_neg2) +
  xlim(0,20000) +
  labs(x = "words of the majority opinion argumentation",
       title = "Posterior predictive check of the Negative Binomial model",
       subtitle = "Dependence of length of majority opinion argumentation on presence of dissenting opinion")


data_length %>% 
  add_epred_draws(object = com_pooled_mod_length_neg2, ndraws = 4) %>%
  ggplot(aes(x = one_dissent, y = argument_length)) +
  geom_line(aes(y = .epred))

prediction_summary(model = com_pooled_mod_length_neg2, data = data_length)

# K-fold crossvalidation - In this case, the results are similar, suggesting that our model is not overfit to our sample data.
prediction_summary_cv_length = prediction_summary_cv(model = com_pooled_mod_length_neg2, data = data_length, k = 6)

# Parameters
mcmc_dens_overlay(com_pooled_mod_length_neg2, pars = c("one_dissent","more_dissent"))
mcmc_areas_length = mcmc_areas(com_pooled_mod_length_neg2, pars = c("one_dissent","more_dissent")) +
  labs(title = "Density plot of estimates of parameters of one or two and more dissents ",
       subtitle = "The inner area is for 50 % posterior credible interval, the outer for 95 %")
mcmc_dens(com_pooled_mod_length_neg2, pars = c("one_dissent","more_dissent"))

mcmc_intervals_length = mcmc_intervals(com_pooled_mod_length_neg2, pars = c("one_dissent","more_dissent")) +
  labs(title = "Plot of uncertainty intervals of estimates of parameters of one or two and more dissents ",
       subtitle = "The inner whisker is for 50 % posterior credible interval, the outer for 95 %")

# Save the output parameters of the model
output_length = tidy(com_pooled_mod_length_neg2, 
                     conf.int = TRUE, 
                     conf.level = 0.80)

