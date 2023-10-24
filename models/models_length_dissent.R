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
library(doMC)
# library(patchwork)
registerDoMC(cores = parallel::detectCores() - 2)

chains = 4L

#Load data filter all Plenum decisions and chamber decisions on merits
data_metadata = readr::read_rds("../data/US_metadata.rds") %>%
  mutate(year_submission = year(date_submission),
         year_decision = year(date_decision)) %>%
  filter(!grepl(pattern = "procesní", x = .$type_verdict)) %>%
  filter(formation == "Plenum" | (formation %in% c("First Chamber", "Second Chamber", "Third Chamber") & type_decision != "Usnesení"))

data_dissents = readr::read_rds("../data/US_dissents.rds")

# RQ1: LENGTH X DISSENT
data_length = readr::read_rds(file = "../data/data_paragraphs_classified.rds") %>%
  filter(doc_id %in% data_metadata$doc_id) %>%
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
  select(-count_dissents) %>%
  left_join(., read_rds("../data/US_citations.rds") %>%
              group_by(doc_id) %>%
              count() %>%
              rename(n_citations = n)) %>%
  mutate(n_citations = replace_na(n_citations, 0))

data_length %>%
  ggplot(aes(x = argument_length)) +
  geom_density()
# 
# # Test correlation between importance proxied by number of citations and by formation of the CC
# final_distributions = inner_join(
#   readr::read_rds("../data/US_metadata.rds") %>%
#     mutate(year_submission = year(date_submission),
#            year_decision = year(date_decision)) %>%
#     select(formation) %>%
#     drop_na() %>%
#     mutate(formation = case_when(
#       formation == "Plenum" ~ "Plenary",
#       .default = "Panel"
#     )) %>%
#     group_by(formation) %>%
#     summarise(Count_total = n()) %>%
#     mutate(Percent_total = paste(round(
#       100 * Count_total / sum(Count_total), 1
#     ), "%")),
#   readr::read_rds("../data/US_citations.rds") %>%
#     left_join(
#       .,
#       y = readr::read_rds("../data/US_metadata.rds") %>%
#         select(case_id, formation),
#       by = join_by(matched_case_id == case_id)
#     ) %>%
#     drop_na() %>%
#     mutate(formation = case_when(
#       formation == "Plenum" ~ "Plenary",
#       .default = "Panel"
#     )) %>%
#     group_by(formation) %>%
#     summarise(Count_cited = n()) %>%
#     mutate(Percent_cited = paste(round(
#       100 * Count_cited / sum(Count_cited), 1
#     ), "%")),
#   by = join_by(formation)
# )
# final_distributions

# Prior intercept mean - the length of a typical decision
prior_intercept = log(mean(data_length$argument_length))

# All models
com_pooled_mod_length_neg2 = linear_reg() %>%
  set_engine("stan",
             chains = chains,
             family = "neg_binomial_2",
             iter = 5000*2,
             seed = 84735,
             prior_intercept = normal(prior_intercept, 1),
             prior = normal(0, 2.5, autoscale = TRUE), # uninformative priors
             prior_aux = exponential(1, autoscale = TRUE),
             cores = chains) %>%
  set_mode("regression") %>% 
  fit(argument_length ~ one_dissent + more_dissent + formation + n_citations, data = data_length) %>% 
  extract_fit_engine()

com_pooled_mod_length_poisson = linear_reg() %>%
  set_engine("stan",
             family = poisson, 
             prior_intercept = normal(prior_intercept, 1),
             prior = normal(0, 2.5, autoscale = TRUE), # uninformative priors 
             chains = chains, 
             iter = 5000*2, 
             seed = 84735, cores = chains) %>%
  set_mode("regression") %>% 
  fit(argument_length ~ one_dissent + more_dissent + formation + n_citations, data = data_length) %>% 
  extract_fit_engine()

# MCMC Diagnosis
mcmc_trace(com_pooled_mod_length_neg2)
mcmc_acf(com_pooled_mod_length_neg2, pars = c("one_dissent","more_dissent"))

# Posterior diagnosis
pp_check_length_negbinom = pp_check(com_pooled_mod_length_neg2) +
  xlim(0,20000)
pp_check_length_negbinom


data_length %>% 
  add_epred_draws(object = com_pooled_mod_length_neg2, ndraws = 4) %>%
  ggplot(aes(x = one_dissent, y = argument_length)) +
  geom_line(aes(y = .epred))

# Posterior accuracy check
predictions = posterior_predict(com_pooled_mod_length_neg2, newdata = data_length)
ppc_intervals(data_length$argument_length, yrep = predictions, 
              prob = 0.5, prob_outer = 0.95)

prediction_summary_length_neg2 = prediction_summary(model = com_pooled_mod_length_neg2, data = data_length) %>% as_tibble() %>% mutate(model = "neg_bin")
prediction_summary_length_poisson = prediction_summary(model = com_pooled_mod_length_poisson, data = data_length) %>% as_tibble() %>% mutate(model = "poisson")
prediction_summary_length = bind_rows(prediction_summary_length_poisson, prediction_summary_length_neg2) %>%
  relocate(model, .before = mae)

# K-fold crossvalidation - In this case, the results are similar, suggesting that our model is not overfit to our sample data.
prediction_summary_cv_length = prediction_summary_cv(model = com_pooled_mod_length_neg2, data = data_length, k = 10)

# Parameters
mcmc_areas_length = mcmc_areas(com_pooled_mod_length_neg2, pars = c("one_dissent","more_dissent"))
mcmc_areas_length

# mcmc_intervals_length = mcmc_intervals(com_pooled_mod_length_neg2, pars = c("one_dissent","more_dissent")) +
#   labs(title = "Fig 3: Plot of uncertainty intervals of estimates of parameters of one or two and more dissents ",
#        subtitle = "The inner whisker is for 50 % posterior credible interval, the outer for 95 %") +
#   theme(plot.title = element_text(hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5))
# mcmc_intervals_length


# Save the output parameters of the model
output_length = tidy(com_pooled_mod_length_neg2, 
                     conf.int = TRUE, 
                     conf.level = 0.80) %>%
  mutate(across(where(is.numeric), ~exp(.x))) %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 2)))
output_length


negbin_distribution = data_length %>%
  ggplot(aes(x = argument_length)) +
  geom_density() +
  xlim(0,10000)

# SAVE data
rm(list=ls(pattern="^data_"))
save.image(file = "models/models_fitted_length_dissent.RData")

# LOAD data
load(file = "models/models_fitted_length_dissent.RData")

