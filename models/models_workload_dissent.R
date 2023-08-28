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
registerDoMC(cores = parallel::detectCores() - 2)

# Set the number of chains & cores
chains = 4L

#Load data
data_metadata = readr::read_rds("../data/US_metadata.rds") %>%
  mutate(year_submission = year(date_submission),
         year_decision = year(date_decision)) %>% 
  

# Create the data CASELOAD
data_dissents = readr::read_rds("../data/US_dissents.rds") %>%
  left_join(., data_metadata %>% select(doc_id, formation, year_submission)) %>%
  filter(year_submission < 2023) %>%
  group_by(dissenting_judge, year_submission) %>%
  summarise(count_dissents = n())

data_caseload = data_metadata %>%
  group_by(judge_rapporteur_name, year_submission) %>%
  summarise(caseload = n())

data_dissents_caseload = right_join(data_dissents, data_caseload, by = join_by(dissenting_judge==judge_rapporteur_name, year_submission)) %>%
  replace(is.na(.), 0) %>%
  mutate(year_submission = factor(year_submission),
         dissenting_judge = factor(dissenting_judge)) %>%
  group_by(dissenting_judge) %>% 
  arrange(dissenting_judge, year_submission) %>% 
  mutate(roc_caseload = 100 * (caseload - dplyr::lag(caseload))/dplyr::lag(caseload),
         rate_dissents = count_dissents/caseload,
         log_roc_raseload = log(roc_caseload),
         log_rate_dissents = log(rate_dissents)) %>%
  ungroup()

# See the development of caseload overtime
data_dissents_caseload %>%
  group_by(year_submission) %>%
  summarise(caseload = sum(caseload)) %>%
  mutate(year_submission = as.numeric(year_submission)) %>%
  ungroup() %>%
  ggplot(aes(x = year_submission, y = caseload)) +
  geom_point() +
  geom_smooth()

# See the development of number of dissents overtime
data_dissents_caseload %>%
  group_by(year_submission) %>%
  summarise(count_dissents = sum(count_dissents)) %>%
  mutate(year_submission = as.numeric(year_submission)) %>%
  ungroup() %>%
  ggplot(aes(x = year_submission, y = count_dissents)) +
  geom_point() +
  geom_smooth()

# development of total dissents to caseload ratio
data_dissents_caseload %>%
  group_by(year_submission) %>%
  summarise(ratio = sum(count_dissents)/sum(caseload)) %>%
  mutate(year_submission = as.numeric(year_submission)) %>%
  ggplot(aes(x = year_submission, y = ratio)) +
  geom_point() +
  geom_smooth(method = "lm") 


# Test correlation between importance proxied by number of citations and by formation of the CC
final_distributions = inner_join(data_metadata %>%
                                   select(formation) %>% 
                                   drop_na() %>%
                                   mutate(formation = case_when(
                                     formation == "Plenum" ~ "Plenary",
                                     .default = "Panel"
                                   )) %>%
                                   group_by(formation) %>%
                                   summarise(Count_total = n()) %>%
                                   mutate(Percent_total = paste(round(100*Count_total/sum(Count_total), 1), "%")), readr::read_rds("../data/US_citations.rds") %>% 
                                   left_join(., y = data_metadata %>% 
                                               select(case_id, formation), by = join_by(matched_case_id == case_id)) %>%
                                   drop_na() %>%
                                   mutate(formation = case_when(
                                     formation == "Plenum" ~ "Plenary",
                                     .default = "Panel"
                                   )) %>%
                                   group_by(formation) %>%
                                   summarise(Count_cited = n()) %>%
                                   mutate(Percent_cited = paste(round(100*Count_cited/sum(Count_cited), 1), "%")), 
                                 by = join_by(formation))
final_distributions
remove(cases_distributions)

# Exploratory analysis

# Data exploration - to me there doesn't seem to be good linear relationship between the two
ggplot(data_dissents_caseload, aes(x = caseload, y = count_dissents)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~dissenting_judge)

# Check the structure of your variables
ggplot(data = data_dissents_caseload, aes(x = caseload)) +
  geom_density()

ggplot(data = data_dissents_caseload, aes(x = count_dissents)) +
  geom_density()

ggplot(data = data_dissents_caseload, aes(x = rate_dissents)) +
  geom_density()

# MODELS 
model_caseload_pooled_abs = linear_reg() %>%
  set_engine("stan",
             family = poisson,
             prior_intercept = normal(0, 0.5),
             prior = normal(0, 5, autoscale = TRUE), 
             chains = chains, 
             iter = 10000*2, 
             seed = 84735,
             cores = chains) %>%
  set_mode("regression") %>%
  fit(count_dissents ~ caseload + year_submission,
      data = data_dissents_caseload) %>%
  extract_fit_engine()

model_caseload_pooled_rate = linear_reg() %>%
  set_engine("stan",
             family = poisson,
             prior_intercept = normal(0, 0.5),
             prior = normal(0, 5, autoscale = TRUE), 
             chains = chains, 
             iter = 10000*2, 
             seed = 84735,
             cores = chains) %>%
  set_mode("regression") %>%
  fit(count_dissents ~ roc_caseload + year_submission + offset(log(caseload)),
      data = data_dissents_caseload) %>%
  extract_fit_engine()

model_caseload_hierarchical_abs = linear_reg() %>%
  set_engine("stan_glmer",
             family = poisson, 
             prior_intercept = normal(0, 0.5),
             prior = normal(0, 2.5, autoscale = TRUE), 
             prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
             chains = 4, 
             iter = 10000*2, 
             seed = 84735,
             cores = chains) %>%
  fit(count_dissents ~ caseload + year_submission + (1 | dissenting_judge), data = data_dissents_caseload) %>%
  extract_fit_engine()

model_caseload_hierarchical_rate = linear_reg() %>%
  set_engine("stan_glmer",
             family = poisson, 
             prior_intercept = normal(0, 0.5),
             prior = normal(0, 2.5, autoscale = TRUE), 
             prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
             chains = 4, 
             iter = 10000*2, 
             seed = 84735,
             cores = chains) %>%
  fit(formula = count_dissents ~ roc_caseload + year_submission + offset(log(caseload)) + (1 | dissenting_judge), data = data_dissents_caseload) %>%
  extract_fit_engine()


# THE POOLED MODEL
# ABSOLUTE
# MCMC Diagnosis
mcmc_trace(model_caseload_pooled_abs)
mcmc_acf(model_caseload_pooled_abs, pars = "caseload")

# Posterior diagnosis
pp_check(model_caseload_pooled_abs) +
  xlab("n_dissents")

data_dissents_caseload %>% 
  add_epred_draws(object = model_caseload_pooled_abs, ndraws = 4) %>%
  ggplot(aes(x = caseload, y = count_dissents)) +
  geom_line(aes(y = .epred, group = paste(dissenting_judge, .draw)))

prediction_summary(model = model_caseload_pooled_abs, data = data_dissents_caseload)

# K-fold crossvalidation - In this case, the results are similar, suggesting that our model is not overfit to our sample data.
prediction_summary_cv(model = model_caseload_pooled_abs, data = data_dissents_caseload, k = 6)

# Parameters
mcmc_dens_overlay(model_caseload_pooled_abs, pars = "roc_caseload")
mcmc_areas(model_caseload_pooled_abs, pars = "roc_caseload")
mcmc_dens(model_caseload_pooled_abs, pars = "roc_caseload")

mcmc_intervals(model_caseload_pooled_abs, pars = "caseload")
tidy(model_caseload_pooled_abs, 
     conf.int = TRUE, conf.level = 0.80)

# RATE
# MCMC Diagnosis
mcmc_trace(model_caseload_pooled_rate)
mcmc_acf(model_caseload_pooled_rate, pars = "caseload")

# Posterior diagnosis
pp_check(model_caseload_pooled_rate) +
  xlab("n_dissents")

data_dissents_caseload %>% 
  add_epred_draws(object = model_caseload_pooled_rate, ndraws = 4) %>%
  ggplot(aes(x = caseload, y = count_dissents)) +
  geom_line(aes(y = .epred, group = paste(dissenting_judge, .draw)))

prediction_summary(model = model_caseload_pooled_rate, data = data_dissents_caseload)

# K-fold crossvalidation - In this case, the results are similar, suggesting that our model is not overfit to our sample data.
prediction_summary_cv(model = model_caseload_pooled_rate, data = data_dissents_caseload, k = 6)

# Parameters
mcmc_dens_overlay(model_caseload_pooled_rate, pars = "roc_caseload")
mcmc_areas(model_caseload_pooled_rate, pars = "roc_caseload")
mcmc_dens(model_caseload_pooled_rate, pars = "roc_caseload")

mcmc_intervals(model_caseload_pooled_rate, pars = "caseload")
tidy(model_caseload_pooled_rate, 
     conf.int = TRUE, conf.level = 0.80)


# THE HIERARCHICAL MODEL
# ABSOLUTE
# MCMC Diagnosis
mcmc_trace(model_caseload_hierarchical_abs)
mcmc_acf(model_caseload_hierarchical_abs, pars = "caseload")

# Parameters
mcmc_dens_hierarchical_absolute = mcmc_dens_overlay(model_caseload_hierarchical_abs, pars = "caseload")
mcmc_areas_hierarchical_absolute = mcmc_areas(model_caseload_hierarchical_abs, pars = "caseload")  +
  labs(title = "Density plot of estimates of parameters of workload of a judge",
       subtitle = "The inner area is for 50 % posterior credible interval, the outer for 95 %")
mcmc_dens(model_caseload_hierarchical_abs, pars = "caseload")

mcmc_intervals_hierarchical_absolute = mcmc_intervals(model_caseload_hierarchical_abs, pars = "caseload") +
  labs(title = "Plot of uncertainty intervals of estimates of parameters of workload of a judge",
       subtitle = "The inner whisker is for 50 % posterior credible interval, the outer for 95 %")

output_caseload = tidy(model_caseload_hierarchical_abs, conf.int = TRUE, conf.level = 0.8, effects = "fixed") %>%
  filter(term %in% c("(Intercept)", "caseload")) %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 2)))

# Posterior diagnosis
pp_check_hierarchical_absolute = pp_check(model_caseload_hierarchical_abs) +
  labs(x = "Dissent count",
       title = "Posterior predictive check of the absolute terms hierarchical model")

data_dissents_caseload %>% 
  add_epred_draws(object = model_caseload_hierarchical_abs, ndraws = 4) %>%
  ggplot(aes(x = caseload, y = count_dissents)) +
  geom_line(aes(y = .epred, group = paste(dissenting_judge, .draw)))

# Accuracy of the model
prediction_summary(model = model_caseload_hierarchical_abs, data = data_dissents_caseload)

# K-fold crossvalidation - In this case, the results are similar, suggesting that our model is not overfit to our sample data.
prediction_summary_cv_case;pad= prediction_summary_cv(model = model_caseload_hierarchical_abs, data = data_dissents_caseload, group = "dissenting_judge", k = 6)

# RATE
# MCMC Diagnosis
mcmc_trace(model_caseload_hierarchical_rate)
mcmc_acf(model_caseload_hierarchical_rate, pars = "caseload")

# Parameters
mcmc_dens_overlay(model_caseload_hierarchical_rate, pars = "caseload")
mcmc_areas(model_caseload_hierarchical_rate, pars = "caseload")
mcmc_dens(model_caseload_hierarchical_rate, pars = "caseload")

mcmc_intervals(model_caseload_hierarchical_rate, pars = "caseload")
tidy(model_caseload_hierarchical_rate, 
     conf.int = TRUE, conf.level = 0.80)

# Posterior diagnosis
pp_check(model_caseload_hierarchical_rate) +
  xlab("n_dissents")

data_dissents_caseload %>% 
  add_epred_draws(object = model_caseload_hierarchical_rate, ndraws = 4) %>%
  ggplot(aes(x = caseload, y = count_dissents)) +
  geom_line(aes(y = .epred, group = paste(dissenting_judge, .draw)))

# Accuracy of the model
prediction_summary(model = model_caseload_hierarchical_rate, data = data_dissents_caseload)

# K-fold crossvalidation - In this case, the results are similar, suggesting that our model is not overfit to our sample data.
prediction_summary_cv(model = model_caseload_hierarchical_rate, data = data_dissents_caseload, k = 6)



# Create the data WORKLOAD
data_dissents_workload = readr::read_rds("../data/US_compositions.rds") %>%
  left_join(., readr::read_rds("../data/US_dissents.rds") %>% select(-judge_id) %>% mutate(dissent = 1), by = join_by(doc_id, judge == dissenting_judge))  %>%
  mutate(dissent = replace_na(dissent, 0)) %>%
  left_join(., data_metadata %>% select(doc_id, date_decision, date_submission))

data_dissents_workload = foreach(i = seq_along(data_dissents_workload$doc_id), .combine = "bind_rows", .packages = c("tidyverse")) %dopar% {
  output =  data_metadata %>%
    filter(date_decision > data_dissents_workload$date_decision[[i]] & 
             date_submission < data_dissents_workload$date_decision[[i]] & 
             judge_rapporteur_name == data_dissents_workload$judge[[i]]) %>%
    summarise(doc_id = data_dissents_workload$doc_id[[i]],
              judge = data_dissents_workload$judge[[i]],
              unfinished_cases = n())
  return(output)
} %>% right_join(., data_dissents_workload, by = join_by(doc_id, judge)) %>% 
  filter(year(date_decision) < 2023)

# As a prior for intercept, we use the frequency of dissenting opinions in our datasets as the mean of a normal distribution
prior_intercept_freq = data_dissents_workload %>%
  group_by(dissent) %>%
  count() %>%
  ungroup() %>%
  mutate(freq = n/sum(n)) %>%
  pluck(3,2)

model_workload_pooled_abs = linear_reg() %>%
  set_engine("stan",
             family = binomial,
             prior_intercept = normal(log(prior_intercept_freq/(1-prior_intercept_freq)), 1),
             # prior = normal(log(0.99), 0.1, autoscale = TRUE), # We assume that each unfinished decision decreases the odds of dissent very slightly
             chains = 4,
             iter = 10000*2,
             seed = 84735,
             cores = chains) %>%
  fit(dissent ~ unfinished_cases, data = data_dissents_workload) %>%
  extract_fit_engine()

model_workload_hierarchical_abs = linear_reg() %>%
  set_engine("stan_glmer",
             family = binomial,
             prior_intercept = normal(log(prior_intercept_freq/(1-prior_intercept_freq)), 1),
             # prior = normal(log(0.99), 0.1, autoscale = TRUE), # We assume that each unfinished decision decreases the odds of dissent very slightly
             prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
             chains = 4,
             iter = 10000*2,
             seed = 84735,
             cores = chains) %>%
  fit(dissent ~ unfinished_cases + (1 | judge), data = data_dissents_workload) %>%
  extract_fit_engine()

mcmc_trace(model_workload_pooled_abs)
mcmc_acf(model_workload_pooled_abs)

# Posterior diagnosis
pp_check(model_workload_pooled_abs)

prediction_summary(model = model_workload_pooled_abs, data = data_dissents_workload)

# K-fold crossvalidation - In this case, the results are similar, suggesting that our model is not overfit to our sample data.
prediction_summary_cv(model = model_workload_pooled_abs, data = data_dissents_workload, k = 6)

# Parameters
mcmc_dens_overlay(model_workload_pooled_abs, pars = c("unfinished_cases"))
mcmc_areas_workload = mcmc_areas(model_workload_pooled_abs, pars = c("unfinished_cases"))
mcmc_dens(model_workload_pooled_abs, pars = c("unfinished_cases"))
mcmc_intervals_workload = mcmc_intervals(model_workload_pooled_abs, pars = c("unfinished_cases"))

output_workload = tidy(model_workload_pooled_abs, 
     conf.int = TRUE, conf.level = 0.80) %>%
  mutate(across(where(is.numeric), ~exp(.x))) %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 3))) %>%
  rename(estimate_odds = estimate) %>%
  slice_head(n = 2)
output_workload


rm(data_caseload, data_dissents, data_metadata)
save.image(file = "models/models_fitted_workload_dissent.RData")
load(file = "models/models_fitted_workload_dissent.RData")

  
