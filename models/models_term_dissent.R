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
data_dissents = readr::read_rds("../data/US_dissents.rds") %>%
  left_join(., readr::read_rds("../data/US_metadata.rds") %>%
              mutate(year_submission = year(date_submission),
                     year_decision = year(date_decision)) %>% select(doc_id, formation, year_decision)) %>%
  filter(year_decision < 2023) %>%
  group_by(dissenting_judge, year_decision) %>%
  summarise(count_dissents = n())

data_term = readr::read_rds("../data/US_judges.rds") %>%
  mutate(end = case_when(
    is.na(end) & reelection == 1 ~ start %m+% years(20),
    is.na(end) & reelection == 0 ~ start %m+% years(10),
    .default = end
  )) %>%
  select(judge_name, yob, gender, start, end, reelection) %>%
  left_join(data_dissents, ., by = join_by("dissenting_judge" == "judge_name")) %>%
  mutate(start = year(start),
         end = year(end),
         end_term = if_else(end - as.numeric(as.character(year_decision)) < 3, 1, 0),
         start_term = if_else(end - as.numeric(as.character(year_decision)) > 7, 1, 0),
         dissenting_judge = factor(dissenting_judge)) %>%
  ungroup()

# Average amount of dissents for prior
prior_intercept = log(mean(data_term$count_dissents))

# prior_end_term = data_term %>%
#   filter(end_term == 1) %>%
#   summarise(mean = mean(count_dissents)) %>%
#   pluck(1,1) %>%
#   log()

# Model
model_hierarchical_term = linear_reg() %>%
  set_engine("stan_glmer",
             family = poisson, 
             prior_intercept = normal(prior_intercept, 0.5),
             prior = normal(0, 2.5, autoscale = TRUE), # weakly informative priors
             prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
             chains = chains, iter = 10000*2, seed = 84735, cores = chains) %>%
  fit(count_dissents ~ end_term + start_term + reelection + (1 | dissenting_judge),
      data = data_term) %>%
  extract_fit_engine()

# MCMC Diagnosis
mcmc_trace(model_hierarchical_term)
mcmc_acf(model_hierarchical_term)

# Parameters
mcmc_dens_term = mcmc_dens_overlay(model_hierarchical_term, pars = c("start_term", "end_term"))   +
  labs(title = "Density plot of estimates of parameters of start and end of a judge's term",
       subtitle = "The inner area is for 50 % posterior credible interval, the outer for 95 %")
mcmc_areas_term = mcmc_areas(model_hierarchical_term, pars = c("start_term", "end_term"))
mcmc_dens(model_hierarchical_term, pars = c("start_term", "end_term"))

mcmc_intervals_term = mcmc_intervals(model_hierarchical_term, pars = c("start_term", "end_term"))  +
  labs(title = "Plot of uncertainty intervals of estimates of parameters of start and end of a judge's term",
       subtitle = "The inner whisker is for 50 % posterior credible interval, the outer for 95 %")

output_term = tidy(model_hierarchical_term, conf.int = TRUE, conf.level = 0.8, effects = "fixed") %>%
  mutate(across(where(is.numeric), ~exp(.x))) %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 2)))

# Posterior diagnosis
pp_check_term = pp_check(model_hierarchical_term) +
  labs(x = "Dissent count",
       title = "Posterior predictive check of the start and end of terms model")

data_term %>% 
  add_epred_draws(object = model_hierarchical_term, ndraws = 4) %>%
  ggplot(aes(x = end_term, y = count_dissents)) +
  geom_line(aes(y = .epred, group = paste(dissenting_judge, .draw)))

# Accuracy of the model
prediction_summary(model = model_hierarchical_term, data = data_term)

# K-fold crossvalidation - In this case, the results are similar, suggesting that our model is not overfit to our sample data.
prediction_summary_cv_term = prediction_summary_cv(data = data_term, model = model_hierarchical_term, group = "dissenting_judge", k = 6)

folds = 6
kfold(x = model_hierarchical_term, K = folds, cores = folds)

rm(list=ls(pattern="^data_"))
save.image(file = "models/models_fitted_term_dissent.RData")
load(file = "models/models_fitted_term_dissent.RData")


