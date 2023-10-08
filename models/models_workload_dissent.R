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

# Create the data WORKLOAD
#Load data and filter only those decision where a dissenting opinion could occur
data_metadata = readr::read_rds("../data/US_metadata.rds") %>%
  mutate(year_submission = year(date_submission),
         year_decision = year(date_decision)) %>%
  filter(!grepl(pattern = "procesní", x = .$type_verdict)) %>%
  filter(formation == "Plenum" | (formation %in% c("First Chamber", "Second Chamber", "Third Chamber") & type_decision != "Usnesení")) %>%
  filter(year(date_decision) < 2023)
  
data_dissents_workload = readr::read_rds("../data/US_compositions.rds") %>%
  left_join(., readr::read_rds("../data/US_dissents.rds") %>% select(-judge_id) %>% mutate(dissent = 1), by = join_by(doc_id, judge == dissenting_judge)) %>%
  mutate(dissent = replace_na(dissent, 0)) %>%
  filter(doc_id %in% data_metadata$doc_id) %>%
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
} %>% 
  right_join(., data_dissents_workload, by = join_by(doc_id, judge))
  



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

# prediction_summary(model = model_workload_pooled_abs, data = data_dissents_workload)
# 
# # K-fold crossvalidation - In this case, the results are similar, suggesting that our model is not overfit to our sample data.
# prediction_summary_cv(model = model_workload_pooled_abs, data = data_dissents_workload, k = 6)

# Parameters
mcmc_dens_overlay(model_workload_pooled_abs, pars = c("unfinished_cases"))
mcmc_areas_workload = mcmc_areas(model_workload_pooled_abs, pars = c("unfinished_cases")) +
  labs(title = "Fig 4: Density plot of estimates of parameter number of unfinished cases",
       subtitle = "The inner area is for 50 % posterior credible interval, the outer for 95 %") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
mcmc_dens(model_workload_pooled_abs, pars = c("unfinished_cases"))
mcmc_intervals_workload = mcmc_intervals(model_workload_pooled_abs, pars = c("unfinished_cases"))

output_workload = tidy(model_workload_pooled_abs, 
     conf.int = TRUE, conf.level = 0.80) %>%
  mutate(across(where(is.numeric), ~exp(.x))) %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 3))) %>%
  rename(estimate_odds = estimate) %>%
  slice_head(n = 2)
output_workload

# Create the data CASELOAD
caseload_ot = readr::read_rds("../data/US_metadata.rds") %>%
  mutate(year_submission = year(date_submission),
         year_decision = year(date_decision)) %>%
  group_by(year_submission) %>%
  count() %>%
  rename(Caseload = n) %>%
  mutate(year_submission = as.numeric(as.character(year_submission))) %>%
  filter(year_submission < 2023) %>%
  ggplot(aes(x = year_submission, y = Caseload)) +
  geom_point() +
  geom_smooth(se = F, color = "black", linewidth = 0.8) +
  scale_x_continuous(breaks = scales::breaks_width(2)) +
  labs(x = element_blank(),
       title = "Fig 3: Development of CCC caseload over time") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
caseload_ot


rm(list=ls(pattern="^data_"))
save.image(file = "models/models_fitted_workload_dissent.RData")
load(file = "models/models_fitted_workload_dissent.RData")

  
