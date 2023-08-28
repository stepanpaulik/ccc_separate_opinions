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

chains = 4L

coalition_one = c("Kateřina Šimáčková", "Vojtěch Šimíček", "Ludvík David", "Jaromír Jirsa", "David Uhlíř", "Jiří Zemánek", "Tomáš Lichovník", "Jan Filip", "Milada Tomková", "Pavel Šámal")
coalition_two = c("Radovan Suchánek","Vladimír Sládeček","Josef Fiala","Jan Musil","Jaroslav Fenyk","Pavel Rychetský")

#Load data
# Filter cases that are of the 3rd CCC and are decided on merits
data_metadata = readr::read_rds("../data/US_metadata.rds") %>%
  mutate(year_submission = year(date_submission),
         year_decision = year(date_decision),
         dissent = if_else(is.na(dissenting_opinion), 0, 1)) %>%
  filter(year_decision > 2013 & type_decision == "Nález" & formation != "Plenum")

# Attach the information about the dissent
data_dissents = readr::read_rds("../data/US_dissents.rds") %>% 
  mutate(dissent = 1) %>%
  left_join(data_metadata, .)

data_coalition_panel = readr::read_rds("../data/US_compositions.rds") %>%
  left_join(data_metadata %>% select(doc_id), .) %>%
  mutate(coalition = if_else(judge %in% c(coalition_one, coalition_two), 1, 0)) %>%
  group_by(doc_id) %>%
  filter(sum(coalition) == 3) %>%
  ungroup() %>%
  mutate(coalition = if_else(judge %in% coalition_one, 1, 0)) %>%
  group_by(doc_id) %>%
  summarise(coalition = sum(coalition)) %>%
  left_join(., data_metadata %>% select(doc_id, dissent)) %>%
  mutate(dissent = replace_na(dissent, 0),
         full_coal_1 = if_else(coalition == 3, 1, 0),
         full_coal_2 = if_else(coalition == 0, 1, 0),
         mixed_coal_1_min = if_else(coalition == 1, 1, 0),
         mixed_coal_2_min = if_else(coalition == 2, 1, 0)) %>%
  ungroup()

prior_intercept_freq = data_coalition_panel %>%
  group_by(dissent) %>%
  count() %>%
  ungroup() %>%
  mutate(freq = n/sum(n)) %>%
  pluck(3,2)

data_coalition_panel %>%
  group_by(dissent, coalition) %>%
  count() %>%
  ungroup() %>%
  mutate(freq = n/sum(n),
         change = prior_intercept_freq - freq)

model_coalition_panel = linear_reg() %>%
  set_engine("stan",
             family = binomial,
             prior_intercept = normal(log(prior_intercept_freq/(1-prior_intercept_freq)), 1),
             chains = chains, iter = 10000*2, seed = 84735, cores = chains) %>%
  set_mode("regression") %>%
  fit(dissent ~ full_coal_1 + full_coal_2 + mixed_coal_1_min + mixed_coal_2_min, data = data_coalition_panel) %>%
  extract_fit_engine()

# MCMC Diagnosis
mcmc_trace(model_coalition_panel)
mcmc_acf(model_coalition_panel)

# Parameters
mcmc_dens_coalition_panel = mcmc_dens_overlay(model_coalition_panel)
mcmc_areas_coalition_panel = mcmc_areas(model_coalition_panel) +
  labs(title = "Density plot of estimates of parameters of our model",
       subtitle = "The inner area is for 50 % posterior credible interval, the outer for 95 %")
mcmc_dens(model_coalition_panel)

mcmc_intervals_coalition_panel = mcmc_intervals(model_coalition_panel)  +
  labs(title = "Plot of uncertainty intervals of estimates of parameters of our model",
       subtitle = "The inner whisker is for 50 % posterior credible interval, the outer for 95 %")

pp_check_coalition_panel = pp_check(model_coalition_panel)

output_coalition_panel = tidy(model_coalition_panel, conf.int = TRUE, conf.level = 0.8, effects = "fixed") %>%
  mutate(across(where(is.numeric), ~exp(.x))) %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 2))) %>%
  rename(estimate_odds = estimate)
  
output_coalition_panel


rm(list=ls(pattern="^data_"))
save.image(file = "models/models_fitted_coalition_panel.RData")
load(file = "models/models_fitted_coalition_panel.RData")


