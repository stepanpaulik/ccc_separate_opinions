library(tidymodels)
library(ggfortify)
library(fixest)
library(marginaleffects)

load = FALSE


if(load == TRUE){
  load("data/model_results.RData")
} else {
  
  source("scripts/load_data.R")
  
  model_no_fe = feglm(
    fml = separate_opinion ~ n_citations + judge_profession * time_in_office + time_until_end + controversial + workload + grounds,
    data = data,
    cluster = "formation",
    family = "binomial"
  )
  
  model_fe_year = feglm(
    fml = separate_opinion ~ n_citations + judge_profession * time_in_office + time_until_end + controversial + workload + grounds | year_decision,
    data = data,
    cluster = "formation",
    family = "binomial"
  )
  
  model_fe_year_chamber = feglm(
    fml = separate_opinion ~ n_citations + judge_profession * time_in_office + time_until_end + controversial + workload + grounds | year_decision + formation,
    data = data,
    cluster = "formation",
    family = "binomial"
  )
  
  model_fe_year_chamber_judge = feglm(
    fml = separate_opinion ~ n_citations + time_in_office + time_until_end + controversial + workload + grounds | year_decision + formation + judge_name,
    data = data,
    cluster = "formation",
    family = "binomial"
  )
  
  model_lpm = feols(
    fml = separate_opinion ~ n_citations + judge_profession * time_in_office + time_until_end + controversial + workload + grounds + year_decision + formation,
    data = data,
    cluster = "formation"
  )
  
  modelsummary::modelsummary(list(
    "(1) LOGIT" = model_no_fe,
    "(2) FE LOGIT" = model_fe_year,
    "(3) FE LOGIT" = model_fe_year_chamber,
    "(4) FE LOGIT" = model_fe_year_chamber_judge),
    estimate = "{estimate}{stars}",
    statistic = "{p.value} [{conf.low}, {conf.high}]",
    exponentiate = TRUE,
    stars = TRUE)

  
  marginal_predictions_fe = marginaleffects::predictions(model_fe_year_chamber)
  
  marginal_probabilities = plot_predictions(model_fe_year_chamber, 
                                            condition = c("time_in_office" ,"judge_profession"), 
                                            type = "response",
                                            gray = TRUE) +
    labs(title = "Probability of a SO conditional on \n Time in Office and Profession" ) +
    xlab("Time in Office") +
    ylab("Probability of a Separate Opinion")
  marginal_probabilities

# Placebo -----------------------------------------------------------------
gen_placebo_models = function(var, estimate, estimate_se, data) {
  form = paste("separate_opinion ~ n_citations + judge_profession * time_in_office + ", var, " + workload + grounds | year_decision + formation")
  mod = feglm(as.formula(form), data = data, 
              family = "binomial")
  output = tidy(hypotheses(mod, equivalence = c(estimate - 0.36*estimate_se, estimate + 0.36*estimate_se)))
  return(output)
}

placebo_models = str_subset(string = colnames(data), "placebo\\d") %>%
  map_df(~gen_placebo_models(var = .x, estimate = tidy(model_fe_year_chamber) |> filter(term == "controversial1") |> pluck(2,1), estimate_se = tidy(model_fe_year_chamber) |> filter(term == "controversial1") |> pluck(3,1), data = data)) %>%
  filter(str_detect(term, "placebo")) |>
  select(term, estimate, p.value, p.value.equiv, p.value.nonsup, p.value.noninf)

placebo_models

# DATA DIAGNOSIS ----------------------------------------------------------
## Multicolinearity --------------------------------------------------------
# corr_acts = data %>% 
#   select(c(n_concerned_acts, n_concerned_constitutional_acts, n_citations)) %>%
#   correlate(method = "spearman") %>%
#   shave(upper = TRUE) %>%
#   fashion(decimals = 2, na_print = "—")
# corr_acts


# PREDICTION ACCURACY -----------------------------------------------------
# data_prediction = data %>%
#   mutate(Prediction = predict(model_fe_year_chamber, type = "response"),
#          Prediction = ifelse(Prediction > .5, 1, 0),
#          Prediction = factor(Prediction, levels = c("0", "1")))

# conf_matrix = caret::confusionMatrix(data_prediction$Prediction, data_prediction$separate_opinion)
# conf_matrix
# conf_matrix$table %>%
#   as_tibble()

# MODEL - COALITIONS ------------------------------------------------------
model_coalitions = feglm(
  fml = separate_opinion ~ coalition,
  data = data_coalition,
  family = "binomial"
)

model_coalitions_fe_outcome = feglm(
  fml = separate_opinion ~ coalition | judge_rapporteur_name,
  data = data_coalition,
  family = "binomial"
)


plot_coalitions = model_coalitions %>%
  tidy(conf.int = TRUE) %>%
  mutate(across(c(estimate, std.error, conf.low, conf.high), ~exp(.x))) |>
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  labs(x = "Term", y = "Estimate")
plot_coalitions
}


# rm(list=ls(pattern="^data"))
# save.image("data/model_results.RData")

