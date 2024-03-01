library(tidymodels)
library(parsnip)
library(rcompanion)
library(ggfortify)
library(corrr)
library(multilevelmod)
library(lme4)
library(performance)
library(parameters)
library(broom.mixed)
source("scripts/load_data.R")

# additional data prep step
data = data %>%
  mutate(separate_opinion = as_factor(separate_opinion)) %>%
  mutate(across(where(is.numeric), ~datawizard::standardize(.x))) %>%
  mutate(judge_profession = if_else(judge_profession %in% c("judge"), true = "within", false = "outside"))

# rmse = function(model){
#   sqrt(mean(model$residuals^2))
# }

# RE - MODEL --------------------------------------------------------------
model_re_base = logistic_reg() %>%
  set_engine("glm") %>%
  fit(separate_opinion ~ 1,
      data = data, 
      family = binomial) %>%
  extract_fit_engine()

model_re_full = logistic_reg() %>%
  set_engine("glm") %>%
  fit(separate_opinion ~ n_concerned_acts + n_concerned_constitutional_acts + n_citations + grounds + judge_profession + time_in_office + judge_profession:time_in_office + controversial + workload,
      data = data,
      family = binomial) %>%
  extract_fit_engine()

# MIXED EFFECTS -----------------------------------------------------------
model_me_base = logistic_reg() %>%
  set_engine("glmer") %>%
  fit(separate_opinion ~ 1 + (1 | formation),
      data = data, REML = T) %>%
  extract_fit_engine()

model_me_full = glmer(separate_opinion ~ n_concerned_acts + n_concerned_constitutional_acts + n_citations + grounds + judge_profession + time_in_office + judge_profession:time_in_office + controversial + workload + (1 | formation),
                      data = data, 
                      control = glmerControl(optimizer="bobyqa",
                                             optCtrl=list(maxfun=2e5)),
                      family = "binomial")

model_me_no_interaction = glmer(separate_opinion ~ n_concerned_acts + n_concerned_constitutional_acts + n_citations + grounds + judge_profession + time_in_office + controversial + workload + (1 | formation),
                      data = data, 
                      control = glmerControl(optimizer="bobyqa",
                                             optCtrl=list(maxfun=2e5)),
                      family = "binomial")


model_me_disagreement = glmer(separate_opinion ~ n_concerned_acts + n_concerned_constitutional_acts + n_citations + grounds + controversial + workload + (1 | formation),
                              data = data, 
                              control = glmerControl(optimizer="bobyqa",
                                                     optCtrl=list(maxfun=2e5)),
                              family = "binomial")

model_me_identification = glmer(separate_opinion ~ grounds + judge_profession + time_in_office + judge_profession:time_in_office + (1 | formation),
                                data = data, 
                                control = glmerControl(optimizer="bobyqa",
                                                       optCtrl=list(maxfun=2e5)),
                                family = "binomial")

model_me_final = glmer(separate_opinion ~ n_concerned_acts + n_concerned_constitutional_acts + grounds + judge_profession + time_in_office + controversial + workload + (1 | formation),
                       data = data, 
                       control = glmerControl(optimizer="bobyqa",
                                              optCtrl=list(maxfun=2e5)),
                       family = "binomial")

# MODEL COMPARISON --------------------------------------------------------
AIC(logLik(model_re_base))
AIC(logLik(model_re_full))
AIC(logLik(model_me_base))
AIC(logLik(model_me_no_interaction))
AIC(logLik(model_me_full))

summary(model_me_full)

anova(model_re_base, model_re_full, test = "Chisq")
anova(model_me_full, model_me_base, test = "Chisq")
anova(model_me_full, model_me_no_interaction, test = "Chisq")

check_collinearity(model_re_full)
check_collinearity(model_me_full)

check_autocorrelation(model_re_full)
check_autocorrelation(model_me_full)

clustered_SE = clubSandwich::coef_test(model_re_full, vcov = "CR2", cluster = data$formation)
clustered_SE_me = clubSandwich::coef_test(model_me_full, vcov = "CR2", cluster = data$formation)

# MODEL SUMMARY -----------------------------------------------------------
modelsummary::modelsummary(list("Pooled" = model_re_full, 
                                "ME_judge" = model_me_judge, 
                                "ME_formation" = model_me_full, 
                                "ME_formation_no_interaction" = model_me_no_interaction), 
                           estimate = "{estimate}{stars}",
                           statistic = "({std.error})",
                           stars = TRUE)

model_parameters(
  model_re_full
)

model_parameters(
  model_re_full
)

clustered_SE = model_parameters(
  model_re_full,
  vcov = "vcovCR",
  vcov_args = list(type = "CR2", cluster = data$formation)
)

# DATA DIAGNOSIS ----------------------------------------------------------
## Multicolinearity --------------------------------------------------------
corr_acts = data %>% 
  select(c(n_concerned_acts, n_concerned_constitutional_acts, n_citations)) %>%
  correlate(method = "spearman") %>%
  shave(upper = TRUE) %>%
  fashion(decimals = 2, na_print = "—")
corr_acts

mixed_assoc = function(df, cor_method="spearman", adjust_cramersv_bias=TRUE){
  df_comb = expand.grid(names(df), names(df),  stringsAsFactors = F) %>% set_names("X1", "X2")
  
  is_nominal = function(x) class(x) %in% c("factor", "character")
  # https://community.rstudio.com/t/why-is-purr-is-numeric-deprecated/3559
  # https://github.com/r-lib/rlang/issues/781
  is_numeric = function(x) { is.integer(x) || is_double(x)}
  
  f = function(xName,yName) {
    x =  pull(df, xName)
    y =  pull(df, yName)
    
    result = if(is_nominal(x) && is_nominal(y)){
      # use bias corrected cramersV as described in https://rdrr.io/cran/rcompanion/man/cramerV.html
      cv = cramerV(as.character(x), as.character(y), bias.correct = adjust_cramersv_bias)
      data.frame(xName, yName, assoc=cv, type="cramersV")
      
    }else if(is_numeric(x) && is_numeric(y)){
      correlation = cor(x, y, method=cor_method, use="complete.obs")
      data.frame(xName, yName, assoc=correlation, type="correlation")
      
    }else if(is_numeric(x) && is_nominal(y)){
      # from https://stats.stackexchange.com/questions/119835/correlation-between-a-nominal-iv-and-a-continuous-dv-variable/124618#124618
      r_squared = summary(lm(x ~ y))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else if(is_nominal(x) && is_numeric(y)){
      r_squared = summary(lm(y ~x))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else {
      warning(paste("unmatched column type combination: ", class(x), class(y)))
    }
    
    # finally add complete obs number and ratio to table
    result %>% mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) %>% rename(x=xName, y=yName)
  }
  
  # apply function to each variable combination
  map2_df(df_comb$X1, df_comb$X2, f)
}

correlation_complete = mixed_assoc(data %>% 
                                     select(c(n_concerned_acts, n_concerned_constitutional_acts, n_citations, grounds, judge_profession, time_in_office, controversial, workload))) %>%
  select(x, y, assoc) %>%
  pivot_wider(names_from = y, values_from = assoc) %>%
  column_to_rownames(var = "x") %>%
  as_cordf() %>%
  shave(upper = TRUE) %>%
  fashion(decimals = 2, na_print = "—")
correlation_complete

car::vif(model_me_full)

# PREDICTION ACCURACY -----------------------------------------------------
data_prediction = data %>%
  mutate(Prediction = predict(model_me_full, type = "response"),
                Prediction = ifelse(Prediction > .5, 1, 0),
                Prediction = factor(Prediction, levels = c("0", "1")))

conf_matrix = caret::confusionMatrix(data_prediction$Prediction, data_prediction$separate_opinion)

conf_matrix$table %>%
  as_tibble()

# Placebo -----------------------------------------------------------------
gen_placebo_models = function(var) {
  form = paste("separate_opinion ~ n_concerned_acts + n_concerned_constitutional_acts + n_citations + grounds + judge_profession + time_in_office + judge_profession:time_in_office + workload + (1 | formation) +", var)
  tidy(glmer(as.formula(form), data = data, 
             control = glmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=2e5)),
             family = "binomial"))
}

placebo_models = str_subset(string = colnames(data), "placebo\\d") %>%
  map_df(gen_placebo_models)

placebo_models

# MODEL - COALITIONS ------------------------------------------------------
# model_coalitions = logistic_reg() %>%
#   set_engine("glm") %>%
#   fit(separate_opinion ~ coalition,
#       data = data_coalition) %>%
#   extract_fit_engine()
# 
# modelsummary::modelsummary(model_coalitions,
#                            estimate = "{estimate}{stars}",
#                            statistic = "({std.error})",
#                            stars = TRUE)
# 
# plot_coalitions = model_coalitions %>%
#   tidy(conf.int = TRUE) %>%
#   ggplot(aes(x = term, y = estimate)) +
#   geom_point() +
#   geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
#   labs(x = "Term", y = "Estimate")
# 
# write_rds(model_coalitions, file = "../ccc_dataset/report/model_coalitions.rds")
# write_rds(plot_coalitions, file = "../ccc_dataset/report/plot_coalitions.rds")
  

rm(list=ls(pattern="^data"))
save.image("report/model_results.RData")




