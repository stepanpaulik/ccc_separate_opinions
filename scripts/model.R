library(tidymodels)
library(parsnip)
library(rcompanion)
library(ggfortify)
library(corrr)
library(multilevelmod)
library(lme4)
source("scripts/load_data.R")

# additional data prep step
data = data %>%
  mutate(dissenting_opinion = as_factor(dissenting_opinion)) %>%
  mutate(across(where(is.numeric), scale))

# %>%
#   mutate(across(where(is.numeric), ~ center_column(.x))) # center and scale the numeric variables



# RE - MODEL --------------------------------------------------------------
model_base = logistic_reg() %>%
  set_engine("glm") %>%
  fit(dissenting_opinion ~ 1,
      data = data, 
      family = binomial) %>%
  extract_fit_engine()

model = logistic_reg() %>%
  set_engine("glm") %>%
  fit(dissenting_opinion ~ n_concerned_acts + n_concerned_constitutional_acts + n_citations + merits_admissibility + judge_profession + time_in_office + judge_profession:time_in_office + controversial + workload,
      data = data,
      family = binomial) %>%
  extract_fit_engine()

AIC(logLik(model_base))
AIC(logLik(model))


# RE - DIAGNOSTICS ---------------------------------------------------------
## Multicolinearity --------------------------------------------------------
# do a LINK TEST 
linktest = function(model){
  dat = model$model
  fit = predict.glm(model, newdata = dat)
  fit2 = fit ^ 2
  resp = model$y
  newdat = data.frame(fit = fit, fit2 = fit2, resp = resp)
  link_model = glm(resp ~ fit + fit2, data = newdat, family = binomial(link = "logit"))
  summary(link_model)
}
linktest(model)

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
  is_numeric <- function(x) { is.integer(x) || is_double(x)}
  
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
                                     select(c(n_concerned_acts, n_concerned_constitutional_acts, n_citations, merits_admissibility, judge_profession, time_in_office, controversial, workload))) %>%
  select(x, y, assoc) %>%
  pivot_wider(names_from = y, values_from = assoc) %>%
  column_to_rownames(var = "x") %>%
  as_cordf() %>%
  shave(upper = TRUE) %>%
  fashion(decimals = 2, na_print = "—")
correlation_complete

car::vif(model)

# Conclusion: no apparent collinearity issue

# MIXED EFFECTS -----------------------------------------------------------
model_me_base = logistic_reg() %>%
  set_engine("glm") %>%
  fit(dissenting_opinion ~ 1,
      data = data, REML = T) %>%
  extract_fit_engine()

model_only_me = logistic_reg() %>%
  set_engine("glmer") %>%
  fit(dissenting_opinion ~ 1 + (1 | formation),
      data = data, REML = T) %>%
  extract_fit_engine()

# The ME models structured around the formation shows decrease in AIC, therefore, we will stick to it 
AIC(logLik(model_me_base))
AIC(logLik(model_only_me))

# MODEL FITTING
# wrapper function for linear mixed-models
glmer.glmulti = function(formula,data, random="",...){
  glmer(paste(deparse(formula),random), 
        family = binomial, 
        data=data, 
        control=glmerControl(optimizer="bobyqa"), ...)
}
# define formula
form_glmulti = as.formula(paste("dissenting_opinion ~ n_concerned_acts + n_concerned_constitutional_acts + n_citations + merits_admissibility + judge_profession + time_in_office + judge_profession:time_in_office + controversial + workload"))
# multi selection for glmer
model_fit = glmulti::glmulti(form_glmulti,random="+(1 | formation)", 
                data = data, method = "h", fitfunc = glmer.glmulti,
                crit = "bic", intercept = TRUE, marginality = FALSE, level = 2)
# extract best models
top = glmulti::weightable(model_fit)
top = top[1:20,]
# inspect top 5 models
top

# FULL MODEL
model_full_me = logistic_reg() %>%
  set_engine("glmer") %>%
  fit(dissenting_opinion ~ n_concerned_acts + n_concerned_constitutional_acts + n_citations + merits_admissibility + judge_profession + time_in_office + judge_profession:time_in_office + controversial + workload + (1 | formation),
      data = data) %>%
  extract_fit_engine()

AIC(logLik(model_full_me))

summary(model_full_me)
plot(model_full_me, formation ~ resid(.), abline = 0 )
plot(model_full_me, resid(., type = "pearson") ~ fitted(.) | formation, id = 0.05, 
     adj = -0.3, pch = 20, col = "gray40")
# start plotting
par(mfrow = c(2, 2))           # display plots in 2 rows and 2 columns
plot(model_full_me, pch = 20, col = "black", lty = "dotted"); par(mfrow = c(1, 1))
qqnorm(model_full_me, pch = 20, col = "black")

# observed responses versus the within-group fitted values
plot(model_full_me, formation ~ fitted(.), id = 0.05, adj = -0.3, 
     xlim = c(80, 220), cex = .8, pch = 20, col = "blue")

# 
probs = 1/(1+exp(-fitted(mlr.glmer)))
somers2(probs, as.numeric(data$dissenting_opinion))

# MODEL DIAGNOSTICS
# a diagnostic that plots the fitted or predicted values against the residuals
plot(model_full_me, pch = 20, col = "black", lty = "dotted")
# model = glm(dissenting_opinion ~ n_concerned_acts + n_concerned_constitutional_acts + n_citations + merits_admissibility + judge_profession + time_in_office + judge_profession:time_in_office + controversial + workload,
#     data = data, family = "binomial")

modelsummary::modelsummary(model,
                           estimate = "{estimate}{stars}",
                           statistic = "({std.error})",
                           stars = TRUE)
autoplot(model)
anova(model_base, model, test = "Chisq")

data2 = data %>%
  mutate(residuals = resid(model),
                standardized.residuals = rstandard(model),
                studentized.residuals = rstudent(model),
                cooks.distance = cooks.distance(model),
                dffit = dffits(model),
                leverage = hatvalues(model),
                covariance.ratios = covratio(model),
                fitted = model$fitted.values)

p5 = ggplot(data2,
             aes(studentized.residuals)) +
  theme(legend.position = "none")+
  geom_histogram(aes(y=..density..),
                 binwidth = .2,
                 colour="black",
                 fill="gray90") +
  labs(x = "Studentized Residual", y = "Density") +
  stat_function(fun = dnorm,
                args = list(mean = mean(data2$studentized.residuals, na.rm = TRUE),
                            sd = sd(data2$studentized.residuals, na.rm = TRUE)),
                colour = "red", size = 1) +
  theme_bw(base_size = 8)
# plot 6
p6 = ggplot(data2, aes(fitted, studentized.residuals)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "Red")+
  theme_bw(base_size = 8)+
  labs(x = "Fitted Values",
       y = "Studentized Residual")

  
# DIAGNOSTICS -------------------------------------------------------------


# PREDICTION ACCURACY -----------------------------------------------------
data = data %>%
  mutate(Prediction = predict(model, type = "response"),
                Prediction = ifelse(Prediction > .5, 1, 0),
                Prediction = factor(Prediction, levels = c("0", "1")))

caret::confusionMatrix(data$Prediction, data$dissenting_opinion)

# MODEL - COALITIONS ------------------------------------------------------
# model_coalitions = logistic_reg() %>%
#   set_engine("glm") %>%
#   fit(dissenting_opinion ~ n_concerned_acts + n_concerned_constitutional_acts + n_citations + judge_profession + time_in_office + judge_profession:time_in_office + controversial + workload + coalition,
#       data = data_coalition) %>%
#   extract_fit_engine()
# 
# modelsummary::modelsummary(model_coalitions,
#                            estimate = "{estimate}{stars}",
#                            statistic = "({std.error})",
#                            stars = TRUE)

rm(list=ls(pattern="^data"))
save.image("report/model_results.RData")



