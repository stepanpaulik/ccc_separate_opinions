library(tidymodels)
library(parsnip)
library(rcompanion)
library(corrr)
library(stargazer)
source("scripts/load_data.R")

data = data %>%
  mutate(dissenting_opinion = as_factor(dissenting_opinion))

# MODEL -------------------------------------------------------------------
# model = logistic_reg() %>%
#   set_engine("glm") %>%
#   fit(dissenting_opinion ~ n_concerned_acts + n_concerned_constitutional_acts + n_citations + merits_admissibility + judge_profession + time_in_office + judge_profession:time_in_office + controversial,
#       data = data) %>%
#   extract_fit_engine()


model = glm(dissenting_opinion ~ n_concerned_acts + n_concerned_constitutional_acts + n_citations + merits_admissibility + judge_profession + time_in_office + judge_profession:time_in_office + controversial,
    data = data, family = "binomial")

summary(model)
with(summary(model), 1 - deviance/null.deviance)
tidy(model)
car::vif(model)

stargazer(model)
  

# DIAGNOSTICS -------------------------------------------------------------
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
              select(c(n_concerned_acts, n_concerned_constitutional_acts, n_citations, merits_admissibility, judge_profession, time_in_office, controversial))) %>%
  select(x, y, assoc) %>%
  pivot_wider(names_from = y, values_from = assoc) %>%
  column_to_rownames(var = "x") %>%
  as_cordf() %>%
  shave(upper = TRUE) %>%
  fashion(decimals = 2, na_print = "—") 

rm(list=ls(pattern="^data_"))
save.image("report/model_results.RData")

