library(tidyverse); theme_set(theme_minimal())
source("scripts/load_data.R")

model_selection = data %>%
  group_by(formation) %>%
  count() %>%
  ungroup() %>%
  summarise(
    units = n(),
    mean_observations = median(n)
  )
