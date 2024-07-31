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

data |>
  filter(n_citations == 0 & separate_opinion == 1)

zero_cca = data |>
  filter(n_concerned_constitutional_acts == 0 & separate_opinion == 1)
