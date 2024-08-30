library(tidyverse); theme_set(theme_minimal())
library(patchwork)
library(geomtextpath)
source("scripts/load_data.R")


# DATA DESCRIPTION --------------------------------------------------------

# Overall data ------------------------------------------------------------
separate_opinion_table = read_rds("../data/ccc_database/rds/ccc_metadata.rds") |>
  mutate(formation = if_else(formation %in% "Plenum", "Plenum", "Chamber")) |>
  mutate(presence_dissent = if_else(is.na(as.character(separate_opinion)), 0, 1)) |>
  group_by(formation, grounds, presence_dissent) |>
  count() |>
  group_by(formation, grounds) |>
  mutate(total = sum(n)) |>
  ungroup() |>
  mutate(freq = paste0(100*round(n/total,4), " %")) |>
  filter(presence_dissent == 1) |>
  select(-presence_dissent)
separate_opinion_table

separate_opinion_ratio_plenum = read_rds("../data/ccc_database/rds/ccc_metadata.rds") |>
  mutate(formation = if_else(formation %in% "Plenum", "Plenum", "Chamber")) |>
  filter(formation %in% "Plenum") |>
  mutate(presence_dissent = if_else(is.na(as.character(separate_opinion)), "No SO", "At least 1 SO")) |>
  mutate(presence_dissent = as_factor(presence_dissent)) |>
  ggplot(aes(x = year_decision, fill = presence_dissent)) +
  geom_bar(alpha = 0.3) +
  scale_fill_grey(start = 0.6, end = 0) +
  labs(x = "Year of the Decision", y = "Number of Decisions", fill = NULL) + 
  scale_x_continuous(guide = guide_axis(angle = 90), breaks=1993:2022)
  

separate_opinion_ratio_chamber = read_rds("../data/ccc_database/rds/ccc_metadata.rds") |>
  mutate(formation = if_else(formation %in% "Plenum", "Plenum", "Chamber")) |>
  filter(formation %in% "Chamber") |>
  mutate(presence_dissent = if_else(is.na(as.character(separate_opinion)), "No SO", "At least 1 SO")) |>
  mutate(presence_dissent = as_factor(presence_dissent)) |>
  ggplot(aes(x = year_decision, fill = presence_dissent)) +
  geom_bar(alpha = 0.3) + 
  scale_fill_grey(start = 0.6, end = 0) +
  labs(x = "Year of the Decision", y = "Number of Decisions", fill = NULL) +
  scale_x_continuous(guide = guide_axis(angle = 90), breaks=1993:2022)

# Alternatively explore guide = guide_axis(n.dodge = 2)

overall_table = data_metadata %>%
  mutate(formation = if_else(formation == "Plenum", "Plenum", "Chamber"),
         presence_dissent = if_else(is.na(as.character(separate_opinion)), 0, 1)) %>%
  group_by(formation, grounds) %>%
  summarise(count = n(),
            ratio_total = scales::percent(x = n()/nrow(.), accuracy = 0.1),
            ratio_dissent = scales::percent(x = sum(presence_dissent)/n(), accuracy = 0.1)) %>%
  ungroup()
overall_table

# Operationalization ------------------------------------------------------
professions = data_judges |> 
  filter(judge_term_court %in% c("2nd","3rd")) |>
  mutate(judge_profession = case_when(
    judge_profession %in% "judge" ~ "Judge",
    judge_profession %in% "scholar" ~ "Scholar",
    judge_profession %in% "lawyer" ~ "Lawyer",
    judge_profession %in% "politician" ~ "Politician",
  )) |>
  ggplot(aes(x = forcats::fct_infreq(judge_profession), fill = forcats::fct_infreq(judge_profession))) +
  geom_bar() +
  labs(x = NULL, y = NULL, fill = NULL) +
  scale_fill_grey() +
  guides(fill="none")
professions

n_within = data_judges |> 
  filter(judge_term_court %in% c("2nd","3rd")) |>
  filter(judge_profession %in% "judge") |>
  nrow()

n_outside = nrow(data_judges |> filter(judge_term_court %in% c("2nd","3rd"))) - n_within


reelected_judges = data_judges |> 
  group_by(judge_name) |>
  filter(n() > 1)
 
reelected_judges = data_judges |> 
  filter(judge_term_court %in% c("2nd","3rd")) |>
  filter(judge_name %in% reelected_judges$judge_name) |>
  distinct(judge_name, .keep_all = TRUE) |>
  nrow()


# Summary table -----------------------------------------------------------
dependent_variables_overview_numeric = data |>
  select(judge_profession, n_citations, controversial, workload, grounds, time_in_office, time_until_end) |>
  skimr::skim(where(is.numeric)) |>
  select(skim_variable, numeric.mean, numeric.sd, numeric.p0, numeric.p25, numeric.p50, numeric.p75, numeric.p100)
dependent_variables_overview_numeric

dependent_variables_overview_factor = data |>
  select(separate_opinion, judge_profession, controversial, grounds) |>
  mutate(separate_opinion = as_factor(if_else(separate_opinion == 1, "Attached a SO", "No SO")),
         judge_profession = as_factor(judge_profession),
         controversial = as_factor(if_else(controversial == 1, "Controversial Topic", "Uncontroversial Topic"))) |>
  map_dfr(~table(.x) |> as.data.frame() |> as_tibble()) |>
  mutate(.x = c("No SO", "Attached a SO", "Outside Judiciary", "Within Judiciary", "Controversial", "Uncontroversial", "Admissibility", "On Merits"),
         group = c("Separate Opinion", "Separate Opinion", "Profession", "Profession", "Issue Salience", "Issue Salience", "Grounds", "Grounds"))
dependent_variables_overview_factor

model_selection = rbind(data |>
  group_by(formation) |>
  count() |>
  ungroup() |>
  summarise(
    units = n(),
    mean_observations = median(n)
  ) |>
    mutate(group = "Formation"),
  data |>
    group_by(year_decision) |>
    count() |>
    ungroup() |>
    summarise(
      units = n(),
      mean_observations = median(n)
    ) |>
    mutate(group = "Year"), 
  data |>
    group_by(judge_name) |>
    count() |>
    ungroup() |>
    summarise(
      units = n(),
      mean_observations = median(n)
    ) |>
    mutate(group = "Judge")) |>
  pivot_longer(cols = c(units, mean_observations)) |>
  mutate(name = if_else(name %in% "units", "Units", "Mean Observations"))
model_selection

# save.image("data/descriptive_statistics.RData")



