rm (list = ls())
source("code/00_functions.R")
set.seed(2019)

# loading excess estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~
dts_fit <- 
  read_rds("data_output/p_scores_excess_deaths.rds") %>% 
  select(Country, Code, Income, Year, Sex, Age, Exposure, Deaths, Rate,
         bsn, bsn_lp, bsn_up, psc, up, lp, Source_dts)

rts_fit <- 
  read_rds("data_output/p_scores_excess_rates.rds") %>% 
  select(Country, Code, Income, Year, Sex, Age, Exposure, Rate, 
         bsn, bsn_lp, bsn_up, psc, up, lp, Source_dts)

pops <- 
  read_rds("data_inter/exposures_5-year_groups.rds")

pops2 <- 
  pops %>% 
  filter(Year == 2020) %>% 
  group_by(Code, Sex) %>% 
  summarise(pop_yng = sum(Population)) %>% 
  ungroup() %>% 
  filter(Sex == "t") %>% 
  select(-Sex)

dts_fit

# merging deaths and rates baselines and p-scores ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

all_fit <- 
  bind_rows(dts_fit, rts_fit) %>% 
  filter(Year <= 2022) %>% 
  arrange(Country, Code, Age, Sex, Year) %>% 
  mutate(Income = factor(Income, levels = c("No income data", "Low", "Lower-mid", 
                                            "Upper-mid", "High"))) %>% 
  left_join(pops2) %>% 
  # identifying what to include according to population size and fitting output
  mutate(pop_in = ifelse(pop_yng >= 5e5, 1, 0),
         fit_in = ifelse(psc == 0 | is.na(psc) | is.na(lp) | is.na(up) |
                           lp == Inf | up == Inf, 0, 1),
         inc_in = ifelse(pop_in == 0 | 
                           fit_in == 0 | 
                           Country %in% c("Azerbaijan", "Armenia"), 
                         0, 1))

write_rds(all_fit, "data_inter/baselines_all_countries_ages.rds")

# saving merged estimates
write_rds(all_fit, "data_output/p_scores_excess_deaths_rates.rds")
write_csv(all_fit, paste0("data_output/preliminary_pscores_", today(), ".csv"))
write_rds(all_fit, paste0("data_output/preliminary_pscores_", today(), ".rds"))

