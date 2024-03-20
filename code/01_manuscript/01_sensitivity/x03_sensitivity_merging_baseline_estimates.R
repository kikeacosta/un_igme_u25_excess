rm (list = ls())
source("Code/00_functions.R")

# loading excess estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~
dts_fit <- 
  read_rds("data_output/sens_analysis_p_scores_excess_deaths.rds") %>% 
  select(Country, Code, Year, Sex, Age, Deaths, Rate, Exposure, Income, psc, up, lp)

rts_fit <- 
  read_rds("data_output/sens_analysis_p_scores_excess_rates.rds") %>% 
  select(Country, Code, Year, Sex, Age, Deaths, Rate, Exposure, Income, psc, up, lp)

pops <- 
  read_rds("data_inter/exposures_5-year_groups.rds")

pops2 <- 
  pops %>% 
  filter(Year == 2020) %>% 
  group_by(Code, Sex) %>% 
  summarise(pop_yng = sum(Population)) %>% 
  ungroup()

all_fit <- 
  bind_rows(dts_fit, rts_fit) %>% 
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

# excluding cases with issues or small populations
all_fit2 <- 
  all_fit %>% 
  filter(inc_in == 1) 

# saving merged estimates
write_rds(all_fit2, "data_output/sensitivity_p_scores_excess_deaths_rates.rds")

unique(all_fit2$Country)
