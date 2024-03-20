rm (list = ls())
source("code/i00_functions.R")

bsn <- read_rds("data_inter/baselines_2015_2021_sex_age_cause_adj.rds")

frc <- read_rds("data_inter/baselines_by_cause_from_fractions.rds")
  
unique(bsn$year)

dt <- 
  frc %>% 
  filter(year == 2020) %>% 
  select(-exposure, -code, -mx) %>% 
  mutate(source = "fractions") %>% 
  arrange(country, sex, age) %>% 
  bind_rows(bsn %>% mutate(source = "baselines"))


write_rds(dt, "data_inter/baselines_by_cause_from_two_methods.rds")

