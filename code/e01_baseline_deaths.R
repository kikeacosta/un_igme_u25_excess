rm (list = ls())
source("code/00_functions.R")
set.seed(2019)

dts_all <- 
  read_rds("data_inter/annual_deaths_rates_2010_2022.rds") %>% 
  filter(type_data == "counts") %>% 
  mutate(Deaths = round(Deaths, 0),
         Exposure = round(Exposure, 0))

unique(dts_all$Code)
unique(dts_all$Age)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fitting the Poisson model in each age group and sex
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dts_all_fit <- 
  dts_all %>% 
  filter(Year >= 2015) %>% 
  group_by(Country, Sex, Age) %>% 
  do(est_baseline_pi(chunk = .data)) %>% 
  ungroup()

out <- 
  dts_all_fit %>% 
  mutate(
    bsn_lp = ifelse(bsn_lp < 0, 0, bsn_lp),
    bsn_up = ifelse(bsn_up < 0, 0, bsn_up),
    psc = Deaths / bsn,
    up = Deaths / bsn_lp,
    lp = Deaths / bsn_up)

write_rds(out, "data_output/p_scores_excess_deaths.rds")
  
