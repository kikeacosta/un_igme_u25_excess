rm (list = ls())
source("Code/00_functions.R")

dts_all <- 
  read_rds("data_inter/annual_deaths_rates_2010_2021.rds") %>% 
  filter(type_data == "counts") %>% 
  mutate(Deaths = round(Deaths, 0),
         Exposure = round(Exposure, 0))

unique(dts_all$Country)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fitting the Poisson model in each age group and sex
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dts_all_fit <- 
  dts_all %>% 
  filter(Year >= 2015) %>% 
  # filter(Country %in% c("Brazil", "Colombia", "USA", "Switzerland", "Sweden",
  #                       "Poland", "Spain")) %>%
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

# write_rds(out, "Output/p_scores_excess_deaths.rds")

dts_all_fit <- 
  dts_all %>% 
  filter(Year >= 2015) %>% 
  # filter(Country %in% c("Brazil", "Colombia", "USA", "Switzerland", "Sweden",
  #                       "Poland", "Spain")) %>% 
  group_by(Country, Sex, Age) %>% 
  do(est_baseline(chunk = .data)) %>% 
  ungroup()

out_ci <- 
  dts_all_fit %>% 
  mutate(
    bsn_lp = ifelse(bsn_lp < 0, 0, bsn_lp),
    bsn_up = ifelse(bsn_up < 0, 0, bsn_up),
    psc = Deaths / bsn,
    up = Deaths / bsn_lp,
    lp = Deaths / bsn_up)

write_rds(out_ci, "Output/p_scores_excess_deaths_ci.rds")
write_rds(out, "Output/p_scores_excess_deaths.rds")
