rm (list = ls())
source("code/i00_functions.R")

dt1 <- 
  read_rds("data_inter/master_causes_who.rds")

dt2 <- 
  dt1 %>% 
  filter(cause != "total",
         cause != "covid19",
         year %in% 2015:2019) %>% 
  group_by(country, year, sex, age) %>% 
  mutate(dts_tot = sum(dts),
         frc = dts/dts_tot) %>% 
  ungroup() %>% 
  arrange(country, year, sex, age) 

dt3 <- 
  dt2 %>% 
  group_by(country, sex, age) %>% 
  do(fit_fractions(chunk = .data)) %>% 
  ungroup() %>% 
  arrange(country, year, sex, age)

write_rds(dt3, "data_inter/fractions_by_cause.rds")
dt3 <- read_rds("data_inter/fractions_by_cause.rds")

# Estimating all-cause baseline
bsn <- 
  dt1 %>%
  rename(exposure = population) %>% 
  filter(cause == "total",
         year %in% 2015:2020) %>% 
  group_by(country, cause, sex, age) %>% 
  do(est_baseline_pi(chunk = .data)) %>% 
  ungroup()

bsn2 <- 
  bsn %>% 
  select(-cause, -w, -t) %>% 
  rename(dts_tot = dts,
         bsn_tot = bsn,
         lp_tot = lp,
         up_tot = up) %>% 
  select(-lc, -uc)

# deaths by C19
c19 <- 
  dt1 %>% 
  rename(exposure = population) %>% 
  filter(cause == "covid19",
         year >= 2015) %>% 
  mutate(bsn = 0,
         lp = 0,
         up = 0)

# putting all together
dt4 <- 
  dt3 %>%
  left_join(dt1 %>% select(-population)) %>% 
  left_join(bsn2) %>% 
  mutate(bsn = bsn_tot*frc,
         lp = lp_tot*frc,
         up = up_tot*frc) %>% 
  select(-ends_with("tot"), -frc) %>% 
  bind_rows(c19) %>% 
  bind_rows(bsn %>% select(-t, -w, -lc, -uc)) %>% 
  arrange(cause, country, year, sex, age) %>% 
  mutate(exc = dts - bsn,
         mx = 1e5*dts/exposure)

write_rds(dt4, "data_inter/baselines_by_cause_from_fractions.rds")
