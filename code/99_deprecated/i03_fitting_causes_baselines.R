rm (list = ls())
source("code/i00_functions.R")

dt1 <- read_rds("data_inter/master_causes_who.rds")

unique(dt1$cause)
unique(dt1$country)

dt2 <- 
  dt1 %>%
  rename(exposure = population) %>% 
  filter(cause != "covid19",
         year %in% 2015:2020) %>% 
  group_by(country, cause, sex, age) %>% 
  do(est_baseline_pi(chunk = .data)) %>% 
  ungroup()

c19 <- 
  dt1 %>% 
  rename(exposure = population) %>% 
  filter(cause == "covid19") %>% 
  mutate(bsn = 0,
         lc = 0,
         uc = 0,
         lp = 0,
         up = 0)

dt3 <- 
  dt2 %>% 
  bind_rows(c19) %>% 
  mutate(mx = 1e5*dts/exposure,
         bsn_r = 1e5*bsn/exposure,
         lp_r =  1e5*lp/exposure,
         up_r =  1e5*up/exposure)

write_rds(dt3, "data_inter/baselines_2015_2021_sex_age_cause.rds")
dt3 <- read_rds("data_inter/baselines_2015_2021_sex_age_cause.rds")

dt3 %>% 
  filter(cause == "total",
         age == 20,
         country == "Colombia") %>% 
  ggplot()+
  geom_point(aes(year, mx))+
  geom_line(aes(year, bsn_r))+
  facet_grid(~sex)

dt3 %>% 
  filter(cause == "cancer",
         age == 20,
         country == "Colombia") %>% 
  ggplot()+
  geom_point(aes(year, mx))+
  geom_line(aes(year, bsn_r))+
  facet_grid(~sex)


