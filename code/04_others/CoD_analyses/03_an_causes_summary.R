source("Code/00_functions.r")
# Cause of deaths analysis

dts2 <- 
  read_rds("output/cod/cod_deaths_pop_country_cause_sex_age.rds")

dts3 <- 
  dts2 %>% 
  group_by(year, country, cause_cod, cause, age) %>% 
  summarise(dts = sum(dts),
            pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(sex = "t") %>% 
  bind_rows(dts2)

unique(dts3$cause)

total_test <- 
  dts3 %>% 
  filter(cause == "ALL CAUSES") %>% 
  select(country, sex, age, year, tot_dts = dts) %>% 
  left_join(dts3 %>% 
              filter(cause != "ALL CAUSES") %>%
              group_by(country, sex, age, year) %>%
              summarise(sum_dts = sum(dts)))

dist_cause <- 
  dts3 %>% 
  filter(cause != "ALL CAUSES") %>% 
  left_join(dts3 %>% 
              filter(cause == "ALL CAUSES") %>% 
              rename(dts_tot = dts) %>% 
              select(year, country, sex, age, dts_tot))
  



