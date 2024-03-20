rm (list = ls())
library(tidyverse)

# source("Code/00_functions.R")
dts <- 
  read_rds("data_inter/dts_causes_who.rds")


unique(dts$cause)
unique(dts$sex)
unique(dts$age)

dts2 <- 
  dts %>% 
  filter(age <= 20)

# selecting causes with annual average above 5% of all causes
causes_sel <- 
  dts2 %>% 
  filter(cause != "total") %>% 
  group_by(country, year, sex, age) %>% 
  mutate(prop = ifelse(dts == 0, 0, dts/sum(dts))) %>% 
  group_by(country, cause, sex, age) %>% 
  mutate(av_prop = mean(prop)) %>% 
  ungroup() %>% 
  filter(av_prop >= 0.1 | cause == "external" | cause == "covid19") %>% 
  filter(!(cause %in% c("other"))) %>% 
  select(country, cause, sex, age) %>% 
  unique()

dts3 <- 
  dts2 %>% 
  inner_join(causes_sel) %>% 
  bind_rows(dts2 %>% 
              filter(cause != "total") %>% 
              anti_join(causes_sel) %>% 
              group_by(country, year, sex, age) %>% 
              summarise(dts = sum(dts)) %>% 
              ungroup() %>% 
              mutate(cause = "other")) %>% 
  arrange(country, year, sex, age, cause)

# test
# ~~~~
dts3 %>% 
  group_by(country, year, sex) %>% 
  summarise(dts2 = sum(dts)) %>% 
  left_join(dts2 %>% 
              filter(cause != "total") %>% 
              group_by(country, year, sex) %>% 
              summarise(dts_or = sum(dts)))

unique(dts3$cause)

# adding all cause mortality
dts4 <- 
  dts3 %>% 
  bind_rows(dts2 %>% 
              filter(cause == "total"))

# adding population exposures
pop <- read_rds("data_inter/exposures_5-year_groups.rds")

pop2 <- 
  pop %>% 
  rename_with(tolower) %>% 
  select(-source)

dts5 <- 
  dts4 %>% 
  # filter(age <= 20) %>% 
  left_join(pop2) %>% 
  arrange(year, sex, age, cause)
  
write_rds(dts5, "data_inter/master_causes_who.rds")

test <- 
  dts2 %>% 
  filter(cause != "total") %>% 
  group_by(country, year, sex, age) %>% 
  mutate(dts_sum = sum(dts)) %>% 
  left_join(dts2 %>% 
              filter(cause == "total") %>% 
              select(country, year, sex, age, dts_tot = dts))

test2 <- 
  dts5 %>% 
  filter(cause != "total") %>% 
  group_by(country, year, sex, age) %>% 
  mutate(dts_sum = sum(dts)) %>% 
  left_join(dts5 %>% 
              filter(cause == "total") %>% 
              select(country, year, sex, age, dts_tot = dts))

