

rm(list=ls())
library(tidyverse)
library(wpp2022)
data()

data(popAge1dt)

data(mx1dt)

ukr_pop <- 
  popAge1dt %>% 
  filter(name == "Ukraine",
         year == 2021) %>% 
  as_tibble() %>% 
  gather(popM, popF, pop, key = sex, value = pop) %>% 
  mutate(pop = pop*1e3,
         sex = case_when(sex == "popM" ~ "m",
                         sex == "popF" ~ "f",
                         sex == "pop" ~ "t"))

ukr_mxs <- 
  mx1dt %>% 
  filter(name == "Ukraine",
         year == 2021) %>% 
  as_tibble() %>% 
  gather(mxM, mxF, mxB, key = sex, value = mx) %>% 
  mutate(sex = case_when(sex == "mxM" ~ "m",
                         sex == "mxF" ~ "f",
                         sex == "mxB" ~ "t"))

ukr <- 
  ukr_mxs %>% 
  left_join(ukr_pop) %>% 
  mutate(dts = pop * mx)


ukr_war <- read_xlsx("Data/ukraine/children_mortality_ukraine_war_jul_23.xlsx")

ukr2 <- 
  ukr %>% 
  left_join(ukr_war %>% rename(cst = dts)) %>% 
  mutate(dts2 = dts + cst,
         mx2 = dts2/pop,
         psc = dts2 / dts)



ukr2 <- 
  ukr %>% 
  gather(popM, popF, pop, key = sex, value = pop) %>% 
  mutate(age = case_when(age == 0 ~ 1,
                         age %in% 1:4 ~ 1,
                         age >= 5 ~ age - age%%5),
         pop = pop*1e3,
         sex = case_when(sex == "pop" ~ "t",
                         sex == "popF" ~ "f",
                         sex == "popM" ~ "m")) %>% 
  group_by(age, sex) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()




data(mx1dt) 
