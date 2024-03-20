library(tidyverse)
library(stringi)
library(lubridate)

Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

out05 <- read.csv("N:/COVerAGE-DB/Data/Output_5_internal.csv", encoding = "UTF-8")
out05

out2 <- 
  out05 %>% 
  mutate(Date = dmy(Date)) %>% 
  filter(Date <= "2022-12-31") %>% 
  group_by(Country, Region) %>% 
  filter(Date == max(Date)) %>% 
  drop_na(Deaths) %>% 
  filter(Region == "All",
         Sex == "b")
  
all <- 
  out2 %>% 
  group_by() %>% 
  summarise(sum(Deaths))

yng <- 
  out2 %>% 
  filter(Age <= 20) %>% 
  group_by() %>% 
  summarise(sum(Deaths))

out2 %>% 
  filter(Age > 20) %>% 
  group_by() %>% 
  summarise(sum(Deaths))

yng / all
