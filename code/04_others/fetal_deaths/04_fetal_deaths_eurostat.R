rm (list = ls())
source("Code/00_functions.R")
library(readxl)


# from demographic indicators
sbs_dm <- 
  read_xlsx("Data/Eurostat/demo_mfoet_page_spreadsheet.xlsx",
            sheet = "Sheet 1",
            skip = 8)

# from cause of death data
sbs_cd <- 
  read_xlsx("Data/Eurostat/hlth_cd_aperro__custom_3065555_spreadsheet.xlsx",
            sheet = "Sheet 1",
            skip = 10)

# live births
bts <- 
  read_xlsx("Data/Eurostat/tps00204_page_spreadsheet.xlsx",
            sheet = "Sheet 1",
            skip = 7)

# neonatal deaths
neo <-
  read_xlsx("Data/Eurostat/demo_minf__custom_3021489_page_spreadsheet.xlsx",
            sheet = "Sheet 1",
            skip = 8)

sbs_dm2 <- 
  sbs_dm %>% 
  rename(country = 1) %>% 
  gather(-country, key = year, value = sbs) %>% 
  mutate(sbs = sbs %>% as.double()) %>% 
  # filter(country != ":") %>% 
  drop_na() %>% 
  group_by(country) %>% 
  filter(max(year) == 2020)

sbs_cd2 <- 
  sbs_cd %>% 
  gather(-1, key = year, value = sbs) %>% 
  mutate(sbs = sbs %>% as.double()) %>% 
  drop_na(sbs) %>% 
  rename(country = 1) %>% 
  group_by(country) %>% 
  filter(max(year) == 2020) %>% 
  ungroup() %>% 
  mutate(country = ifelse(country == "Germany (until 1990 former territory of the FRG)", "Germany (FRG)", country))


unique(sbs_dm2$country)
unique(sbs_cd2$country)

bts2 <- 
  bts %>% 
  rename(country = TIME) %>% 
  gather(-country, key = year, value = bts) %>% 
  mutate(bts = bts %>% as.double()) %>% 
  filter(country != ":") %>% 
  drop_na() %>% 
  group_by(country) %>% 
  filter(max(year) == 2020) %>% 
  ungroup() %>% 
  mutate(country = ifelse(country == "Germany (until 1990 former territory of the FRG)", "Germany (FRG)", country))

neo2 <- 
  neo %>% 
  rename(country = TIME) %>% 
  gather(-country, key = year, value = neo) %>% 
  mutate(neo = neo %>% as.double()) %>% 
  filter(country != ":",
         country != "Germany including former GDR") %>% 
  drop_na() %>% 
  group_by(country) %>% 
  filter(max(year) == 2020) %>% 
  ungroup() %>% 
  mutate(country = ifelse(country == "Germany (until 1990 former territory of the FRG)", "Germany (FRG)", country))

db <- 
  bts2 %>% 
  left_join(sbs_cd2 %>% rename(sbs_cd = sbs)) %>% 
  left_join(sbs_dm2 %>% rename(sbs_dm = sbs)) %>%
  left_join(neo2) %>% 
  filter(!str_detect(country, "Euro"))







%>% 
  mutate(exposure = bts + sbs,
         outcome = sbs)