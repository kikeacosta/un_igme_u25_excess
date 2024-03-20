rm (list = ls())
source("code/00_functions.R")

zaf <- read_xlsx("data_input/unicef/230425_south_africa_rates.xlsx",
                sheet = 2)

zaf2 <- 
  zaf %>% 
  select(-sex) %>% 
  gather(-measure, key = year, value = value) %>% 
  filter(measure %in% c("Stillbirth rate", "NMR")) %>% 
  drop_na() %>% 
  mutate(year = year %>% as.double) %>% 
  filter(year >= 2014) %>% 
  mutate(measure = recode(measure,
                          'Stillbirth rate' = "sbs_r",
                          'NMR' = 'neo_r'),
         country = "South Africa",
         code = "ZAF",
         source = "rms_zaf")

write_rds(zaf2, "data_inter/neo_sbs_zaf.rds")
