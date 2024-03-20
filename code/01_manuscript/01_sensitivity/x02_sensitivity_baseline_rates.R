rm (list = ls())
library(readxl)
source("code/00_functions.R")

rts_all <- 
  read_rds("data_inter/annual_deaths_rates_2010_2021.rds") %>% 
  filter(type_data == "rates") %>% 
  mutate(Deaths = round(Deaths, 0),
         Exposure = round(Exposure, 0))

unique(rts_all$Country)

# ypre = 2020

fit_sensitivity_rts <- 
  function(ypre = 2019,
           ymin = 2014,
           ymax = 2018){
    
    cts_noenough_data <- 
      rts_all %>% 
      filter(Year %in% ymin:ymax) %>% 
      group_by(Country, Sex, Age) %>% 
      filter(n() < 3) %>% 
      select(Country, Sex, Age) %>% 
      unique()
    
    rts_all_fit <- 
      rts_all %>% 
      filter(Year %in% ymin:ypre) %>% 
      anti_join(cts_noenough_data, by = c("Age", "Country", "Sex")) %>% 
      mutate(w = ifelse(Year == ymax, 0, 1)) %>% 
      group_by(Country, Sex, Age) %>% 
      do(fit_log_lm_sensit_pi(chunk = .data)) %>% 
      ungroup() %>% 
      mutate(
        psc = Rate / bsn,
        bsn_lp = ifelse(bsn_lp < 0, 0, bsn_lp),
        bsn_up = ifelse(bsn_up < 0, 0, bsn_up),
        lp = Rate / bsn_up,
        up = Rate / bsn_lp) %>% 
      select(Country, Code, Year, Age, Sex, Exposure, Deaths, Rate, Income,
             bsn, bsn_lp, bsn_up, psc, up, lp) %>% 
      mutate(year_pred = ypre)
    
  }

sensitivity_2017 <- fit_sensitivity_rts(ypre = 2017, ymin = 2012, ymax = 2016)
sensitivity_2018 <- fit_sensitivity_rts(ypre = 2018, ymin = 2013, ymax = 2017)
sensitivity_2019 <- fit_sensitivity_rts(ypre = 2019, ymin = 2014, ymax = 2018)
sensitivity_2020 <- fit_sensitivity_rts(ypre = 2020, ymin = 2015, ymax = 2019)
sensitivity_2021 <- fit_sensitivity_rts(ypre = 2021, ymin = 2015, ymax = 2019)

sensit_2017_2021 <- 
  bind_rows(sensitivity_2017 %>% filter(Year == 2017),
            sensitivity_2018 %>% filter(Year == 2018),
            sensitivity_2019 %>% filter(Year == 2019),
            sensitivity_2020 %>% filter(Year == 2020),
            sensitivity_2021 %>% filter(Year == 2021))

write_rds(sensit_2017_2021, "data_output/sens_analysis_p_scores_excess_rates.rds")


