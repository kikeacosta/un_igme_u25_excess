rm (list = ls())
source("Code/00_functions.R")

# ~~~~~~~~~~~~~~~~~~~~
# Sensitivity analysis
# ~~~~~~~~~~~~~~~~~~~~

# Running analyses, predicting excess for previous years (2017:2019)

dts_all <- 
  read_rds("data_inter/annual_deaths_rates_2010_2021.rds") %>% 
  filter(type_data == "counts") %>% 
  mutate(Deaths = round(Deaths, 0),
         Population = round(Population, 0))

unique(dts_all$Country)

# # Including only countries with at least 50 infant deaths in 2015-2019
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# cts_exc <- 
#   dts_inf2 %>% 
#   filter(Year %in% 2015:2019,
#          Age == "Infant",
#          Sex == "t") %>% 
#   group_by(Country) %>% 
#   summarise(av_dts = mean(Deaths)) %>% 
#   ungroup() %>% 
#   filter(av_dts <= 50) %>% 
#   pull(Country)
# 
# cts_exc <- c()
# 
# dts_all2 <- 
#   dts_all %>% 
#   drop_na(Population) %>% 
#   filter(!Country %in% cts_exc)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fitting the Poisson model in each age group and sex
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fit_sensitivity <- 
  function(ypre = 2019,
           ymin = 2014,
           ymax = 2018){
    
    cts_noenough_data <- 
      dts_all %>% 
      filter(Year %in% ymin:ymax) %>% 
      group_by(Country, Sex, Age) %>% 
      filter(n() < 3) %>% 
      select(Country, Sex, Age) %>% 
      unique() %>% 
      mutate(exc = 1)
    
    dts_all_fit <- 
      dts_all %>% 
      filter(Year %in% ymin:ypre) %>% 
      left_join(cts_noenough_data, 
                by = c("Country", "Age", "Sex")) %>% 
      filter(is.na(exc)) %>% 
      mutate(w = ifelse(Year %in% ymin:ymax, 1, 0)) %>% 
      group_by(Country, Sex, Age) %>% 
      do(est_bsn_sens_analysis_pi(chunk = .data)) %>% 
      ungroup() %>% 
      mutate(
        psc = Deaths / bsn,
        bsn_lp = ifelse(bsn_lp < 0, 0, bsn_lp),
        bsn_up = ifelse(bsn_up < 0, 0, bsn_up),
        up = Deaths / bsn_lp,
        lp = Deaths / bsn_up) %>% 
      select(Country, Code, Year, Age, Sex, Population, Deaths, Rate, Income,
             bsn, bsn_lp, bsn_up, psc, up, lp) %>% 
      mutate(year_pred = ypre)
  }

sensitivity_2017 <- fit_sensitivity(ypre = 2017, ymin = 2012, ymax = 2016)
sensitivity_2018 <- fit_sensitivity(ypre = 2018, ymin = 2013, ymax = 2017)
sensitivity_2019 <- fit_sensitivity(ypre = 2019, ymin = 2014, ymax = 2018)
sensitivity_2020 <- fit_sensitivity(ypre = 2020, ymin = 2015, ymax = 2019)
sensitivity_2021 <- fit_sensitivity(ypre = 2021, ymin = 2015, ymax = 2019)

sensit_2017_2021 <- 
  bind_rows(sensitivity_2017 %>% filter(Year == 2017),
            sensitivity_2018 %>% filter(Year == 2018),
            sensitivity_2019 %>% filter(Year == 2019),
            sensitivity_2020 %>% filter(Year == 2020),
            sensitivity_2021 %>% filter(Year == 2021))

write_rds(sensit_2017_2021, "Output/sens_analysis_p_scores_excess_deaths.rds")

