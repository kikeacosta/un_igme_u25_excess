rm (list = ls())
source("code/00_functions.R")

# ~~~~~~~~~~~~~~~~~~~~
# Sensitivity analysis
# ~~~~~~~~~~~~~~~~~~~~
# Running analyses, predicting excess for previous years (2017:2019)


cts_exc <- c("Uzbekistan", "Armenia")

dts_all <- 
  read_rds("data_inter/annual_deaths_rates_2010_2022.rds") %>% 
  filter(type_data == "counts",
         !Country %in% cts_exc) %>% 
  mutate(Deaths = round(Deaths, 0),
         Exposure = round(Exposure, 0))
# %>% 
  # filter(Country %in% c("Brazil", "Colombia", "USA", "Switzerland", "Sweden",
  #                       "Poland", "Spain"))

dts_all %>% 
  filter(Sex == "t") %>% 
  select(Country, Year) %>% 
  unique() %>% 
  group_by(Year) %>% 
  summarise(n = n())

unique(dts_all$Country)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fitting the Poisson model in each age group and sex
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ypre = 2017
ymin = 2012
ymax = 2016

# chunk <- 
#   dts_all_fit %>% 
#   filter(Country == "Montenegro",
#          Sex == "f",
#          Age == "5-9")

ypre = 2017; ymin = 2012; ymax = 2016

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
      unique()
    
    # unique(cts_noenough_data$Country)
    
    dts_all_fit <- 
      dts_all %>% 
      filter(Year %in% ymin:ypre) %>% 
      anti_join(cts_noenough_data, 
                by = c("Country", "Age", "Sex")) %>% 
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
      select(Country, Code, Year, Age, Sex, Exposure, Deaths, Rate, Income,
             bsn, bsn_lp, bsn_up, psc, up, lp) %>% 
      mutate(year_pred = ypre)
  }

test <- 
  dts_all_fit %>% 
  filter(Sex == "t") %>% 
  pull(Country) %>% unique()

count_cts <- 
  function(ypre = 2019,
           ymin = 2014,
           ymax = 2018){
    
    cts_noenough_data <- 
      dts_all %>% 
      filter(Year %in% ymin:ymax) %>% 
      group_by(Country, Sex, Age) %>% 
      filter(n() < 3) %>% 
      select(Country, Sex, Age) %>% 
      unique()
    
    cts_inc <- 
      dts_all %>% 
      filter(Year %in% ymin:ypre) %>% 
      anti_join(cts_noenough_data, 
                by = c("Country", "Age", "Sex")) %>% 
      filter(Sex == "t") %>% 
      pull(Country) %>% 
      unique()
    
  }

c17 <- count_cts(ypre = 2017, ymin = 2012, ymax = 2016)
c18 <- count_cts(ypre = 2018, ymin = 2013, ymax = 2017)
c19 <- count_cts(ypre = 2019, ymin = 2014, ymax = 2018)

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

write_rds(sensit_2017_2021, "data_output/sens_analysis_p_scores_excess_deaths.rds")

