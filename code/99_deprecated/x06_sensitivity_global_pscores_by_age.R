rm (list = ls())
source("Code/00_functions.R")

dts_all <- 
  read_rds("data_inter/annual_deaths_rates_2010_2021.rds") %>% 
  filter(type_data == "counts",
         Income != "No income data",
         !Age %in% c("Stillbirths", "Neonatal")) %>% 
  mutate(Deaths = round(Deaths, 0),
         Exposure = round(Exposure, 0))

unique(dts_all$Country)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fitting the Poisson model to aggregated deaths in each age group and sex
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# db <- dts_2012_2020
# ypre = 2021
# ymin = 2015
# ymax = 2019

fit_global_sensitivity <- 
  function(db,
           ypre = 2019,
           ymin = 2014,
           ymax = 2018){
    
    comm_cts <- 
      db %>% 
      filter(Year %in% ymin:ypre) %>% 
      select(Country, Year, Age, Sex, Deaths) %>% 
      unique() %>% 
      spread(Year, Deaths) %>% 
      drop_na() %>% 
      select(Country, Age, Sex)
    
    dts_tgh <- 
      db %>% 
      filter(Year %in% ymin:ypre) %>% 
      semi_join(comm_cts) %>% 
      group_by(Income, Sex, Age, Year) %>% 
      summarise(Deaths = sum(Deaths),
                Exposure = sum(Exposure)) %>% 
      ungroup()
    
    dts_tgh_tot <- 
      dts_tgh %>% 
      group_by(Sex, Age, Year) %>% 
      summarise(Deaths = sum(Deaths),
                Exposure = sum(Exposure)) %>% 
      ungroup() %>% 
      mutate(Income = "Total")
    
    dts_tgh_all <- 
      dts_tgh %>% 
      bind_rows(dts_tgh_tot)
    
    dts_all_fit <- 
      dts_tgh_all %>% 
      filter(Year %in% ymin:ypre) %>% 
      mutate(w = ifelse(Year %in% ymin:ymax, 1, 0)) %>% 
      group_by(Income, Sex, Age) %>% 
      do(est_bsn_sens_analysis_pi(chunk = .data)) %>% 
      ungroup() %>% 
      mutate(
        psc = Deaths / bsn,
        bsn_lp = ifelse(bsn_lp < 0, 0, bsn_lp),
        bsn_up = ifelse(bsn_up < 0, 0, bsn_up),
        up = Deaths / bsn_lp,
        lp = Deaths / bsn_up) %>% 
      select(Income, Year, Age, Sex, Deaths, Exposure,
             bsn, bsn_lp, bsn_up, psc, up, lp) %>% 
      mutate(year_pred = ypre)
    
    return(dts_all_fit)
  }


# not the same countries in all estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  sensitivity_2017 <- fit_global_sensitivity(dts_all, ypre = 2017, ymin = 2012, ymax = 2016)
  sensitivity_2018 <- fit_global_sensitivity(dts_all, ypre = 2018, ymin = 2013, ymax = 2017)
  sensitivity_2019 <- fit_global_sensitivity(dts_all, ypre = 2019, ymin = 2014, ymax = 2018)
  sensitivity_2020 <- fit_global_sensitivity(dts_all, ypre = 2020, ymin = 2015, ymax = 2019)
  sensitivity_2021 <- fit_global_sensitivity(dts_all, ypre = 2021, ymin = 2015, ymax = 2019)
  
  sensit_2017_2021 <- 
    bind_rows(sensitivity_2017 %>% filter(Year == 2017),
              sensitivity_2018 %>% filter(Year == 2018),
              sensitivity_2019 %>% filter(Year == 2019),
              sensitivity_2020 %>% filter(Year == 2020),
              sensitivity_2021 %>% filter(Year == 2021))
  
  write_rds(sensit_2017_2021, "Output/sens_analysis_global_p_scores_excess_deaths_cts_independent.rds")
}

# same countries in all estimates 2017-2020
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  cts_2012_2020 <- 
    dts_all %>% 
    filter(Year %in% 2012:2020) %>% 
    select(Country, Year, Age, Sex, Deaths) %>% 
    unique() %>% 
    spread(Year, Deaths) %>% 
    drop_na() %>% 
    select(Country, Age, Sex)
  
  dts_2012_2020 <- 
    dts_all %>% 
    filter(Year %in% 2012:2021) %>% 
    semi_join(cts_2012_2020)
    
  sensitivity_2017 <- 
    fit_global_sensitivity(dts_2012_2020, 
                           ypre = 2017, ymin = 2012, ymax = 2016)
  sensitivity_2018 <- 
    fit_global_sensitivity(dts_2012_2020, 
                           ypre = 2018, ymin = 2013, ymax = 2017)
  sensitivity_2019 <- 
    fit_global_sensitivity(dts_2012_2020, 
                           ypre = 2019, ymin = 2014, ymax = 2018)
  sensitivity_2020 <- 
    fit_global_sensitivity(dts_2012_2020, 
                           ypre = 2020, ymin = 2015, ymax = 2019)
  sensitivity_2021 <- 
    fit_global_sensitivity(dts_2012_2020, 
                           ypre = 2021, ymin = 2015, ymax = 2019)
  
  sensit_2017_2021 <- 
    bind_rows(sensitivity_2017 %>% filter(Year == 2017),
              sensitivity_2018 %>% filter(Year == 2018),
              sensitivity_2019 %>% filter(Year == 2019),
              sensitivity_2020 %>% filter(Year == 2020),
              sensitivity_2021 %>% filter(Year == 2021))
  
  write_rds(sensit_2017_2021, "Output/sens_analysis_global_p_scores_excess_deaths_cts_2017_2020.rds")
}

# same countries in all estimates 2017-2021
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  cts_2012_2021 <- 
    dts_all %>% 
    filter(Year %in% 2012:2021) %>% 
    select(Country, Year, Age, Sex, Deaths) %>% 
    unique() %>% 
    spread(Year, Deaths) %>% 
    drop_na() %>% 
    select(Country, Age, Sex)
  
  dts_2012_2021 <- 
    dts_all %>% 
    filter(Year %in% 2012:2021) %>% 
    semi_join(cts_2012_2021)
  
  sensitivity_2017 <- 
    fit_global_sensitivity(dts_2012_2021, 
                           ypre = 2017, ymin = 2012, ymax = 2016)
  sensitivity_2018 <- 
    fit_global_sensitivity(dts_2012_2021, 
                           ypre = 2018, ymin = 2013, ymax = 2017)
  sensitivity_2019 <- 
    fit_global_sensitivity(dts_2012_2021, 
                           ypre = 2019, ymin = 2014, ymax = 2018)
  sensitivity_2020 <- 
    fit_global_sensitivity(dts_2012_2021, 
                           ypre = 2020, ymin = 2015, ymax = 2019)
  sensitivity_2021 <- 
    fit_global_sensitivity(dts_2012_2021, 
                           ypre = 2021, ymin = 2015, ymax = 2019)
  
  sensit_2017_2021 <- 
    bind_rows(sensitivity_2017 %>% filter(Year == 2017),
              sensitivity_2018 %>% filter(Year == 2018),
              sensitivity_2019 %>% filter(Year == 2019),
              sensitivity_2020 %>% filter(Year == 2020),
              sensitivity_2021 %>% filter(Year == 2021))
  
  write_rds(sensit_2017_2021, "Output/sens_analysis_global_p_scores_excess_deaths_cts_2017_2021.rds")
}




