rm (list = ls())
source("code/00_functions.R")
set.seed(2019)

# loading excess estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~
all_fit <- 
  read_rds("data_inter/baselines_all_countries_ages.rds") 


# quick summary
# ~~~~~~~~~~~~~
all_fit %>% 
  filter(Year %in% 2020:2022,
         Sex == "t") %>% 
  select(Country, Year, Age) %>% 
  unique() %>% 
  group_by(Year, Age) %>% 
  summarise(n = n()) %>% 
  spread(Year, n)

# excluded after pop threshold (<500K)
all_fit %>% 
  filter(Year %in% 2020:2022,
         Sex == "t",
         pop_in == 0) %>% 
  select(Country, Year, Age) %>% 
  unique() %>% 
  group_by(Year, Age) %>% 
  summarise(n = n()) %>% 
  spread(Year, n)

# quick summary after pop threshold and fitting ok
all_fit %>% 
  filter(Year %in% 2020:2022,
         Sex == "t",
         inc_in == 1) %>% 
  select(Country, Year, Age) %>% 
  unique() %>% 
  group_by(Year, Age) %>% 
  summarise(n = n()) %>% 
  spread(Year, n)

# fitting issues
all_fit %>% 
  filter(Year %in% 2020:2022,
         Sex == "t",
         fit_in == 0) %>% 
  select(Country, Year, Age) %>% 
  unique() %>% 
  group_by(Year, Age) %>% 
  summarise(n = n()) %>% 
  spread(Year, n)

# fitting issues after pop threshold
all_fit %>% 
  filter(Year %in% 2020:2022,
         Sex == "t",
         pop_in == 1,
         fit_in == 0) %>% 
  select(Country, Year, Age) %>% 
  unique() %>% 
  group_by(Year, Age) %>% 
  summarise(n = n()) %>% 
  spread(Year, n)

# difference in sample population after exclusion
all_fit %>% 
  filter(Year %in% 2020:2022,
         Sex == "t") %>% 
  group_by(Year, Age, inc_in) %>% 
  summarise(Exposure = sum(Exposure)) %>% 
  spread(inc_in, Exposure) %>% 
  mutate(red = 100 * `0` / `1`) %>% 
  select(Year, Age, red) %>% 
  spread(Year, red)

test <- 
  all_fit %>% 
  filter(Year %in% 2020:2022,
         Sex == "t")

# excluded groups bcs low pop size
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# identifying issues with baseline fitting
cts_low_pop <- 
  all_fit %>% 
  filter(Year %in% 2020:2022,
         pop_in == 0, Sex == "t") %>% 
  select(Country, Sex, Age) %>% 
  unique() %>% 
  arrange(Country, Sex, Age)

# summary of issues
sum_exc <- 
  cts_low_pop %>% 
  group_by(Country, Sex) %>% 
  summarise(Ages = unique(Age) %>% paste(collapse = ", "))


# excluded groups by fitting issues
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# identifying issues with baseline fitting
cts_exc <- 
  all_fit %>% 
  filter(Year %in% 2020:2022,
         fit_in == 0, Sex == "t") %>% 
  select(Country, Sex, Age) %>% 
  unique() %>% 
  arrange(Country, Sex, Age)

# summary of issues
sum_exc <- 
  cts_exc %>% 
  group_by(Country) %>% 
  summarise(Ages = unique(Age) %>% paste(collapse = ", "))

pop_cts <- 
  all_fit %>% 
  filter(Sex == "t",
         Year == 2020) %>% 
  group_by(Country) %>% 
  summarise(pop = sum(Exposure))

av_pop <- read_rds("data_inter/average_observed_population_0_24.rds") %>% pull(av_pop)

# exc_sex_t2 <- 
#   exc_sex_t %>% 
#   left_join(pop_cts) %>% 
#   mutate(rel_av = 100 * round(pop / av_pop, 4))
# 
# write.excel(exc_sex_t2)
# write_csv(exc_sex_t2, "Tables/tableS04_fitting_issues.csv")

# # excluding estimates with issues in the lower prediction 
# ll_0 <- 
#   all_fit %>% 
#   filter(Year %in% 2020:2021) %>% 
#   filter(bsn_lp == 0) %>% 
#   arrange(Country, Age, Year)

# excluding cases with issues
all_fit2 <- 
  all_fit %>% 
  filter(inc_in == 1)

# quick summary after excluding issues
all_fit2 %>% 
  filter(Year %in% 2020:2022,
         Sex == "t") %>% 
  select(Country, Year, Age) %>% 
  unique() %>% 
  group_by(Year, Age) %>% 
  summarise(n = n()) %>% 
  ungroup()


# saving merged estimates
write_rds(all_fit, "data_output/p_scores_excess_deaths_rates.rds")

write_csv(all_fit, paste0("data_output/preliminary_pscores_", today(), ".csv"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# summary of fitted populations
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# update of table1 ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  all <- 
    all_fit2 %>% 
    filter(Year %in% 2020:2022,
           Sex == "t") %>% 
    group_by(Year, Age) %>% 
    summarise(n = n()) %>% 
    spread(Year, n) %>% 
    rename(var = Age)
  all
  write.excel(all)
  
  all2 <- 
    all_fit2 %>% 
    filter(Year %in% 2020:2022,
           Sex == "t") %>% 
    select(Code, Country, Year, Income) %>% 
    unique() %>% 
    group_by(Year, Income) %>% 
    summarise(n = n()) %>% 
    spread(Year, n) %>% 
    rename(var = Income)
  all2
  write.excel(all2)
  
  births <- 
    read_rds("data_inter/annual_births.rds") %>% 
    filter(Year %in% 2020:2022) %>% 
    select(Code, Year) %>% 
    mutate(bts = 1)
  
  bts <- 
    all_fit2 %>% 
    filter(Year %in% 2020:2022,
           Sex == "t") %>% 
    select(Code, Country, Year) %>% 
    unique() %>% 
    left_join(births) %>% 
    drop_na() %>% 
    group_by(Year) %>% 
    summarise(n = n()) %>% 
    spread(Year, n) %>% 
    mutate(var = "Births")
  
  tot <- 
    all_fit2 %>% 
    filter(Year %in% 2020:2022,
           Sex == "t") %>% 
    select(Country, Year) %>% 
    unique() %>% 
    group_by(Year) %>% 
    summarise(n = n()) %>% 
    spread(Year, n) %>% 
    mutate(var = "Total")
  
  all3_ad <- 
    bind_rows(tot,
              all,
              bts,
              all2) %>% 
    select(var, everything()) %>% 
    rename(ad2020 = 2,
           ad2021 = 3,
           ad2022 = 4)
  
  all3_ad
  write.excel(all3_ad)
}


# update of table1 ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  all <- 
    all_fit %>% 
    filter(Year %in% 2020:2022,
           Sex == "t") %>% 
    group_by(Year, Age) %>% 
    summarise(n = n()) %>% 
    spread(Year, n) %>% 
    rename(var = Age)
  
  all
  write.excel(all)
  
  all2 <- 
    all_fit %>% 
    filter(Year %in% 2020:2022,
           Sex == "t") %>% 
    select(Code, Country, Year, Income) %>% 
    unique() %>% 
    group_by(Year, Income) %>% 
    summarise(n = n()) %>% 
    spread(Year, n) %>% 
    rename(var = Income)
  
  all2
  write.excel(all2)
  
  births <- 
    read_rds("data_inter/annual_births.rds") %>% 
    filter(Year %in% 2020:2022) %>% 
    select(Code, Year) %>% 
    mutate(bts = 1)
  
  bts <- 
    all_fit %>% 
    filter(Year %in% 2020:2022,
           Sex == "t") %>% 
    select(Code, Country, Year) %>% 
    unique() %>% 
    left_join(births) %>% 
    drop_na() %>% 
    group_by(Year) %>% 
    summarise(n = n()) %>% 
    spread(Year, n) %>% 
    mutate(var = "Births")
  
  tot <- 
    all_fit %>% 
    filter(Year %in% 2020:2022,
           Sex == "t") %>% 
    select(Country, Year) %>% 
    unique() %>% 
    group_by(Year) %>% 
    summarise(n = n()) %>% 
    spread(Year, n) %>% 
    mutate(var = "Total")
  
  all3 <- 
    bind_rows(tot,
              bts,
              all,
              all2) %>% 
    select(var, everything())
  
  all3
  write.excel(all3)
  
  all3 %>% 
    left_join(all3_ad) %>% 
    select(var, `2020`, ad2020, `2021`,  ad2021)
}

tableS01 <- 
  all3 %>% 
  left_join(all3_ad) %>% 
  replace_na(list(ad2020 = 0,
                  ad2021 = 0,
                  ad2022 = 0))

# write_csv(tableS01, "Tables/tableS01_available_data.csv")


pop_income <- 
  read_rds("data_inter/population_by_income_2022.rds")
unique(pop_income$Income)

pop_inc2 <- 
  pop_income %>% 
  filter(Sex == "t",
         Income == "Total",
         Year %in% 2020:2022) %>% 
  mutate(Age = factor(Age, levels = c("Stillbirths", "Neonatal", "Infant", 
                                      "1-4", "0-4", "5-9", "10-14", "15-19", 
                                      "20-24"))) %>% 
  rename(age2 = Age)

pop_ests <- 
  all_fit %>% 
  filter(Year %in% 2020:2022,
         Sex == "t") %>% 
  group_by(Age, Year) %>% 
  summarise(pop = sum(Exposure)) %>% 
  ungroup() %>% 
  mutate(age2 = as.character(Age),
         age2 = ifelse(age2 %in% c("Stillbirths", "Neonatal"), "Infant", age2),
         age2 = factor(age2, levels = c("Stillbirths", "Neonatal", "Infant", 
                                        "1-4", "0-4", "5-9", "10-14", "15-19", 
                                        "20-24")))

prop <- 
  pop_inc2 %>% 
  left_join(pop_ests) %>% 
  mutate(prop = pop / pop_tot)

tx <- 12
prop %>% 
  ggplot()+
  geom_bar(aes(Age, prop), stat = "identity")+
  facet_grid(~Year)+
  scale_y_continuous(limits = c(0,1))+
  coord_flip()+
  theme_bw()+
  theme(strip.text = element_text(size = tx - 2, face = "bold"),
        strip.background = element_blank())

ggsave("figures/coverage.png",
       w = 6,
       h = 3)
