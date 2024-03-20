
# Exposures data
# ~~~~~~~~~~~~~~

# exposures data from:
# WPP projection estimates by age and sex for all ages
# HMD exposures for countries with HMD deaths
# STFF data on births in 2020 (version 13.10.2021)
# Country Consultation data (CCD)
# other sources for births in 2020

library(here)
source(here("Code", "00_functions.R"))
library(HMDHFDplus)
library(countrycode)

# mortality data
# ~~~~~~~~~~~~~~
db_d <- read_rds("Output/data2021/stmf_2021.rds")

all_cts <- 
  db_d %>% 
  pull(Country) %>% 
  unique()

# ~~~~~~~~~~~~~~~~~~~~~~~~
# population data from HMD
# ~~~~~~~~~~~~~~~~~~~~~~~~

# hmd_exps2 <- 
#   hmd_exps %>% 
#   select(-OpenInterval) %>% 
#   gather(Female, Male, Total, key = Sex, value = Population) %>% 
#   mutate(Sex = recode(Sex,
#                       "Female" = "f",
#                       "Male" = "m",
#                       "Total" = "t"),
#          Country = countrycode(Code, origin = "iso3c",
#                                destination = "country.name"),
#          Country = case_when(Code == "GBRTENW" ~ "England and Wales",
#                              Code == "GBR_NIR" ~ "Northern Ireland",
#                              Code == "GBR_SCO" ~ "Scotland",
#                              TRUE ~ Country))

# ~~~~~~~~~~~~~~~~~~~~~~~~
# population data from WPP
# ~~~~~~~~~~~~~~~~~~~~~~~~

ctrs <- 
  db_d %>% 
  filter(Source != "hmd") %>% 
  pull(Country) %>% 
  unique()

pop_f <- read_xlsx(here("Data", "WPP2019_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.xlsx"),
                   skip = 16) %>% 
  select(3, 8:109) %>% 
  rename(Country = 1, 
         Year = 2) %>% 
  mutate(Country = case_when(
    Country == "United States of America" ~ "USA",
    Country == "Republic of Korea" ~ "South Korea",
    Country == "Russian Federation" ~ "Russia",
    Country == "China, Taiwan Province of China" ~ "Taiwan",
    Country == "Iran (Islamic Republic of)" ~ "Iran",
    Country == "Republic of Moldova" ~ "Moldova",
    TRUE ~ Country)
  ) %>% 
  filter(Country %in% ctrs, 
         Year >= 2015) %>% 
  gather(-Country, -Year, key = "Age", value = "Population") %>% 
  mutate(Age = as.integer(Age),
         Population = as.double(Population) * 1000,
         Sex = "f") %>% 
  arrange(Country, Age)

pop_m <- read_xlsx(here("Data", "WPP2019_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.xlsx"),
                   skip = 16) %>% 
  select(3, 8:109) %>% 
  rename(Country = 1, 
         Year = 2) %>% 
  mutate(Country = case_when(
    Country == "United States of America" ~ "USA",
    Country == "Republic of Korea" ~ "South Korea",
    Country == "Russian Federation" ~ "Russia",
    Country == "China, Taiwan Province of China" ~ "Taiwan",
    Country == "Iran (Islamic Republic of)" ~ "Iran",
    Country == "Republic of Moldova" ~ "Moldova",
    TRUE ~ Country)
  ) %>% 
  filter(Country %in% ctrs, 
         Year >= 2015) %>% 
  gather(-Country, -Year, key = "Age", value = "Population") %>% 
  mutate(Age = as.integer(Age),
         Population = as.double(Population) * 1000,
         Sex = "m") %>% 
  arrange(Country, Age)

pop <- 
  bind_rows(pop_f, pop_m)

pop2 <- 
  pop %>% 
  group_by(Country, Year, Age) %>% 
  summarise(Population = sum(Population)) %>% 
  ungroup() %>% 
  mutate(Sex = "t") %>% 
  bind_rows(pop) %>% 
  mutate(Code = countrycode(Country, origin = "country.name",
                            destination = "iso3c"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# merging WPP and HMD exposures
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pop3 <- 
  pop2 

c(unique(pop$Country), unique(db_d$Country))

country_list <- 
  tibble(Country = c(unique(pop3$Country), unique(db_d$Country))) %>% 
  left_join(tibble(Country = unique(pop3$Country)) %>% 
              mutate(pop = 1))


# write_rds(pop3, "Output/exposures_single-years.rds")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Population data at age 0 from births data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# db_births <- 
#   read_rds("Output/annual_births_2015_2020.rds")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merging Population data from WPP, HMD, CCS, and STFF
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# comparing data from STFF and WPP
db_births2 <- 
  db_births %>% 
  select(-Code) %>% 
  mutate(Age = 0)

# Sex imputation for births, based on previous sex distribution
# Estimate a correction factor for all sex combined to be applied to each sex
adjust_age0 <- 
  pop3 %>% 
  filter(Sex == "t") %>% 
  left_join(db_births2) %>% 
  drop_na() %>% 
  mutate(factor = Births / Population) %>% 
  select(Country, Year, factor)

# births not included in population estimates
cts_bth_in_pop <- 
  adjust_age0 %>% 
  select(Country, Year) %>% 
  unique() %>% 
  mutate(incl = 1)

bts_nonpop <- 
  db_births %>% 
  left_join(cts_bth_in_pop) %>% 
  filter(Country %in% all_cts,
         is.na(incl)) %>% 
  mutate(Age = 0,
         Sex = "t") %>% 
  rename(Population = Births) %>% 
  select(-incl)

# adjusting age 0 using births data
pop_age0 <- 
  pop3 %>% 
  filter(Age == 0) %>% 
  left_join(adjust_age0) %>% 
  mutate(Population = ifelse(!is.na(factor), 
                             Population * factor, Population)) %>% 
  select(-factor) %>% 
  mutate(Age = 0) %>% 
  bind_rows(bts_nonpop)

# merging population with adjusted age 0
pop4 <- 
  pop3 %>% 
  filter(Age != 0) %>% 
  bind_rows(pop_age0) %>% 
  arrange(Country, Year, Sex, Age)

unique(pop4$Country)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# grouping population age groups in same categories as deaths 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

group_exposures <- function(chunk){
  
  ct <- unique(chunk$Country)
  sx <- unique(chunk$Sex)
  yr <- unique(chunk$Year)
  
  int <- 
    c(db_d %>% 
        filter(Country == ct,
               Year == yr,
               Sex == sx) %>% 
        pull(Age) %>% 
        unique(), 25, 200)
  
  labs <- int[1:length(int)-1]
  
  chunk %>% 
    mutate(Age_int = cut(Age, 
                         breaks = int, 
                         include.lowest = TRUE, 
                         right = F, 
                         labels = labs),
           Age_int = as.numeric(as.character(Age_int))) %>% 
    select(-Age) %>% 
    rename(Age = Age_int) %>% 
    group_by(Age) %>% 
    summarise(Population = sum(Population)) %>% 
    ungroup()
  
}

# periods of death data
ct_dths_pers <- 
  db_d %>% 
  select(Country, Year) %>% 
  unique() %>% 
  mutate(incl = 1)

pop_ints <- 
  pop4 %>% 
  left_join(ct_dths_pers) %>% 
  drop_na() %>% 
  group_by(Country, Sex, Year) %>% 
  do(group_exposures(chunk = .data))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merging deaths and population 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db_d_p <- 
  db_d %>% 
  left_join(pop_ints) %>% 
  arrange(Country, Year, Sex)

# only keeping data that will be useful for comparing to 2020
useful_for_2020 <- 
  db_d_p %>% 
  filter(Year == 2020) %>% 
  drop_na() %>% 
  select(Country, Sex, Age, age_up) %>% 
  unique() %>% 
  mutate(incl = 1)

db_d_p2 <- 
  db_d_p %>% 
  left_join(useful_for_2020) %>% 
  filter(incl == 1) %>% 
  select(-incl)

write_rds(db_d_p2, "Output/annual_deaths_pop.rds")


# db_d_p %>% 
#   filter(Age == 0) %>% 
#   ggplot()+
#   geom_line(aes(Age, Deaths, col = Year, group = Year))+
#   scale_y_log10()+
#   facet_wrap(Country ~ Sex)


# ~~~~~~~~~~~~~~
# comparing data
# ~~~~~~~~~~~~~~

cts <- c()

db_d_p2 %>% 
  # filter(Country == "Iran") %>% 
  mutate(mx = Deaths / Population,
         Age_int = Age + 0.5*age_up) %>% 
  ggplot()+
  geom_line(aes(Age_int, mx, col = Year, group = Year))+
  facet_wrap(Country ~ Sex)+
  scale_y_log10()+
  theme_bw()


