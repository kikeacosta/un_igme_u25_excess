rm (list = ls())
source("code/00_functions.R")

cts_dts <- 
  read_rds("data_inter/annual_deaths_rates_2010_2022.rds") %>% 
  pull(Country) %>% 
  unique()

# ~~~~~~~~~~~~~~~~~~~~~~~~
# population data from WPP
# ~~~~~~~~~~~~~~~~~~~~~~~~

pop_f <- 
  read_xlsx("data_input/WPP2022_POP_F01_3_POPULATION_SINGLE_AGE_FEMALE.xlsx",
            skip = 16) %>% 
  select(3, 9, 11:112) %>% 
  rename(Country = 1, 
         Type = 2,
         Year = 3) %>% 
  filter(Type %in% c("Country/Area"),
         Year == 2020) %>% 
  gather(-Type, -Country, -Year, key = "Age", value = "Population") %>% 
  mutate(Age = as.integer(Age),
         Population = as.double(Population) * 1000,
         Sex = "f") %>% 
  arrange(Country, Type, Age)

pop_m <- 
  read_xlsx("data_input/WPP2022_POP_F01_2_POPULATION_SINGLE_AGE_MALE.xlsx",
            skip = 16) %>% 
  select(3, 9, 11:112) %>% 
  rename(Country = 1, 
         Type = 2,
         Year = 3) %>% 
  filter(Type %in% c("Country/Area"),
         Year == 2020) %>% 
  gather(-Type, -Country, -Year, key = "Age", value = "Population") %>% 
  mutate(Age = as.integer(Age),
         Population = as.double(Population) * 1000,
         Sex = "m") %>% 
  arrange(Country, Type, Age)

pop <- 
  bind_rows(pop_f, pop_m) %>%
  filter(Age <= 24) %>% 
  mutate(Age = case_when(Age == 0 ~ 0,
                         Age %in% 1:4 ~ 1,
                         Age >= 5 ~ Age - Age%%5)) %>% 
  group_by(Country, Year, Sex) %>% 
  summarise(Population = sum(Population)) %>% 
  ungroup()

pop_wpp <- 
  pop %>% 
  group_by(Country, Year) %>% 
  summarise(Population = sum(Population)) %>% 
  ungroup() %>% 
  mutate(Sex = "t")

av_pop <- 
  pop_wpp %>% 
  summarise(av_pop = mean(Population))

av_pop_obs <- 
  pop_wpp %>% 
  filter(Country %in% cts_dts) %>% 
  summarise(av_pop = mean(Population))

write_rds(av_pop_obs , "data_inter/average_observed_population_0_24.rds")
