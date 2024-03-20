rm(list=ls())
source("code/00_functions.R")

# exposures for females and males from WPP
# data downloaded from the WPP website
# https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/2_Population/WPP2022_POP_F01_2_POPULATION_SINGLE_AGE_MALE.xlsx
# https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/2_Population/WPP2022_POP_F01_3_POPULATION_SINGLE_AGE_FEMALE.xlsx

pop_f_1021 <- read_xlsx("data_input/WPP2022_POP_F01_3_POPULATION_SINGLE_age_FEMALE.xlsx",
                   skip = 16) %>% 
  select(3, 9, 11:112) %>% 
  filter(Type == "Country/Area") %>% 
  select(-Type) %>% 
  rename(country = 1, 
         year = 2) %>% 
  filter(year >= 2005) %>% 
  gather(-country, -year, key = "age", value = "pop") %>% 
  mutate(age = as.integer(age),
         pop = as.double(pop) * 1000,
         sex = "f") %>% 
  arrange(country, age)

pop_f_2225 <- read_xlsx("data_input/WPP2022_POP_F01_3_POPULATION_SINGLE_age_FEMALE.xlsx",
                   sheet = 2,
                   skip = 16) %>% 
  select(3, 9, 11:112) %>% 
  filter(Type == "Country/Area") %>% 
  select(-Type) %>% 
  rename(country = 1, 
         year = 2) %>% 
  filter(year <= 2025) %>% 
  gather(-country, -year, key = "age", value = "pop") %>% 
  mutate(age = as.integer(age),
         pop = as.double(pop) * 1000,
         sex = "f") %>% 
  arrange(country, age)

pop_m_1021 <- read_xlsx("data_input/WPP2022_POP_F01_2_POPULATION_SINGLE_age_MALE.xlsx",
                   skip = 16) %>% 
  select(3, 9, 11:112) %>% 
  filter(Type == "Country/Area") %>% 
  select(-Type) %>% 
  rename(country = 1, 
         year = 2) %>% 
  filter(year >= 2005) %>% 
  gather(-country, -year, key = "age", value = "pop") %>% 
  mutate(age = as.integer(age),
         pop = as.double(pop) * 1000,
         sex = "m") %>% 
  arrange(country, age)

pop_m_2225 <- read_xlsx("data_input/WPP2022_POP_F01_2_POPULATION_SINGLE_age_MALE.xlsx",
                        sheet = 2,
                        skip = 16) %>% 
  select(3, 9, 11:112) %>% 
  filter(Type == "Country/Area") %>% 
  select(-Type) %>% 
  rename(country = 1, 
         year = 2) %>% 
  filter(year <= 2025) %>% 
  gather(-country, -year, key = "age", value = "pop") %>% 
  mutate(age = as.integer(age),
         pop = as.double(pop) * 1000,
         sex = "m") %>% 
  arrange(country, age)

pop <- 
  bind_rows(pop_f_1021, 
            pop_f_2225, 
            pop_m_1021, 
            pop_m_2225) %>%
  mutate(country = case_when(
    country == "United States of America" ~ "USA",
    country == "Republic of Korea" ~ "South Korea",
    country == "Dem. People's Republic of Korea" ~ "North Korea",
    country == "Russian Federation" ~ "Russia",
    country == "China, Taiwan Province of China" ~ "Taiwan",
    country == "China, Hong Kong SAR" ~ "Hong Kong",
    country == "China, Macao SAR" ~ "Macao",
    country == "Iran (Islamic Republic of)" ~ "Iran",
    country == "Republic of Moldova" ~ "Moldova",
    country == "Bolivia (Plurinational State of)" ~ "Bolivia",
    country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    country == "Kosovo (under UNSC res. 1244)" ~ "Kosovo",
    str_detect(country, "rkiye") ~ "Turkey",
    TRUE ~ country)) %>% 
  select(country, year, sex, age, pop)

unique(pop$country)

pop_wpp <- 
  pop %>% 
  group_by(country, year, age) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(sex = "t") %>% 
  bind_rows(pop) %>% 
  mutate(code = countrycode(country, origin = "country.name",
                            destination = "iso3c"),
         code = ifelse(country == "Kosovo", "RKS", code),
         source = "wpp",
         age = ifelse(is.na(age), 100, age))

unique(pop_wpp$country)
unique(pop_wpp$year)
unique(pop_wpp$age)

write_rds(pop_wpp, "data_inter/wpp_populations_ages_0_24_v2022.rds",
          compress = "xz")
