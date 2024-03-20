source("Code/00_functions.r")
# Cause of deaths analysis

dts <- 
  read_rds("Output/cod/dts_cause_country_sex_age.rds")

ctrs <- 
  dts %>% 
  select(country) %>% 
  unique()

pop_f <- 
  read_xlsx("Data/WPP2019_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.xlsx",
            skip = 16) %>% 
  select(3, 8:109) %>% 
  rename(country = 1, 
         year = 2) %>% 
  filter(country %in% ctrs$country, 
         year >= 2015) %>% 
  gather(-country, -year, key = "age", value = "pop") %>% 
  mutate(age = as.double(age),
         pop = as.double(pop) * 1000,
         sex = "f") %>% 
  arrange(country, sex, age)

pop_m <- read_xlsx(here("Data", "WPP2019_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.xlsx"),
                   skip = 16) %>% 
  select(3, 8:109) %>% 
  rename(country = 1, 
         year = 2) %>% 
  filter(country %in% ctrs$country, 
         year >= 2015) %>% 
  gather(-country, -year, key = "age", value = "pop") %>% 
  mutate(age = as.double(age),
         pop = as.double(pop) * 1000,
         sex = "m") %>% 
  arrange(country, sex, age)

pop <- 
  bind_rows(pop_f, pop_m)

pop2 <- 
  pop %>% 
  left_join(ctrs) %>% 
  mutate(age = case_when(
    age == 0 ~ 0,
    age %in% 1:4 ~ 1,
    age %in% 5:99 ~ age - age %% 5,
    age >= 100 ~ 95
  )) %>% 
  group_by(country, sex, age, year) %>% 
  summarise(pop = sum(pop)) 

# ==========

# births ====
# ~~~~~~~~~~~

# Costa Rica
# ==========
bs_cr_out <- 
  read_xlsx(here("Data", "Costa Rica", "repoblacevcygbmiisem2021.xlsx"),
            sheet = "Cuadro 1",
            skip = 6) %>% 
  select(year = 1,
         Births = 2) %>% 
  drop_na() %>% 
  mutate(year = str_remove(year, "a/"),
         year = year %>% as.double()) %>% 
  filter(year >= 2015 & year <= 2020) %>% 
  mutate(Country = "Costa Rica",
         Code = "CRI") %>% 
  select(year,
         bts = Births,
         country = Country) %>% 
  mutate(age = 0)



# STMF
# ~~~~~~
# loading data from Human Fertility Database (HFD)
db_stff <- read_csv("https://www.humanfertility.org/STFF/stff.csv")

# db_stff <- 
#   read_csv("Data/births/stff.csv")

db_stff2 <- 
  db_stff %>% 
  select(CountryCode, year = Year, TOT) %>% 
  mutate(Births = TOT %>% as.double()) %>% 
  drop_na() %>% 
  select(-TOT)

# countries with full births data on 2020
cts_complete_2020 <- 
  db_stff2 %>% 
  filter(year == 2020) %>% 
  pull(CountryCode)

db_stff3 <- 
  db_stff2 %>% 
  filter(year >= 2010 & year <= 2020,
         CountryCode %in% cts_complete_2020)

db_stff_annual <- 
  db_stff3 %>% 
  mutate(CountryCode = recode(CountryCode,
                              "DEUTNP" = "DEU",
                              "FRATNP" = "FRA"),
         Country = countrycode(CountryCode, 
                               origin = 'iso3c', 
                               destination = 'country.name'),
         Country = ifelse(Country == "United States", "USA", Country),
         Country = case_when(CountryCode == "GBR_SCO" ~ "Scotland",
                             CountryCode == "GBRTENW" ~ "England and Wales",
                             TRUE ~ Country)) %>% 
  drop_na() %>% 
  select(year,
         bts = Births,
         country = Country) %>% 
  filter(country %in% ctrs$country,
         year >= 2015) %>% 
  mutate(age = 0)


# merging births
# ~~~~~~~~~~~~~~

bts_all <- 
  bind_rows(db_stff_annual,
            bs_cr_out)

# =====




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# merging births and population ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pop0 <- 
  pop2 %>% 
  filter(age == 0) %>% 
  group_by(country, age, year) %>% 
  mutate(fr = pop / sum(pop)) %>% 
  ungroup() %>% 
  select(-pop, -age)

bts_sex <- 
  bts_all %>% 
  left_join(pop0) %>% 
  mutate(pop = bts * fr) %>% 
  select(-bts, -fr)

pop3 <- 
  pop2 %>% 
  filter(age != 0) %>% 
  bind_rows(bts_sex) %>% 
  arrange(country, sex, age, year)

# =====





# merging deaths and population ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dts_pop <- 
  dts %>% 
  filter(!age %in% c("tot", "0d", "1-6d", "7-27d", "1-11m")) %>% 
  mutate(age = age %>% as.double()) %>% 
  left_join(pop3) %>% 
  select(-List) %>% 
  arrange(country, cause_cod, sex, age, year) %>%
  filter(age <= 20) %>% 
  mutate(age = age %>% as.character())
  
unique(dts_pop$age)

dts_neo <- 
  dts %>% 
  filter(age %in% c("0d", "1-6d", "7-27d", "1-11m")) %>% 
  # mutate(age_neo = age,
  #        age = 0) %>% 
  left_join(bts_sex %>% 
              select(-age)) %>% 
  select(-List)

dts_cod <- 
  bind_rows(dts_pop, 
          dts_neo)  

write_rds(dts_cod, "output/cod/cod_deaths_pop_country_cause_sex_age.rds")
# write_rds(dts_neo, "output/cod/cod_neonatal_deaths_pop_country_sex_age.rds")








