
# Exposures data
# ~~~~~~~~~~~~~~

# exposures data from:
# WPP projection estimates by age and sex for all ages
# STFF data on births in 2020
# other sources for births in 2020

library(here)
source(here("Code", "00_functions.R"))

# mortality data
# ~~~~~~~~~~~~~~

db_d <- read_csv("Data/annual_deaths_countries_selected_sources.csv")


# ~~~~~~~~~~~~~~~~~~~~~~~~
# population data from WPP
# ~~~~~~~~~~~~~~~~~~~~~~~~

ctrs <- unique(db_d$Country)

pop_f <- read_xlsx(here("Data", "WPP2019_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.xlsx"),
                   skip = 16) %>% 
  select(3, 8:109) %>% 
  rename(Country = 1, 
         Year = 2) %>% 
  mutate(Country = case_when(Country == "United States of America" ~ "USA",
                             Country == "Republic of Korea" ~ "South Korea",
                             Country == "Russian Federation" ~ "Russia",
                             Country == "China, Taiwan Province of China" ~ "Taiwan",
                             TRUE ~ Country)) %>% 
  filter(Country %in% ctrs, 
         Year >= 2010) %>% 
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
  mutate(Country = case_when(Country == "United States of America" ~ "USA",
                             Country == "Republic of Korea" ~ "South Korea",
                             Country == "Russian Federation" ~ "Russia",
                             Country == "China, Taiwan Province of China" ~ "Taiwan",
                             TRUE ~ Country)) %>% 
  filter(Country %in% ctrs, 
         Year >= 2010) %>% 
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
  bind_rows(pop)

c(unique(pop$Country), unique(db_d$Country))

country_list <- 
  tibble(Country = c(unique(pop$Country), unique(db_d$Country))) %>% 
  left_join(tibble(Country = unique(pop$Country)) %>% 
              mutate(pop = 1))

write_rds(pop2, "Output/exposures_single-years.rds")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Population data at age 0 from STFF database
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# loading data from Human Fertility Database (HFD)
# https://www.humanfertility.org/STFF/stff.csv

db_stff <- 
  read_csv("Data/births/stff.csv")

db_stff2 <- 
  db_stff %>% 
  gather(-CountryCode, - Area, -Year, key = Month, value = Births) %>% 
  mutate(Births = Births %>% as.double(), 
         Date = make_date(y = Year, m = match(Month, month.name), d = 15)) 

# imputation of unknown month of birth
unks <- 
  db_stff2 %>% 
  filter(Month == "UNK") %>% 
  drop_na(Births) %>% 
  select(CountryCode, Year, unks = Births)

db_stff3 <- 
  db_stff2 %>% 
  filter(!Month %in% c("UNK", "TOT")) %>% 
  drop_na(Births) %>% 
  left_join(unks, by = c("CountryCode", "Year")) %>% 
  replace_na(list(unks = 0)) %>% 
  group_by(CountryCode, Year) %>% 
  mutate(prop = Births / sum(Births),
         Births2 = Births + unks * prop) %>% 
  select(CountryCode, Year, Month, Date, Births = Births2)

# countries with full births data on 2020
cts_complete_2020 <- 
  db_stff3 %>% 
  filter(Date == "2020-12-15") %>% 
  pull(CountryCode)

db_stff4 <- 
  db_stff3 %>% 
  filter(Year >= 2010 & Year <= 2020,
         CountryCode %in% cts_complete_2020)

db_stff_annual <- 
  db_stff4 %>% 
  group_by(CountryCode, Year) %>% 
  summarise(Births = sum(Births)) %>% 
  ungroup() %>% 
  mutate(CountryCode = recode(CountryCode,
                              "DEUTNP" = "DEU",
                              "FRATNP" = "FRA"),
         Country = countrycode(CountryCode, 
                               origin = 'iso3c', 
                               destination = 'country.name'),
         Country = ifelse(Country == "United States", "USA", Country),
         Country = case_when(CountryCode == "GBR_SCO" ~ "Scotland",
                             CountryCode == "GBRTENW" ~ "England_Wales",
                             TRUE ~ Country))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merging Population data from WPP and STFF
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# comparing data from STFF and WPP
db_stff_annual2 <- 
  db_stff_annual %>% 
  select(-CountryCode) %>% 
  mutate(Age = 0)

# Estimate a correction factor for all sex combined to be applied to each sex
adjust_age0 <- 
  pop2 %>% 
  filter(Sex == "t") %>% 
  left_join(db_stff_annual2) %>% 
  drop_na() %>% 
  mutate(factor = Births / Population) %>% 
  select(Country, Year, factor)
  
pop_age0 <- 
  pop2 %>% 
  filter(Age == 0) %>% 
  left_join(adjust_age0) %>% 
  mutate(Population = ifelse(!is.na(factor), Population * factor, Population)) %>% 
  select(-factor) %>% 
  mutate(Age = 0)

pop3 <- 
  pop2 %>% 
  filter(Age != 0) %>% 
  bind_rows(pop_age0) %>% 
  arrange(Country, Year, Sex, Age)
  

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# grouping population age groups in same categories as deaths 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cts <- unique(db_d$Country) %>% sort()
pop_ints <- tibble()
# ct <- "Brazil"

for(ct in cts){
  
  temp_pop <- assign_age_intervals(db_d, pop3, ct)
  
  pop_ints <- pop_ints %>% 
    bind_rows(temp_pop)
}

pop_all <- pop_ints %>% 
  group_by(Country, Year, Sex, Age_int) %>% 
  summarise(Population = sum(Population)) %>% 
  ungroup() %>% 
  rename(Age = Age_int)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merging deaths and population 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
deaths2 <- 
  db_d %>% 
  left_join(pop_all) %>% 
  filter(Year <= 2020) %>% 
  mutate(mx = 100000 * Deaths / Population,
         Year = factor(Year)) %>% 
  drop_na()

write_rds(deaths2, "Output/annual_deaths_pop.rds")
deaths2 <- read_rds("Output/annual_deaths_pop.rds")

deaths2 %>% 
  filter(Age <= 90) %>% 
  ggplot()+
  geom_line(aes(Age, mx, col = Year, group = Year))+
  scale_y_log10()+
  facet_wrap(Country ~ Sex)


# ~~~~~~~~~~~~~~~~~~~~
# comparing Chile data
# ~~~~~~~~~~~~~~~~~~~~

deaths_test <- 
  bind_rows(db_d) %>% 
  filter(Country == "Chile") %>% 
  mutate(age_int = floor(Age / 5) * 5) %>% 
  group_by(Source, Country, Year, Sex, age_int) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()

deaths_test %>% 
  ggplot()+
  geom_line(aes(age_int, Deaths, col = Source))+
  facet_wrap(Year ~ Sex)
