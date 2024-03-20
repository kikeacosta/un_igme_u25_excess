library(readxl)
source("Code/00_functions.R")


# countries with mortality data for 2020
cts_mort <- 
  read_csv("Data/WHO/icd_raw.csv",
           col_types = cols(.default = "c")) %>% 
  filter(Year == 2020 & Frmat != "09") %>% 
  pull(name) %>% 
  unique() %>% 
  sort()


# Births from WHO mortality database
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
who <-
  read_csv("Data/WHO/mort_pop.zip")

ctry_names <- 
  read_csv(file.path("Data", "WHO", "mort_country_codes.zip")) %>% 
  rename(Country = country)

cts_who <- 
  who %>% 
  left_join(ctry_names) %>% 
  filter(Year == 2020) %>% 
  pull(name) %>% 
  unique() %>% 
  sort()

bs_who <- 
  who %>%
  left_join(ctry_names) %>% 
  filter(Year >= 2015,
         name %in% cts_mort) %>%
  select(Country = name, Year, Sex, Births = Lb) %>% 
  drop_na() %>% 
  mutate(Country = recode(Country,
                          "United Kingdom, England and Wales" = "England and Wales",
                          "United Kingdom, Scotland" = "Scotland",
                          "United Kingdom, Northern Ireland" = "Northern Ireland",
                          "Czech Republic" = "Czechia"),
         Code = countrycode(Country, origin = 'country.name', destination = 'iso3c'),
         Code = case_when(Country == "England and Wales" ~ "GBR-ENW",
                          Country == "Scotland" ~ "GBR-SCT",
                          Country == "Northern Ireland" ~ "GBR-NIR",
                          TRUE ~ Code)) %>% 
  # filter(!Country %in% cts_stff) %>% 
  group_by(Country, Code, Year) %>% 
  summarise(Births = sum(Births)) %>% 
  ungroup() %>%
  mutate(Source = "who_mort_db") %>% 
  group_by(Country) %>% 
  filter(max(Year) >= 2020) %>% 
  ungroup()



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# population data from WHO db
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
pop_who <-
  who %>% 
  left_join(ctry_names) %>% 
  filter(Year >= 2015,
         name %in% cts_mort) %>%
  select(Country = name, Year, Sex, starts_with("Pop")) %>% 
  drop_na() %>% 
  gather(-Country, -Year, -Sex, key = Age, value = Pop) %>%
  filter(Age %in% c('Pop2', 'Pop3', 'Pop4', 'Pop5', 'Pop6', 
                    'Pop7', 'Pop8', 'Pop9', 'Pop10')) %>%
  mutate(Age = recode(Age,
                      'Pop2' = '0',
                      'Pop3' = '1',
                      'Pop4' = '1',
                      'Pop5' = '1',
                      'Pop6' = '1',
                      'Pop7' = '5',
                      'Pop8' = '10',
                      'Pop9' = '15',
                      'Pop10' = '20'),
         Age = Age %>% as.double(),
         Pop = ifelse(is.na(Pop), 0, as.double(Pop)),
         Sex = recode(Sex,
                      "1" = "m",
                      "2" = "f"),
         Country = recode(Country,
                          "United Kingdom, England and Wales" = "England and Wales",
                          "United Kingdom, Scotland" = "Scotland",
                          "United Kingdom, Northern Ireland" = "Northern Ireland",
                          "Czech Republic" = "Czechia",
                          "Russian Federation" = "Russia")) %>%
  group_by(Country, Year, Sex, Age) %>%
  summarise(Population = sum(Pop)) %>%
  ungroup() %>%
  mutate(Code = countrycode(Country, origin = 'country.name', destination = 'iso3c'),
         Code = case_when(Country == "England and Wales" ~ "GBR-ENW",
                          Country == "Scotland" ~ "GBR-SCO",
                          Country == "Northern Ireland" ~ "GBR-NIR",
                          TRUE ~ Code)) %>% 
  group_by(Country) %>% 
  filter(max(Year) >= 2020) %>% 
  ungroup() %>% 
  group_by(Country, Sex, Age) %>% 
  mutate(seq = ifelse(Year == 2020, 1, lead(Year) - Year)) %>% 
  ungroup() %>% 
  group_by(Country) %>% 
  filter(max(seq) == 1) %>% 
  ungroup() %>% 
  select(-seq)

# only total sex values
pop_who2 <- 
  pop_who %>% 
  group_by(Code, Country, Year, Age) %>% 
  summarise(Population = sum(Population)) %>% 
  ungroup()

# exposures at age 0, Births or WHO Population 
age0 <- 
  pop_who2 %>% 
  filter(Age == "0") %>% 
  select(-Age) %>% 
  left_join(bs_who %>% select(Country, Year, Births)) %>% 
  mutate(diff = Births - Population) %>% 
  group_by(Code) %>% 
  filter(n() > 3) %>% 
  ungroup() %>% 
  mutate(exposure = ifelse(!is.na(Births), Births, Population),
         Age = 0) %>% 
  select(-Births, -Population, -diff)

# adjusting exposures at age 0
pop_who3 <- 
  pop_who2 %>% 
  filter(Age != 0) %>% 
  bind_rows(age0 %>% rename(Population = exposure)) %>% 
  mutate(Source = "who") %>% 
  arrange(Code, Year, Age)

# write_rds(pop_who3, "output/exposures_who.rds")
cts <- pop_who3$Country %>% unique()

# ~~~~~~~~~~~~~~~~~~~~~~~~
# population data from WPP
# ~~~~~~~~~~~~~~~~~~~~~~~~

cts <- pop_who3$Country %>% unique()

pop_wpp <- 
  read_xlsx("Data/WPP2019_INT_F03_1_POPULATION_BY_AGE_ANNUAL_BOTH_SEXES.xlsx",
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
  filter(Country %in% cts_mort, 
         Year >= 2015) %>% 
  gather(-Country, -Year, key = "Age", value = "Population") %>% 
  mutate(Age = as.integer(Age),
         Population = as.double(Population) * 1000) %>% 
  arrange(Country, Age) %>%
  filter(Age <= 24) %>% 
  mutate(Age = case_when(Age == 0 ~ 0,
                         Age %in% 1:4 ~ 1,
                         Age >= 5 ~ Age - Age %% 5)) %>% 
  group_by(Country, Year, Age) %>% 
  summarise(Population = sum(Population)) %>% 
  ungroup() %>% 
  mutate(Code = countrycode(Country, origin = "country.name",
                            destination = "iso3c"),
         Source = "wpp")

cts_wpp <- 
  unique(pop_wpp$Country)


pop_out <- 
  pop_wpp %>% 
  rename(pop_wpp = Population) %>% 
  select(-Source) %>% 
  left_join(pop_who3 %>% 
              rename(pop_who = Population) %>% 
              select(-Source))

pop_out <- 
  pop_who3 %>% 
  rename(pop_wpp = Population) %>% 
  select(-Source) %>% 
  left_join(pop_wpp %>% 
              rename(pop_who = Population) %>% 
              select(-Source))

unique(pop_out$Country)

dts_exp_yng <- read_rds("Output/annual_deaths_pop_5y_groups.rds")
dts_exp_inf <- read_rds("Output/annual_deaths_pop_infant_child.rds")



# from the exposure consolidation in Global excess estimates









# population and births data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
pop <- 
  read_rds("Output/exposures_5-year_groups.rds")

bts <- 
  read_rds("Output/annual_births_2015_2020.rds")

# Adjusting population with birth counts where possible
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
adj_fr <- 
  pop %>% 
  filter(Sex == "t",
         Age == 0,
         Country %in% unique(bts$Country)) %>% 
  left_join(bts, by = c("Code", "Year", "Country")) %>% 
  mutate(adj0 = Births / Population) %>% 
  select(Country, Code, Year, Age, adj0)

adj_pop0 <- 
  pop %>% 
  filter(Age == 0) %>% 
  left_join(adj_fr) %>% 
  mutate(adj0 = ifelse(is.na(adj0), 1, adj0),
         Population = Population * adj0) %>% 
  left_join(bts %>% 
              select(Country, Year, Source2 = Source), by = c("Year", "Country")) %>% 
  mutate(Source = ifelse(is.na(Source2), Source, Source2)) %>% 
  select(Country, Code, Year, Sex, Age, Population, Source)

adj_pop_all <- 
  adj_pop0 %>% 
  bind_rows(pop %>% 
              filter(Age != 0)) %>% 
  arrange(Country, Sex, Age, Year)

# Population for infant and child mortality
pop_inf_chl <- 
  adj_pop_all %>% 
  filter(Age %in% 0:1)

# Population for 5-year age groups
pop_5y_grps <- 
  adj_pop_all %>% 
  mutate(Age = ifelse(Age == 1, 0, Age)) %>% 
  group_by(Country, Code, Year, Sex, Age) %>% 
  summarise(Population = sum(Population),
            Source = unique(Source) %>% paste(collapse = ", ")) %>% 
  ungroup()
