library(here)
source(here("Code", "00_functions.R"))

# mortality data
# ~~~~~~~~~~~~~~

db_age <- read_csv("Data/annual_deaths_countries_selected_sources.csv")
# db_latam <- read_rds("Output/latam_deaths.rds")
# deaths_stmf <- read_rds("Output/stmf_deaths.rds") 

unique(db_age$Country)
unique(db_latam$Country)

# excluding some countries according to the best available data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# excluding countries/regions from the UK
exc_who <- c("Chile", "England and Wales", "Northern Ireland", "Scotland")
# excluding data from Celade for which better data is available in WHO
exc_latam <- c("Brazil", "Peru", "Mexico")

# merging both datasets
deaths <- 
  bind_rows(
    db_who %>% 
      filter(!Country %in% exc_who),
    db_latam %>% 
      filter(!Country %in% exc_latam)
  )

available <- 
  deaths %>% 
  group_by(Country, Code, Year, Source, Sex) %>% 
  summarise(Age_groups = n(),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  group_by(Country, Code, Year, Source, Age_groups) %>% 
  summarise(Sex_groups = n(),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  group_by(Country, Code, Source, Age_groups, Sex_groups) %>% 
  summarise(min_year = min(Year),
            max_year = max(Year),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  group_by(Country) %>% 
  mutate(n_sources = n()) %>% 
  ungroup() 

available2 <- 
  available %>% 
  group_by(Country, Code) %>% 
  summarise(Age_groups = min(Age_groups),
            Sex_groups = min(Sex_groups),
            Period = paste(min(min_year), max(max_year), sep = "-"),
            Deaths = sum(Deaths), 
            Sources = paste0(Source, collapse = "; "))

# save summary of data availability by country
write_csv(available2, "Output/01_summary_data_by_country.csv")


# population data
# ~~~~~~~~~~~~~~~

ctrs <- unique(deaths$Country)

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
         Year >= 2000) %>% 
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
         Year >= 2000) %>% 
  gather(-Country, -Year, key = "Age", value = "Population") %>% 
  mutate(Age = as.integer(Age),
         Population = as.double(Population) * 1000,
         Sex = "m") %>% 
  arrange(Country, Age)

pop <- 
  bind_rows(pop_f, pop_m)

c(unique(pop$Country), unique(deaths$Country))

country_list <- 
  tibble(Country = c(unique(pop$Country), unique(deaths$Country))) %>% 
  left_join(tibble(Country = unique(pop$Country)) %>% 
              mutate(pop = 1))

write_rds(pop, "Output/exposures_single-years.rds")

# grouping population age groups in same categories as deaths 
cts <- unique(deaths$Country) %>% sort()
pop_ints <- tibble()
# ct <- "Brazil"

for(ct in cts){
  
  temp_pop <- assign_age_intervals(deaths, ct)
  
  pop_ints <- pop_ints %>% 
    bind_rows(temp_pop)
}

pop_all <- pop_ints %>% 
  group_by(Country, Year, Sex, Age_int) %>% 
  summarise(Population = sum(Population)) %>% 
  ungroup() %>% 
  rename(Age = Age_int)

# Merging deaths and population 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
deaths2 <- 
  deaths %>% 
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
  bind_rows(db_latam,
            db_who) %>% 
  filter(Country == "Chile") %>% 
  mutate(age_int = floor(Age / 5) * 5) %>% 
  group_by(Source, Country, Year, Sex, age_int) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()

deaths_test %>% 
  ggplot()+
  geom_line(aes(age_int, Deaths, col = Source))+
  facet_wrap(Year ~ Sex)
