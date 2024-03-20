rm (list = ls()); gc()
source("Code/00_functions.R")

DT <- read_rds("data_inter/unpd_neonatal_raw.rds")
# ==============================================================================

unique(DT$LocName)
neo <- 
  DT %>% 
  as_tibble() %>% 
  select(LocID, 
         Country = LocName, 
         Date = TimeStart, 
         Sex = SexName, 
         neo_r = DataValue,
         Source = DataSourceName,
         src_yr = DataSourceYear) %>% 
  unique() %>% 
  mutate(Date = dmy(Date),
         Year = year(Date),
         Code = countrycode::countrycode(LocID, "un", "iso3c"),
         Sex = case_when(Sex == "Male" ~ "m",
                         Sex == "Female" ~ "f",
                         Sex == "Both sexes" ~ "t",
                         Sex == "Unknown" ~ "unk")) %>% 
  select(-Date, -LocID) %>% 
  drop_na(Sex) %>% 
  group_by(Country, Source) %>% 
  filter(max(Year) >= 2020) %>% 
  ungroup() %>% 
  mutate(Country = recode(Country,
                          "Czech Republic" = "Czechia",
                          "Iran (Islamic Republic of)" = "Iran",
                          "Bolivia (Plurinational State of)" = "Bolivia",
                          "United States of America" = "USA",
                          "Republic of Korea" = "South Korea",
                          "Russian Federation" = "Russia",
                          "Kosovo (United Nations administered region under security council resolution 1244)" = "Kosovo",
                          "Bosnia & Herzegovina" = "Bosnia and Herzegovina",
                          "China, Hong Kong SAR" = "Hong Kong",
                          "Hong Kong SAR China" = "Hong Kong SAR China",
                          "China, Macao SAR" = "Macao",
                          "Venezuela (Bolivarian Republic of)" = "Venezuela",
                          "Republic of Moldova" = "Moldova")) %>% 
  unique()

yrs1 <- neo %>% select(Country, Source) %>% unique()


# removing duplicate values
# ~~~~~~~~~~~~~~~~~~~~~~~~~
rep_sources <- 
  neo %>% 
  group_by(Country, Source, Year, Sex) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  arrange(Country, Source, Year, Sex) %>% 
  unique() %>% 
  group_by(Country, Source, Year, Sex) %>% 
  filter(src_yr == max(src_yr)) %>% 
  filter(neo_r == max(neo_r)) %>% 
  ungroup() %>% 
  unique()

neo2 <- 
  neo %>% 
  group_by(Country, Source, Year, Sex) %>% 
  filter(n() == 1) %>% 
  ungroup() %>% 
  bind_rows(rep_sources) %>% 
  arrange(Country, Source, Year, Sex) 

# saving raw unpd data 
# ~~~~~~~~~~~~~~~~~~~~
write_rds(neo2, "data_inter/unpd_neo_raw.rds")
neo2 <- read_rds("data_inter/unpd_neo_raw.rds")


# imputation of unknown sex ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# enough data for forecasting the baseline
enough_yrs <- 
  neo2 %>% 
  filter(Year %in% 2015:2019) %>% 
  group_by(Country, Source, Sex) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  filter(n >= 3) %>% 
  select(Country, Source, Sex) %>% 
  unique()

neo3 <- 
  neo2 %>% 
  inner_join(enough_yrs)

unique(neo3$Source)

neo4 <- 
  neo3 %>% 
  mutate(Source = case_when(Source == "Eurostat Database" ~ "unpd_eur",
                            Source == "UN IGME Total Under-5 Mortality Rate, Infant Mortality Rate and Neonatal mortality database 2021." ~ "unpd_igme",
                            TRUE ~ "crvs"),
         neo_r = ifelse(Source == "unpd_igme", neo_r * 1e3, neo_r))

rep_sources2 <- 
  neo4 %>% 
  group_by(Country, Source, Year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  arrange(Country, Source, Year) %>% 
  unique() %>% 
  group_by(Country, Source, Year, Sex) %>% 
  filter(src_yr == max(src_yr)) %>% 
  filter(Births == max(Births)) %>% 
  ungroup() %>% 
  unique()


# appending births
bts <- read_rds(("data_inter/annual_births_unpd.rds"))

bts2 <- 
  bts %>% 
  filter(Sex == "t")


neo5 <- 
  neo4 %>% 
  arrange(Country, Source, Year, Sex) %>% 
  select(-src_yr) %>% 
  # selecting countries and sources with data in 2020 
  group_by(Country, Sex, Source) %>% 
  filter(max(Year) >= 2020) %>% 
  ungroup() %>% 
  mutate(measure = "neo_r") %>% 
  left_join(bts2 %>% select(-Source, -Code)) %>% 
  select(country = Country, 
         code = Code,
         year = Year,
         measure,
         source = Source,
         value = neo_r,
         bts = Births)

# %>% 
#   select(country = Country,
#          year = Year,
#          source = Source,
#          measure,
#          value = neo_r)
  

unique(neo5$country)

write_rds(neo5, "data_inter/annual_neonatal_unpd.rds")
