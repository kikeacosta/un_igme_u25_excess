rm (list = ls()); gc()
source("Code/00_functions.R")

DT <- read_rds("data_inter/unpd_births_raw.rds")

unique(DT$LocName)

bts <- 
  DT %>% 
  as_tibble() %>% 
  select(LocID, 
         Country = LocName, 
         Date = TimeStart, 
         Sex = SexName, 
         Births = DataValue,
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

yrs1 <- bts %>% select(Country, Source) %>% unique()

# removing duplicate values
# ~~~~~~~~~~~~~~~~~~~~~~~~~
rep_sources <- 
  bts %>% 
  group_by(Country, Source, Year, Sex) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  arrange(Country, Source, Year, Sex) %>% 
  unique() %>% 
  group_by(Country, Source, Year, Sex) %>% 
  filter(src_yr == max(src_yr)) %>% 
  filter(Births == max(Births)) %>% 
  ungroup() %>% 
  unique()

bts2 <- 
  bts %>% 
  group_by(Country, Source, Year, Sex) %>% 
  filter(n() == 1) %>% 
  ungroup() %>% 
  bind_rows(rep_sources) %>% 
  arrange(Country, Source, Year, Sex) 


# imputation of unknown sex ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Total sex
# identifying missing total sex values
miss_tot_sex <- 
  bts2 %>% 
  select(Country, Source, Year, Sex) %>%
  unique() %>%
  group_by(Country, Source, Year) %>% 
  filter(n() == 2) %>% 
  ungroup()

# total sex for those missing it
tot_miss_tot_sex <- 
  bts2 %>% 
  inner_join(miss_tot_sex) %>% 
  group_by(Country, Source, Year) %>% 
  summarise(Births = sum(Births)) %>% 
  ungroup() %>% 
  mutate(Sex = "t")

bts3 <- 
  bts2 %>% 
  bind_rows(tot_miss_tot_sex) %>% 
  arrange(Country, Source, Year, Sex)

# enough data for forecasting the baseline
enough_yrs <- 
  bts3 %>% 
  filter(Year %in% 2015:2019) %>% 
  group_by(Country, Source, Sex) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  filter(n >= 3) %>% 
  select(Country, Source, Sex) %>% 
  unique()

bts4 <- 
  bts3 %>% 
  inner_join(enough_yrs)

unique(bts4$Source)

bts5 <- 
  bts4 %>% 
  mutate(Source = case_when(Source == "Demographic Yearbook" ~ "unpd_dy",
                            Source == "Eurostat Database" ~ "unpd_eur",
                            Source == "Human Mortality Database" ~ "unpd_hmd",
                            Source == "Human Fertility Database" ~ "unpd_hfd",
                            Source == "WHO-IGME Mortality Data base" ~ "unpd_who",
                            TRUE ~ "crvs"))

rep_sources2 <- 
  bts5 %>% 
  group_by(Country, Source, Year, Sex) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  arrange(Country, Source, Year, Sex) %>% 
  unique() %>% 
  group_by(Country, Source, Year, Sex) %>% 
  filter(src_yr == max(src_yr)) %>% 
  filter(Births == max(Births)) %>% 
  ungroup() %>% 
  unique()

bts6 <- 
  bts5 %>% 
  group_by(Country, Source, Year, Sex) %>% 
  filter(n() == 1) %>% 
  ungroup() %>%
  bind_rows(rep_sources2) %>% 
  arrange(Country, Source, Year, Sex) %>% 
  select(-src_yr) %>% 
  # selecting countries and sources with data in 2020 
  group_by(Country, Sex, Source) %>% 
  filter(max(Year) >= 2020) %>% 
  ungroup()

unique(bts6$Country)

write_rds(bts6, "data_inter/annual_births_unpd.rds")
