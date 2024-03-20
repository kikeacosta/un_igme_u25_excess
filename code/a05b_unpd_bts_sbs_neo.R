rm (list = ls()); gc()
source("code/00_functions.R")

DT_neo <- read_rds("data_inter/unpd_neonatal_raw.rds")
DT_sbs <- read_rds("data_inter/unpd_stillbirths_raw.rds")
DT_bts <- read_rds("data_inter/unpd_births_raw.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# births ====
# ~~~~~~~~~~~

# functions 
{
  sum_bts_source <- function(db){
    sum_out <- 
      db %>% 
      filter(Year >= 2015) %>% 
      group_by(Country, Source, Source_int, Sex) %>% 
      filter(max(Year) >= 2020) %>% 
      ungroup() %>% 
      group_by(Country, Source, Source_int, Year) %>% 
      mutate(sexs = n()) %>% 
      ungroup() %>% 
      group_by(Country, Source, Source_int, Sex) %>% 
      mutate(years = n()) %>% 
      ungroup() %>% 
      group_by(Country, Source, Source_int) %>% 
      filter(!(sexs == 3 & Sex == "t")) %>% 
      summarise(Births = sum(Births),
                sexs = min(sexs),
                years = min(years),
                Source = unique(Source) %>% paste(collapse = ", ")) %>% 
      ungroup() %>% 
      unique()  
  }
  
  sum_neo_source <- function(db){
    sum_out <- 
      db %>% 
      filter(Year >= 2015) %>% 
      group_by(Country, Source, Source_int, Sex) %>% 
      filter(max(Year) >= 2020) %>% 
      ungroup() %>% 
      group_by(Country, Source, Source_int, Year) %>% 
      mutate(sexs = n()) %>% 
      ungroup() %>% 
      group_by(Country, Source, Source_int, Sex) %>% 
      mutate(years = n()) %>% 
      ungroup() %>% 
      group_by(Country, Source, Source_int) %>% 
      filter(!(sexs == 3 & Sex == "t")) %>% 
      summarise(neo_r = sum(neo_r),
                sexs = min(sexs),
                years = min(years),
                Source = unique(Source) %>% paste(collapse = ", ")) %>% 
      ungroup() %>% 
      unique()  
  }
  
  sum_sbs_source <- function(db){
    sum_out <- 
      db %>% 
      filter(Year >= 2015) %>% 
      group_by(Country, Source, Source_int, Sex) %>% 
      filter(max(Year) >= 2020) %>% 
      ungroup() %>% 
      group_by(Country, Source, Source_int, Year) %>% 
      mutate(sexs = n()) %>% 
      ungroup() %>% 
      group_by(Country, Source, Source_int, Sex) %>% 
      mutate(years = n()) %>% 
      ungroup() %>% 
      group_by(Country, Source, Source_int) %>% 
      filter(!(sexs == 3 & Sex == "t")) %>% 
      summarise(sbs_r = sum(sbs_r),
                sexs = min(sexs),
                years = min(years),
                Source = unique(Source) %>% paste(collapse = ", ")) %>% 
      ungroup() %>% 
      unique()  
  }
  
}

bts <- 
  DT_bts %>% 
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
  unique() %>% 
  group_by(Country, Code, Source, Year, Sex) %>% 
  # most recent update of the same source
  filter(src_yr == max(src_yr)) %>% 
  ungroup() %>% 
  select(-src_yr)
  
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

srs_bts <- 
  bts4 %>% 
  select(Country, Source) %>% 
  unique()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

unique(bts4$Source)

bts5 <- 
  bts4 %>% 
  mutate(Source_int = case_when(Source == "Demographic Yearbook" ~ "unpd_dy",
                            Source == "Eurostat Database" ~ "unpd_eur",
                            Source == "Human Mortality Database" ~ "unpd_hmd",
                            Source == "Human Fertility Database" ~ "unpd_hfd",
                            Source == "WHO-IGME Mortality Data base" ~ "unpd_who",
                            TRUE ~ "unpd_crvs"))

rep_sources2 <- 
  bts5 %>% 
  group_by(Country, Source, Source_int, Year, Sex) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  arrange(Country, Source, Source_int, Year, Sex) %>% 
  unique() %>% 
  group_by(Country, Source, Source_int, Year, Sex) %>% 
  filter(Births == max(Births)) %>% 
  ungroup() %>% 
  unique()

bts6 <- 
  bts5 %>% 
  group_by(Country, Source, Source_int, Year, Sex) %>% 
  filter(n() == 1) %>% 
  ungroup() %>%
  bind_rows(rep_sources2) %>% 
  arrange(Country, Source, Source_int, Year, Sex) %>% 
  # selecting countries and sources with data in 2020 
  group_by(Country, Sex, Source, Source_int) %>% 
  filter(max(Year) >= 2020) %>% 
  ungroup() %>% 
  mutate(Births = round(Births)) %>% 
  unique()

unique(bts6$Country)

summ_bts1021 <- 
  sum_bts_source(bts6)

comp_bts <- 
  summ_bts1021 %>% 
  unique() %>% 
  mutate(Births = round(Births)) %>% 
  arrange(Country) %>% 
  # unique sources
  group_by(Country) %>% 
  # max age definition
  filter(years == max(years)) %>% 
  filter(sexs == max(sexs)) %>% 
  filter(Births == max(Births)) %>% 
  filter((any(Source_int == "unpd_crvs") & Source_int == "unpd_crvs") | all(Source_int != "unpd_crvs")) %>%
  filter((any(Source_int == "unpd_hmd") & Source_int == "unpd_hmd") | all(Source_int != "unpd_hmd")) %>%
  filter((any(Source_int == "unpd_dy") & Source_int == "unpd_dy") | all(Source_int != "unpd_dy")) %>%
    # filter(!(n() == 2 & Source %in% c("who_mort_db", "unpd", "hmd"))) %>% 
  mutate(best = ifelse(n() == 1, Source, NA))

sel_bts <- 
  comp_bts %>% 
  select(Country, Source)

out_bts_srs <- 
  bts6 %>% 
  inner_join(sel_bts) %>% 
  arrange(Country, Sex, Year) %>% 
  unique()

out_bts <- 
  out_bts_srs %>% 
  select(-Source) %>% 
  rename(Source = Source_int)
  
unique(out_bts$Country)

write_rds(out_bts, "data_inter/annual_births_unpd.rds")
write_rds(out_bts_srs, "data_inter/sources_unpd_bts.rds")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# neonatal ====
# ~~~~~~~~~~~~~

unique(DT_neo$LocName)
unique(DT_sbs$LocName)

neo <- 
  DT_neo %>% 
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

srs_neo <- 
  neo3 %>% 
  select(Country, Source) %>% 
  unique()

# write_rds(srs_neo, "data_inter/sources_unpd_neo.rds")


neo4 <- 
  neo3 %>% 
  mutate(Source_int = case_when(Source == "Eurostat Database" ~ "unpd_eur",
                            Source == "UN IGME Total Under-5 Mortality Rate, Infant Mortality Rate and Neonatal mortality database 2021." ~ "unpd_igme",
                            TRUE ~ "unpd_crvs"),
         neo_r = ifelse(Source_int == "unpd_igme", neo_r * 1e3, neo_r))

rep_sources2 <- 
  neo4 %>% 
  group_by(Country, Source, Source_int, Year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  arrange(Country, Source, Source_int, Year) %>% 
  unique() %>% 
  group_by(Country, Source, Source_int, Year, Sex) %>% 
  filter(src_yr == max(src_yr)) %>% 
  ungroup() %>% 
  unique()

neo5 <- 
  neo4 %>% 
  arrange(Country, Source, Source_int, Year, Sex) %>% 
  select(-src_yr) %>% 
  # selecting countries and sources with data in 2020 
  group_by(Country, Sex, Source, Source_int) %>% 
  filter(max(Year) >= 2020) %>% 
  ungroup() %>% 
  mutate(measure = "neo_r")

summ_neo <- 
  sum_neo_source(neo5)

comp_neo <- 
  summ_neo %>% 
  unique() %>% 
  # mutate(Births = round(Births)) %>% 
  arrange(Country) %>% 
  # unique sources
  group_by(Country) %>% 
  # max age definition
  filter(years == max(years)) %>% 
  filter(sexs == max(sexs)) %>% 
  filter(neo_r == max(neo_r)) %>% 
  filter((any(Source_int == "unpd_igme") & Source_int == "unpd_igme") | all(Source_int != "unpd_igme")) %>%
  filter((any(Source_int == "unpd_crvs") & Source_int == "unpd_crvs") | all(Source_int != "unpd_crvs")) %>%
  filter((any(Source_int == "unpd_eur") & Source_int == "unpd_eur") | all(Source_int != "unpd_eur")) %>%
  # filter(!(n() == 2 & Source %in% c("who_mort_db", "unpd", "hmd"))) %>% 
  mutate(best = ifelse(n() == 1, Source, NA)) %>% 
  ungroup()

sel_neo <- 
  comp_neo %>% 
  select(Country, Source)

out_neo_srs <- 
  neo5 %>% 
  inner_join(sel_neo) %>% 
  arrange(Country, Sex, Year) %>% 
  unique() %>% 
  ungroup() %>% 
  select(country = Country, 
         code = Code,
         year = Year,
         measure,
         source = Source,
         source_int = Source_int,
         value = neo_r)

out_neo <- 
  out_neo_srs %>% 
  select(-source) %>% 
  rename(source = source_int)

unique(out_bts$Country)

# write_rds(out_neo, "data_inter/annual_births_unpd.rds")
write_rds(out_neo_srs, "data_inter/sources_unpd_neo.rds")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# stillbirths ====
# ~~~~~~~~~~~~~

unique(DT_sbs$LocName)

sbs <- 
  DT_sbs %>% 
  as_tibble() %>% 
  select(LocID, 
         Country = LocName, 
         Date = TimeStart, 
         Sex = SexName, 
         sbs_r = DataValue,
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

yrs1 <- sbs %>% select(Country, Source) %>% unique()

# removing duplicate values
# ~~~~~~~~~~~~~~~~~~~~~~~~~
rep_sources <- 
  sbs %>% 
  group_by(Country, Source, Year, Sex) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  arrange(Country, Source, Year, Sex) %>% 
  unique() %>% 
  group_by(Country, Source, Year, Sex) %>% 
  filter(src_yr == max(src_yr)) %>% 
  filter(sbs_r == max(sbs_r)) %>% 
  ungroup() %>% 
  unique()

sbs2 <- 
  sbs %>% 
  group_by(Country, Source, Year, Sex) %>% 
  filter(n() == 1) %>% 
  ungroup() %>% 
  bind_rows(rep_sources) %>% 
  arrange(Country, Source, Year, Sex) 

# enough data for forecasting the baseline
enough_yrs <- 
  sbs2 %>% 
  filter(Year %in% 2015:2019) %>% 
  group_by(Country, Source, Sex) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  filter(n >= 3) %>% 
  select(Country, Source, Sex) %>% 
  unique()

sbs3 <- 
  sbs2 %>% 
  inner_join(enough_yrs)

unique(sbs3$Source)

srs_sbs <- 
  sbs3 %>% 
  select(Country, Source) %>% 
  unique()

# write_rds(srs_sbs, "data_inter/sources_unpd_sbs.rds")
sbs4 <- 
  sbs3 %>% 
  mutate(Source_int = case_when(Source == "Eurostat Database" ~ "unpd_eur",
                            Source == "UN IGME Total Under-5 Mortality Rate, Infant Mortality Rate and Neonatal mortality database 2021." ~ "unpd_igme",
                            TRUE ~ "unpd_crvs"),
         neo_r = ifelse(Source_int == "unpd_igme", sbs_r * 1e3, sbs_r))

rep_sources2 <- 
  sbs4 %>% 
  group_by(Country, Source, Source_int, Year) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  arrange(Country, Source, Source_int, Year) %>% 
  unique() %>% 
  group_by(Country, Source, Source_int, Year, Sex) %>% 
  filter(src_yr == max(src_yr)) %>% 
  filter(sbs_r == max(sbs_r)) %>% 
  ungroup() %>% 
  unique()

sbs5 <- 
  sbs4 %>% 
  arrange(Country, Source, Source_int, Year, Sex) %>% 
  select(-src_yr) %>% 
  # selecting countries and sources with data in 2020 
  group_by(Country, Sex, Source, Source_int) %>% 
  filter(max(Year) >= 2020) %>% 
  ungroup() %>% 
  mutate(measure = "sbs_r")

unique(sbs5$Country)

summ_sbs <- 
  sum_sbs_source(sbs5)

comp_sbs <- 
  summ_sbs %>% 
  unique() %>% 
  # mutate(Births = round(Births)) %>% 
  arrange(Country) %>% 
  # unique sources
  group_by(Country) %>% 
  # max age definition
  filter(years == max(years)) %>% 
  filter(sexs == max(sexs)) %>% 
  filter(sbs_r == max(sbs_r)) %>% 
  filter((any(Source_int == "unpd_igme") & Source_int == "unpd_igme") | all(Source_int != "unpd_igme")) %>%
  filter((any(Source_int == "unpd_crvs") & Source_int == "unpd_crvs") | all(Source_int != "unpd_crvs")) %>%
  filter((any(Source_int == "unpd_eur") & Source_int == "unpd_eur") | all(Source_int != "unpd_eur")) %>%
  # filter(!(n() == 2 & Source %in% c("who_mort_db", "unpd", "hmd"))) %>% 
  mutate(best = ifelse(n() == 1, Source, NA)) %>% 
  ungroup()

sel_sbs <- 
  comp_sbs %>% 
  select(Country, Source)

out_sbs_srs <- 
  sbs5 %>% 
  inner_join(sel_sbs) %>% 
  arrange(Country, Sex, Year) %>% 
  unique() %>% 
  ungroup() %>% 
  select(country = Country, 
         code = Code,
         year = Year,
         measure,
         source = Source,
         source_int = Source_int,
         value = sbs_r)

out_sbs <- 
  out_sbs_srs %>% 
  select(-source) %>% 
  rename(source = source_int)

unique(out_sbs$country)

write_rds(out_sbs_srs, "data_inter/sources_unpd_sbs.rds")


# appending sbs and neo together ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sbs_neo <- 
  bind_rows(out_neo, out_sbs) %>% 
  left_join(out_bts %>% 
              filter(Sex == "t") %>% 
              select(country = Country, bts = Births, year = Year)) 

write_rds(sbs_neo, "data_inter/neo_sbs_unpd.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
