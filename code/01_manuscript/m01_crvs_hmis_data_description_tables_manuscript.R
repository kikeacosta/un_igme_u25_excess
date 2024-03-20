rm(list = ls())
source("Code/00_functions.R")

# loading excess estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~
all_fit <- 
  read_rds("data_output/preliminary_pscores_2024-01-16.rds") %>% 
  filter(Year %in% 2010:2021)

all_data <- 
  read_rds("data_output/annual_deaths_rates_2010_2022_2024-01-16.rds") %>% 
  filter(Year %in% 2010:2021)

pop_in <- 
  all_fit %>% 
  filter(Sex == "t") %>% 
  select(Code, pop_in) %>% 
  unique()

all_data_fit <- 
  all_data %>% 
  filter(Sex == "t") %>% 
  select(Code, Country, Age, Year) %>%  
  unique() %>% 
  left_join(all_fit %>% 
              filter(Sex == "t") %>% 
              select(Country, Age, Year, fit_in, inc_in) %>% 
              unique()) %>% 
  left_join(pop_in) %>% 
  mutate(fit_in = ifelse(Year >= 2015, fit_in, 0),
         inc_in = ifelse(Year >= 2015, inc_in, 0)) %>% 
  filter(Year %in% 2010:2021)

bts_crvs <- 
  read_rds("data_inter/annual_births.rds") %>% 
  filter(Year %in% 2010:2021,
         Code %in% c(all_data_fit %>% pull(Code) %>% unique())) %>% 
  select(Code, Year) %>% 
  unique() %>% 
  arrange()

# data available summary ====
# ~~~~~~~~~~~~~~~~~~~~~~

unique(all_fit$Country)

var_levs <- c("Total", 
              "Births",
              "Stillbirths",
              "Neonatal",
              "Infant",
              "1-4",
              "0-4",
              "5-9",
              "10-14",
              "15-19",
              "20-24",
              "Low",         
              "Lower-mid",
              "Upper-mid",
              "High", 
              "CRVS",
              "HMIS")

avail_all <- 
  all_fit %>% 
  filter(Year %in% 2020:2021,
         Sex == "t") %>% 
  select(Code, Country, Year) %>% 
  unique() %>% 
  group_by(Year) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  mutate(var = "Total")

avail_bts <- 
  all_fit %>% 
  filter(Year %in% 2020:2021,
         Sex == "t") %>% 
  select(Code, Country, Year) %>% 
  unique() %>% 
  inner_join(bts_crvs) %>% 
  group_by(Year) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  mutate(var = "Births")

avail_age <- 
  all_fit %>% 
  filter(Year %in% 2020:2021,
         Sex == "t") %>% 
  select(Code, Year, Age) %>% 
  unique() %>% 
  group_by(Year, Age) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  rename(var = Age)

avail_inc <- 
  all_fit %>% 
  filter(Year %in% 2020:2021,
         Sex == "t") %>% 
  select(Code, Year, Income) %>% 
  unique() %>% 
  group_by(Year, Income) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  rename(var = Income)

avail <- 
  bind_rows(avail_all,
            # avail_bts,
            avail_age,
            avail_inc) %>% 
  select(var, everything()) %>% 
  mutate(var = factor(var, levels = var_levs)) %>% 
  arrange(var) %>% 
  rename(a_2020 = `2020`,
         a_2021 = `2021`)

# population threshold summary ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pop_all <- 
  all_fit %>% 
  filter(Year %in% 2020:2021,
         !Country %in% c("Azerbaijan", "Armenia"),
         Sex == "t",
         pop_in == 1) %>% 
  select(Country, Year) %>% 
  unique() %>% 
  group_by(Year) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  mutate(var = "Total")

pop_age <- 
  all_fit %>% 
  filter(Year %in% 2020:2021,
         Sex == "t",
         pop_in == 1) %>% 
  select(Country, Year, Age) %>% 
  unique() %>% 
  group_by(Year, Age) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  rename(var = Age)

pop_inc <- 
  all_fit %>% 
  filter(Year %in% 2020:2021,
         Sex == "t",
         pop_in == 1) %>% 
  select(Code, Year, Income) %>% 
  unique() %>% 
  group_by(Year, Income) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  rename(var = Income)

pop <- 
  bind_rows(pop_all,
            pop_age,
            pop_inc) %>% 
  select(var, everything()) %>% 
  mutate(var = factor(var, levels = var_levs)) %>% 
  arrange(var) %>% 
  rename(p_2020 = `2020`,
         p_2021 = `2021`)


# fitting issues summary ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
fitok_all <- 
  all_fit %>% 
  filter(Year %in% 2020:2021,
         Sex == "t",
         fit_in == 1) %>% 
  select(Code, Year) %>% 
  unique() %>% 
  group_by(Year) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  mutate(var = "Total")

fitok_age <- 
  all_fit %>% 
  filter(Year %in% 2020:2021,
         Sex == "t",
         fit_in == 1) %>% 
  select(Code, Year, Age) %>% 
  unique() %>% 
  group_by(Year, Age) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  rename(var = Age)

fitok_inc <- 
  all_fit %>% 
  filter(Year %in% 2020:2021,
         Sex == "t",
         fit_in == 1) %>% 
  select(Code, Year, Income) %>% 
  unique() %>% 
  group_by(Year, Income) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  rename(var = Income)

fitok <- 
  bind_rows(fitok_all,
            fitok_age,
            fitok_inc) %>% 
  select(var, everything()) %>% 
  mutate(var = factor(var, levels = var_levs)) %>% 
  arrange(var) %>% 
  rename(f_2020 = `2020`,
         f_2021 = `2021`)


# included summary ====
# ~~~~~~~~~~~~~~~~~~~~~
inc_all <- 
  all_fit %>% 
  filter(Year %in% 2020:2021,
         Sex == "t",
         inc_in == 1) %>% 
  select(Code, Year) %>% 
  unique() %>% 
  group_by(Year) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  mutate(var = "Total")

inc_age <- 
  all_fit %>% 
  filter(Year %in% 2020:2021,
         Sex == "t",
         inc_in == 1) %>% 
  select(Code, Year, Age) %>% 
  unique() %>% 
  group_by(Year, Age) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  rename(var = Age)

inc_inc <- 
  all_fit %>% 
  filter(Year %in% 2020:2021,
         Sex == "t",
         inc_in == 1) %>% 
  select(Code, Year, Income) %>% 
  unique() %>% 
  group_by(Year, Income) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  rename(var = Income)

inc <- 
  bind_rows(inc_all,
            inc_age,
            inc_inc) %>% 
  select(var, everything()) %>% 
  mutate(var = factor(var, levels = var_levs)) %>% 
  arrange(var) %>% 
  rename(i_2020 = `2020`,
         i_2021 = `2021`)


# Table S01 ====
# ~~~~~~~~~~~~~~
# putting all together
table_s1_v1 <- 
  avail %>% 
  left_join(pop) %>% 
  left_join(fitok) %>% 
  left_join(inc) %>% 
  replace_na(list(a_2020 = 0, 
                  a_2021 = 0,
                  p_2020 = 0,
                  p_2021 = 0,
                  f_2020 = 0,
                  f_2021 = 0,
                  i_2020 = 0, 
                  i_2021 = 0))

table_s1 <- 
  table_s1_v1 %>% 
  select(var, a_2020, a_2021, i_2020, i_2021) 

table_s1

write_csv(table_s1, "Tables/01_manuscript/tableS01_crvs_data.csv")



# loading country contextual variables from World Bank
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
income_levels <- read_xlsx("Data/WPP2022_F01_LOCATIONS.XLSX") %>% 
  select(Code = 5, type = 9, Region = 13, 
         hi = 27, umi = 29, lmi = 30, li = 31) %>% 
  filter(type == "Country/Area") %>% 
  mutate(Income = case_when(!is.na(hi) ~ "High",
                            !is.na(umi) ~ "Upper-mid",
                            !is.na(lmi) ~ "Lower-mid",
                            !is.na(li) ~ "Low",
                            TRUE ~ "No income data")) %>% 
  select(-hi, -umi, -lmi, -li)

# HMIS data ====
# ~~~~~~~~~~~~~~
hmis <- read_rds("data_inter/hmis_all_countries.rds")

hmis2 <- 
  hmis %>% 
  filter(!measure %in% c("pos", "mat", "dvs", "inf",
                         "1_4", "5_9", "10_14", "15_19", "20_24")) %>% 
  mutate(year = year(date)) %>% 
  select(country, year, measure) %>% 
  unique() %>% 
  mutate(age = case_when(str_detect(measure, "bts") ~ "Births",
                         str_detect(measure, "sbs") ~ "Stillbirths",
                         str_detect(measure, "neo") ~ "Neonatal",
                         str_detect(measure, "0_4") ~ "0-4",
                         TRUE ~ measure)) %>% 
  select(Country = country,
         Year = year,
         Age = age) %>% 
  # filter(Year >= 2020) %>% 
  mutate(Source = "hmis",
         Code = countrycode::countrycode(Country, "country.name", "iso3c")) %>% 
  left_join(income_levels) %>% 
  select(-type, -Region)


# table S2 ====
# ~~~~~~~~~~~~~

hmis_all <- 
  hmis2 %>% 
  filter(Year %in% 2020:2021) %>% 
  select(Country, Year) %>% 
  unique() %>% 
  group_by(Year) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  mutate(var = "Total")

hmis_age <- 
  hmis2 %>% 
  filter(Year %in% 2020:2021) %>% 
  select(Country, Year, Age) %>% 
  unique() %>% 
  group_by(Year, Age) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  rename(var = Age)

hmis_inc <- 
  hmis2 %>% 
  filter(Year %in% 2020:2021) %>% 
  select(Country, Year, Income) %>% 
  unique() %>% 
  group_by(Year, Income) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  rename(var = Income)

table_s2 <- 
  bind_rows(hmis_all,
            hmis_age,
            hmis_inc) %>% 
  select(var, everything()) %>% 
  mutate(var = factor(var, levels = var_levs)) %>% 
  arrange(var) 

table_s2
write_csv(table_s2, "Tables/01_manuscript/tableS02_hmis_data.csv")



# table 01 ====
# ~~~~~~~~~~~~~

# merging CRVS and HMIS data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

crvs2 <- 
  all_fit %>% 
  filter(Year %in% 2020:2021,
         Sex == "t",
         inc_in == 1) %>% 
  select(Country, Code, Year, Age, Income) %>% 
  unique() %>% 
  mutate(Source = "crvs")

all <- 
  bind_rows(crvs2, hmis2) %>% 
  unique() %>% 
  group_by(Country, Age) %>% 
  filter((any(Source == "crvs") & Source == "crvs") | all(Source != "crvs")) %>%
  ungroup() %>% 
  left_join(bts_crvs %>% mutate(bts = 1)) %>% 
  mutate(bts = ifelse(Source == "hmis", 1, bts)) %>% 
  replace_na(list(bts = 0)) %>% 
  filter(Age != "Births")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~ 

all_all <- 
  all %>% 
  filter(Year %in% 2020:2021) %>% 
  select(Country, Year) %>% 
  unique() %>% 
  group_by(Year) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  mutate(var = "Total")

# births
all_bts <- 
  all %>% 
  filter(Year %in% 2020:2021,
         bts == 1) %>% 
  select(Country, Year) %>% 
  unique() %>% 
  group_by(Year) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  mutate(var = "Births")

crvs_bts <- 
  all %>% 
  filter(Year %in% 2020:2021,
         bts == 1,
         Source == "crvs") %>% 
  select(Country, Year) %>% 
  unique() %>% 
  group_by(Year) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  mutate(var = "Births")


all_age <- 
  all %>% 
  filter(Year %in% 2020:2021) %>% 
  select(Country, Year, Age) %>% 
  unique() %>% 
  group_by(Year, Age) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  rename(var = Age)

all_inc <- 
  all %>% 
  filter(Year %in% 2020:2021) %>% 
  select(Country, Year, Income) %>% 
  unique() %>% 
  group_by(Year, Income) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  rename(var = Income)

all_src <- 
  all %>% 
  filter(Year %in% 2020:2021) %>% 
  select(Country, Year, Source) %>% 
  unique() %>% 
  group_by(Year, Source) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  rename(var = Source) %>% 
  mutate(var = str_to_upper(var))

var_levs1 <- c("Total countries/territories", 
              "Live births",
              "Stillbirths (>28w)",
              "Neonatal (<28d)",
              "Infant (<1y)",
              "1-4y",
              "0-4y",
              "5-9y",
              "10-14y",
              "15-19y",
              "20-24y",
              "Low",         
              "Lower-middle",
              "Upper-middle",
              "High", 
              "CRVS",
              "HMIS")

table_01 <- 
  bind_rows(all_all,
            all_bts,
            all_age,
            all_inc,
            all_src) %>% 
  select(var, everything()) %>% 
  arrange(var) %>% 
  rename(i_2020 = `2020`,
         i_2021 = `2021`) %>% 
  mutate(var = case_when(var == "Total" ~ "Total countries/territories",
                         var == "Births" ~ "Live births",
                         var == "Stillbirths" ~ "Stillbirths (>28w)",
                         var == "Neonatal" ~ "Neonatal (<28d)",
                         var == "Infant" ~ "Infant (<1y)",
                         var == "1-4" ~ "1-4y",
                         var == "0-4" ~ "0-4y",
                         var == "5-9" ~ "5-9y",
                         var == "10-14" ~ "10-14y",
                         var == "15-19" ~ "15-19y",
                         var == "20-24" ~ "20-24y",
                         var == "Lower-mid" ~ "Lower-middle",
                         var == "Upper-mid" ~ "Upper-middle",
                         TRUE ~ var),
         var = factor(var, levels = var_levs1)) %>% 
  arrange(var)

table_01
write_csv(table_01, "Tables/01_manuscript/table01_included_data.csv")

# Table S03 years of data available
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# available years of data by age and country
unique(all_fit$Age)

# deaths
ages_dts <- 
  all_data %>% 
  filter(Age %in% c("1-4", "0-4", "5-9", "10-14", "15-19", "20-24"),
         Sex == "t",
         !Country %in% c("Azerbaijan", "Armenia")) %>% 
  separate(Age, c("amin", "amax")) %>% 
  mutate(amin = amin %>% as.double(),
         amax = amax %>% as.double()) %>% 
  # ungroup() %>% 
  select(Country, amin, amax) %>%
  unique() %>%
  group_by(Country) %>% 
  summarise(Ages = paste0(min(amin), "-", max(amax))) %>% 
  ungroup()

ages_dts_hmis <- 
  hmis2 %>% 
  filter(Age %in% c("0-4", "5-9", "10-14", "15-19", "20-24"),
         !Country %in% c("Azerbaijan", "Armenia")) %>% 
  select(Country, Ages = Age) %>% unique()

ages_dts2 <- 
  bind_rows(ages_dts, ages_dts_hmis) %>% arrange(Country)

years_dts <- 
  all_data %>% 
  filter(Age %in% c("0-4", "5-9", "10-14", "15-19", "20-24"),
         Sex == "t",
         !Country %in% c("Azerbaijan", "Armenia")) %>% 
  # mutate(amin = amin %>% as.double(),
  #        amax = amax %>% as.double()) %>% 
  # ungroup() %>% 
  # select(Country, amin, amax) %>%
  # unique() %>%
  group_by(Country) %>% 
  summarise(Deaths = paste0(min(Year), "-", max(Year))) %>% 
  ungroup()

years_dts_hmis <- 
  hmis2 %>% 
  filter(Age %in% c("0-4", "5-9", "10-14", "15-19", "20-24"),
         !Country %in% c("Azerbaijan", "Armenia")) %>% 
  # select(Country, Ages = Age) %>% unique() %>% 
  group_by(Country) %>% 
  summarise(Deaths = paste0(min(Year), "-", max(Year))) %>% 
  ungroup()

years_dts2 <- 
  bind_rows(years_dts, years_dts_hmis) %>% arrange(Country)

years_bts <- 
  read_rds("data_inter/annual_births.rds") %>% 
  select(Code, Year) %>% 
  unique() %>% 
  inner_join(all_fit %>% 
               filter(Sex == "t",
                      !Country %in% c("Azerbaijan", "Armenia")) %>% 
               select(Country, Code) %>% unique()) %>% 
  filter(Year %in% 2010:2021) %>% 
  group_by(Country) %>% 
  summarise(Births = paste0(min(Year), "-", max(Year))) %>% 
  ungroup()

years_bts_hmis <-
  hmis2 %>%
  filter(Age %in% c("Births"),
         !Country %in% c("Azerbaijan", "Armenia")) %>%
  # select(Country, Ages = Age) %>% unique() %>%
  filter(Year %in% 2010:2021) %>% 
  group_by(Country) %>%
  summarise(Births = paste0(min(Year), "-", max(Year))) %>%
  ungroup()

years_bts2 <- 
  bind_rows(years_bts, years_bts_hmis) %>% arrange(Country)

# years stillbirths
years_sbs <- 
  all_data_fit %>% 
  filter(Age %in% c("Stillbirths"),
         !Country %in% c("Azerbaijan", "Armenia")) %>% 
  group_by(Country) %>% 
  summarise(Stillbirths = paste0(min(Year), "-", max(Year))) %>% 
  ungroup()

years_sbs_hmis <- 
  hmis2 %>% 
  filter(Age %in% c("Stillbirths"),
         !Country %in% c("Azerbaijan", "Armenia")) %>% 
  # select(Country, Ages = Age) %>% unique() %>% 
  filter(Year %in% 2010:2021) %>% 
  group_by(Country) %>% 
  summarise(Stillbirths = paste0(min(Year), "-", max(Year))) %>% 
  ungroup()

years_sbs2 <- 
  bind_rows(years_sbs, years_sbs_hmis) %>% arrange(Country)

# years neonatal
years_neo <- 
  all_data_fit %>% 
  filter(Age %in% c("Neonatal"),
         # Sex == "t",
         !Country %in% c("Azerbaijan", "Armenia")) %>% 
  group_by(Country) %>% 
  summarise(Neonatal = paste0(min(Year), "-", max(Year))) %>% 
  ungroup()

years_neo_hmis <- 
  hmis2 %>% 
  filter(Age %in% c("Neonatal"),
         !Country %in% c("Azerbaijan", "Armenia")) %>% 
  # select(Country, Ages = Age) %>% unique() %>% 
  filter(Year %in% 2010:2021) %>% 
  group_by(Country) %>% 
  summarise(Neonatal = paste0(min(Year), "-", max(Year))) %>% 
  ungroup()

years_neo2 <- 
  bind_rows(years_neo, years_neo_hmis) %>% arrange(Country)

# years infant
years_inf <- 
  all_data_fit %>% 
  filter(Age %in% c("Infant"),
         # Sex == "t",
         !Country %in% c("Azerbaijan", "Armenia")) %>% 
  group_by(Country) %>% 
  summarise(Infant = paste0(min(Year), "-", max(Year))) %>% 
  ungroup()

years_inf_hmis <- 
  hmis2 %>% 
  filter(Age %in% c("Infant"),
         !Country %in% c("Azerbaijan", "Armenia")) %>% 
  # select(Country, Ages = Age) %>% unique() %>% 
  filter(Year %in% 2010:2021) %>% 
  group_by(Country) %>% 
  summarise(Infant = paste0(min(Year), "-", max(Year))) %>% 
  ungroup()

years_inf2 <- 
  bind_rows(years_inf, years_inf_hmis) %>% arrange(Country)


table_S03 <- 
  years_bts2 %>% 
  full_join(years_sbs2) %>% 
  full_join(years_neo2) %>% 
  full_join(years_inf2) %>% 
  full_join(ages_dts2) %>% 
  full_join(years_dts2) %>% 
  arrange(Country) %>% 
  filter(!is.na(Stillbirths) | !is.na(Neonatal) | !is.na(Infant) | !is.na(Deaths)) %>% 
  replace_na(list(Births = "-",
                  Stillbirths = "-",
                  Neonatal = "-",
                  Infant = "-",
                  Ages = "-",
                  Deaths = "-")) %>% 
  rename('Live births' = Births) %>% 
  mutate(Country = case_when(Country == "China, Hong Kong SAR" ~ "Hong Kong - China SAR",
                             Country == "Taiwan" ~ "Taiwan - Province of China",
                             TRUE ~ Country)) %>% 
  arrange(Country)
  
table_S03
write_csv(table_S03, "Tables/01_manuscript/tableS03_years_data.csv")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# table S04 ====
# ~~~~~~~~~~~~~~

srs <- 
  all_data %>% 
  filter(Sex == "t",
         !Country %in% c("Azerbaijan", "Armenia"),
         Year %in% 2015:2021) %>% 
  select(Country, Code, Age, starts_with("Source"))

srs_dts <- 
  srs %>% 
  filter(Age %in% c("0-4", "5-9", "10-14", "15-19", "20-24")) %>% 
  select(-Age) %>%
  unique() %>% 
  rename(Deaths = Source_dts,
         Births = Source_bts,
         Population = Source_pop)

unique(hmis2$Age)

srs_dts_hmis <- 
  hmis2 %>% 
  filter(Age %in% c("0-4", "Births")) %>% 
  select(-Year, -Income, -Year) %>% 
  unique() %>% 
  spread(Age, Source) %>% 
  rename(Deaths = `0-4`) %>% 
  mutate(Population = NA)

srs_dts2 <- 
  bind_rows(srs_dts, srs_dts_hmis)

srs_sbs <- 
  srs %>% 
  filter(Age %in% c("Stillbirths")) %>% 
  select(Country, Code, Stillbirths = Source_dts) %>%
  unique()
  
srs_sbs_hmis <- 
  hmis2 %>% 
  filter(Age %in% c("Stillbirths")) %>% 
  select(Country, Code, Stillbirths = Source) %>% 
  unique()

srs_sbs2 <- 
  bind_rows(srs_sbs, srs_sbs_hmis)


srs_neo <- 
  srs %>% 
  filter(Age %in% c("Neonatal")) %>% 
  select(Country, Code, Neonatal = Source_dts) %>%
  unique()

srs_neo_hmis <- 
  hmis2 %>% 
  filter(Age %in% c("Neonatal")) %>% 
  select(Country, Code, Neonatal = Source) %>% 
  unique()

srs_neo2 <- 
  bind_rows(srs_neo, srs_neo_hmis)


srs_inf <- 
  srs %>% 
  filter(Age %in% c("Infant")) %>% 
  select(Country, Code, Infant = Source_dts, Births = Source_bts,
         Population = Source_pop) %>%
  unique()

srs_inf_hmis <- 
  hmis2 %>% 
  filter(Age %in% c("Infant")) %>% 
  select(Country, Code, Infant = Source) %>% 
  unique()

srs_inf2 <- 
  bind_rows(srs_inf, srs_inf_hmis)


table_S04 <- 
  srs_dts2 %>% 
  full_join(srs_sbs2) %>% 
  full_join(srs_neo2) %>% 
  full_join(srs_inf2) %>% 
  replace_na(list(Births = "-",
                  Stillbirths = "-",
                  Neonatal = "-",
                  Infant = "-",
                  Deaths = "-")) %>% 
  arrange(Country)
  
table_S04

# write_csv(table_S04, "Tables/manuscript/tableS03_sources_data.csv")

# UNICEF sources ====
# ~~~~~~~~~~~~~~~~~~~
uncf <- read_xlsx("Data/unicef/Table Sources Covid.xlsx")

uncf2 <- 
  uncf %>% 
  select(Country, source = 'UNICEF data source') %>% 
  mutate(database = "unicef",
         source = case_when(str_detect(source, "DANE") ~ "National Administrative Department of Statistics (DANE) - CRVS. Data submitted to UNICEF via e-mail",
                            str_detect(source, "Philippine Statistics Authority") ~ "Philippine Statistics Authority - CRVS. Data submitted to UNICEF via e-mail",
                            str_detect(source, "Honduras") ~ "Secretaria de Salud of Honduras. Area Estadistica de la Salud. Data submitted to UNICEF via e-mail",
                            str_detect(source, "Finland") ~ "Statistics Finland. Digital and Population Data Services Agency. Data submitted to UNICEF via e-mail.",
                            source == "Agency for Statistics of Bosnia and Herzegovina" ~ "Agency for Statistics of Bosnia and Herzegovina - CRVS. Data submitted to UNICEF via e-mail",
                            source == "Andorra Ministry of Health" ~ "Andorra Ministry of Health. Mortality and birth register - CRVS. Data submitted to UNICEF via e-mail",
                            source == "Australian Bureau of Statistics" ~ "Australian Bureau of Statistics - CRVS. Data submitted to UNICEF via e-mail",
                            source == "Czech Statistical Office" ~ "Czech Statistical Office - CRVS. Data submitted to UNICEF via e-mail",
                            Country == "Brazil" ~ "Ministerio da Saude - Datasus https://datasus.saude.gov.br/",
                            Country == "Ecuador" ~ "Instituto Nacional de Estadística y Censos (INEC) - CRVS. Data submitted to UNICEF via e-mail",
                            Country == "USA" ~ "Centers for Disease Control and Prevention of the United States (CDC) - CRVS. Data submitted to UNICEF via e-mail",
                            Country == "Argentina" ~ "Ministerio de Salud - CRVS. Data submitted to UNICEF via e-mail",
                            TRUE ~ source),
         Country = ifelse(Country == "Réunion", "Reunion", Country))


# UNPD sources ====
# ~~~~~~~~~~~~~~~~~
unpd <- 
  read_csv("data_inter/sources_unpd_details.csv")
  
unpd2 <- 
  unpd %>% 
  rename(source = Source) %>% 
  mutate(database = "unpd",
         source = case_when(Country  == "Bulgaria" & measure == "deaths" ~ DataSourceAuthor,
                            Country  == "Brazil" & measure == "deaths" ~ DataSourceAuthor,
                            Country  == "Chile" & measure == "deaths" ~ DataSourceAuthor,
                            Country  == "Dominican Republic" & measure == "deaths" & source != "UNSD" ~ DataSourceAuthor,
                            Country  == "Peru" & measure == "deaths" ~ DataSourceAuthor,
                            Country  == "Paraguay" & measure == "deaths" ~ DataSourceAuthor,
                            Country  == "Luxembourg" & measure == "deaths" ~ "Institut national de la statistique et des études économiques du Grand-Duché de Luxembourg - STATEC",
                            Country  == "Lithuania" & measure == "deaths" ~ DataSourceAuthor,
                            Country  == "Lithuania" & measure == "births" ~ DataSourceAuthor,
                            Country  == "Saint Helena" & measure == "births" ~ DataSourceAuthor,
                            Country  == "Ecuador" & measure == "births" ~ DataSourceAuthor,
                            Country  == "Brazil" & measure == "births" ~ DataSourceAuthor,
                            Country  == "South Africa" & measure == "births" ~ DataSourceAuthor,
                            # Country  == "Ecuador" & measure == "births" ~ DataSourceAuthor,
                            Code  == "BES" & measure == "births" ~ DataSourceAuthor,
                            TRUE ~ source),
         source = case_when(source == "UNSD" ~ "Demographic Yearbook", 
                            source == "WHO" ~ "WHO All-Cause Mortality Data Call", 
                            TRUE ~ source),
         Country = ifelse(Country == "Réunion", "Reunion", Country)) %>% 
  select(Country, Code, measure, database, source_unpd = source)


# all sources together ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~
# srs_all <- 
#   table_S04 %>% 
#   left_join(uncf2)

srs_all <- 
  table_S04 %>% 
  gather(-Country, -Code, key = measure, value = database) %>% 
  filter(database != "-") %>% 
  mutate(database = case_when(str_detect(database, "unicef") ~ "unicef",
                              str_detect(database, "unpd") ~ "unpd",
                              TRUE ~ database),
         measure = str_to_lower(measure),
         Country = ifelse(Country == "Réunion", "Reunion", Country)) %>% 
  left_join(uncf2) %>% 
  left_join(unpd2, Joining, by = c("Country", "Code", "measure", "database")) %>% 
  mutate(source = ifelse(database == "unpd", source_unpd, source), 
         source = case_when(database == "hmis" & Country != "Mozambique" ~ "Health Management Information System (HMIS)", 
                            database == "who_mort_db" ~ "WHO Mortality Database", 
                            database == "hmd" ~ "Human Mortality Database (HMD)", 
                            database == "stmf" ~ "Short-term mortality fluctuations database (STMF)", 
                            database == "stff" ~ "Short-term fertility fluctuations database (STFF)", 
                            database == "wpp" ~ "UNPD - World Population Prospects 2022 (WPP)", 
                            # database == "unpd_dy" ~ "UNPD - Demographic Yearbook", 
                            # database == "unpd_who_ccd" ~ "UNPD - Demographic Yearbook", 
                            str_detect(database, "eur") ~ "Eurostat Database",
                            database == "brazil_sim" ~ "Ministerio da Saude",
                            database == "colombia_dane" ~ "National Administrative Department of Statistics (DANE) - CRVS",
                            database == "rms_zaf" ~ "Dorrington, R.",
                            Country == "South Africa" & database == "country_specific" ~ "Dorrington, R.",
                            Country == "Bangladesh" & database == "country_specific" ~ "Bangladesh Bureau of Statistics - CRVS",
                            Country == "China" & database == "country_specific" ~ "National Bureau of Statistics of China - SVR",
                            Country == "India" & database == "country_specific" ~ "Office of the Registrar General & Census Commissioner, India (ORGI) - Sample Registration System (SRS)",
                            Country == "Mexico" & database %in% c("country_public", "inegi") ~ "Instituto Nacional de Estadistica y Geografia (INEGI) - CRVS",
                            Country == "United Kingdom" & database == "unicef" ~ "Mothers and Babies: Reducing Risk through Audits and Confidential Enquiries across the UK (MBRRACE-UK) - CRSV",
                            Country == "Greece" & database == "unicef" ~ "Hellenic Statistical Authority - CRSV",
                            Country == "France" & database == "unicef" ~ "Système National des Données de Santé (SNDS) - CRVS",
                            Country == "Iceland" & database == "unicef" ~ "Icelandic Medical Birth Registry",
                            Country == "Slovenia" & database == "unicef" ~ "National Institute of Public Health - CRVS",
                            Country == "Sri Lanka" & database == "unicef" ~ "Perinatal death Surveillance unit -Family Health Bureau - CRVS",
                            Country == "Mozambique" & database == "hmis" ~ "Countrywide Mortality Surveillance for Action (COMSA)",
                            database == "unpd" ~ paste0("UNPD - ", source),
                            TRUE ~ source),
         database = case_when(database %in% c("country_specific", 
                                              "country_public",
                                              "inegi",
                                              "brazil_sim",
                                              "colombia_dane",
                                              "rms_zaf") ~ "country_specific",
                              str_detect(database, "eur") ~ "eurostat",
                              Country == "Mozambique" ~ "comsa",
                              TRUE ~ database)) %>%
  select(-source_unpd)

srs_all

# write_csv(srs_all, "Tables/manuscript/tableS05_sources_details.csv")

miss <- 
  srs_all %>% 
  filter(is.na(source)) %>% 
  pull(Country) %>% unique()
  
# revised sources by Enrique Acosta and Lucia Hug
srs_rev <- read_xlsx("Tables/230601_tableS05_sources_details_ea_lh.xlsx")

miss_srs <- 
  srs_all %>% 
  select(Country, Code, measure) %>% 
  anti_join(srs_rev %>% 
              select(Country, Code, measure))

srs_all2 <- 
  srs_rev %>% 
  mutate(source2 = str_replace(source, "Data submitted to UNICEF via e-mail.", ""),
         source2 = case_when(database == "unicef" ~ paste0("UNICEF data call. ", source2),
                             TRUE ~ source2)) %>% 
  select(-source, -database) %>% 
  unique() %>% 
  group_by(Code, measure) %>% 
  mutate(n = n()) %>% 
  filter(!(measure == "births" & 
             source2 == "UNPD - World Population Prospects 2022 (WPP)" & 
             n == 2)) %>% 
  select(-n, -Code) %>% 
  mutate(Country = case_when(Country == "China, Hong Kong SAR" ~ "Hong Kong - China SAR",
                             Country == "Taiwan" ~ "Taiwan - Province of China",
                             TRUE ~ Country),
         measure = case_when(measure == "births" ~ "live births", 
                             measure == "deaths" ~ "deaths 0-24",
                             TRUE ~ measure),
         measure = factor(measure, levels = c("live births",
                                              "population",
                                              "stillbirths",
                                              "neonatal",
                                              "infant",
                                              "deaths 0-24")),
         source2 = ifelse(source2 == "Rapid Mortality Surveillance. by R. Dorrington." |
                               source2 == "Data from District Health Information System  vial email by R. Dorrington.", 
                               "UNICEF data call. Rapid Mortality Surveillance, supplied by R. Dorrington.",
                          source2)) %>% 
  arrange(measure, Country) %>% 
  rename(Measure = measure,
         Source = source2)

unique(srs_all2$Source) %>% sort()

write_csv(srs_all2, "Tables/01_manuscript/tableS04_sources_details.csv")

  
  
  
  