rm (list = ls())
source("code/00_functions.R")

# countries to be excluded due to quality issues or 
# because are subnational divisions (e.g, England, Scotland...)
cts_exclude <- c(
  "Kenya",  # issues with data quality
  "Lesotho", # issues with data quality
  "Northern Ireland", # subnational division of the UK
  "Scotland", # subnational division of the UK
  "England and Wales" # subnational division of the UK
)

cts_exclude <- c(
  "KEN",  # issues with data quality
  "LSO", # issues with data quality
  "GBR-NIR", # subnational division of the UK
  "GBR-SCO", # subnational division of the UK
  "GBR-ENW", # subnational division of the UK
  "GBR-ENG" # subnational division of the UK
)

# Exposures data
# ~~~~~~~~~~~~~~

# exposures data from:
# WPP 2022 estimates by age and sex for all ages
# HMD exposures for countries with HMD deaths
# STFF data on births in 2010-2021
# HMD data on births in 2010-2021
# Country Consultation data (CCD)
# other sources for births in 2020 and 2021

# mortality data
# ~~~~~~~~~~~~~~
{
dts_yng <- 
  read_rds("data_inter/annual_young_deaths_all_countries.rds")

dts_inf <- 
  read_rds("data_inter/annual_infant_deaths_all_countries.rds")

dts_sbs <- 
  read_rds("data_inter/neonatal_and_stillbirths_all_countries.rds")

rts_yng <- 
  read_rds("data_inter/annual_rates_5y_groups.rds")

rts_inf <- 
  read_rds("data_inter/annual_rates_infant_child.rds")



all_cts <- 
  bind_rows(dts_yng %>% 
              select(Code),
            dts_inf %>% 
              select(Code),
            dts_sbs %>% 
              select(Code),
            rts_yng %>% 
              select(Code),
            rts_inf %>% 
              select(Code)) %>% 
  pull(Code) %>% 
  unique() %>% 
  sort()
}

# population and births data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
pop <- 
  read_rds("data_inter/exposures_5-year_groups.rds")

bts <- 
  read_rds("data_inter/annual_births.rds")

# Adjusting population with birth counts where possible
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
adj_fr <- 
  pop %>% 
  filter(Sex == "t",
         Age == 0,
         Code %in% unique(bts$Code)) %>% 
  left_join(bts, by = c("Code", "Year")) %>% 
  mutate(adj0 = Births / Population) %>% 
  select(Code, Year, Age, adj0)

adj_pop0 <- 
  pop %>% 
  filter(Age == 0) %>% 
  left_join(adj_fr) %>% 
  mutate(adj0 = ifelse(is.na(adj0), 1, adj0),
         Population = Population * adj0) %>% 
  left_join(bts %>% 
              select(Code, Year, Source_bts = Source), 
            by = c("Year", "Code")) %>% 
  mutate(Source = ifelse(is.na(Source_bts), Source, Source_bts)) %>% 
  select(Code, Year, Sex, Age, Population, Source) 


# sources of births and population data
srs_bts <- 
  adj_pop0 %>% 
  select(Code, Year, Source_bts = Source) %>% 
  unique()

srs_pop <- 
  pop %>% 
  filter(Age != 0) %>% 
  select(Code, Year, Source_pop = Source) %>% 
  unique()


adj_pop_all <- 
  adj_pop0 %>% 
  bind_rows(pop %>% 
              filter(Age != 0)) %>% 
  arrange(Code, Sex, Age, Year) %>% 
  select(-Source) %>% 
  left_join(srs_bts) %>% 
  left_join(srs_pop)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Population for infant and child mortality
pop_inf_chl <- 
  adj_pop_all %>% 
  filter(Age %in% 0:1)

# Population for 5-year age groups
pop_5y_grps <- 
  adj_pop_all %>% 
  mutate(Age = ifelse(Age == 1, 0, Age)) %>% 
  group_by(Code, Year, Sex, Age) %>% 
  summarise(Population = sum(Population),
            Source_bts = unique(Source_bts) %>% paste(collapse = ", "),
            Source_pop = unique(Source_pop) %>% paste(collapse = ", ")) %>% 
  ungroup()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merging Deaths and Exposures 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# stillbirths and neonatal
dts_exp_sbs <-
  dts_sbs %>%
  left_join(pop_inf_chl %>%
              # rename(Source_pop = Source) %>% 
              filter(Age == 0,
                     Sex == "t") %>% 
              select(-Age)) %>%
  mutate(Births = ifelse(is.na(Births), Population, Births),
         type_data = case_when(Age %in% c("sbs", "neo") ~ "counts",
                               Age %in% c("sbs_r", "neo_r") ~ "rates"),
         Age = case_when(Age %in% c("sbs_r", "sbs") ~ "Stillbirths",
                         Age %in% c("neo_r", "neo") ~ "Neonatal"),
         Exposure = case_when(Age %in% c("Stillbirths") ~ Births + Deaths,
                              Age %in% c("Neonatal") ~ Births)) %>% 
  select(Code, Year, Sex, Age, Deaths, 
         Exposure, Source, Source_bts, 
         Source_pop, type_data) %>% 
  mutate(Rate = round(1e5 * Deaths / Exposure, 1))

# infant and child mortality
dts_exp_inf <- 
  dts_inf %>% 
  left_join(pop_inf_chl) %>% 
  drop_na(Deaths, Population) %>% 
  mutate(type_data = "counts",
         Age = recode(Age,
                      "0" = "Infant",
                      "1" = "1-4"),
         Rate = round(1e5 * Deaths / Population, 1)) %>% 
  rename(Exposure = Population) %>% 
  select(-age_up)

# ages 0-4 in 5-year age groups
dts_exp_yng <- 
  dts_yng %>% 
  left_join(pop_5y_grps) %>% 
  drop_na(Deaths, Population) %>% 
  mutate(type_data = "counts",
         Age = recode(Age,
                      "0" = "0-4",
                      "5" = "5-9",
                      "10" = "10-14",
                      "15" = "15-19",
                      "20" = "20-24"),
         Rate = round(1e5 * Deaths / Population, 1)) %>% 
  rename(Exposure = Population) %>% 
  select(-age_up)

# infant and child mortality in rates
rts_exp_inf <- 
  rts_inf %>% 
  left_join(pop_inf_chl) %>% 
  mutate(type_data = "rates",
         Age = recode(Age,
                      "0" = "Infant",
                      "1" = "1-4")) %>% 
  rename(Exposure = Population) %>% 
  select(-age_up)

# ages 0-4 in 5-year age groups in rates
rts_exp_yng <- 
  rts_yng %>% 
  left_join(pop_5y_grps) %>% 
  mutate(type_data = "rates",
         Age = recode(Age,
                      "0" = "0-4",
                      "5" = "5-9",
                      "10" = "10-14",
                      "15" = "15-19",
                      "20" = "20-24")) %>% 
  rename(Exposure = Population) %>% 
  select(-age_up)

dts_exp <- 
  bind_rows(dts_exp_sbs,
            dts_exp_inf,
            dts_exp_yng, 
            rts_exp_inf,
            rts_exp_yng) %>% 
  arrange(Code, Age, Sex, Year) %>% 
  mutate(Age = factor(Age, 
                      levels = c("Stillbirths", "Neonatal", 
                                 "Infant", "1-4", "0-4", "5-9", 
                                 "10-14", "15-19", "20-24"))) %>% 
  filter(!Code %in% cts_exclude) %>% 
  rename(Source_dts = Source)

# loading country contextual variables from World Bank
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# From the World Bank: current classification by income in XLSX format
# http://databank.worldbank.org/data/download/site-content/CLASS.xlsx
# income_levels <- read_xlsx("Data/world_bank/CLASS.xlsx") %>% 
#   drop_na(Region) %>% 
#   select(Code_join = 2,
#          Region = 3,
#          Income = 4) %>% 
#   mutate(Income = case_when(Income == "High income" ~ "High",
#                             Income == "Low income" ~ "Low",
#                             Income == "Upper middle income" ~ "Upper-mid",
#                             Income == "Lower middle income" ~ "Lower-mid",
#                             TRUE ~ Income))
income_levels <- read_xlsx("data_input/WPP2022_F01_LOCATIONS.XLSX") %>% 
  select(Code_join = 5, type = 9, Region = 13, 
         hi = 27, umi = 29, lmi = 30, li = 31) %>% 
  filter(type == "Country/Area") %>% 
  mutate(Income = case_when(!is.na(hi) ~ "High",
                            !is.na(umi) ~ "Upper-mid",
                            !is.na(lmi) ~ "Lower-mid",
                            !is.na(li) ~ "Low",
                            TRUE ~ "No income data"),
         Code_join = ifelse(Code_join == "XKX", "RKS", Code_join)) %>% 
  select(-hi, -umi, -lmi, -li)

unique(income_levels$Income)

# merging income data
# ~~~~~~~~~~~~~~~~~~~
income_lvs <- c("High", "Upper-mid", "Lower-mid", "Low", "No income data")
# uk_cts <- c("England and Wales", "Northern Ireland", "Scotland")

dts_exp2 <- 
  dts_exp %>% 
  mutate(Code_join = Code) %>%
  left_join(income_levels, by = "Code_join") %>% 
  # Imputing High-income level for Montserrat-MSR (French territory) and 
  # Anguilla-AIA (UK territory), as for other overseas territories (French Polynesia, 
  # Guadeloupe, )
  mutate(Income = ifelse(Code %in% c("AIA", "MSR"), "High", Income),
         Income = factor(Income, levels = income_lvs)) %>% 
  select(-Code_join, -type)

unique(dts_exp2$Code)

# Populations without data on income
dts_exp2 %>% 
  filter(Income == "No income data") %>% 
  select(Code, Income) %>% 
  unique()

# adding country names
dts_exp3 <- 
  dts_exp2 %>% 
  mutate(Country = countrycode(Code, origin = "iso3c",
                               destination = "country.name"),
         Country = case_when(Code == "GBR-ENW" ~ "England and Wales",
                             Code == "GBR-NIR" ~ "Northern Ireland",
                             Code == "GBR-SCO" ~ "Scotland",
                             Code == "USA" ~ "USA",
                             Code == "RKS" ~ "Kosovo",
                             Code == "PSE" ~ "State of Palestine",
                             TRUE ~ Country)) %>% 
  select(Code, Country, everything())

unique(dts_exp3$Country)


# saving master deaths and rates database
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_rds(dts_exp3, "data_inter/annual_deaths_rates_2010_2022.rds")
write_rds(dts_exp3, paste0("data_output/annual_deaths_rates_2010_2022_", today(), ".rds"))

