rm (list = ls())
source("Code/00_functions.R")

# countries to be excluded due to quality issues or 
# because are subnational divisions (e.g, England, Scotland...)
cts_exclude <- c(
  "Kenya",  # issues with data quality
  "Lesotho", # issues with data quality
  "Northern Ireland", # subnational division of the UK
  "Scotland", # subnational division of the UK
  "England and Wales" # subnational division of the UK
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
              select(Country),
            dts_inf %>% 
              select(Country),
            dts_sbs %>% 
              select(Country),
            rts_yng %>% 
              select(Country),
            rts_inf %>% 
              select(Country)) %>% 
  pull(Country) %>% 
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
         Country %in% unique(bts$Country)) %>% 
  left_join(bts, by = c("Code", "Year", "Country")) %>% 
  mutate(adj0 = Births / Population) %>% 
  select(Country, Code, Year, Age, adj0)

# bts_2021 <- 
#   bts %>% 
#   filter(Year == 2021) %>% 
#   mutate(Age = 0,
#          Sex = "t") %>% 
#   rename(Population = Births)

adj_pop0 <- 
  pop %>% 
  filter(Age == 0) %>% 
  left_join(adj_fr) %>% 
  mutate(adj0 = ifelse(is.na(adj0), 1, adj0),
         Population = Population * adj0) %>% 
  left_join(bts %>% 
              select(Country, Year, Source2 = Source), 
            by = c("Year", "Country")) %>% 
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
  


  
  # extrapolating population at age <1
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # pop_inf <- 
  #   pop_inf_chl %>% 
  #   filter(Age == 0) %>% 
  #   bind_rows(bts_2021) %>% 
  #   arrange(Country, Year, Sex) %>% 
  #   unique()
  
  # cts_inf_21 <-
  #   pop_inf %>%
  #   filter(Year == 2021) %>%
  #   pull(Country) %>%  unique()
  # 
  # pop_inf_21 <- 
  #   pop_inf %>%
  #   filter(!Country %in% cts_inf_21) %>% 
  #   group_by(Country, Code, Sex, Age) %>% 
  #   do(extrapopulation(.data)) %>% 
  #   ungroup() %>% 
  #   bind_rows(pop_inf %>% 
  #               filter(Country %in% cts_inf_21))
  
  
  # extrapolating population at age 1-4
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # pop_chl <- 
  #   pop_inf_chl %>% 
  #   filter(Age == 1)
  
  # cts_chl_21 <-
  #   pop_chl %>%
  #   filter(Year == 2021) %>%
  #   pull(Country) %>%  unique()
  # 
  # pop_chl_21 <- 
  #   pop_chl %>%
  #   filter(!Country %in% cts_chl_21) %>% 
  #   group_by(Country, Code, Sex, Age) %>% 
  #   do(extrapopulation(.data)) %>% 
  #   ungroup() %>% 
  #   bind_rows(pop_chl %>% 
  #               filter(Country %in% cts_chl_21))
# }

# merging ages <1 and 1-4
# ~~~~~~~~~~~~~~~~~~~~~~~
# pop_inf_chl <- 
#   bind_rows(pop_inf,
#             pop_chl) %>% 
#   arrange(Country, Sex, Year)
  
# 
# # visual inspection
# ct <- "England and Wales"
# sx <- "t"
# ag <- "1"
# 
# pop_inf_chl_21 %>% 
#   filter(Country == ct,
#          Sex == sx,
#          Age == ag) %>% 
#   ggplot()+
#   geom_line(aes(Year, Population))
# 
# unique(pop_inf_chl_21$Age)
# unique(pop_5y_grps_21$Age)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merging Deaths and Exposures 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# stillbirths and neonatal
dts_exp_sbs <-
  dts_sbs %>%
  left_join(pop_inf_chl %>%
              rename(Source_pop = Source) %>% 
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
  select(Country, Code, Year, Sex, Age, Deaths, Exposure, Source, Source_pop, type_data) %>% 
  mutate(Rate = round(1e5 * Deaths / Exposure, 1))

# infant and child mortality
dts_exp_inf <- 
  dts_inf %>% 
  left_join(pop_inf_chl %>% 
              rename(Source_pop = Source)) %>% 
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
  left_join(pop_5y_grps %>% 
              rename(Source_pop = Source)) %>% 
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
  left_join(pop_inf_chl %>% 
              rename(Source_pop = Source)) %>% 
  mutate(type_data = "rates",
         Age = recode(Age,
                      "0" = "Infant",
                      "1" = "1-4")) %>% 
  rename(Exposure = Population) %>% 
  select(-age_up)

# ages 0-4 in 5-year age groups in rates
rts_exp_yng <- 
  rts_yng %>% 
  left_join(pop_5y_grps %>% 
              rename(Source_pop = Source)) %>% 
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
  arrange(Country, Code, Age, Sex, Year) %>% 
  mutate(Age = factor(Age, 
                      levels = c("Stillbirths", "Neonatal", 
                                 "Infant", "1-4", "0-4", "5-9", 
                                 "10-14", "15-19", "20-24"))) %>% 
  filter(!Country %in% cts_exclude)

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
income_levels <- read_xlsx("Data/WPP2022_F01_LOCATIONS.XLSX") %>% 
  select(Code_join = 5, type = 9, Region = 13, 
         hi = 27, umi = 29, lmi = 30, li = 31) %>% 
  filter(type == "Country/Area") %>% 
  mutate(Income = case_when(!is.na(hi) ~ "High",
                            !is.na(umi) ~ "Upper-mid",
                            !is.na(lmi) ~ "Lower-mid",
                            !is.na(li) ~ "Low",
                            TRUE ~ "No income data")) %>% 
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
  # Imputing High-income level for Montserrat (French territory) and 
  # Anguilla (UK territory), as for other overseas territories (French Polynesia, 
  # Guadeloupe, )
  mutate(Income = ifelse(Country %in% c("Anguilla", "Montserrat"), "High", Income),
         Income = factor(Income, levels = income_lvs)) %>% 
  select(-Code_join, -type)


# Populations without data on income
dts_exp2 %>% 
  filter(Income == "No income data") %>% 
  select(Country, Income) %>% 
  unique()



# saving master deaths and rates database
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_rds(dts_exp2, "data_inter/annual_deaths_rates_2010_2021.rds")

# 



# write_rds(dts_exp_inf, "data_inter/annual_deaths_pop_infant_child.rds")
# 
# write_rds(rts_exp_yng, "data_inter/annual_rates_pop_5y_groups.rds")
# write_rds(rts_exp_inf, "data_inter/annual_rates_pop_infant_child.rds")

# write_rds(pop_inf_chl_21, "data_inter/annual_exposure_infant_child.rds")
# write_rds(pop_5y_grps_21, "data_inter/annual_exposure_5y_groups.rds")
