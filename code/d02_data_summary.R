rm (list = ls())
source("code/00_functions.R")

# loading master deaths database
crvs <- read_rds("data_inter/annual_deaths_rates_2010_2022.rds")
hmis <- read_rds("data_inter/hmis_all_countries.rds") 

crvs <- 
  crvs %>% 
  unique()

unique(crvs$Age)

unique(crvs$Source_dts)
unique(crvs$Source_bts)
unique(crvs$Source_pop)
unique(crvs$Age)

unique(hmis$source)
unique(hmis$measure)

test <- 
  crvs %>% 
  group_by(Code, Year, Sex, Age, Source_dts) %>% 
  filter(n() > 1)
test

# countries with data on births
bts_crvs <- 
  read_rds("data_inter/annual_births.rds") %>% 
  select(Code, Source) %>% 
  unique() %>% 
  arrange()

bts_hmis <- 
  hmis %>% 
  filter(measure == "bts") %>% 
  select(Code = code, 
         Source = source) %>% 
  unique() %>% 
  arrange()

bts_cts <- 
  bind_rows(bts_crvs, bts_hmis)

birth_counts_cts <- 
  bind_rows(bts_crvs, bts_hmis) %>% 
  pull(Code) %>% 
  unique() %>% 
  sort()

cts_rts <- 
  crvs %>% 
  filter(type_data == "rates") %>% 
  pull(Code) %>% 
  unique()

cts_bts <- 
  c(birth_counts_cts, cts_rts) %>% sort

unique(crvs$Age)
unique(hmis$measure)

hmis2 <- 
  hmis %>% 
  filter(!measure %in% c("bts", "pos", "mat", "dvs")) %>% 
  mutate(year = year(date)) %>% 
  select(code, country, region, year, measure, income) %>% 
  unique() %>% 
  mutate(age = case_when(str_detect(measure, "sbs") ~ "Stillbirths",
                         str_detect(measure, "neo") ~ "Neonatal",
                         str_detect(measure, "inf") ~ "Infant",
                         str_detect(measure, "0_4") ~ "0-4",
                         str_detect(measure, "1_4") ~ "1-4",
                         str_detect(measure, "5_9") ~ "5-9",
                         str_detect(measure, "10_14") ~ "10-14",
                         str_detect(measure, "15_19") ~ "15-19",
                         str_detect(measure, "20_24") ~ "20-24",
                         TRUE ~ measure),
         sex = "t") %>% 
  select(Code = code,
         Country = country,
         Year = year,
         Sex = sex,
         Age = age,
         Region = region,
         Income = income) %>% 
  mutate(Source_type = "hmis",
         Source_dts = "hmis", 
         Source_bts = "hmis", 
         Source_pop = "hmis")

unique(hmis2$Age)

crvs2 <- 
  crvs %>% 
  select(Code, Country, Year, Sex, Age, Region, Income, Source_dts, Source_bts, Source_pop) %>% 
  unique() %>% 
  mutate(Source_type = "crvs")

all <- 
  bind_rows(crvs2, hmis2) %>% 
  unique()

write_rds(all, "data_inter/summary_data_all.rds")

# table1 ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all2 <- 
  all %>% 
  filter(Year %in% 2020:2022,
         Sex == "t") %>% 
  group_by(Year, Age) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  rename(var = Age)

all2
write.excel(all2)

all3 <- 
  all %>% 
  filter(Year %in% 2020:2022,
         Sex == "t") %>% 
  select(Code, Country, Year, Income) %>% 
  unique() %>% 
  group_by(Year, Income) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  rename(var = Income) %>% 
  mutate(var = fct_rev(var)) %>% 
  arrange(var)

all3
write.excel(all3)

births <- 
  read_rds("data_inter/annual_births.rds") %>% 
  select(Code, Year, Source)
  
bts <- 
  all %>% 
  filter(Year %in% 2020:2022,
         Sex == "t") %>% 
  select(Code, Country, Year) %>% 
  unique() %>% 
  left_join(births) %>% 
  drop_na() %>% 
  group_by(Year) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  mutate(var = "Births")

tot <- 
  all %>% 
  filter(Year %in% 2020:2022,
         Sex == "t") %>% 
  select(Country, Year) %>% 
  unique() %>% 
  group_by(Year) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  mutate(var = "Total")
  
source <- 
  all %>% 
  filter(Year %in% 2020:2022,
         Sex == "t") %>% 
  select(Country, var = Source_type, Year) %>% 
  unique() %>% 
  group_by(var, Year) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  mutate(var = str_to_upper(var))

table01 <- 
  bind_rows(tot,
          all2,
          bts,
          all3, 
          source) %>% 
  select(var, everything()) %>% 
  mutate(var = factor(var, levels = c("Total", 
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
                                      "HMIS"))) %>% 
  arrange(var)

table01
write_csv(table01, "tables/table01_available_data.csv")


all %>% 
  filter(Year %in% 2020:2022,
         Sex == "t") %>% 
  select(Country, Source_type, Year) %>% 
  unique() %>% 
  group_by(Country, Year) %>% 
  filter(n() > 1)
  

hmis2 %>% 
  filter(Year %in% 2020:2022,
         Age %in% c("Stillbirths",
                    "Neonatal",
                    "Infant",
                    "0-4",
                    "1-4")) %>% 
  select(Country, Year, var = Age) %>% 
  unique() %>% 
  group_by(Year, var) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  mutate(var = factor(var, levels = c("Stillbirths",
                                      "Neonatal",
                                      "Infant",
                                      "0-4",
                                      "1-4"))) %>% 
  arrange(var)

  
hmis2 %>% 
  filter(Year %in% 2020:2022,
         Age %in% c("Stillbirths",
                    "Neonatal",
                    "Infant",
                    "0-4",
                    "1-4")) %>% 
  select(Country, Region) %>% unique() %>% group_by(Region) %>% summarise(n = n())

hmis2 %>% 
  filter(Year %in% 2020:2022,
         Age %in% c("Stillbirths",
                    "Neonatal",
                    "Infant",
                    "0-4",
                    "1-4")) %>% 
  select(Country, Year, Income) %>% unique() %>% group_by(Year, Income) %>% summarise(n = n())


# table CRVS ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all2 <- 
  all %>% 
  filter(Year %in% 2020:2022,
         Sex == "t",
         Source_type == "crvs") %>% 
  group_by(Year, Age) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  rename(var = Age)

all2
write.excel(all2)

all3 <- 
  all %>% 
  filter(Year %in% 2020:2022,
         Sex == "t",
         Source_type == "crvs") %>% 
  select(Code, Country, Year, Income) %>% 
  unique() %>% 
  group_by(Year, Income) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  rename(var = Income) %>% 
  mutate(var = fct_rev(var)) %>% 
  arrange(var)

all3
write.excel(all3)

births <- 
  read_rds("data_inter/annual_births.rds") %>% 
  select(Code, Year, Source)

bts <- 
  all %>% 
  filter(Year %in% 2020:2022,
         Sex == "t",
         Source_type == "crvs") %>% 
  select(Code, Country, Year) %>% 
  unique() %>% 
  left_join(births) %>% 
  drop_na() %>% 
  group_by(Year) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  mutate(var = "Births")

tot <- 
  all %>% 
  filter(Year %in% 2020:2022,
         Sex == "t",
         Source_type == "crvs") %>% 
  select(Country, Year) %>% 
  unique() %>% 
  group_by(Year) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  mutate(var = "Total")

source <- 
  all %>% 
  filter(Year %in% 2020:2022,
         Sex == "t") %>% 
  select(Country, var = Source_type, Year) %>% 
  unique() %>% 
  group_by(var, Year) %>% 
  summarise(n = n()) %>% 
  spread(Year, n) %>% 
  mutate(var = str_to_upper(var))

table01 <- 
  bind_rows(tot,
            all2,
            bts,
            all3) %>% 
  select(var, everything()) %>% 
  mutate(var = factor(var, levels = c("Total", 
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
                                      "HMIS"))) %>% 
  arrange(var)

table01
write_csv(table01, "tables/table01_available_crvs_data.csv")
