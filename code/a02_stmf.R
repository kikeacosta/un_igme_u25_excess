rm(list=ls())
source("code/00_functions.R")

# Loading STMF data
# ~~~~~~~~~~~~~~~~~

# downloading the last version of STMF Mortality input data zip 
# this version as of 25 May 2021
download.file("https://www.mortality.org/File/GetDocument/Public/STMF/Inputs/STMFinput.zip", 
              "data_input/STMFinput.zip")
# "https://www.mortality.org/File/GetDocument/Public/STMF/Inputs/STMFinput.zip"
# list of country codes in STMF
zipdf <- unzip("data_input/STMFinput.zip", list = TRUE)

# loading all cause deaths from all countries in STMF
db_d <- tibble()
for(i in 1:length(zipdf$Name)){
  csv_file <- zipdf$Name[i]
  print(csv_file)
  temp <- 
    read_csv(unz("data_input/STMFinput.zip", csv_file)) %>% 
    mutate(Week = as.double(Week))
  db_d <- db_d %>% 
    bind_rows(temp)
}

unique(db_d$PopCode)

# countries with full 2020
cts_2020 <- 
  db_d %>% 
  drop_na(Week) %>% 
  filter(Year == 2020) %>% 
  group_by(PopCode) %>% 
  filter(max(Week) >= 52) %>% 
  pull(PopCode) %>% unique()

# countries with full 2021
cts_2021 <- 
  db_d %>% 
  filter(Year == 2021) %>% 
  group_by(PopCode) %>% 
  filter(max(Week) == 52) %>% 
  pull(PopCode) %>% unique()

# filtering periods 2015-2021 with full annual information
dts <- 
  db_d %>% 
  mutate(PopCode = ifelse(PopCode == "a", "NOR", PopCode)) %>% 
  filter(Year %in% 2010:2021) %>% 
  filter(PopCode %in% cts_2020) %>% 
  filter(Year <= 2020 | PopCode %in% cts_2021) %>% 
  select(-Access, -Type, -Area)

unique(dts$PopCode)

# countries with changes in age groups
unique_ages_year <- 
  dts %>% 
  select(PopCode, Age, AgeInterval) %>% 
  unique() %>% 
  group_by(PopCode, Age) %>% 
  summarise(n = n()) %>% 
  filter(n >= 2)

# countries with changes in sex
unique_sex_year <- 
  dts %>% 
  select(PopCode, Sex, Week, Year) %>% 
  unique() %>% 
  group_by(PopCode, Week, Year) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  select(PopCode, Year, n) %>% 
  unique() %>% 
  filter(n < 3)

# manual homogenization ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# France: leave it as it is
# USA: exclude it
# Scotland: exclude it
# England and Wales: exclude it
# Northern Ireland: exclude it

# ====
exc <- c("USA", "GBR_NIR", "GBRTENW", "GBR_SCO")

dts2 <- 
  dts %>% 
  # filter(!PopCode %in% c(exc, "RUS")) %>% 
  # bind_rows(rus) %>% 
  group_by(PopCode, Year, Sex, Age, AgeInterval) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  filter(Age != "UNK") %>% 
  mutate(Sex = recode(Sex,
                      "b" = "t"))

# imputing unknown ages and sexes
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# provisionally not rescaling sexes, as data is analyzed for total sex
# small issue not resolved yet
dts3 <- 
  dts2 %>% 
  group_by(PopCode, Sex, Year) %>% 
  do(rescale_age(chunk = .data)) %>% 
  # ungroup() %>% 
  # group_by(PopCode, Age, Year) %>% 
  # do(rescale_sex(chunk = .data)) %>%
  ungroup()

dts4 <- 
  dts3 %>% 
  mutate(Age = as.double(Age),
         AgeInterval = as.double(AgeInterval),
         Deaths = round(Deaths)) %>% 
  arrange(PopCode, Year, Sex, Age, AgeInterval) %>% 
  mutate(PopCode = recode(PopCode,
                          "AUS2" = "AUS",
                          "DEUTNP" = "DEU",
                          "FRATNP" = "FRA",
                          "NZL_NP" = "NZL",
                          "GBR_NIR" = "GBR-NIR", 
                          "GBR_SCO" = "GBR-SCO", 
                          "GBRTENW" = "GBR-ENW")) %>%
  rename(Code = PopCode) %>% 
  drop_na(Deaths) %>% 
  group_by(Code, Year, Sex, Age, AgeInterval) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Source = "stmf",
         Country = countrycode(sourcevar = Code, 
                               origin = "iso3c", 
                               destination = "country.name"),
         Country = case_when(Code == "GBR-NIR" ~ "Northern Ireland", 
                             Code == "GBR-SCO" ~ "Scotland", 
                             Code == "GBR-ENW" ~ "England and Wales",
                             Code == "USA" ~ "USA", 
                             TRUE ~ Country)) %>% 
  select(Country, Code, Year, Sex, Age, AgeInterval, Deaths, Source)


dts_stf <- 
  dts4 %>% 
  filter(Age + AgeInterval <= 25)

unique(dts_stf$Code) %>% sort

write_rds(dts_stf, "data_inter/stmf.rds")


