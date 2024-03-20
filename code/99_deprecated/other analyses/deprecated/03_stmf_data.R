library(here)
source(here("Code", "00_functions.R"))

# info on country names and codes
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ctr_codes <- read_csv(here("Data", "country_codes.csv")) %>% 
#   select(Country, PopCode)


# Loading STMF data
# ~~~~~~~~~~~~~~~~~

# downloading the last version of STMF Mortality input data zip 
# this version as of 25 May 2021
# download.file("https://www.mortality.org/Public/STMF/Inputs/STMFinput.zip", here("Data/STMFinput.zip"))

# list of country codes in STMF
zipdf <- unzip(here("Data", "STMFinput.zip"), list = TRUE)

# loading all cause deaths from all countries in STMF
db_d <- tibble()
for(i in 1:length(zipdf$Name)){
  csv_file <- zipdf$Name[i]
  print(csv_file)
  temp <- read_csv(unz(here("Data", "STMFinput.zip"), csv_file))
  db_d <- db_d %>% 
    bind_rows(temp)
}

db_stmf <- 
  db_d %>% 
  select(-Access, -Type, -AgeInterval, -Area) %>% 
  mutate(PopCode = recode(PopCode,
                          "AUS2" = "AUS",
                          "DEUTNP" = "DEU",
                          "FRATNP" = "FRA",
                          "NZL_NP" = "NZL"),
         Sex = recode(Sex,
                      "b" = "t"), 
         Week = as.character(Week)) %>%
  rename(Code = PopCode) %>% 
  drop_na(Deaths) %>% 
  group_by(Code, Year, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  filter(Age != "UNK",
         Year >= 2015,
         Year <= 2020) %>% 
  mutate(Country = countrycode(sourcevar = Code, 
                               origin = "iso3c", 
                               destination = "country.name"),
         Country = case_when(Code == "GBR_NIR" ~ "Northern Ireland", 
                             Code == "GBR_SCO" ~ "Scotland", 
                             Code == "GBRTENW" ~ "England and Wales",
                             Code == "USA" ~ "USA", 
                             TRUE ~ Country)) %>% 
  select(Country, Code, Year, Sex, Age, Deaths)

db_stmf2 <- 
  db_stmf %>% 
  # rescaling age
  group_by(Country, Code, Sex, Year) %>%
  do(rescale_age(chunk = .data)) %>%
  ungroup() %>%
  # rescaling sex
  group_by(Country, Code, Age, Year) %>%
  do(rescale_sex(chunk = .data)) %>%
  ungroup() %>%
  mutate(Age = as.integer(Age)) %>% 
  arrange(Country, Code, Year, Sex, suppressWarnings(as.numeric(Age)))

write_rds(db_stmf2, "Output/stmf_deaths.rds")
