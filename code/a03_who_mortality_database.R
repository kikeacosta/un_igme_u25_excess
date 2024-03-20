rm (list = ls())
source("code/00_functions.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Updating WHO files
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# downloading last version of WHO files
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# these come from here: https://www.who.int/data/data-collection-tools/who-mortality-database
# icd_base_url <-"https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/morticd10_part1.zip?sfvrsn=e2a4f93a_17&ua=1
icd_base_url <-"https://cdn.who.int/media/docs/default-source/world-health-data-platform/mortality-raw-data/"
icd_files    <- c("mort_country_codes.zip","morticd10_part1.zip","morticd10_part2.zip",
                  "morticd10_part3.zip","morticd10_part4.zip","morticd10_part5.zip")
for (i in 1:length(icd_files)){
  url_i   <- paste0(icd_base_url,icd_files[i])
  local_i <- file.path("data_input", "WHO", icd_files[i])
  download.file(url_i, destfile = local_i, overwrite = TRUE)
}

# a lookup table to match country names to codes
ctry_names <- read_csv(file.path("data_input", "WHO", "mort_country_codes.zip")) %>% 
  rename(Country = country)

icd_all    <- list()
icd_files2 <- icd_files[-1]
#ICD download each of the 5 files
for (i in 1:length(icd_files2)){
  icd_i <- 
    read_csv(file.path("data_input", "WHO", icd_files2[i]),
             col_types = cols(Admin1 = col_character(),SubDiv = col_character(),
                              List = col_character(), Cause = col_character(), 
                              Frmat = col_character(), IM_Frmat = col_character(),
                              .default = col_double())) %>% 
    left_join(ctry_names, by = "Country") %>% 
    dplyr::filter(Sex %in% c(1,2)) 
  
  icd_all[[i]] <- icd_i
}

# stick together
# ~~~~~~~~~~~~~~
icd_all <- 
  bind_rows(icd_all) %>% 
  select(name, everything())

# saving a consolidated file with all WHO data
write_rds(icd_all, "data_inter/who_raw.rds",
          compress = "xz")

# ~~~~~~~~~~~~~~~~~~~~~~~~
# processing data for 2020
# ~~~~~~~~~~~~~~~~~~~~~~~~
icd_all <-
  read_rds("data_inter/who_raw.rds")

# # not yet data for 2022
test22 <-
  icd_all %>%
  mutate(Year = Year %>% as.double()) %>%
  group_by(Country) %>%
  filter(max(Year) >= 2022 &  Frmat != "09") %>%
  ungroup()

# # summary of 2020 and 2021
who_has_20 <-
  icd_all %>%
  mutate(Year = Year %>% as.double()) %>%
  group_by(Country) %>%
  filter(max(Year) >= 2020 &  Frmat != "09") %>%
  ungroup() %>% 
  filter(Year >= 2020) %>% 
  select(name, Year) %>% unique() %>% 
  mutate(avail = "x") %>% 
  spread(Year, avail)

db20 <- 
  icd_all %>% 
  mutate(Year = Year %>% as.double()) %>% 
  group_by(Country) %>% 
  filter(max(Year) >= 2020 &  Frmat != "09") %>% 
  ungroup() %>% 
  filter(Cause %in% c("1000", "AAA"),
         Year >= 2010) %>% 
  select(-Admin1, -SubDiv, -Cause, -List, -Country) %>% 
  rename(Country = name) %>% 
  arrange(Country, Year)

db20_2 <- 
  db20 %>% 
  select(Country, Year, Sex, starts_with("Deaths")) %>% 
  gather(starts_with("Deaths"), key = Age, value = Deaths) %>% 
  filter(Age %in% paste0("Deaths", 1:25)) %>% 
  mutate(Age = recode(Age,
                      'Deaths1' = "TOT",
                      'Deaths2' = "0",
                      'Deaths3' = "1",
                      'Deaths4' = "1",
                      'Deaths5' = "1",
                      'Deaths6' = "1",
                      'Deaths7' = "5",
                      'Deaths8' = "10",
                      'Deaths9' = "15",
                      'Deaths10' = "20",
                      'Deaths11' = "25",
                      'Deaths12' = "30",
                      'Deaths13' = "35",
                      'Deaths14' = "40",
                      'Deaths15' = "45",
                      'Deaths16' = "50",
                      'Deaths17' = "55",
                      'Deaths18' = "60",
                      'Deaths19' = "65",
                      'Deaths20' = "70",
                      'Deaths21' = "75",
                      'Deaths22' = "80",
                      'Deaths23' = "85",
                      'Deaths24' = "90",
                      'Deaths25' = "95"),
         Deaths = Deaths %>% as.double(),
         Sex = recode(Sex,
                      "1" = "m",
                      "2" = "f")) %>%
  drop_na(Deaths) %>% 
  group_by(Country, Year, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Code = countrycode(Country, origin = 'country.name', destination = 'iso3c'),
         Country = recode(Country,
                          "United Kingdom, England and Wales" = "England and Wales",
                          "United Kingdom, Northern Ireland" = "Northern Ireland",
                          "United Kingdom, Scotland" = "Scotland",
                          "Czech Republic" = "Czechia"),
         Code = case_when(Country == "England and Wales" ~ "GBR-ENW",
                          Country == "Scotland" ~ "GBR-SCO",
                          Country == "Northern Ireland" ~ "GBR-NIR",
                          TRUE ~ Code)) %>% 
  arrange(Code, Year, Sex, Age)


unique(db20_2$Code)
unique(db20_2$Age)
unique(db20_2$Sex)

# adding total sex
db20_3 <- 
  db20_2 %>% 
  group_by(Country, Code, Year, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  mutate(Sex = "t") %>% 
  bind_rows(db20_2) %>% 
  select(Country, Code, Year, Sex, Age, Deaths) %>% 
  arrange(Country, Code, Year, Sex, suppressWarnings(as.integer(Age)))

# imputing unknown ages and sexes
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# provisionally not rescaling sexes, as data is analyzed for total sex
# small issue not resolved yet
db20_4 <- 
  db20_3 %>% 
  group_by(Country, Code, Sex, Year) %>% 
  do(rescale_age(chunk = .data)) %>% 
  ungroup()

db20_5 <- 
  db20_4 %>% 
  mutate(Age = Age %>% as.double()) %>% 
  filter(Age <= 24) %>% 
  group_by(Country, Year, Sex) %>% 
  mutate(age_up = ifelse(Age == 20, 24, lead(Age) - 1),
         Source = "who_mort_db") %>% 
  ungroup()

unique(db20_5$Code)

write_rds(db20_5, "data_inter/who.rds")


