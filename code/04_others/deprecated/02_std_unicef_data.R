library(here)
source(here("Code", "00_functions.R"))

brz <- read_csv("Data/brazil.csv")
col <- read_csv("Data/colombia.csv")
chl <- read_csv("Data/chile.csv")
mex <- read_csv("Data/mexico.csv")
per <- read_csv("Data/peru.csv")

deaths <- 
  bind_rows(std_db(brz), 
            std_db(col), 
            std_db(chl), 
            std_db(mex), 
            std_db(per)) %>% 
  mutate(Sex = recode(Sex,
                      "Male" = "m",
                      "Female" = "f", 
                      "Both sexes" = "t"),
         Age = ifelse(Age > 100, 100, Age)) %>% 
  group_by(Country, Year, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Age = as.character(Age))

deaths2 <- 
  deaths %>% 
  filter(Age != "-2") %>% 
  bind_rows(deaths %>% 
              group_by(Country, Year, Sex) %>% 
              summarise(Deaths = sum(Deaths)) %>% 
              ungroup() %>% 
              mutate(Age = "TOT")) %>% 
  arrange(Country, Year, Sex, suppressWarnings(as.numeric(Age)))

unique(deaths2$Age) %>% sort()

deaths_adj <- 
  deaths2 %>% 
  group_by(Country, Sex, Year) %>% 
  do(rescale_age(chunk = .data)) %>% 
  ungroup() %>% 
  group_by(Country, Age, Year) %>% 
  do(rescale_sex(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(Age = as.double(Age)) %>% 
  arrange(Country, Year, Sex, Age) 

deaths_adj2 <- 
  deaths_adj %>% 
  filter(Year <= 2020) %>% 
  mutate(Code = countrycode(sourcevar = Country, 
                            origin = "country.name", 
                            destination = "iso3c"),
         Source = "celade")

write_rds(deaths_adj2, "Output/latam_deaths.rds")
