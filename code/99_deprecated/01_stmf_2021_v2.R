library(here)
library(tidyverse)
library(countrycode)

# Loading STMF data
# ~~~~~~~~~~~~~~~~~

# downloading the last version of STMF Mortality input data zip 
# this version as of 25 May 2021
download.file("https://www.mortality.org/Public/STMF/Inputs/STMFinput.zip", here("Data/STMFinput.zip"))

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

d2021 <- 
  db_d %>% 
  filter(Year == 2021)

test <- 
  d2021 %>% 
  group_by(PopCode) %>% 
  filter(Week == max(Week),
         Age == "TOT",
         Sex == "b")

cts_2021 <- 
  d2021 %>% 
  filter(Week == 52,
         Age == "TOT",
         Sex == "b") %>% 
  pull(PopCode) %>% 
  unique()

d2021 <- 
  db_d %>% 
  filter(PopCode %in% cts_2021)

unique(d2021$PopCode)

db_d2 <- 
  d2021 %>% 
  # filter(PopCode != "RUS") %>% 
  select(-Access, -Type, -AgeInterval, -Area) %>% 
  mutate(PopCode = ifelse(PopCode == "a", "NOR", PopCode))

unique(db_d2$PopCode)

db_stmf <- 
  db_d2 %>% 
  filter(Age != "UNK",
         Year >= 2015 & Year <= 2021) %>% 
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
  mutate(Source = "stmf",
         Country = countrycode(sourcevar = Code, 
                               origin = "iso3c", 
                               destination = "country.name"),
         Country = case_when(Code == "GBR_NIR" ~ "Northern Ireland", 
                             Code == "GBR_SCO" ~ "Scotland", 
                             Code == "GBRTENW" ~ "England and Wales",
                             Code == "USA" ~ "USA", 
                             TRUE ~ Country)) %>% 
  select(Country, Code, Year, Sex, Age, Deaths, Source)


write_csv(db_stmf, "Output/stmf_2021.csv")

d2021 <- 
  db_d %>% 
  filter(Year == 2021)

test <- 
  d2021 %>% 
  filter(Age == "TOT",
         Sex == "b",
         Year == 2021) %>% 
  group_by(PopCode) %>% 
  filter(Week == max(Week))

avail_2021 <- 
  d2021 %>% 
  filter(Age == "TOT",
         Sex == "b",
         Year == 2021) %>% 
  group_by(PopCode) %>% 
  filter(Week == max(Week)) %>% 
  ungroup() %>% 
  select(PopCode, Week) %>% 
  mutate(PopCode = recode(PopCode,
                         "AUS2" = "AUS",
                         "DEUTNP" = "DEU",
                         "FRATNP" = "FRA",
                         "NZL_NP" = "NZL")) %>% 
  rename(Code = PopCode) %>% 
  mutate(Country = countrycode(sourcevar = Code, 
                               origin = "iso3c", 
                               destination = "country.name"),
         Country = case_when(Code == "GBR_NIR" ~ "Northern Ireland", 
                             Code == "GBR_SCO" ~ "Scotland", 
                             Code == "GBRTENW" ~ "England and Wales",
                             Code == "USA" ~ "USA", 
                             TRUE ~ Country))

write_csv(avail_2021, "Output/data2021/stmf_available_2021.csv")

avail_2021 %>% 
  ggplot()+
  geom_point(aes(Week, Country))+
  scale_x_continuous(limits = c(1, 52), breaks = seq(0, 52, 4))+
  theme_bw()+
  labs(title = "Available weekly deaths in 2021")

ggsave(paste0("Figures/last version/y_2021/deaths_data_availability_2021.png"), 
       dpi = 600,
       width = 6, height = 4)

