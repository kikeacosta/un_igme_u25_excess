library(here)
source(here("Code", "00_functions.R"))
library(HMDHFDplus)
library(countrycode)

# from STFF database
# ~~~~~~~~~~~~~~~~~~

# loading data from Human Fertility Database (HFD)
db_stff <- read_csv("https://www.humanfertility.org/STFF/stff.csv")

# db_stff <- 
#   read_csv("Data/births/stff.csv")

db_stff2 <- 
  db_stff %>% 
  select(CountryCode, Year, TOT) %>% 
  mutate(Births = TOT %>% as.double()) %>% 
  drop_na() %>% 
  select(-TOT)

# countries with full births data on 2021
cts_complete_2021 <- 
  db_stff2 %>% 
  filter(Year == 2021) %>% 
  pull(CountryCode)

avail_2021 <- 
  db_stff %>% 
  select(1, 3:15) %>% 
  gather(3:14, key = month, value = bts) %>% 
  mutate(bts = bts %>% as.double()) %>% 
  drop_na() %>% 
  group_by(CountryCode, Year) %>% 
  summarise(Months = n()) %>% 
  filter(Year == 2021) %>% 
  mutate(CountryCode = recode(CountryCode,
                              "DEUTNP" = "DEU",
                              "FRATNP" = "FRA"),
         Country = countrycode(CountryCode, 
                               origin = 'iso3c', 
                               destination = 'country.name'),
         Country = ifelse(Country == "United States", "USA", Country),
         Country = case_when(CountryCode == "GBR_SCO" ~ "Scotland",
                             CountryCode == "GBRTENW" ~ "England and Wales",
                             TRUE ~ Country)) %>% 
  drop_na() %>% 
  rename(Code = CountryCode)

write_csv(avail_2021, "Output/data2021/stff_available_2021.csv")

avail_2021 %>% 
  ggplot()+
  geom_point(aes(Months, Country))+
  scale_x_continuous(limits = c(1, 12), breaks = 1:12)+
  theme_bw()+
  labs(title = "Available monthly births in 2021")

# ggsave(paste0("Figures/last version/y_2021/births_data_availability_2021.png"), 
#        dpi = 600,
#        width = 6, height = 4)
