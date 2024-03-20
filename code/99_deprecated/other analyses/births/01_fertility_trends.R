# data on births 2020 from HFD and other sources

library(here)
source(here("Code", "00_functions.R"))

# loading data from Human Fertility Database (HFD)
# https://www.humanfertility.org/STFF/stff.csv

db_stff <- 
  read_csv("Data/births/stff.csv")

db_stff2 <- 
  db_stff %>% 
  gather(-CountryCode, - Area, -Year, key = Month, value = Births) %>% 
  mutate(Date = make_date(y = Year, m = match(Month, month.name), d = 15),
         Births = Births %>% as.double()) 

# imputation of unknown month of birth
unks <- 
  db_stff2 %>% 
  filter(Month == "UNK") %>% 
  drop_na(Births) %>% 
  select(CountryCode, Year, unks = Births)
  
db_stff3 <- 
  db_stff2 %>% 
  filter(!Month %in% c("UNK", "TOT")) %>% 
  drop_na(Births) %>% 
  left_join(unks, by = c("CountryCode", "Year")) %>% 
  replace_na(list(unks = 0)) %>% 
  group_by(CountryCode, Year) %>% 
  mutate(prop = Births / sum(Births),
         Births2 = Births + unks * prop) %>% 
  select(CountryCode, Year, Month, Date, Births = Births2)

# countries with full births data on 2020
cts_complete_2020 <- 
  db_stff3 %>% 
  filter(Date == "2020-12-15") %>% 
  pull(CountryCode)

db_stff4 <- 
  db_stff3 %>% 
  filter(Year >= 2010 & Year <= 2020,
         CountryCode %in% cts_complete_2020)

db_annual <- 
  db_stff4 %>% 
  group_by(CountryCode, Year) %>% 
  summarise(Births = sum(Births)) %>% 
  ungroup()

write_rds(db_annual, "Output/annual_births_stff.rds")

db_annual_plot <- 
  db_annual %>% 
  mutate(is_2020 = ifelse(Year == 2020, "y", "n")) %>% 
  mutate(CountryCode = recode(CountryCode,
                              "DEUTNP" = "DEU",
                              "FRATNP" = "FRA"),
         Country = countrycode(CountryCode, 
                               origin = 'iso3c', 
                               destination = 'country.name'),
         Country = ifelse(Country == "United States", "USA", Country),
         Country = ifelse(CountryCode == "GBRTENW", "England_Wales", Country), 
         Country = ifelse(CountryCode == "GBR_SCO", "Scotland", Country)) 

tx <- 5
db_annual_plot %>% 
  ggplot()+
  geom_line(aes(Year, Births), alpha = 0.5, size = 0.4)+
  geom_line(data = db_annual_plot %>% 
              filter(Year %in% c(2019, 2020)), 
                     aes(Year, Births), color = "red", alpha = 0.5, size = 0.4)+
  geom_point(aes(Year, Births, col = is_2020), alpha = 0.3, size = 0.8)+
  scale_x_continuous(breaks = c(2010, 2015, 2020))+
  facet_wrap(~ Country, scales = "free", ncol = 7)+
  scale_color_manual(values = c("black", "red"))+
  theme_bw()+
  theme(legend.position = "none",
        strip.text.x = element_text(margin = margin(b = 0.5, t = 0.5),
                                    size = tx + 1.5),
        axis.text = element_text(size = 3.5),
        axis.title = element_text(size = tx+2),
        strip.background = element_rect(fill = "transparent"))

ggsave("Figures/annual_births_stff.png", width = 9, height = 4, dpi = 800)
