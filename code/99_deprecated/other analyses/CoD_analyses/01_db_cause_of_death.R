source("Code/00_functions.r")
# Cause of deaths analysis
db_in <- 
  read_csv("Data/unicef/selected_cod_2015_2020.csv")

unique(db_in$Country)
unique(db_in$Admin1)
unique(db_in$SubDiv)
unique(db_in$Year)
unique(db_in$List)
unique(db_in$Cause)
unique(db_in$Sex)
unique(db_in$Frmat)
unique(db_in$IM_Frmat)

db2 <- 
  db_in %>% 
  mutate(sex = case_when(Sex == 1 ~ "m",
                         Sex == 2 ~ "f",
                         TRUE ~ NA_character_),
         country = case_when(
           Country == "4010" ~ "Austria",
           Country == "4055" ~ "Estonia",
           Country == "4210" ~ "Netherlands",
           Country == "4186" ~ "Latvia",
           Country == "2140" ~ "Costa Rica")) %>% 
  select(-Admin1, - SubDiv, -Sex, - Country) %>% 
  gather(starts_with("Deaths") | starts_with("IM_Deaths"), 
         key = age_gr, value = dts) %>% 
  drop_na() %>% 
  rename(year = Year)

# db2 <- 
#   db_in %>% 
#   mutate(sex = case_when(Sex == 1 ~ "m",
#                          Sex == 2 ~ "f",
#                          TRUE ~ NA_character_)) %>% 
#   select(-Admin1, - SubDiv, -Sex) %>% 
#   select(starts_with("Deaths") | starts_with("IM_Deaths"))

unique(db2$Frmat)
unique(db2$age_gr)

frmts <- 
  db2 %>% 
  drop_na(dts) %>% 
  # filter(Frmat == "02") %>% 
  select(country, Frmat, year) %>% 
  unique()

f0 <- 
  db2 %>% 
  drop_na(dts) %>% 
  filter(Frmat == "00") %>% 
  select(country, age_gr, year) %>% 
  unique()

f2 <- 
  db2 %>% 
  drop_na(dts) %>% 
  filter(Frmat == "02") %>% 
  select(country, age_gr, year) %>% 
  unique()

cr <- 
  db2 %>% 
  filter(country == "Costa Rica",
         year == 2020)

# Costa Rica has age 1-4 in 2020
# We can apply the same age aggregation to all countries

db3 <- 
  db2 %>% 
  mutate(age = case_when(
    Frmat == '00' & age_gr == 'Deaths1' ~ 'tot',
    Frmat == '00' & age_gr == 'Deaths2' ~ '0',
    Frmat == '00' & age_gr == 'Deaths3' ~ '1',
    Frmat == '00' & age_gr == 'Deaths4' ~ '1',
    Frmat == '00' & age_gr == 'Deaths5' ~ '1',
    Frmat == '00' & age_gr == 'Deaths6' ~ '1',
    Frmat == '00' & age_gr == 'Deaths7' ~ '5',
    Frmat == '00' & age_gr == 'Deaths8' ~ '10',
    Frmat == '00' & age_gr == 'Deaths9' ~ '15',
    Frmat == '00' & age_gr == 'Deaths10' ~ '20',
    Frmat == '00' & age_gr == 'Deaths11' ~ '25',
    Frmat == '00' & age_gr == 'Deaths12' ~ '30',
    Frmat == '00' & age_gr == 'Deaths13' ~ '35',
    Frmat == '00' & age_gr == 'Deaths14' ~ '40',
    Frmat == '00' & age_gr == 'Deaths15' ~ '45',
    Frmat == '00' & age_gr == 'Deaths16' ~ '50',
    Frmat == '00' & age_gr == 'Deaths17' ~ '55',
    Frmat == '00' & age_gr == 'Deaths18' ~ '60',
    Frmat == '00' & age_gr == 'Deaths19' ~ '65',
    Frmat == '00' & age_gr == 'Deaths20' ~ '70',
    Frmat == '00' & age_gr == 'Deaths21' ~ '75',
    Frmat == '00' & age_gr == 'Deaths22' ~ '80',
    Frmat == '00' & age_gr == 'Deaths23' ~ '85',
    Frmat == '00' & age_gr == 'Deaths24' ~ '90',
    Frmat == '00' & age_gr == 'Deaths25' ~ '95',
    Frmat == '00' & age_gr == 'Deaths26' ~ 'unk',
    
    Frmat == '00' & age_gr == 'IM_Deaths1' ~ '0d',
    Frmat == '00' & age_gr == 'IM_Deaths2' ~ '1-6d',
    Frmat == '00' & age_gr == 'IM_Deaths3' ~ '7-27d',
    Frmat == '00' & age_gr == 'IM_Deaths4' ~ '1-11m',
    
    Frmat == '02' & age_gr == 'Deaths1' ~ 'tot',
    Frmat == '02' & age_gr == 'Deaths2' ~ '0',
    Frmat == '02' & age_gr == 'Deaths3' ~ '1',
    # Frmat == '02' & age_gr == 'Deaths4' ~ '',
    # Frmat == '02' & age_gr == 'Deaths5' ~ '',
    # Frmat == '02' & age_gr == 'Deaths6' ~ '',
    Frmat == '02' & age_gr == 'Deaths7' ~ '5',
    Frmat == '02' & age_gr == 'Deaths8' ~ '10',
    Frmat == '02' & age_gr == 'Deaths9' ~ '15',
    Frmat == '02' & age_gr == 'Deaths10' ~ '20',
    Frmat == '02' & age_gr == 'Deaths11' ~ '25',
    Frmat == '02' & age_gr == 'Deaths12' ~ '30',
    Frmat == '02' & age_gr == 'Deaths13' ~ '35',
    Frmat == '02' & age_gr == 'Deaths14' ~ '40',
    Frmat == '02' & age_gr == 'Deaths15' ~ '45',
    Frmat == '02' & age_gr == 'Deaths16' ~ '50',
    Frmat == '02' & age_gr == 'Deaths17' ~ '55',
    Frmat == '02' & age_gr == 'Deaths18' ~ '60',
    Frmat == '02' & age_gr == 'Deaths19' ~ '65',
    Frmat == '02' & age_gr == 'Deaths20' ~ '70',
    Frmat == '02' & age_gr == 'Deaths21' ~ '75',
    Frmat == '02' & age_gr == 'Deaths22' ~ '80',
    Frmat == '02' & age_gr == 'Deaths23' ~ '85',
    Frmat == '02' & age_gr == 'Deaths26' ~ 'unk',
    # Frmat == '02' & age_gr == 'Deaths24' ~ '',
    # Frmat == '02' & age_gr == 'Deaths25' ~ '',
    
    Frmat == '02' & age_gr == 'IM_Deaths1' ~ '0_v2'
    # Frmat == '02' & age_gr == 'IM_Deaths2' ~ '',
    # Frmat == '02' & age_gr == 'IM_Deaths3' ~ '',
    # Frmat == '02' & age_gr == 'IM_Deaths4' ~ '',
  )) %>% 
  group_by(year, country, List, Cause, sex, age) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup()
# ===== 



test_age0 <- 
  db3 %>% 
  filter(age %in% c("0", "0_v2")) %>% 
  spread(age, dts) %>% 
  drop_na(`0_v2`)

# proportion of unknown age
db3 %>% 
  filter(age == "unk",
         dts > 0) %>% 
  summarise(sum(dts)) /
  db3 %>% 
  summarise(sum(dts))

# Cause of death ====
# ~~~~~~~~~~~~~~~~~~~
db4 <- 
  db3 %>%
  filter(age != "0_v2",
         age != "unk") %>% 
  mutate(cause = case_when(
    Cause == '0000' ~ 'ALL CAUSES',
    Cause == '0010' ~ 'Communicable, maternal, perinatal and nutritional conditions',
    Cause == '0020' ~ 'HIV/AIDS',
    Cause == '0030' ~ 'Diarrhoeal diseases',
    Cause == '0040' ~ 'Pertussis',
    Cause == '0050' ~ 'Tetanus',
    Cause == '0060' ~ 'Measles',
    Cause == '0070' ~ 'Meningitis/encephalitis',
    Cause == '0071' ~ 'Malaria',
    Cause == '0080' ~ 'Acute lower respiratory infections',
    Cause == '0090' ~ 'Prematurity',
    Cause == '0091' ~ 'All perinatal causes',
    Cause == '0100' ~ 'Birth asphyxia and birth trauma',
    Cause == '0110' ~ 'Sepsis and other infectious conditions of the newborn',
    Cause == '0120' ~ 'Other Group 1',
    Cause == '0130' ~ 'Noncommunicable diseases',
    Cause == '0140' ~ 'Congenital anomalies',
    Cause == '0141' ~ 'Chronic obstructive pulmonary disease',
    Cause == '0150' ~ 'Other noncommunicable diseases',
    Cause == '0160' ~ 'Injuries',
    Cause == '0900' ~ 'Ill-defined causes'
  )) %>% 
  rename(cause_cod = Cause)


unique(db4$cause)
unique(db4$sex)
unique(db4$age)


db4 %>% 
  filter(age == "tot",
         cause == "ALL CAUSES") %>% 
  ggplot()+
  geom_line(aes(year, dts))+
  facet_grid(country ~ sex, scales = "free")
    
# ====
write_rds(db4, "output/cod/dts_cause_country_sex_age.rds")
# ====


# Comparison with all cause mortality
by_cause <- read_rds("output/cod/dts_cause_country_sex_age.rds")
all_cause <- read_rds("Output/annual_young_deaths_all_countries.rds")

excl <- c("0d", "1-11m", "1-6d", "7-27d", "tot")

by_cause2 <- 
  by_cause %>% 
  filter(cause == "ALL CAUSES",
         !age %in% excl) %>% 
  mutate(age = age %>% as.double()) %>% 
  select(-List, -cause_cod, -cause) %>% 
  filter(age < 25) %>% 
  rename(dts_css = dts)

by_cause3 <- 
  by_cause2 %>% 
  bind_rows(by_cause2 %>% 
              group_by(year, country, age) %>% 
              summarise(dts_css = sum(dts_css)) %>% 
              ungroup() %>% 
              mutate(sex = "t")) %>% 
  mutate(age = ifelse(age == 1 & !country %in% c("Costa Rica", "Estonia"),
                      0, age)) %>% 
  group_by(year, country, sex, age) %>% 
  summarise(dts_css = sum(dts_css)) %>% 
  ungroup() %>% 
  arrange(year, country, sex, age)

unique(by_cause2$age)

cts_cause <- 
  by_cause2 %>% 
  pull(country) %>% 
  unique()

all_cause2 <- 
  all_cause %>% 
  filter(Country %in% cts_cause) %>% 
  select(country = Country,
         year = Year,
         sex = Sex,
         age = Age,
         dts_tot = Deaths) %>% 
  filter(age < 25)

unique(all_cause2$country)
ages <- 
  all_cause2 %>% 
  select(country, age) %>% 
  unique()

comp <- 
  by_cause3 %>% 
  left_join(all_cause2)


comp %>% 
  filter(sex == "t") %>% 
  gather(dts_css, dts_tot, key = source, value = dts) %>% 
  ggplot()+
  geom_point(aes(age, dts, col = source), alpha = 0.5)+
  facet_grid(year~country)+
  theme_bw()
  





