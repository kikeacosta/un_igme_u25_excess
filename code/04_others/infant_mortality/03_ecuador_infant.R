library(here)
source(here("Code", "00_functions.R"))

# Ecuador mortality data
# ~~~~~~~~~~~~~~~~~~~~~
# # files from Ecuador (Version 1) as of 15 June 2021 

# for single-year ages without sex, according to the Indice, file 1.1.2D:
# "Número de defunciones en el año (t+1) por edades simples (años) a nivel 
# nacional. Periodo 1990 - 2020"

# loading deaths in hours
var_names_hours <- c("Year", "TOT", paste("t_", 0:23), "noth", "noth2") 
db_ec_hours <- 
  read_csv(here("Data", "Ecuador", "1.1.2A.csv"),
           skip = 4,
           col_names = var_names_hours,
           col_types = cols(.default = "c"))

# loading deaths in days
var_names_days <- c("Year", "TOT", paste("t_", 1:30), "noth", "noth2") 
db_ec_days <- 
  read_csv(here("Data", "Ecuador", "1.1.2B.csv"),
           skip = 4,
           col_names = var_names_days,
           col_types = cols(.default = "c"))

# loading deaths in months
var_names_months <- c("Year", "TOT", paste("t_", 1:11), "noth", "noth2") 
db_ec_months <- 
  read_csv(here("Data", "Ecuador", "1.1.2C.csv"),
           skip = 4,
           col_names = var_names_months,
           col_types = cols(.default = "c"))

db_ec_hours2 <- 
  db_ec_hours %>% 
  drop_na(TOT) %>% 
  select(-noth, -noth2, -TOT) %>% 
  gather(-Year, key = Age, value = Deaths) %>% 
  mutate(Deaths = str_replace(Deaths, ",", ""),
         Deaths = ifelse(Deaths == "-" | is.na(Deaths), "0", Deaths),
         Deaths = Deaths %>% as.integer(),
         Age = str_replace(Age, "t_", "") %>% as.integer(),
         Year = ifelse(str_detect(Year, "2020") , "2020", Year) %>% as.integer(),
         Age2 = "neo_ear") %>% 
  group_by(Year, Age2) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()

db_ec_days2 <- 
  db_ec_days %>% 
  drop_na(TOT) %>% 
  select(-noth, -noth2, -TOT) %>% 
  gather(-Year, key = Age, value = Deaths) %>% 
  mutate(Deaths = str_replace(Deaths, ",", ""),
         Deaths = ifelse(Deaths == "-" | is.na(Deaths), "0", Deaths),
         Deaths = Deaths %>% as.integer(),
         Age = str_replace(Age, "t_", "") %>% as.integer(),
         Year = ifelse(str_detect(Year, "2020") , "2020", Year) %>% as.integer(),
         Age2 = case_when(Age <= 7 ~ "neo_ear",
                          TRUE ~ "neo_lat")) %>% 
  group_by(Year, Age2) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()

db_ec_months2 <- 
  db_ec_months %>% 
  drop_na(TOT) %>% 
  select(-noth, -noth2, -TOT) %>% 
  gather(-Year, key = Age, value = Deaths) %>% 
  mutate(Deaths = str_replace(Deaths, ",", ""),
         Deaths = ifelse(Deaths == "-" | is.na(Deaths), "0", Deaths),
         Deaths = Deaths %>% as.integer(),
         Age = str_replace(Age, "t_", "") %>% as.integer(),
         Year = ifelse(str_detect(Year, "2020") , "2020", Year) %>% as.integer(),
         Age2 = "post_neo") %>% 
  group_by(Year, Age2) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()

db_ec_infant <- 
  bind_rows(db_ec_hours2,
            db_ec_days2,
            db_ec_months2) %>% 
  group_by(Year, Age2) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  rename(Age = Age2)%>% 
  mutate(Age = factor(Age, levels = c("neo_ear", 
                                      "neo_lat", 
                                      "post_neo")))


db_ec_infant %>% 
  filter(Year >= 2015) %>% 
  ggplot()+
  geom_line(aes(Year, Deaths))+
  geom_point(aes(Year, Deaths))+
  facet_wrap(~Age, scales = "free")+
  theme_bw()
# ggsave("Figures/infant/infant_mortality_ecuador_year.png", dpi = 1000)


db_ec_infant %>% 
  filter(Year >= 2010) %>% 
  ggplot()+
  geom_area(aes(Year, Deaths, fill = Age))+
  scale_x_continuous(breaks = seq(2010, 2020, 2))+
  theme_bw()+
  labs(title = "Ecuador: infant mortality over time")
# ggsave("Figures/infant/infant_mortality_by_year_ecuador.png")

db_ec_infant %>% 
  group_by(Year) %>% 
  summarise(Deaths = sum(Deaths))


# mortality change
db_0_year_ch <- 
  db_ec_infant %>% 
  filter(Year >= 2010) %>% 
  arrange(Age, Year) %>% 
  group_by(Age) %>% 
  mutate(ch = Deaths / lag(Deaths) - 1)

db_0_year_ch %>% 
  ggplot()+
  geom_line(aes(Year, ch, col = Age))+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = seq(2010, 2020, 2))+
  scale_y_continuous(labels = percent_format(accuracy = 1))+
  theme_bw()+
  labs(title = "Ecuador: infant mortality change")
# ggsave("Figures/infant/infant_mortality_change_ecuador.png")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Births and infant mortality by province
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

names_grid <- 
  expand_grid(Measure = c("Deaths", "Births", "Rates"), Year = 2008:2020) %>% 
  mutate(names = paste(Measure, Year, sep = "_"))

var_names <- c("Region", names_grid$names, "noth")

db_ec_births <- 
  read_csv(here("Data", "Ecuador", "1.2.3.csv"),
           skip = 4,
           col_names = var_names,
           col_types = cols(.default = "c"))

db_ec_births2 <- 
  db_ec_births %>% 
  filter(Region == "Total Nacional") %>% 
  select(-noth) %>% 
  gather(-Region, key = temp_names, value = Value) %>% 
  mutate(Value = str_replace(Value, ",", ""),
         Value = ifelse(Value == "-" | is.na(Value), "0", Value),
         Value = Value %>% as.integer()) %>% 
  separate(temp_names, c("Measure", "Year"), sep = "_") %>% 
  filter(Measure == "Births") %>% 
  select(Year, bts = Value) %>% 
  mutate(Year = Year %>% as.double())


# death rates
# ===========

rates <- 
  db_ec_infant %>% 
  filter(Year >= 2008) %>% 
  left_join(db_ec_births2) %>% 
  mutate(mx = Deaths / bts)

db_inf <- 
  rates %>% 
  rename(year = Year,
         age = Age, 
         deaths = Deaths,
         births = bts)

write_rds(db_inf, "Output/infant/ecuador_annual_inf_deaths_births_2008_2020.rds")


rates %>% 
  filter(Year >= 2015) %>% 
  ggplot()+
  geom_line(aes(Year, mx))+
  geom_point(aes(Year, mx))+
  facet_wrap(~Age, scales = "free")+
  theme_bw()
ggsave("Figures/infant/infant_mortality_ecuador_year.png", dpi = 1000)



# 
# db_ec_births2 <- 
#   db_ec_births %>% 
#   drop_na(Deaths_2008) %>% 
#   select(-noth) %>% 
#   gather(-Region, key = temp_names, value = Value) %>% 
#   mutate(Value = str_replace(Value, ",", ""),
#          Value = ifelse(Value == "-" | is.na(Value), "0", Value),
#          Value = Value %>% as.integer()) %>% 
#   separate(temp_names, c("Measure", "Year"), sep = "_")
#   group_by(Year, Age2) %>% 
#   summarise(Deaths = sum(Deaths)) %>% 
#   ungroup()
# 
# # Total births at the national level 2008-2020
# db_births_nal <- 
#   db_ec_births2 %>% 
#   filter(Region == "Total Nacional",
#          Measure == "Births")
# 
# # Infant mortality rates at the national level 2008-2020
# db_infant_mort_nal <- 
#   db_ec_births2 %>% 
#   filter(Region == "Total Nacional",
#          Measure == "Rates")
# 
# 










