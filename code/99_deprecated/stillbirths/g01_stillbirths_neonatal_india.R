rm (list = ls())
source("Code/00_functions.R")


db <- read_csv("Data/HMIS/HMIS_data_India_byState_2015_2021.csv")

unique(db$Indicator)
unique(db$Region)
unique(db$Year) %>% sort

good_cov <- c("Maharashtra", "Tamil Nadu", "Kerala")
examps <- c("All India", "Maharashtra", "Tamil Nadu", "Kerala", "Delhi", 
            "Madhya Pradesh", "Uttar Pradesh", "West Bengal")

unique(db$Indicator)
# Dadra & Nagar Haveli and Daman & Diu were integrated somewhere in 2020 
# Ladakh only has information for 2020

db2 <- 
  db %>% 
  mutate(Indicator = recode(Indicator,
                            "Infant deaths" = "inf",
                            "Neonatal deaths" = "neo", 
                            "Stillbirths" = "sbs",
                            "Facility deliveries" = "dvs_fct",
                            "Home deliveries" = "dvs_home",
                            "Live birth" = "bts",
                            "Postneonatal deaths" = "pos",
                            "Child deaths age 1 to 4" = "1_4",
                            "Maternal deaths" = "mat",
                            "Under-five deaths" = "0_4",
                            "Deaths 1 month to 5 years" = "1m_4y"),
         date = make_date(d = 15, 
                          month = match(Month, month.abb), 
                          year = Year),
         Region = ifelse(Region %in% c("Dadra & Nagar Haveli",
                                       "Daman & Diu", 
                                       "The Dadra And Nagar Haveli And Daman And Diu"), 
                         "D&NH&D&D",
                         Region)) %>% 
  filter(Region != 	"Ladakh") %>% 
  group_by(Region, date, Indicator) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()

unique(db2$Region)

db3 <- 
  db2 %>%
  filter(Indicator %in% c("bts",
                          "sbs",
                          "neo")) %>% 
  rename(state = Region,
         measure = Indicator) %>% 
  mutate(year = year(date)) %>% 
  filter(year <= 2020) %>% 
  group_by(state, year, measure) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()

bts <- 
  db3 %>% 
  filter(measure == "bts") %>% 
  rename(bts = value) %>% 
  select(-measure)

db4 <- 
  db3 %>% 
  filter(measure != "bts") %>% 
  left_join(bts)

write_rds(db4, "data_inter/neo_sbs_india_state.rds")
