rm (list = ls())
source("code/00_functions.R")

# births
# ~~~~~~
bts <- 
  read_rds("data_inter/annual_births.rds") %>% 
  rename(code = Code,
         year = Year,
         # country = Country,
         bts = Births,
         source = Source) %>% 
  filter(code == "USA") %>% 
  mutate(country = "USA") %>% 
  select(country, year, bts)


bts_st <- 
  read_tsv("data_input/USA/births/wonder_births_state.txt") %>% 
  drop_na(Year) %>% 
  select(state = 2,
         year = 4,
         bts = 6)


# infant deaths
# ~~~~~~~~~~~~~
inf <- 
  read_tsv("data_input/USA/wonder_infant.txt") %>% 
  drop_na(Year) %>% 
  select(year = 2,
         measure = 5,
         value = 6) %>% 
  mutate(measure = case_when(measure %in% c("1d", "1-6d", "7-27d") ~ "neo",
                             measure %in% c("28-364d") ~ "pos")) %>% 
  group_by(year, measure) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()

inf2 <- 
  inf %>% 
  group_by(year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  mutate(measure = "inf") %>% 
  bind_rows(inf)


# stillbirths
# ~~~~~~~~~~~
sbs <- 
  read_tsv("data_input/USA/fetal_deaths/fetal_deaths_us_weeks_2015-2019.txt") %>% 
  drop_na(Year) %>% 
  select(year = 2,
         weeks = 4,
         value = 6) %>% 
  mutate(value = value %>% as.double(),
         value = ifelse(is.na(value), 0, value))

sbs_tot <- 
  sbs %>% 
  group_by(year) %>% 
  summarise(sbs_tot = sum(value)) %>% 
  ungroup()

sbs2 <- 
  sbs %>% 
  filter(!weeks %in% c("Unknown", "Not Available")) %>% 
  replace_na(list(value = 0)) %>% 
  group_by(year) %>% 
  mutate(fr = value / sum(value)) %>% 
  left_join(sbs_tot) %>% 
  mutate(value2 = fr * sbs_tot) %>% 
  filter(weeks %in% c("28 weeks or more")) %>% 
  select(year, value = value2)

sbs_20 <- 
  read_fwf("data_input/USA/fetal_deaths/Fetal2020US_COD.txt" , 
           fwf_positions(c(331), c(332), 
                         c("weeks")),
           col_types = cols(.default = "d")) %>% 
  mutate(weeks = case_when(weeks %in% 1:27 ~ 1,
                          weeks %in% 28:47 ~ 28,
                          TRUE ~ 99)) %>% 
  group_by(weeks) %>% 
  summarise(value = n()) %>% 
  ungroup()
  
tot_20 <- 
  sbs_20 %>% 
  group_by() %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  pull(value)

sbs_20_2 <- 
  sbs_20 %>% 
  filter(weeks != 99) %>% 
  mutate(fr = value / sum(value)) %>% 
  mutate(value2 = round(fr * tot_20),
         year = 2020) %>% 
  filter(weeks == 28) %>% 
  select(year, value = value2)

sbs3 <- 
  bind_rows(sbs2,
            sbs_20_2) %>% 
  mutate(measure = "sbs")

# putting all together
all <-
  bind_rows(inf2,
            sbs3) %>% 
  left_join(bts) %>% 
  mutate(code = "USA",
         source = "cdc")

# saving
write_rds(all, "data_inter/neo_sbs_usa.rds")

  
# data by state (>= 20 weeks)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sbs_st <- 
  read_tsv("data_input/USA/fetal_deaths/fetal_deaths_state_weeks_2015-2019.txt") %>% 
  drop_na(Year) %>% 
  select(year = 6,
         state = 2,
         weeks = 4,
         value = 8) %>% 
  mutate(value = value %>% as.double(),
         value = ifelse(is.na(value), 0, value))

sbs_st_tot <- 
  sbs_st %>% 
  group_by(year, state) %>% 
  summarise(sbs_tot = sum(value)) %>% 
  ungroup()

sbs_st2 <- 
  sbs_st %>% 
  filter(!weeks %in% c("Unknown", "Not Available")) %>% 
  replace_na(list(value = 0)) %>% 
  group_by(year, state) %>% 
  mutate(fr = value / sum(value)) %>% 
  left_join(sbs_st_tot) %>% 
  mutate(value2 = fr * sbs_tot) %>% 
  summarise(value = sum(value2)) %>% 
  ungroup()

sbs_st_20 <- 
  read_csv("data_input/USA/fetal_deaths/us_fetal_deaths_state_2018_2020.csv") %>% 
  select(state = State, value = `2020`) %>% 
  mutate(year = 2020)
  
sbs_st2$state %>% unique
sbs_st_20$state %>% unique
bts_st$state %>% unique


sbs_st3 <- 
  bind_rows(sbs_st2,
            sbs_st_20) %>% 
  mutate(measure = "sbs")

# neonatal
neo_st <- 
  read_tsv("data_input/USA/wonder_neonatal_state.txt") %>% 
  drop_na(Year) %>% 
  select(year = 4,
         state = 2,
         value = 6) %>% 
  mutate(value = value %>% as.double(),
         value = ifelse(is.na(value), 0, value),
         measure = "neo")

all_st <- 
  bind_rows(sbs_st3,
            neo_st) %>% 
  left_join(bts_st, by = c("year", "state"))

all_st2 <- 
  all_st %>% 
  bind_rows(all_st %>% 
              group_by(year, measure) %>% 
              summarise(value = sum(value),
                        bts = sum(bts)) %>% 
              ungroup() %>% 
              mutate(state = "All") %>% 
              ungroup())

write_rds(all_st2, "data_inter/neo_sbs_usa_state.rds")
