# rm (list = ls())
source("Code/00_functions.R")

db_ccd <- 
  read_xlsx("Data/unicef/220601_Covid analysis data.xlsx",
            sheet = 1)

# loading pre-processed births
bts <- 
  read_rds("data_inter/annual_births.rds") %>% 
  rename(code = Code,
         year = Year,
         country = Country,
         bts = Births,
         source = Source)

unique(db_ccd$whoname) %>% sort


# several countries with repeated observations, keeping the last date of update
db_ccd2 <- 
  db_ccd %>% 
  select(country = whoname, sex,
         year, month, quarter,
         date,
         neo = ndth, 
         sbs = sb_x28wks,
         note) %>% 
  mutate(date = ymd(date)) %>% 
  # only one observation (keeping last update)
  group_by(country, sex, year, month) %>%
  filter(date == max(date)) %>% 
  ungroup() %>% 
  select(-date)

# for 2015-2020, all months seem to included
# for 2021, some sources have a few months


# 2015-2020: all to annual
ccd_1520 <- 
  db_ccd2 %>% 
  filter(year <= 2020) %>% 
  select(-note) %>% 
  replace_na(list(neo = 0, sbs = 0)) %>% 
  group_by(country, sex, year) %>% 
  summarise(neo = sum(neo),
            sbs = sum(sbs)) %>% 
  ungroup()

unique(ccd_1520$country) %>% sort()

# 2021
# checking if data comes annual, quarterly, or monthly 
# sources with annual data
ccd_21 <- 
  db_ccd2 %>% 
  filter(year == 2021) %>% 
  group_by(country, sex) %>%
  mutate(mths = n()) %>% 
  ungroup() %>% 
  replace_na(list(neo = 0, sbs = 0))

unique(ccd_21$note)

# 2021: from annual 
ccd_21_ann <- 
  ccd_21 %>% 
  filter(str_detect(note, "total|yearly")) %>% 
  select(-note, -month, -quarter, -note, -mths) %>%
  group_by(country, sex, year) %>% 
  summarise(neo = sum(neo),
            sbs = sum(sbs)) %>% 
  ungroup()

# 2021: from monthly 
ccd_21_mth <- 
  ccd_21 %>% 
  filter(!str_detect(note, "total|yearly|quarterly") | is.na(note)) %>% 
  filter(mths == 12) %>% 
  select(-note, -month, -quarter, -note, -mths) %>%
  group_by(country, sex, year) %>% 
  summarise(neo = sum(neo),
            sbs = sum(sbs)) %>% 
  ungroup()

db_ccd3 <- 
  bind_rows(ccd_1520,
            ccd_21_ann,
            ccd_21_mth) %>% 
  arrange(country, sex, year) %>% 
  mutate(country = case_when(country == "Russian Federation" ~ "Russia", 
                             country == "Republic of Korea" ~ "South Korea", 
                             TRUE ~ country),
         sex = recode(sex,
                      "b" = "t")) %>% 
  filter(sex == "t") %>% 
  select(-sex) %>% 
  filter(!country %in% c("Andorra")) %>% 
  gather(neo, sbs, key = measure, value = value) %>% 
  filter(value != 0)


# countries with at least 3 periods before 2020
cts_yrs <- 
  db_ccd3 %>% 
  filter(year < 2020) %>% 
  group_by(country, measure) %>% 
  filter(n() >= 3) %>% 
  select(country, measure) %>% 
  unique() %>% 
  mutate(available = 1)

# merging with births data
# ~~~~~~~~~~~~~~~~~~~~~~~~
annual <- 
  db_ccd3 %>%  
  left_join(cts_yrs) %>% 
  filter(available == 1) %>% 
  select(-available) %>% 
  group_by(country, measure) %>% 
  filter(max(year) >= 2020) %>% 
  ungroup() %>% 
  left_join(bts) %>% 
  arrange(country, measure, year)

write_rds(annual, "data_inter/neo_sbs_unicef.rds")

