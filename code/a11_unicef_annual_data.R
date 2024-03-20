rm (list = ls())
source("code/00_functions.R")

file_name <- "data_input/unicef/221121_Admin_SBR_excess.xlsx"

db_in <- 
  read_xlsx(file_name)

db_in2 <- 
  db_in %>% 
  select(code = 1,
         country = 2,
         year = 10,
         bts = 11,
         bts_sbs = 12,
         sbs = 13,
         neo = 16) %>% 
  mutate(bts = ifelse(is.na(bts), bts_sbs - sbs, bts)) %>% 
  gather(sbs, neo, key = measure, value = value) %>% 
  drop_na() %>% 
  mutate(country = case_when(country == "Korea, South" ~ "South Korea",
                             country == "Gambia, The" ~ "Gambia",
                             country == "United States" ~ "USA",
                             TRUE ~ country)) %>% 
  unique()

unique(db_in2$country)

# countries with more than one source and different data
cts_double_data <- 
  db_in2 %>% 
  unique() %>% 
  group_by(country, year, measure) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  filter(n > 1) %>% 
  select(country, measure) %>% 
  unique()

# excluding countries with repeated data issues (more than one source)
db_in3 <- 
  db_in2 %>% 
  anti_join(cts_double_data)

# countries with data for 2020 or 2021 and three years minimum
cts_ok <- 
  db_in3 %>%
  group_by(country, measure) %>% 
  filter(max(year) >= 2020) %>% 
  filter(year < 2020) %>% 
  filter(n() >= 3) %>% 
  ungroup() %>% 
  select(country, measure) %>% 
  unique()
  
# filtering countries and measures with enough data
db_in4 <- 
  db_in3 %>% 
  inner_join(cts_ok)

# excluded
db_in3 %>% 
  anti_join(cts_ok)

# countries with HMIS data
hmis_cts <- c("Timor-Leste", "Gambia", "Madagascar", 
              "Sri Lanka", "Ethiopia", "Namibia")

db_ann22 <- 
  db_in4 %>% 
  filter(!country %in% hmis_cts) %>% 
  select(-bts_sbs) %>% 
  mutate(source = "unicef_ann")


# data on births
bts <- 
  db_in3 %>% 
  filter(!country %in% hmis_cts) %>% 
  select(Country = country, Year = year, Births = bts) %>% 
  mutate(Source = "unicef_ann") %>% 
  unique()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# files received at the end of 2023 ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# rm(list = ls())
# source("Code/00_functions.R")

ccd_name1 <- "data_input/unicef/dth0yrDBwide_20231207.csv"
ccd_name2 <- "data_input/unicef/MasterMDB_20231207.csv"
ccd_name3 <- "data_input/unicef/SBR_data_cc 2023.xlsx"

d1 <- read_csv(ccd_name1)
d2 <- read_csv(ccd_name2)
d3 <- read_xlsx(ccd_name3)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# infant and neonatal deaths
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

unique(d1$d999)
# d28 refers to postneonatal deaths
# Y0 refers to all infant deaths
# d0+d1+d7 refers to neonatal deaths

neo1 <- 
  d1 %>% 
  mutate(d999 = ifelse(is.na(d999), 0, d999),
         t1 = d0+d1+d7+d28+d999,
         diff = y0-t1)
neo1 %>% 
  filter(diff != 0)

# issues so far 
# ~~~~~~~~~~~~~
# wrong total infants for Peru (re-estimating them here)
# only totals for Peru and the Netherlands in 2021 (excluded)

neo2 <- 
  neo1 %>% 
  filter(year >= 2010) %>% 
  group_by(iso3) %>% 
  filter(max(year) >= 2020) %>% 
  replace_na(list(d0 = 0, d1 = 0, d7 = 0, d28 = 0)) %>% 
  mutate(y0 = ifelse(iso3 == "PER", d0+d1+d7+d28+d999, y0),
         neo = d0+d1+d7,
         neo2 = ifelse(y0>0, neo*y0/(d0+d1+d7+d28), 0),
         diff_neo = neo-neo2)

neo3 <- 
  neo2 %>%
  select(code = iso3, country = whoname, year,
         inf = y0, neo = neo2, Births = lb) %>% 
  filter(!(country == "Italy" & year == 2021),
         !(country == "Netherlands" & year == 2021))

neo <- 
  neo3 %>% 
  select(code, year, bts = Births, value = neo) %>% 
  mutate(measure = "neo") 

unique(neo$code)

# stillbirths
# ~~~~~~~~~~~
sbs <- 
  d3 %>% 
  select(code = iso3, 
         year = 5, 
         neo,
         bts = lb,
         sbs = sb) %>% 
  gather(neo, sbs, key = measure, value = value) %>% 
  drop_na()

unique(sbs$code)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# merging data before and after 2023 ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# births
# ~~~~~~
dbann_bts <- 
  db_ann22 %>% 
  select(code, year, bts1 = bts) %>% 
  unique() %>% 
  full_join(neo %>% select(code, year, bts2 = bts)) %>% 
  full_join(sbs %>% 
              select(code, year, bts3 = bts)) %>%
  gather(bts1, bts2, bts3, key = source, value = bts) %>% 
  replace_na(list(bts = 0)) %>% 
  group_by(code, year) %>% 
  filter(max(bts) == bts) %>% 
  ungroup() %>% 
  arrange(code, year) %>% 
  select(-source) %>% 
  unique() %>% 
  mutate(source = "unicef_ann")

write_rds(dbann_bts, "data_inter/annual_births_ann.rds")


# neonatal and stillbirths
# ~~~~~~~~~~~~~~~~~~~~~~~~
dbann_neo <- 
  db_ann22 %>% 
  select(code, year, measure, value1 = value) %>% 
  filter(measure == "neo") %>% 
  unique() %>% 
  full_join(neo %>% select(code, year, measure, value2 = value)) %>% 
  full_join(sbs %>% 
              filter(measure == "neo") %>% 
              select(code, year, value3 = value)) %>% 
  gather(value1, value2, value3, key = source, value = value) %>% 
  replace_na(list(value = 0)) %>% 
  group_by(code, year) %>% 
  filter(max(value) == value) %>% 
  ungroup() %>% 
  arrange(code, year) %>% 
  select(-source) %>% 
  unique()
  
dbann_sbs <- 
  db_ann22 %>% 
  select(code, year, measure, value1 = value) %>% 
  filter(measure == "sbs") %>% 
  unique() %>% 
  full_join(sbs %>% 
              filter(measure == "sbs") %>% 
              select(code, year, measure, value2 = value)) %>% 
  gather(value1, value2, key = source, value = value) %>% 
  replace_na(list(value = 0)) %>% 
  group_by(code, year) %>% 
  filter(max(value) == value) %>% 
  ungroup() %>% 
  arrange(code, year) %>% 
  select(-source) %>% 
  unique()
  
db_out <- 
  bind_rows(dbann_neo, dbann_sbs) %>% 
  mutate(source = "unicef_ann")
  
write_rds(db_out, "data_inter/neo_sbs_annual_unicef.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~
# youth deaths ====
# ~~~~~~~~~~~~~~~~~

unique(d2$comments)
unique(d2$var)

pop <- 
  d2 %>% 
  filter(var == "pop") %>% 
  gather(starts_with("x"), key = age, value = pop) %>% 
  select(iso3, year, sex, age, pop)

unique(d2$sex)

dt <- 
  d2 %>% 
  filter(var == "dth",
         sex != "b",
         year >= 2010) %>% 
  group_by(iso3, sex) %>% 
  filter(max(year) >= 2020) %>% 
  gather(starts_with("x"), key = age, value = dts) %>% 
  replace_na(list(dts = 0)) %>% 
  select(iso3, year, sex, age, dts, total) %>% 
  group_by(iso3, year, sex) %>% 
  mutate(dts_tot = sum(dts)) %>% 
  mutate(diff = dts_tot - total) %>% 
  ungroup()

iss1 <- 
  dt %>% 
  mutate(diff = round(diff)) %>% 
  filter(abs(diff) > 20) %>% 
  select(iso3, year, sex, dts_tot, total, diff) %>% 
  unique() %>% 
  left_join(d2 %>% select(iso3, country = whoname) %>% 
              unique())

iss2 <- 
  dt %>% 
  filter(diff != 0) %>% 
  group_by(iso3, year) %>% 
  summarise(dts_tot = sum(dts_tot), 
            total = sum(total),
            diff = dts_tot - total) %>% 
  ungroup() %>% 
  filter(diff != 0) %>% 
  select(iso3, year, dts_tot, total, diff) %>% 
  unique() %>% 
  mutate(diff = round(diff))




write_csv(iss1, "data_inter/20240121_issues_ccd.csv")

# several differences adding the totals together,
# taking Total value by sex (for now) and redistributing deaths
dt2 <- 
  dt %>% 
  filter(age != "x999") %>% 
  group_by(iso3, year, sex) %>% 
  mutate(dts_tot = sum(dts),
         dx = ifelse(total == 0 | dts_tot == 0, 
                     0,
                     dts*total/dts_tot)) %>% 
  select(iso3, year, sex, age, dx) %>% 
  mutate(age = str_remove(age, "x"),
         age = age %>% as.double())

# re-scaling sex
dt3 <- 
  dt2 %>% 
  group_by(iso3, year, age) %>% 
  mutate(dts = sum(dx)) %>% 
  filter(sex != "u") %>% 
  mutate(dx = ifelse(dts > 0 & dx > 0, 
                     dx*dts/sum(dx),
                     0)) %>% 
  ungroup() %>% 
  select(-dts)

# taking only under-25 and grouping ages
dt4 <- 
  dt3 %>% 
  filter(age <= 24) %>% 
  mutate(age = case_when(age == 0 ~ 0,
                         age %in% 1:4 ~ 1,
                         age %in% 5:24 ~ age-age%%5)) %>% 
  group_by(iso3, year, sex, age) %>% 
  summarise(dx = sum(dx)) %>% 
  ungroup()

# adding total sex and out
dt5 <- 
  dt4 %>% 
  bind_rows(dt4 %>% 
              group_by(iso3, year, age) %>% 
              summarise(dx = sum(dx),
                        sex = "t") %>% 
              ungroup()) %>% 
  drop_na() %>% 
  select(Code = iso3,
         Year = year,
         Sex = sex,
         Age = age,
         Deaths = dx) %>% 
  mutate(age_up = case_when(Age == 0 ~ 0,
                            Age == 1 ~ 4,
                            Age == 5 ~ 9,
                            Age == 10 ~ 14,
                            Age == 15 ~ 19,
                            Age == 20 ~ 24),
         Deaths = round(Deaths),
         Source = "ccd23") %>% 
  arrange(Code, Sex, Age, Year)

write_rds(dt5, "data_inter/ccd_2023.rds")

# Adding information on sources
scr <- 
  d2 %>% 
  filter(var == "dth",
         year >= 2010,
         sex %in% c("m", "f")) %>% 
  select(iso3, year, source = from) %>% 
  unique() %>% 
  mutate(source = ifelse(str_detect(source, "cc"), "ccd", str_to_lower(source)))

scr %>% 
  group_by(iso3, year) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

scr2 <- 
  scr %>% 
  group_by(iso3, year) %>% 
  summarise(source = unique(source) %>% paste(collapse = ", ")) %>% 
  ungroup()

