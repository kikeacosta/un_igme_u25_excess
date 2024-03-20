rm (list = ls())
source("code/00_functions.R")

ccd_file_name <- "data_input/unicef/221109_Covid analysis data.xlsx"

# issues nov 9 2022
# Qatar has 0_4 smaller than infant 
# Palestina reports COVID deaths
cts_exclude <- c(
  "State of Palestine" # data corresponds to COVID deaths
  # "Libya" # weird data for ages 0-1 and 0-4
)

db_ccd_in <- 
  read_xlsx(ccd_file_name,
            sheet = "data") %>% 
  filter(!whoname %in% cts_exclude)

unique(db_ccd_in$whoname)

# countries without data before 2020
db_ccd_in %>% 
  filter(year >= 2020) %>% 
  select(whoname) %>% 
  unique() %>% 
  anti_join(db_ccd_in %>% 
              filter(year < 2020) %>% 
              select(whoname) %>% 
              unique())

# Albania and Kiribati without data before 2020

# repeated data with different dates
rep_dates <- 
  db_ccd_in %>% 
  semi_join(
    db_ccd_in %>% 
      select(whoname, sex, year, date) %>% 
      unique() %>% 
      group_by(whoname, sex, year) %>% 
      summarise(n = n()) %>% 
      filter(n > 1)
  )

cts_rep <- ifelse(dim(rep_dates)[1]==0, "", unique(rep_dates$whoname))

# No countries with two update dates
reps <- 
  db_ccd_in %>% 
  filter(whoname == cts_rep) %>% 
  rename(update = date) %>% 
  mutate(date = make_date(d = 15, m = month, y = year))

# reps %>% 
#   ggplot()+
#   geom_line(aes(date, d0_4, col = update))+
#   facet_wrap(~whoname)

cts_exc <- c("")

db_ccd_in2 <- 
  db_ccd_in %>% 
  rename(country = whoname, update = date, code = iso3) %>% 
  filter(country != cts_exc) %>% 
  mutate(date = make_date(d = 15, m = month, y = year)) %>% 
  select(-('p0':'p20'), -from, -whocode)
  
unique(db_ccd_in2$country) %>% sort

# looking at deaths 0 and 0-4
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
d04 <-
  db_ccd_in2 %>%
  select(code, sex, year, month, d0, d0_4) %>%
  # mutate(d0 = ifelse(is.na(d0), 0, d0),
  #        d0_4 = ifelse(is.na(d0_4), 0, d0_4)) %>%
  group_by(code, sex, year) %>%
  summarise(d0 = sum(d0, na.rm=T),
            d0_4 = sum(d0_4, na.rm=T)) %>%
  ungroup() %>%
  mutate(d1_4 = d0_4 - d0)

cts_weird_inf <- 
  d04 %>% 
  filter(d1_4<0) %>% 
  pull(code) %>% unique

cts_weird_inf
# some issues:
# Kazakhstan has less deaths in 0_4 than in 0 in 2017, Qatar in all periods.
# we assume that deaths in 0_4 refers actually to deaths 1-4
# Kazakhstan data is too weird, exclude for now

# some issues with infant and child mortality:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Countries having fewer deaths in 0_4 than in 0
#   - Kazakhstan females in 2017, 
#   - Qatar in all periods
#   - Ukraine in all periods
#   - Libya in 2017 and missing infant deaths in most periods
# - Belarus has no data for ages 0-4
# - Libya has no data for ages 0-1 in many periods
# Then:
# - In Qatar and Ukraine, we assume that deaths in 0_4 refers actually to deaths 1-4
# - In Belarus, we exclude deaths 1-4
# - Kazakhstan data by sex and Libya for all sexes are too weird: exclude for now

db_ccd_in2 %>% 
  filter(code %in% cts_weird_inf,
         month != 0) %>% 
  ggplot()+
  geom_line(aes(date, d0_4, col = sex))+
  facet_wrap(.~country, scales = "free")

db_ccd_adj1 <- 
  db_ccd_in2 %>% 
  mutate(d0_4 = ifelse(country %in% c("Qatar", "Ukraine"), d0 + d0_4, d0_4)) %>% 
  filter(!(country == "Kazakhstan" & sex != "b")) %>% 
  filter(!(country == "Libya")) %>% 
  mutate(d0_4 = ifelse(country == "Belarus", NA, d0_4)) %>% 
  gather(c("ndth":"sb_x28wks"), key = measure, value = value)
  

# more or fewer months in a year
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test <- 
  db_ccd_adj1 %>% 
  group_by(code, year, sex, measure) %>% 
  filter(n() != 12)

# 13 lines (totals?)
test13 <- 
  db_ccd_adj1 %>% 
  group_by(code, year, sex, measure) %>% 
  filter(n() == 13) %>% 
  ungroup()

# exclude mth = 0, and verify what is missing
wthot_mth0 <- 
  test13 %>% 
  filter(month != 0) %>% 
  group_by(code, year, sex, measure) %>% 
  summarise(val_sum = sum(value)) %>% 
  ungroup()

# annual data from 13 months
yr13 <- 
  test13 %>% 
  filter(month == 0) %>% 
  select(code, year, sex, measure, value) %>% 
  left_join(wthot_mth0) %>% 
  mutate(value = round(value),
         val_sum = round(val_sum),
         diff = value - val_sum,
         value2 = case_when(is.na(value) & !is.na(val_sum) ~ val_sum,
                          !is.na(value) & is.na(val_sum) ~ value,
                          is.na(value) & is.na(val_sum) ~ NA_real_,
                          (!is.na(value) & !is.na(val_sum)) & (value >= val_sum) ~ value,
                          (!is.na(value) & !is.na(val_sum)) & (value < val_sum) ~ val_sum)) %>% 
  select(code, year, sex, measure, value = value2) %>% 
  drop_na(value)

notes_ok <- c("yearly|total|quarter")

db_ccd_adj2 <- 
  db_ccd_adj1 %>% 
  drop_na(value) %>% 
  group_by(code, year, sex, measure) %>% 
  filter(n() == 12 | str_detect(note, notes_ok)) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  anti_join(yr13, by = c("code", "year", "measure", "sex")) %>% 
  bind_rows(yr13) %>% 
  mutate(sex = recode(sex,
                      "b" = "t")) %>% 
  arrange(code, sex, measure, year) %>% 
  unique() %>% 
  # excluding countries without data before 2020 or after 2020
  group_by(code, sex, measure) %>% 
  filter(max(year) >= 2020 & min(year) < 2020) %>% 
  ungroup()

unique(db_ccd_adj2$measure)
unique(db_ccd_adj2$code) %>% sort()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Annual infant, child and young deaths ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_dts_04 <- 
  db_ccd_adj2 %>% 
  filter(measure %in% c("d0", "d0_4")) %>% 
  pivot_wider(names_from = measure, values_from = value) %>% 
  mutate(d1 = d0_4 - d0) %>% 
  select(-d0_4) %>% 
  gather(d0, d1, key = Age, value = Deaths) %>% 
  mutate(Age = str_replace(Age, "d", ""),
         Deaths = ifelse(Deaths < 0, 0, Deaths),
         Age = Age %>% as.double()) %>% 
  drop_na()

db_dts_5plus <- 
  db_ccd_adj2 %>% 
  filter(measure %in% c("d5", "d10", "d15", "d20")) %>% 
  rename(Age = measure,
         Deaths = value) %>% 
  mutate(Age = str_replace(Age, "d", ""),
         Age = Age %>% as.double())

ccd_dts <- 
  bind_rows(db_dts_04, 
            db_dts_5plus) %>% 
  rename(Code = code,
         Sex = sex,
         Year = year) %>% 
  arrange(Year, Sex, Code, Age) %>% 
  group_by(Code, Sex, Age) %>% 
  filter(max(Year) >= 2020) %>% 
  filter(Code %in% unique(db_dts_04$code)) %>% 
  ungroup() %>% 
  mutate()


cts_dts <- unique(ccd_dts$Code)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~
# data prior to 2020
# ~~~~~~~~~~~~~~~~~~
old <- read_csv("data_input/unicef/Master2022-05-04.csv")

old2 <- 
  old %>% 
  select(Code = iso3,
         Year = year,
         Sex = sex,
         var,
         x0:x20)

old_dts <- 
  old2 %>% 
  filter(var == "dth") %>% 
  gather(x0:x20, key = Age, value = Deaths) %>% 
  mutate(Age = str_replace(Age, "x", "") %>% as.double(),
         Age = ifelse(Age %in% 1:4, 1, Age)) %>% 
  group_by(Code, Year, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()

# adding deaths between 2010 and 2014
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
old_dts2 <- 
  old_dts %>% 
  group_by(Code, Year, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Sex = "t") %>% 
  bind_rows(old_dts) %>% 
  filter(Year %in% 2010:2014,
         Code %in% cts_dts)

out <- 
  bind_rows(old_dts2,
            ccd_dts) %>% 
  mutate(age_up = case_when(Age == 0 ~ 0,
                            Age == 1 ~ 4,
                            Age == 5 ~ 9,
                            Age == 10 ~ 14,
                            Age == 15 ~ 19,
                            Age == 20 ~ 24),
         Source = "unicef_ccs",
         Deaths = round(Deaths)) %>% 
  arrange(Code, Year, Sex, Age)

write_rds(out, "data_inter/ccd.rds")


# ~~~~~~~~~~~
# births ====
# ~~~~~~~~~~~
bts <- 
  db_ccd_adj2 %>% 
  filter(measure == "lb") %>% 
  arrange(code, sex, year) %>% 
  mutate(Source = "unicef_ccd") %>% 
  filter(sex == "t") %>% 
  select(Code = code,
         Year = year,
         Births = value,
         Source) %>% 
  ungroup()

write_rds(bts, "data_inter/annual_births_ccd.rds")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# stillbirths and neonatal ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# loading pre-processed births
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bts <- 
  read_rds("data_inter/annual_births_ccd.rds") %>% 
  rename(year = Year,
         code = Code,
         bts = Births,
         source = Source)

unique(db_ccd_adj2$code) %>% sort

# excluding stillbirths of any gestational age
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unique(db_ccd_adj1$note)
# no_sbs <- c("SB refers to any SB|SB any gestational age")

# exclude Colombia, Kwait, and Uzbekistan
cts_sbs_exc <- 
  c("COL", "KWT", "UZB")

sbsneo <- 
  db_ccd_adj2 %>% 
  filter(measure %in% c("ndth", "sb_x28wks")) %>% 
  filter(sex == "t",
         !(code %in% cts_sbs_exc & measure == "sb_x28wks")) %>% 
  select(-sex)


# countries with at least 3 periods before 2020
cts_yrs <- 
  sbsneo %>% 
  filter(year < 2020) %>% 
  group_by(code, measure) %>% 
  filter(n() >= 3) %>% 
  select(code, measure) %>% 
  unique()

# Stillbirths Colombia
sbs_col <- 
  read_xlsx("data_input/unicef/221111_SB for Colombia.xlsx") %>% 
  select(code = 1, 
         year = 3, 
         value = 7,
         bts = 5) %>% 
  mutate(measure = "sbs",
         source = "colombia_dane")


# merging with births data
# ~~~~~~~~~~~~~~~~~~~~~~~~
sbsneo2 <- 
  sbsneo %>%  
  semi_join(cts_yrs) %>% 
  group_by(code, measure) %>% 
  filter(max(year) >= 2020) %>% 
  ungroup() %>% 
  left_join(bts) %>% 
  arrange(code, measure, year) %>% 
  mutate(measure = case_when(measure == "sb_x28wks" ~ "sbs", 
                             measure == "ndth" ~ "neo")) %>% 
  drop_na() %>% 
  bind_rows(sbs_col)

write_rds(sbsneo2, "data_inter/neo_sbs_unicef.rds")


sbsneo2 %>% 
  select(code, measure) %>% 
  unique() %>% 
  group_by(measure) %>% 
  summarise(n())

