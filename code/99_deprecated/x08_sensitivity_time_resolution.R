rm (list = ls())
source("code/00_functions.R")

ccd_file_name <- "data_input/unicef/221109_Covid analysis data.xlsx"

# issues nov 9 2022
# Qatar has 0_4 smaller than infant 
# Palestina reports COVID deaths
cts_exclude <- c(
  "State of Palestine" # data corresponds to COVID deaths
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
  select(country, sex, year, month, d0, d0_4) %>%
  # mutate(d0 = ifelse(is.na(d0), 0, d0),
  #        d0_4 = ifelse(is.na(d0_4), 0, d0_4)) %>%
  group_by(country, sex, year) %>%
  summarise(d0 = sum(d0),
            d0_4 = sum(d0_4), na.rm=T) %>%
  ungroup() %>%
  mutate(d1_4 = d0_4 - d0, na.rm=T)

cts_weird_inf <- 
  d04 %>% 
  filter(d1_4<0) %>% 
  pull(country) %>% unique
cts_weird_inf
# some issues:
# Kazakhstan has less deaths in 0_4 than in 0 in 2017, Qatar in all periods.
# we assume that deaths in 0_4 refers actually to deaths 1-4
# Kazakhstan data is too weird, exclude for now

db_ccd_in2 %>% 
  filter(country %in% cts_weird_inf,
         month != 0) %>% 
  ggplot()+
  geom_line(aes(date, d0_4, col = sex))+
  facet_wrap(.~country, scales = "free")

db_ccd_adj1 <- 
  db_ccd_in2 %>% 
  mutate(d0_4 = ifelse(country == "Qatar", d0 + d0_4, d0_4)) %>% 
  filter(country != "Kazakhstan") %>% 
  gather(c("ndth":"sb_x28wks"), key = measure, value = value)


# more or fewer months in a year
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test <- 
  db_ccd_adj1 %>% 
  group_by(country, year, sex, measure) %>% 
  filter(n() != 12)

# 13 lines (totals?)
test13 <- 
  db_ccd_adj1 %>% 
  group_by(country, year, sex, measure) %>% 
  filter(n() == 13) %>% 
  ungroup()

# exclude mth = 0, and verify what is missing
wthot_mth0 <- 
  test13 %>% 
  filter(month != 0) %>% 
  group_by(country, year, sex, measure) %>% 
  summarise(val_sum = sum(value)) %>% 
  ungroup()

# annual data from 13 months
yr13 <- 
  test13 %>% 
  filter(month == 0) %>% 
  select(country, year, sex, measure, value) %>% 
  left_join(wthot_mth0) %>% 
  mutate(value = round(value),
         val_sum = round(val_sum),
         diff = value - val_sum,
         value2 = case_when(is.na(value) & !is.na(val_sum) ~ val_sum,
                            !is.na(value) & is.na(val_sum) ~ value,
                            is.na(value) & is.na(val_sum) ~ NA_real_,
                            (!is.na(value) & !is.na(val_sum)) & (value >= val_sum) ~ value,
                            (!is.na(value) & !is.na(val_sum)) & (value < val_sum) ~ val_sum)) %>% 
  select(country, year, sex, measure, value = value2) %>% 
  drop_na(value)

notes_ok <- c("yearly|total|quarter")


db_ccd_adj2 <- 
  db_ccd_adj1 %>% 
  drop_na(value) %>% 
  group_by(country, year, sex, measure) %>% 
  filter(n() == 12 | str_detect(note, notes_ok)) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  anti_join(yr13, by = c("country", "year", "measure", "sex")) %>% 
  bind_rows(yr13) %>% 
  mutate(country = case_when(country == "Russian Federation" ~ "Russia", 
                             country == "Republic of Korea" ~ "South Korea", 
                             TRUE ~ country),
         sex = recode(sex,
                      "b" = "t")) %>% 
  arrange(country, sex, measure, year) %>% 
  unique() %>% 
  # excluding countries without data before 2020 or after 2020
  group_by(country, sex, measure) %>% 
  filter(max(year) >= 2020 & min(year) < 2020) %>% 
  ungroup()

unique(db_ccd_adj2$measure)
unique(db_ccd_adj2$country) %>% sort()

