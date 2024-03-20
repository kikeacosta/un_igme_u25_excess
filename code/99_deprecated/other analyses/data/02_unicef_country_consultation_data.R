source("Code/00_functions.R")

db_ccs <- 
  read_xlsx("Data/unicef/211102_Covid analysis data.xlsx",
            sheet = 1)

unique(db_ccs$whoname) %>% sort

db_ccs2 <- 
  db_ccs %>% 
  select(country = whoname,
         year, month, quarter,
         ndth, d0, d0_4, d5, d10, d15, d20, lb, sb_x28wks) %>% 
  gather(ndth, d0, d0_4, d5, d10, d15, d20, lb, sb_x28wks,
         key = measure,
         value = value) %>% 
  drop_na() %>% 
  mutate(value = ifelse(value == "-", "0", value),
         value = value %>% as.double())

unique(db_ccs2$measure)
unique(db_ccs2$country)

# Annual infant, child and young deaths
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db_dts <- 
  db_ccs2 %>% 
  filter(measure %in% c("d0", "d0_4", "d5", "d10", "d15", "d20"),
         year <= 2020) %>% 
  group_by(country, year, measure) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()

db_dts_04 <- 
  db_dts %>% 
  filter(measure %in% c("d0", "d0_4")) %>% 
  spread(measure, value) %>% 
  mutate(d1 = d0_4 - d0) %>% 
  select(-d0_4) %>% 
  gather(d0, d1, key = Age, value = Deaths) %>% 
  mutate(Age = str_replace(Age, "d", ""))

db_dts_5plus <- 
  db_dts %>% 
  filter(measure %in% c("d5", "d10", "d15", "d20")) %>% 
  rename(Age = measure,
         Deaths = value) %>% 
  mutate(Age = str_replace(Age, "d", ""))

db_dts2 <- 
  bind_rows(db_dts_04, 
            db_dts_5plus) %>% 
  rename(Country = country,
         Year = year) %>% 
  mutate(Code = countrycode(Country, origin = "country.name",
                            destination = "iso3c")) %>% 
  arrange(Year, Country, suppressWarnings(as.integer(Age))) %>% 
  mutate(Source = "unicef_ccs")

write_rds(db_dts2, "Output/annual_deaths_0_24_country_consultation.rds")


# annual births
# ~~~~~~~~~~~~~
lbs <- 
  db_ccs2 %>% 
  filter(measure == "lb") %>% 
  rename(lbs = value) %>% 
  select(-measure)

ann_lbs <- 
  lbs %>% 
  filter(year <= 2020) %>% 
  group_by(country, year) %>% 
  summarise(bts = sum(lbs)) %>% 
  ungroup() %>% 
  mutate(Code = countrycode(country, origin = "country.name",
                                   destination = "iso3c"))

write_rds(ann_lbs, "Output/annual_births_country_consultation.rds")

# monthly stillbirths and live births
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sbs <- 
  db_ccs2 %>% 
  filter(measure == "sb_x28wks") %>% 
  left_join(lbs) %>% 
  mutate(exposure = value + lbs,
         measure = "sbs") %>% 
  select(-lbs, -quarter)

neo <- 
  db_ccs2 %>% 
  filter(measure == "ndth") %>% 
  left_join(lbs) %>% 
  rename(exposure = lbs) %>% 
  mutate(measure = "neo") %>% 
  select(-quarter)

sbs_neo <- 
  bind_rows(sbs, neo)

write_rds(sbs_neo, "Output/monthly_stillbirths_neonataldths_country_consultation.rds")
