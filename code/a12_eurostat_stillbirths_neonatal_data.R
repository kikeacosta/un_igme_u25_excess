source("code/00_functions.R")

toc <- get_eurostat_toc()


# stillbirths
sbs1 <- get_eurostat("demo_mfoet")

sbs2 <- 
  sbs1 %>% 
  filter(age == "TOTAL") %>% 
  mutate(year = year(TIME_PERIOD)) %>% 
  filter(year %in% 2010:2021) %>% 
  select(-freq, -unit, -age, -TIME_PERIOD,
         code = geo, value = values, year) %>% 
  mutate(country = suppressWarnings(countrycode(sourcevar = code, 
                                                origin = "iso2c", 
                                                destination = "country.name")),
         country = case_when(code == "EL" ~ "Greece",
                             code == "UK" ~ "United Kingdom",
                             TRUE ~ country),
         code = countryname(country, destination = "iso3c"),
         source = "eurs",
         measure = "sbs")


neo <- get_eurostat("hlth_cd_aperrto")

unique(neo$resid)
unique(neo$indic_de)

# stillbirths rates
r_sbs2 <- 
  neo %>% 
  mutate(year = year(TIME_PERIOD)) %>% 
  filter(year %in% 2010:2021,
         indic_de == "LFOEMORRT") %>% 
  select(-freq, -unit, -resid, -indic_de, -TIME_PERIOD,
         code = geo, value = values, year) %>% 
  mutate(country = suppressWarnings(countrycode(sourcevar = code, 
                                                origin = "iso2c", 
                                                destination = "country.name")),
         country = case_when(code == "EL" ~ "Greece",
                             code == "UK" ~ "United Kingdom",
                             TRUE ~ country),
         code = countryname(country, destination = "iso3c"),
         source = "eurs",
         measure = "sbs_r")

# neonatal mortality rates
neo2 <- 
  neo %>% 
  mutate(year = year(TIME_PERIOD)) %>% 
  filter(year %in% 2010:2021,
         indic_de == "NEOMORRT") %>% 
  select(-freq, -unit, -resid, -indic_de, -TIME_PERIOD,
         code = geo, value = values, year) %>% 
  mutate(country = suppressWarnings(countrycode(sourcevar = code, 
                                                origin = "iso2c", 
                                                destination = "country.name")),
         country = case_when(code == "EL" ~ "Greece",
                             code == "UK" ~ "United Kingdom",
                             TRUE ~ country),
         code = countryname(country, destination = "iso3c"),
         source = "eurs",
         measure = "neo_r")

# births
bts <- get_eurostat("demo_fmonth")
  
bts2 <- 
  bts %>% 
  mutate(year = year(TIME_PERIOD)) %>% 
  filter(str_length(geo) == 2,
         year %in% 2010:2021) %>% 
  group_by(code = geo, year) %>% 
  summarise(bts = sum(values)) %>% 
  ungroup() %>% 
  mutate(country = suppressWarnings(countrycode(sourcevar = code, 
                                                origin = "iso2c", 
                                                destination = "country.name")),
         country = case_when(code == "EL" ~ "Greece",
                             code == "UK" ~ "United Kingdom",
                             TRUE ~ country),
         code = countryname(country, destination = "iso3c"))
  

# all together
sbs_neo <- 
  bind_rows(sbs2, r_sbs2, neo2) %>% 
  left_join(bts2) %>% 
  drop_na(country)
  
out <- 
  sbs_neo %>% 
  mutate(pre = ifelse(year %in% 2015:2019, 1, 0),
         pan = ifelse(year >= 2020, 1, 0)) %>% 
  group_by(code, measure, source) %>% 
  filter(sum(pre) >= 3 & sum(pan) >= 1) %>% 
  ungroup() %>% 
  select(-pre, -pan) %>% 
  drop_na(value)

write_rds(out, "data_inter/neo_sbs_eur.rds")

