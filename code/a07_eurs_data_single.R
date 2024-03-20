source("code/00_functions.R")

toc <- get_eurostat_toc()
dts <- get_eurostat("demo_magec")

unique(dts$time)
unique(dts$unit)

dts2 <- 
  dts %>% 
  mutate(year = year(TIME_PERIOD)) %>% 
  filter(year %in% 2010:2021) %>% 
  select(-unit, -freq) %>% 
  mutate(age = case_when(age == "Y_LT1" ~ "0",
                         age == "Y_OPEN" ~ "100",
                         age == "TOTAL" ~ "TOT",
                         age == "UNK" ~ "UNK",
                         TRUE ~ str_replace(age, "Y", "")),
         sex = sex %>% str_to_lower()) %>% 
  select(Sex = sex, Age = age, Code = geo, Deaths = values, Year = year)

dts3 <- 
  dts2 %>% 
  filter(!Age %in% c("TOT", "UNK")) %>% 
  mutate(Age = Age %>% as.double(), 
         Age = case_when(Age %in% 1:4 ~ 1,
                         Age >= 5 ~ Age - Age%%5,
                         TRUE ~ 0)) %>% 
  group_by(Code, Sex, Age, Year) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Age = Age %>% as.character()) %>% 
  bind_rows(dts2 %>% 
                filter(Age %in% c("TOT", "UNK"))) %>% 
  mutate(Country = suppressWarnings(countrycode(sourcevar = Code, 
                                                origin = "iso2c", 
                                                destination = "country.name")),
         Country = case_when(Code == "EL" ~ "Greece",
                             Code == "UK" ~ "United Kingdom",
                             TRUE ~ Country),
         Code = countryname(Country, destination = "iso3c")) %>% 
  drop_na(Country)

dts4 <- 
  dts3 %>% group_by(Country, Code, Sex, Year) %>% 
  do(rescale_age(chunk = .data)) %>% 
  ungroup() %>%
  group_by(Country, Code, Age, Year) %>%
  do(rescale_sex(chunk = .data)) %>%
  ungroup() %>% 
  mutate(Source = "eurs_x1") %>% 
  mutate(Age = Age %>% as.double()) %>% 
  group_by(Country, Code, Year, Sex) %>%
  arrange(Country, Code, Year, Sex, Age) %>%
  mutate(age_up = lead(Age - 1)) %>%
  filter(Age <= 24 & age_up <= 25) %>%
  ungroup()

write_rds(dts4, "data_inter/eurs_single.rds")

