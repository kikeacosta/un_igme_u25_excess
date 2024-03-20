source("code/00_functions.R")
IN <- get_eurostat("demo_r_mwk_05")
unique(IN$age)

db_eurs <-
  IN %>% 
  mutate(year = year(TIME_PERIOD),
         mth = month(TIME_PERIOD)) %>% 
  dplyr::filter(year %in% 2010:2022, 
                age != "UNK") %>% 
  group_by(geo, year) %>% 
  mutate(complete = any(mth == 12)) %>% 
  ungroup() %>% 
  dplyr::filter(complete) %>% 
  group_by(geo) %>% 
  filter(max(year) >= 2020) %>% 
  ungroup() %>% 
  group_by(geo, sex, year, age) %>% 
  summarize(deaths = sum(values), .groups = "drop") %>% 
  mutate(age = recode(age,
                      "TOTAL" = "TOT",
                      "Y_GE90" = "90",
                      "Y_LT5" = "0",
                      "Y5-9" = "5",
                      "Y10-14" = "10",
                      "Y15-19" = "15",
                      "Y20-24" = "20",
                      "Y25-29" = "25",
                      "Y30-34" = "30",
                      "Y35-39" = "35",
                      "Y40-44" = "40",
                      "Y45-49" = "45",
                      "Y50-54" = "50",
                      "Y55-59" = "55",
                      "Y60-64" = "60",
                      "Y65-69" = "65",
                      "Y70-74" = "70",
                      "Y75-79" = "75",
                      "Y80-84" = "80",
                      "Y85-89" = "85"),
         sex = tolower(sex),
         Country = suppressWarnings(countrycode(sourcevar = geo, 
                               origin = "iso2c", 
                               destination = "country.name")),
         Country = case_when(geo == "EL" ~ "Greece",
                             geo == "UK" ~ "United Kingdom",
                             TRUE ~ Country),
         Code = countryname(Country, destination = "iso3c")) %>% 
  dplyr::select(Country, Code, Year = year, Sex = sex, Age = age, Deaths = deaths)
  
# excluding series with incomplete ages
unique(db_eurs$Age)

inc_age <- 
  db_eurs %>% 
  filter(Age != "TOT") %>% 
  mutate(Age = Age %>% as.integer()) %>% 
  arrange(Country, Year, Sex, Age) %>% 
  group_by(Country, Sex, Year) %>% 
  mutate(age_spn = ifelse(Age == max(Age), 0, lead(Age) - Age)) %>% 
  filter(sum(age_spn) != max(Age)) %>% 
  ungroup() %>% 
  select(Country, Year, Sex) %>% 
  unique()


# imputing unknown ages and sexes
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_eurs2 <- 
  db_eurs %>% 
  anti_join(inc_age) %>% 
  group_by(Country, Code, Sex, Year) %>% 
  do(rescale_age(chunk = .data)) %>% 
  ungroup() %>%
  group_by(Country, Code, Age, Year) %>%
  do(rescale_sex(chunk = .data)) %>%
  ungroup() %>% 
  mutate(Source = "eurs") %>% 
  mutate(Age = Age %>% as.double()) %>% 
  group_by(Country, Year, Sex) %>%
  arrange(Country, Year, Sex, Age) %>%
  mutate(age_up = lead(Age - 1)) %>%
  filter(Age <= 24 & age_up <= 25) %>%
  ungroup()

write_rds(db_eurs2, "data_inter/eurs.rds")





  

