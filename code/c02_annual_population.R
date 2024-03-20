
# Exposures data
# ~~~~~~~~~~~~~~

# exposures data from:
# WPP projection estimates by age and sex for all ages
# HMD exposures for countries with HMD deaths
# STFF data on births in 2020 (version 13.10.2021)
# Country Consultation data (CCD)
# Eurostat
# other sources for births in 2020
rm (list = ls())
source("code/00_functions.R")

# countries to be excluded due to quality issues or 
# because are subnational divisions (e.g, England, Scotland...)
cts_exclude <- c(
  "Kenya",  # issues with data quality
  "Northern Ireland", # subnational division of the UK
  "Scotland", # subnational division of the UK
  "England and Wales" # subnational division of the UK
)

cts_exclude <- c(
  "KEN",  # issues with data quality
  "GBR-NIR", # subnational division of the UK
  "GBR-SCO", # subnational division of the UK
  "GBR-ENG" # subnational division of the UK
)

# functions ====
# ~~~~~~~~~~~~~~
{
  sum_pop_source <- function(db){
    sum_out <- 
      db %>% 
      group_by(Code, Year, Sex) %>% 
      mutate(ages = n()) %>% 
      ungroup() %>% 
      group_by(Code, Year, Age) %>% 
      mutate(sexs = n()) %>% 
      ungroup() %>% 
      group_by(Code, Sex, Age) %>% 
      mutate(years = n()) %>% 
      ungroup() %>% 
      group_by(Code) %>% 
      filter(!(sexs == 3 & Sex == "t")) %>% 
      summarise(ages = min(ages),
                sexs = min(sexs),
                years = min(years),
                Source = unique(Source) %>% paste(collapse = ", ")) %>% 
      ungroup() %>% 
      unique()  
  }
}

# mortality data
# ~~~~~~~~~~~~~~
{
  db_dts_yng <- 
    read_rds("data_inter/annual_young_deaths_all_countries.rds")
  
  db_dts_inf <- 
    read_rds("data_inter/annual_infant_deaths_all_countries.rds")
  
  db_sbs <- 
    read_rds("data_inter/neonatal_and_stillbirths_all_countries.rds")
  
  db_rts_yng <- 
    read_rds("data_inter/annual_rates_5y_groups.rds")
  
  db_rts_inf <- 
    read_rds("data_inter/annual_rates_infant_child.rds")
  
  db_hmis <- 
    read_rds("data_inter/annual_rates_infant_child.rds")
  
}

unique(db_dts_yng$Code) %>% sort

all_cts <- 
  bind_rows(db_dts_yng %>% 
              select(Code),
            db_dts_inf %>% 
              select(Code),
            db_sbs %>% 
              select(Code),
            db_rts_yng %>% 
              select(Code),
            db_rts_inf %>% 
              select(Code)) %>% 
  select(Code) %>% 
  unique() %>% 
  arrange(Code)


# ~~~~~~~~~~~~~~~~~~~~~~~~
# population data from WPP
# ~~~~~~~~~~~~~~~~~~~~~~~~
{
  ctrs <- 
    all_cts %>% 
    # filter(!(Country %in% cts_hmd)) %>% 
    pull(Code) %>% 
    unique()
  
  pop_wpp <- 
    read_rds("data_inter/wpp_populations_ages_0_24_v2022.rds") %>% 
    select(Code = code, Year = year, Sex = sex, Age = age, Pop = pop) %>% 
    mutate(Age = case_when(Age == 0 ~ 0,
                           Age %in% 1:4 ~ 1,
                           Age >= 5 ~ Age - Age%%5)) %>% 
    group_by(Code, Year, Sex, Age) %>% 
    summarise(Population = sum(Pop)) %>% 
    ungroup() %>% 
    mutate(Source = "wpp")
  
  cts_wpp <- 
    unique(pop_wpp$Code)
  
  sum_pop_wpp <- sum_pop_source(pop_wpp)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~
# population data from HMD
# ~~~~~~~~~~~~~~~~~~~~~~~~
{
  # looking for countries in the HMD
  hmd_codes <- 
    read_csv("data_input/country_codes_hmd.csv") 
  
  cts_hmd <- 
    bind_rows(db_dts_yng %>% 
                select(Code),
              db_dts_inf %>% 
                select(Code)) %>% 
    unique() %>% 
    left_join(hmd_codes) %>% 
    drop_na() %>% 
    pull(PopCode)
  
  hmd_us <- Sys.getenv("hmd_us")
  hmd_pw <- Sys.getenv("hmd_pw")
  
  hmd_exps <- tibble()
  for(ct in cts_hmd){
    chunk_p <- readHMDweb(ct, "Exposures_1x1", hmd_us, hmd_pw) %>%
      filter(Year >= 2010) %>%
      as_tibble() %>%
      mutate(Code = ct)
    
    hmd_exps <- hmd_exps %>%
      bind_rows(chunk_p)
  }
  
  # selecting countries with data in 2020
  pop_hmd0 <- 
    hmd_exps %>% 
    group_by(Code) %>% 
    filter(max(Year) >= 2020,
           !Code %in% c("GBRCENW", "GBRTENW", "GBR_SCO", "GBR_NIR",
                        "DEUTE", "DEUTW", 
                        "FRACNP", 
                        "NZL_NM", "NZL_MA")) %>% 
    ungroup() %>% 
    select(-OpenInterval) %>% 
    gather(Female, Male, Total, key = Sex, value = Population) %>% 
    filter(Age <= 24) %>% 
    mutate(Age = case_when(Age == 0 ~ 0,
                           Age %in% 1:4 ~ 1,
                           Age >= 5 ~ Age - Age%%5)) %>% 
    group_by(Code, Year, Sex, Age) %>% 
    summarise(Population = sum(Population)) %>% 
    ungroup() %>% 
    mutate(Sex = recode(Sex,
                        "Female" = "f",
                        "Male" = "m",
                        "Total" = "t"),
           Code = case_when(Code == "GBR_NP" ~ "GBR",
                            Code == "NZL_NP" ~ "NZL",
                            Code == "FRATNP" ~ "FRA",
                            Code == "DEUTNP" ~ "DEU",
                            TRUE ~ Code),
           Source = "hmd")
  
  # Adding population in 2021 and 2022 to the HMD where it is not available
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # increases of HMD countries 2020 to 2021, according to WPP
  
  cts_hmd <- unique(pop_hmd0$Code)
  
  inc_wpp_hmd <- 
    pop_wpp %>% 
    filter(Code %in% cts_hmd,
           Year >= 2020) %>% 
    spread(Year, Population) %>% 
    mutate(fct_prj21 = `2021` / `2020`,
           fct_prj22 = `2022` / `2021`) %>% 
    select(Code, Age, Sex, fct_prj21, fct_prj22)
  
  hmd_max_2020 <- 
    pop_hmd0 %>% 
    group_by(Code) %>% 
    filter(max(Year) == 2020) %>%
    ungroup() %>% 
    pull(Code) %>% unique
  
  hmd_max_2021 <- 
    pop_hmd0 %>% 
    group_by(Code) %>% 
    filter(max(Year) == 2021) %>%
    ungroup() %>%
    pull(Code) %>% unique
  
  pop_hmd <- 
    bind_rows(pop_hmd0 %>% filter(Code %in% hmd_max_2020, Year == 2020),
              pop_hmd0 %>% filter(Code %in% hmd_max_2021, Year == 2021)) %>% 
    left_join(inc_wpp_hmd) %>% 
    drop_na() %>% 
    mutate(Population21 = ifelse(Year == 2020, 
                                   Population * fct_prj21,
                                   Population),
           Population22 = Population21 * fct_prj22) %>% 
    select(-Population) %>% 
    gather(starts_with("Pop"), key = Year, value = Population) %>% 
    mutate(Year = case_when(Year == "Population21" ~ 2021,
                            Year == "Population22" ~ 2022)) %>% 
    select(Code, Year, Sex, Age, Population, Source) %>% 
    filter(!(Code %in% hmd_max_2021 & Year == 2021)) %>% 
    bind_rows(pop_hmd0) %>% 
    arrange(Code, Year, Sex, Age)
  
  test <- 
    pop_hmd %>% 
    group_by(Code, Year, Sex, Age) %>% 
    summarise(n = n()) %>% 
    filter(n > 1)
    
  sum_pop_hmd <- sum_pop_source(pop_hmd)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# population data from WHO db
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  ctry_names <- 
    read_csv(file.path("data_input", "WHO", "mort_country_codes.zip")) %>%
    rename(Country = country)
  
  who <-
    read_csv("data_input/WHO/mort_pop.zip") %>% 
    left_join(ctry_names) %>% 
    rename(Code = Country,
           Country = name) 
  
  cts_who <- 
    who %>% 
    filter(Year >= 2020) %>% 
    pull(Country) %>% 
    unique
  
  who2 <-
    who %>% 
    filter(Year >= 2010) %>%
    select(-Admin1, -SubDiv, -Frmat, -Code) %>%
    gather(-Country, -Year, -Sex, key = Age, value = Pop) %>%
    filter(Age %in% c('Pop2', 'Pop3', 'Pop4', 'Pop5', 'Pop6', 
                      'Pop7', 'Pop8', 'Pop9', 'Pop10')) %>%
    mutate(Age = recode(Age,
                        'Pop2' = '0',
                        'Pop3' = '1',
                        'Pop4' = '1',
                        'Pop5' = '1',
                        'Pop6' = '1',
                        'Pop7' = '5',
                        'Pop8' = '10',
                        'Pop9' = '15',
                        'Pop10' = '20'),
           Age = Age %>% as.double(),
           Pop = ifelse(is.na(Pop), 0, Pop),
           Sex = recode(Sex,
                        "1" = "m",
                        "2" = "f"),
           Country = recode(Country,
                            "United Kingdom, England and Wales" = "England and Wales",
                            "United Kingdom, Scotland" = "Scotland",
                            "United Kingdom, Northern Ireland" = "Northern Ireland",
                            "Czech Republic" = "Czechia")) %>%
    group_by(Country, Year, Sex, Age) %>%
    summarise(Population = sum(Pop)) %>%
    ungroup() %>%
    mutate(Code = countrycode(Country, origin = 'country.name', destination = 'iso3c'),
           Code = case_when(Country == "England and Wales" ~ "GBR-ENW",
                            Country == "Scotland" ~ "GBR-SCO",
                            Country == "Northern Ireland" ~ "GBR-NIR",
                            TRUE ~ Code)) %>% 
    drop_na(Code) %>% 
    group_by(Code) %>% 
    filter(max(Year) >= 2020) %>% 
    ungroup() %>% 
    group_by(Code, Sex, Age) %>% 
    mutate(seq = ifelse(Year == 2020, 1, lead(Year) - Year)) %>% 
    ungroup() %>% 
    group_by(Code) %>% 
    filter(max(seq) == 1) %>% 
    ungroup() %>% 
    select(-seq) %>% 
    filter(Code %in% all_cts$Code) %>% 
    select(-Country)
  
  pop_who <- 
    who2 %>% 
    group_by(Code, Year, Age) %>% 
    summarise(Population = sum(Population)) %>% 
    ungroup() %>% 
    mutate(Sex = "t") %>% 
    bind_rows(who2) %>% 
    mutate(Source = "who_mort_db")
  
  cts_who <- 
    unique(pop_who$Code)
  
  sum_pop_who <- sum_pop_source(pop_who)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# population data from Eurostat
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  
  eur1 <- 
    get_eurostat("demo_pjan")
  
  pop_eur <-
    eur1 %>% 
    mutate(age = str_replace(age, "Y", ""),
           age = recode(age, 
                        "_LT1" = "0",
                        "_OPEN" = "100"),
           age = age %>% as.double(),
           year = year(TIME_PERIOD),
           values = values %>% as.double()) %>% 
    drop_na(age) %>% 
    select(-freq, -unit, -TIME_PERIOD, Age = age, Sex = sex, Year = year, Code = geo, Population = values) %>% 
    mutate(Country = countrycode(Code, origin = "iso2c",
                                 destination = "country.name"),
           Country = case_when(Code == "EL" ~ "Greece",
                               Code == "UK" ~ "United Kingdom",
                               TRUE ~ Country),
           Year = Year %>% as.double(),
           Sex = str_to_lower(Sex),
           Code = countrycode(Country, origin = 'country.name', destination = 'iso3c')) %>% 
    filter(Age <= 24,
           Year %in% 2010:2022,
           Code %in% all_cts$Code) %>% 
    mutate(Age = case_when(Age == 0 ~ 0,
                           Age %in% 1:4 ~ 1,
                           Age >= 5 ~ Age - Age%%5)) %>% 
    group_by(Code, Year, Sex, Age) %>% 
    summarise(Population = sum(Population)) %>% 
    ungroup() %>% 
    mutate(Source = "eurostat") %>% 
    drop_na(Code)
  
  cts_eur <- 
    unique(pop_eur$Code)
  
  sum_pop_eur <- sum_pop_source(pop_eur)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# merging WPP, HMD, and WHO exposures
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

comp_pop <- 
  bind_rows(sum_pop_hmd,
            sum_pop_wpp,
            sum_pop_who,
            sum_pop_eur) %>% 
  arrange(Code) %>% 
  # unique sources
  group_by(Code) %>% 
  # max age definition
  filter(ages == max(ages)) %>% 
  filter(years == max(years)) %>% 
  filter(sexs == max(sexs)) %>% 
  filter((any(Source == "hmd") & Source == "hmd") | all(Source != "hmd")) %>%
  filter((any(Source == "eurostat") & Source == "eurostat") | all(Source != "eurostat")) %>%
  filter((any(Source == "wpp") & Source == "wpp") | all(Source != "wpp")) %>%
  filter((any(Source == "who_mort_db") & Source == "who_mort_db") | all(Source != "who_mort_db")) %>%
  mutate(best = ifelse(n() == 1, "yes", NA)) %>% 
  arrange(Code) 

sel_pop <- 
  comp_pop %>% 
  select(Code, Source)

out_pop <- 
  bind_rows(pop_hmd,
            pop_wpp,
            pop_who,
            pop_eur) %>% 
  inner_join(sel_pop) %>% 
  # filter(keep == "y") %>% 
  # select(-keep) %>% 
  arrange(Code, Year) %>% 
  filter(!Code %in% cts_exclude)

unique(sel_pop$Code)

country_list <- 
  tibble(Code = c(unique(out_pop$Code), unique(all_cts$Code))) %>% 
  left_join(tibble(Code = unique(out_pop$Code)) %>% 
              mutate(pop = 1)) %>% 
  unique()

write_rds(out_pop, "data_inter/exposures_5-year_groups.rds")

