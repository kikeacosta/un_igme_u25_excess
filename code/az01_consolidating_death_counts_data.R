rm (list = ls())
source("code/00_functions.R")

# countries to be excluded due to quality issues or 
# because are subnational divisions (e.g, England, Scotland...)
cts_exclude <- c(
  # "Kenya",  # issues with data quality
  # "Lesotho", # issues with data quality
  "Northern Ireland", # subnational division of the UK
  "United Kingdom, Northern Ireland",
  "Northern Ireland",
  "Scotland", # subnational division of the UK
  "England and Wales" # subnational division of the UK
)

# functions ====
# ~~~~~~~~~~~~~~
{
  group5 <- function(db){
    db %>%
      # filter(Year >= 2015) %>% 
      drop_na() %>% 
      mutate(Age = Age - Age %% 5) %>% 
      group_by(Code, Year, Sex, Age, Source) %>% 
      summarise(Deaths = sum(Deaths)) %>% 
      ungroup() %>% 
      group_by(Code, Year, Source, Sex) %>% 
      arrange(Age) %>% 
      mutate(age_up = case_when(last(Age) == 20 & Age == 20 ~ 24,
                                last(Age) == 0 & Age == 0 ~ 4,
                                TRUE ~ lead(Age - 1))) %>% 
      ungroup()
  }
  
  sum_source <- function(db){
    sum_out <- 
      db %>% 
      filter(Year >= 2015) %>% 
      group_by(Code, Source, Sex, Age) %>% 
      filter(max(Year) >= 2020) %>% 
      ungroup() %>% 
      group_by(Code, Source, Year, Sex) %>% 
      mutate(ages = n()) %>% 
      ungroup() %>% 
      group_by(Code, Source, Year, Age) %>% 
      mutate(sexs = n()) %>% 
      ungroup() %>% 
      group_by(Code, Source, Sex, Age) %>% 
      mutate(years = n()) %>% 
      ungroup() %>% 
      group_by(Code, Source) %>% 
      filter(!(sexs == 3 & Sex == "t")) %>% 
      summarise(Deaths = sum(Deaths),
                ages = min(ages),
                sexs = min(sexs),
                years = min(years),
                Source = unique(Source) %>% paste(collapse = ", ")) %>% 
      ungroup() %>% 
      unique()  
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data reported to UNICEF by countries
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  db_ccd22 <- 
    read_rds("data_inter/ccd.rds")
  
  db_ccd23 <- 
    read_rds("data_inter/ccd_2023.rds")
  
  db_ccd <- 
    full_join(db_ccd22 %>% select(Code, Year, Sex, Age, dts1 = Deaths),
              db_ccd23 %>% select(Code, Year, Sex, Age, dts2 = Deaths)) %>% 
    filter(!is.na(dts1) | !is.na(dts2)) %>% 
    group_by(Code, Sex, Age) %>% 
    filter(max(Year) >= 2020) %>% 
    ungroup() %>% 
    mutate(diff = dts1-dts2) %>% 
    mutate(Deaths = case_when(is.na(dts1) & !is.na(dts2) ~ dts2,
                              is.na(dts2) & !is.na(dts1) ~ dts1,
                              dts1 >= dts2 & !is.na(dts1) ~ dts1,
                              dts2 > dts1 & !is.na(dts2) ~ dts2)) %>% 
    select(-dts1, -dts2, -diff) %>% 
    mutate(age_up = case_when(Age == 0 ~ 0,
                              Age == 1 ~ 4,
                              Age == 5 ~ 9,
                              Age == 10 ~ 14,
                              Age == 15 ~ 19,
                              Age == 20 ~ 24),
           Source = "unicef_ccs")
  
  inf_ccd <- 
    db_ccd %>% 
    filter(Age == 0 & age_up == 0,
           Sex == "t")
  
  chd_ccd <- 
    db_ccd %>% 
    filter(Age == 1 & age_up == 4)
  
  all_ccd <- group5(db_ccd)
  sum_all_ccd <- sum_source(all_ccd)
  sum_inf_ccd <- sum_source(inf_ccd)
  sum_chd_ccd <- sum_source(chd_ccd)
}

# ~~~~~~~~~~~~~~
# data from Brazil
# ~~~~~~~~~~~~~~
{
  db_bra <- 
    read_rds("data_inter/brazil.rds") %>% 
    mutate(Age = case_when(Age == 0 ~ 0,
                           Age %in% 1:4 ~ 1,
                           Age %in% 5:9 ~ 5,
                           Age %in% 10:14 ~ 10,
                           Age %in% 15:19 ~ 15,
                           Age %in% 20:24 ~ 20,
                           Age >= 25 ~ 25)) %>% 
    group_by(Code, Year, Sex, Age, Source) %>% 
    summarise(Deaths = sum(Deaths), .groups = "drop") %>% 
    group_by(Code, Year, Sex) %>%
    arrange(Code, Year, Sex, Age) %>%
    mutate(age_up = lead(Age - 1)) %>%
    filter(Age <= 24 & age_up <= 25) %>%
    ungroup()
  
  inf_bra <- 
    db_bra %>% 
    filter((Age == 0 & age_up == 0),
           Sex == "t")
  
  chd_bra <- 
    db_bra %>% 
    filter((Age == 1 & age_up == 4))
  
  all_bra <- group5(db_bra)
  sum_all_bra <- sum_source(all_bra)
  sum_inf_bra <- sum_source(inf_bra)
  sum_chd_bra <- sum_source(chd_bra)
}

# ~~~~~~~~~~~~~~
# data from UNPD
# ~~~~~~~~~~~~~~
{
  db_unpd <- 
    read_rds("data_inter/unpd.rds") %>% 
    filter(!Country %in% c("Brazil", "Peru", "Australia")) %>% 
    filter(!(Country == "Kenya" & Age > 1)) %>% 
    select(-pre, -pan, -Country)
  
  inf_unpd <- 
    db_unpd %>% 
    filter((Age == 0 & age_up == 0),
           Sex == "t")
  
  chd_unpd <- 
    db_unpd %>% 
    filter((Age == 1 & age_up == 4))
  
  all_unpd <- group5(db_unpd)
  sum_all_unpd <- sum_source(all_unpd)
  sum_inf_unpd <- sum_source(inf_unpd)
  sum_chd_unpd <- sum_source(chd_unpd)
}
# ~~~~~~~~~~~~~
# data from HMD
# ~~~~~~~~~~~~~
{
  db_hmd <- 
    read_rds("data_inter/hmd.rds") %>% 
    group_by(Code, Year, Sex) %>%
    arrange(Code, Year, Sex, Age) %>%
    mutate(age_up = ifelse(Age < 20, lead(Age - 1), 24)) %>%
    filter(Age <= 24 & age_up <= 25) %>%
    ungroup() %>% 
    select(-Country)
  
  inf_hmd <- 
    db_hmd %>% 
    filter(Age == 0 & age_up == 0,
           Sex == "t")
  
  chd_hmd <- 
    db_hmd %>% 
    filter((Age == 1 & age_up == 4))
  
  all_hmd <- group5(db_hmd)
  sum_all_hmd <- sum_source(all_hmd)
  sum_inf_hmd <- sum_source(inf_hmd)
  sum_chd_hmd <- sum_source(chd_hmd)
}

# ~~~~~~~~~~~~~~~~~~
# data from Eurostat
# ~~~~~~~~~~~~~~~~~~

# in 5 year age groups
# ~~~~~~~~~~~~~~~~~~~~
{
  db_eurs <- 
    read_rds("data_inter/eurs.rds") %>% 
    select(-Country)
  all_eurs <- group5(db_eurs)
  sum_all_eurs <- sum_source(all_eurs)
  # no infant deaths in Eurostat!!!
  
  # in single-year age groups
  # ~~~~~~~~~~~~~~~~~~~~~~~
  db_eurs2 <- 
    read_rds("data_inter/eurs_single.rds") %>% 
    select(-Country)
  
  inf_eurs <- 
    db_eurs2 %>% 
    filter((Age == 0 & age_up == 0),
           Sex == "t")
  chd_eurs <- 
    db_eurs2 %>% 
    filter((Age == 1 & age_up == 4))
  
  sum_inf_eurs <- sum_source(inf_eurs)
  sum_chd_eurs <- sum_source(chd_eurs)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data from WHO mortality database
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  db_who <- 
    read_rds("data_inter/who.rds") %>% 
    select(-Country)
  
  inf_who <- 
    db_who %>% 
    filter((Age == 0 & age_up == 0),
           Sex == "t")
  
  chd_who <- 
    db_who %>% 
    filter((Age == 1 & age_up == 4))
  
  all_who <- group5(db_who)
  sum_all_who <- sum_source(all_who)
  sum_inf_who <- sum_source(inf_who)
  sum_chd_who <- sum_source(chd_who)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data from STMF
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  db_stm <- 
    read_rds("data_inter/stmf.rds") %>%
    select(-AgeInterval) %>% 
    arrange(Code, Year, Sex, Age) %>% 
    group_by(Code, Sex, Year) %>% 
    mutate(age_up = ifelse(Age == 20, 24, lead(Age - 1))) %>% 
    ungroup() %>% 
    select(-Country)
  
  inf_stm <- 
    db_stm %>% 
    filter((Age == 0 & age_up == 0),
           Sex == "t")
  
  chd_stm <- 
    db_stm %>% 
    filter((Age == 1 & age_up == 4))
  
  all_stm <- group5(db_stm)
  sum_all_stm <- sum_source(all_stm)
  sum_inf_stm <- sum_source(inf_stm)
  sum_chd_stm <- sum_source(chd_stm)
}

# ~~~~~~~~~~~~~~~~~~~
# data from USA
# ~~~~~~~~~~~~~~~~~~~
{
  # CDC
  db_usa1 <- 
    read_tsv("data_input/USA/wonder_under_25_2010_2020.txt") %>% 
    select(-Notes, Year, Age = 4, Sex = 7) %>% 
    drop_na() %>% 
    mutate(Age = str_sub(Age, 1, 2),
           Age = recode(Age,
                        "< " = "0",
                        "1-" = "1",
                        "5-" = "5"),
           Age = Age %>% as.double(),
           Sex = str_to_lower(Sex)) %>% 
    select(Year, Sex, Age, Deaths) %>% 
    mutate(Source = "cdc",
           Code = "USA") %>% 
    group_by(Year, Sex) %>% 
    mutate(age_up = ifelse(Age == 20, 24, lead(Age) - 1)) %>% 
    filter(Year <= 2019)
  
  db_usa2 <- 
    read_tsv("data_input/USA/wonder_under_25_2018_2022.txt") %>% 
    select(Year = 7, Age = 2, Sex = 5, Deaths = 8) %>% 
    drop_na() %>% 
    mutate(Age = str_sub(Age, 1, 2),
           Age = recode(Age,
                        "< " = "0",
                        "1-" = "1",
                        "5-" = "5"),
           Age = Age %>% as.double(),
           Sex = str_to_lower(Sex)) %>% 
    select(Year, Sex, Age, Deaths) %>% 
    mutate(Source = "cdc",
           Code = "USA") %>% 
    group_by(Year, Sex) %>% 
    mutate(age_up = ifelse(Age == 20, 24, lead(Age) - 1)) %>% 
    filter(Year >= 2020)
  
  db_usa <- 
    bind_rows(db_usa1, db_usa2) %>% 
    group_by(Code, Year, Age, age_up, Source) %>% 
    summarise(Deaths = sum(Deaths)) %>% 
    ungroup() %>% 
    mutate(Sex = "t") %>% 
    bind_rows(db_usa1, db_usa2) %>% 
    select(Code, Year, Sex, Age, age_up, Deaths, Source) %>% 
    arrange(Sex, Age, Year)
  
  inf_usa <- 
    db_usa %>% 
    filter(Age == 0,
           Sex == "t")
  
  chd_usa <- 
    db_usa %>% 
    filter(Age == 1)
  
  all_usa <- group5(db_usa)
  sum_all_usa <- sum_source(all_usa)
  sum_inf_usa <- sum_source(inf_usa)
  sum_chd_usa <- sum_source(chd_usa)
}

# ~~~~~~~~~~~~~~~~~~~
# data from Spain
# ~~~~~~~~~~~~~~~~~~~
{
  db_esp <- 
    read_csv("data_input/Spain/muertes_causa_0_24_2010_2021.csv") %>% 
    rename("Cause" = 1) %>% 
    select(Year = Periodo, Sex = Sexo, Age = Edad, Deaths = Total) %>% 
    mutate(Age = str_sub(Age, 4, 5),
           Age = str_trim(Age),
           Age = recode(Age,
                        "os" = "0"),
           Age = Age %>% as.double(),
           Sex = recode(Sex,
                        "Total" = "t",
                        "Hombres" = "m",
                        "Mujeres" = "f")) %>% 
    select(Year, Sex, Age, Deaths) %>% 
    mutate(Source = "ine",
           Code = "ESP") %>% 
    group_by(Year, Sex) %>% 
    mutate(age_up = ifelse(Age == 20, 24, lead(Age) - 1)) %>% 
    ungroup()
  
  inf_esp <- 
    db_esp %>% 
    filter((Age == 0 & age_up == 0),
           Sex == "t")
  
  chd_esp <- 
    db_esp %>% 
    filter((Age == 1 & age_up == 4))
  
  all_esp <- group5(db_esp)
  sum_all_esp <- sum_source(all_esp)
  sum_inf_esp <- sum_source(inf_esp)
  sum_chd_esp <- sum_source(chd_esp)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# data from Northern Ireland
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  db_nir_t <- 
    read_xls("data_input/Northern_Ireland/deaths_by_age_1955_2020.xls",
             sheet = "Persons",
             skip = 3) %>% 
    mutate(Age = Age %>% as.double()) %>% 
    filter(Age %in% 0:24) %>% 
    gather(-Age, key = Year, value = Deaths) %>% 
    mutate(Year = Year %>% as.double(),
           Deaths = Deaths %>% as.double(),
           Sex = "t")
  
  db_nir_f <- 
    read_xls("data_input/Northern_Ireland/deaths_by_age_1955_2020.xls",
             sheet = "Females",
             skip = 3) %>% 
    mutate(Age = Age %>% as.double()) %>% 
    filter(Age %in% 0:24) %>% 
    gather(-Age, key = Year, value = Deaths) %>% 
    mutate(Year = Year %>% as.double(),
           Deaths = Deaths %>% as.double(),
           Sex = "f")
  
  db_nir_m <- 
    read_xls("data_input/Northern_Ireland/deaths_by_age_1955_2020.xls",
             sheet = "Males",
             skip = 3) %>% 
    mutate(Age = Age %>% as.double()) %>% 
    filter(Age %in% 0:24) %>% 
    gather(-Age, key = Year, value = Deaths) %>% 
    mutate(Year = Year %>% as.double(),
           Deaths = Deaths %>% as.double(),
           Sex = "m")
  
  db_nir <- 
    bind_rows(db_nir_t,
              db_nir_f,
              db_nir_m) %>% 
    filter(Year %in% 2010:2020) %>% 
    mutate(Deaths = ifelse(is.na(Deaths), 0, Deaths)) %>% 
    mutate(Code = "GBR-NIR",
           Source = "nisra")
  
  db_nir2 <- 
    db_nir %>% 
    filter(Age <= 4) %>% 
    mutate(Age = case_when(Age == 0 ~ 0,
                           TRUE ~ 1)) %>% 
    group_by(Year, Sex, Age) %>% 
    summarise(Deaths = sum(Deaths)) %>% 
    ungroup() %>% 
    group_by(Year, Sex) %>% 
    mutate(age_up = ifelse(Age == 1, 4, lead(Age) - 1)) %>% 
    ungroup() %>% 
    mutate(Code = "GBR-NIR",
           Source = "nisra")
  
  inf_nir <- 
    db_nir2 %>% 
    filter((Age == 0 & age_up == 0),
           Sex == "t")
  
  chd_nir <- 
    db_nir2 %>% 
    filter((Age == 1 & age_up == 4))
  
  all_nir <- group5(db_nir)
  sum_all_nir <- sum_source(all_nir)
  sum_inf_nir <- sum_source(inf_nir)
  sum_chd_nir <- sum_source(chd_nir)
}


# ~~~~~~~~~~~~~~~~~~~
# data from Australia
# ~~~~~~~~~~~~~~~~~~~
{
  db_aus <- 
    read_csv("data_input/Australia/aus_deaths_sex_age_2009_2020.csv") %>%
    select(Sex = 3,
           Age = 4,
           Year = 8,
           Deaths = 9) %>% 
    mutate(Sex = case_when(Sex == "1: Males" ~ "m",
                           Sex == "2: Females" ~ "f"),
           Age = str_sub(Age, 2, 3),
           Age = case_when(Age == "04" ~ "0", 
                           Age == "59" ~ "5", 
                           TRUE ~ Age),
           Age = Age %>% as.double()) %>% 
    filter(Age <= 24,
           Year >= 2010) %>% 
    arrange(Sex, Age, Year)
  
  unique(db_aus$Age)
  
  db_aus2 <- 
    db_aus %>% 
    group_by(Year, Age) %>% 
    summarise(Deaths = sum(Deaths)) %>% 
    ungroup() %>% 
    mutate(Sex = "t") %>% 
    bind_rows(db_aus) %>% 
    mutate(Code = "AUS",
           Source = "abs")
  
  inf_aus <- 
    read_xlsx("data_input/Australia/aihw_CWS_69_Health_2022.xlsx",
              sheet = "Table 41",
              skip = 6) %>% 
    select(Year = 1,
           m = 2,
           f = 3, 
           t = 4) %>% 
    mutate(Year = Year %>% as.double()) %>% 
    filter(Year >= 2010) %>% 
    gather(-Year, key = Sex, value = Deaths) %>% 
    mutate(Age = 0,
           Code = "AUS",
           Source = "aihw")
  
  chd_aus <- 
    inf_aus %>% 
    select(Year, Sex, inf = Deaths) %>% 
    left_join(db_aus2 %>% 
                filter(Age == 0) %>% 
                rename(chd = Deaths)) %>% 
    mutate(Deaths = chd - inf) %>% 
    select(-chd, -inf) %>% 
    mutate(Age = 1,
           Code = "AUS",
           Source = "abs, aihw")
  
  all_aus <- group5(db_aus2)
  sum_all_aus <- sum_source(all_aus)
  sum_inf_aus <- sum_source(inf_aus)
  sum_chd_aus <- sum_source(chd_aus)
}

# ~~~~~~~~~~~~~~~~~~~
# data from Malaysia
# ~~~~~~~~~~~~~~~~~~~
{
  db_mys <- read_xlsx("data_input/Malaysia.xlsx")
  
  db_mys2 <- 
    db_mys %>% 
    mutate(tod = `Under-5 deaths` - `Infant deaths`) %>% 
    gather(-year, key = age, value = value) %>% 
    filter(age %in% c("Infant deaths", "tod")) %>% 
    mutate(age = recode(age,
                        "tod" = "1",
                        "Infant deaths" = "0"),
           age = age %>% as.double(),
           Code = "MYS", Source = "wmd",
           age_up = ifelse(age == 0, 0, 4),
           Sex = "t") %>% 
    rename(Year = year,
           Age = age,
           Deaths = value)
  
  inf_mys <- 
    db_mys2 %>% 
    filter(Age == 0 & age_up == 0,
           Sex == "t")
  
  chd_mys <- 
    db_mys2 %>% 
    filter((Age == 1 & age_up == 4))
  
  all_mys <- group5(db_mys2)
  sum_all_mys <- sum_source(all_mys)
  sum_inf_mys <- sum_source(inf_mys)
  sum_chd_mys <- sum_source(chd_mys)
}
# ===========


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# merging all data together
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

# infant and child mortality (0, 1-4)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
comp_inf <- 
  bind_rows(sum_inf_ccd,
            sum_inf_bra,
            sum_inf_unpd,
            sum_inf_hmd,
            sum_inf_stm,
            sum_inf_eurs,
            sum_inf_who,
            sum_inf_usa,
            sum_inf_esp,
            sum_inf_aus,
            sum_inf_nir) %>% 
  unique() %>% 
  mutate(Deaths = round(Deaths)) %>% 
  arrange(Code) %>% 
  # unique sources
  group_by(Code) %>% 
  # max age definition
  filter(ages == max(ages)) %>% 
  filter(years == max(years)) %>% 
  filter(sexs == max(sexs)) %>% 
  filter(Deaths == max(Deaths)) %>% 
  filter((any(Source == "ine") & Source == "ine") | all(Source != "ine")) %>%
  filter((any(Source == "hmd") & Source == "hmd") | all(Source != "hmd")) %>%
  filter((any(Source == "unicef_ccs") & Source == "unicef_ccs") | all(Source != "unicef_ccs")) %>%
  filter((any(Source == "stmf") & Source == "stmf") | all(Source != "stmf")) %>%
  filter((any(Source == "wpp") & Source == "wpp") | all(Source != "wpp")) %>%
  filter((any(Source == "eurostat") & Source == "eurostat") | all(Source != "eurostat")) %>%
  filter((any(Source == "who_mort_db") & Source == "who_mort_db") | all(Source != "who_mort_db")) %>%
  # filter(!(n() == 2 & Source %in% c("who_mort_db", "unpd", "hmd"))) %>% 
  mutate(best = ifelse(n() == 1, Source, NA))
  
sel_inf <- 
  comp_inf %>% 
  select(Code, Source) %>% 
  unique()
  
out_inf <- 
  bind_rows(inf_ccd,
            inf_bra,
            inf_unpd,
            inf_hmd,
            inf_stm,
            inf_eurs,
            inf_who,
            inf_usa,
            inf_esp,
            inf_aus,
            inf_nir) %>% 
  inner_join(sel_inf) %>% 
  arrange(Code, Sex, Age, Year) %>% 
  unique()

unique(sel_inf$Code)

# child mortality (1-4)
# ~~~~~~~~~~~~~~~~~~~~~
comp_chd <- 
  bind_rows(sum_chd_ccd,
            sum_chd_bra,
            sum_chd_unpd,
            sum_chd_hmd,
            sum_chd_stm,
            sum_chd_eurs,
            sum_chd_who,
            sum_chd_usa,
            sum_chd_esp,
            sum_chd_aus,
            sum_chd_nir) %>% 
  unique() %>% 
  # filter(!Country %in% cts_exclude) %>% 
  mutate(Deaths = round(Deaths)) %>% 
  arrange(Code) %>% 
  # unique sources
  group_by(Code) %>% 
  # max age definition
  filter(ages == max(ages)) %>% 
  filter(years == max(years)) %>% 
  filter(sexs == max(sexs)) %>% 
  filter(Deaths == max(Deaths)) %>% 
  filter((any(Source == "hmd") & Source == "hmd") | all(Source != "hmd")) %>%
  filter((any(Source == "unicef_ccs") & Source == "unicef_ccs") | all(Source != "unicef_ccs")) %>%
  filter((any(Source == "stmf") & Source == "stmf") | all(Source != "stmf")) %>%
  filter((any(Source == "wpp") & Source == "wpp") | all(Source != "wpp")) %>%
  filter((any(Source == "eurostat") & Source == "eurostat") | all(Source != "eurostat")) %>%
  filter((any(Source == "who_mort_db") & Source == "who_mort_db") | all(Source != "who_mort_db")) %>%
  mutate(best = ifelse(n() == 1, Source, NA))

sel_chd <- 
  comp_chd %>% 
  select(Code, Source)

out_chd <- 
  bind_rows(chd_ccd,
            chd_bra,
            chd_unpd,
            chd_hmd,
            chd_stm,
            chd_eurs,
            chd_who,
            chd_usa,
            chd_esp,
            chd_aus,
            chd_nir) %>% 
  inner_join(sel_chd) %>% 
  arrange(Code, Sex, Age, Year) %>% 
  unique()

unique(sel_chd$Code)

out_inf_chd <- 
  bind_rows(out_inf, out_chd)

# young ages in 5y age groups
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
comp_all <- 
  bind_rows(sum_all_ccd,
            sum_all_bra,
            sum_all_unpd,
            sum_all_hmd,
            sum_all_eurs,
            sum_all_stm,
            sum_all_who,
            sum_all_usa,
            sum_all_esp,
            sum_all_aus,
            sum_all_nir) %>% 
  mutate(Deaths = round(Deaths)) %>% 
  arrange(Code) %>% 
  unique() %>% 
  # filter(!Country %in% cts_exclude) %>% 
  # unique sources
  group_by(Code) %>% 
  # max age definition
  filter(ages == max(ages)) %>% 
  filter(years == max(years)) %>% 
  filter(sexs == max(sexs)) %>% 
  filter(Deaths == max(Deaths)) %>% 
  filter((any(Source == "hmd") & Source == "hmd") | all(Source != "hmd")) %>%
  filter((any(Source == "unicef_ccs") & Source == "unicef_ccs") | all(Source != "unicef_ccs")) %>%
  filter((any(Source == "stmf") & Source == "stmf") | all(Source != "stmf")) %>%
  filter((any(Source == "wpp") & Source == "wpp") | all(Source != "wpp")) %>%
  filter((any(Source == "eurostat") & Source == "eurostat") | all(Source != "eurostat")) %>%
  filter((any(Source == "who_mort_db") & Source == "who_mort_db") | all(Source != "who_mort_db")) %>%
  unique() %>% 
  filter(1:n() == n()) %>% 
  mutate(best = ifelse(n() == 1, Source, NA))

sel_all <- 
  comp_all %>% 
  select(Code, Source)

out_all <- 
  bind_rows(all_ccd,
            all_bra,
            # all_per,
            all_unpd,
            all_hmd,
            all_eurs,
            all_stm,
            all_who,
            all_usa,
            all_esp,
            all_aus,
            all_nir) %>% 
  inner_join(sel_all) %>% 
  arrange(Code, Sex, Age, Year)

unique(out_all$Code)
unique(out_inf$Code)
unique(out_chd$Code)


# removing countries with insufficient periods
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# minimum 3 periods in 2015-2019

inc_per_all <- 
  out_all %>% 
  filter(Year %in% 2015:2019) %>% 
  group_by(Code, Sex, Age) %>% 
  summarise(pers = n()) %>% 
  ungroup() %>% 
  filter(pers >= 3) %>% 
  select(-pers)

out_all2 <- 
  out_all %>% 
  inner_join(inc_per_all)

inc_per_inf_chd <- 
  out_inf_chd %>% 
  filter(Year %in% 2015:2019) %>% 
  group_by(Code, Sex, Age) %>% 
  summarise(pers = n()) %>% 
  ungroup() %>% 
  filter(pers >= 3) %>% 
  select(-pers)

out_inf_chd2 <- 
  out_inf_chd %>% 
  inner_join(inc_per_inf_chd) 

unique(out_inf_chd2$Code)
unique(out_all2$Code)

write_rds(out_all2, "data_inter/annual_young_deaths_all_countries.rds")
write_rds(out_inf_chd2, "data_inter/annual_infant_deaths_all_countries.rds")


