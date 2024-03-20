rm (list = ls())
source("code/00_functions.R")

# functions 
{
  sum_bts_source <- function(db){
    sum_out <- 
      db %>% 
      group_by(Code, Source) %>% 
      summarise(Births = sum(Births),
                years = n(),
                Source = unique(Source) %>% paste(collapse = ", ")) %>% 
      ungroup() %>% 
      unique()  
  }
}

# from UNPD database
# ~~~~~~~~~~~~~~~~~~
bs_unpd <- read_rds("data_inter/annual_births_unpd.rds") %>% 
  filter(Sex == "t") %>% 
  mutate(Code = ifelse(Country == "Kosovo", "RKS", Code)) %>% 
  select(-Sex, -Country)

sum_bs_unpd_1521 <- 
  sum_bts_source(bs_unpd %>% 
                   filter(Year >= 2015))

sum_bs_unpd_1014 <- 
  sum_bts_source(bs_unpd %>% 
                   filter(Year < 2015))


# from STFF database
# ~~~~~~~~~~~~~~~~~~

# loading data from Human Fertility Database (HFD)
# db_stff <- read_csv("https://www.humanfertility.org/STFF/stff.csv")
db_stff <- read_csv("https://www.humanfertility.org/File/GetDocumentFree/STFF/stff.csv")

bs_stf <- 
  db_stff %>% 
  select(CountryCode, Year, TOT) %>% 
  mutate(Births = TOT %>% as.double()) %>% 
  drop_na() %>% 
  select(-TOT) %>% 
  filter(Year %in% 2010:2021) %>% 
  # countries with full births data on 2020
  group_by(CountryCode) %>% 
  filter(max(Year) >= 2020) %>% 
  ungroup() %>% 
  filter(!CountryCode %in% c("CAN_QUE", "GBR_NP")) %>% 
  mutate(CountryCode = recode(CountryCode,
                              "DEUTNP" = "DEU",
                              "FRATNP" = "FRA",
                              "GBRTENW" = "GBR-ENW",
                              "GBR_NIR" = "GBR-NIR",
                              "GBR_SCO" = "GBR-SCO")) %>%
  drop_na() %>% 
  rename(Code = CountryCode) %>% 
  mutate(Source = "stff")

sum_bs_stf_1521 <- sum_bts_source(bs_stf %>% 
                               filter(Year >= 2015))

sum_bs_stf_1014 <- sum_bts_source(bs_stf %>% 
                               filter(Year < 2015))


# from HMD
# ~~~~~~~~
cds_hmd <- getHMDcountries() %>% pull(CNTRY)

hmd_us <- Sys.getenv("hmd_us")
hmd_pw <- Sys.getenv("hmd_pw")

# identifying those with data for 2020
hmd <- tibble()
for(ct in cds_hmd){
  chunk_d <- 
    readHMDweb(ct, "Births", hmd_us, hmd_pw) %>%
    filter(Year >= 2010) %>%
    as_tibble() %>%
    mutate(Code = ct)
  
  hmd <- 
    hmd %>%
    bind_rows(chunk_d)
}

# c("CAN_QUE", "GBRCENW", "DEUTE", "DEUTW", 
#   "FRACNP", "NZL_NM", "NZL_NM")

bs_hmd <- 
  hmd %>% 
  select(Code, Year, Births = Total) %>% 
  group_by(Code) %>% 
  filter(max(Year) >= 2020,
         !Code %in% c("GBRCENW", "GBRTENW", "GBR_SCO", "GBR_NIR",
                      "DEUTE", "DEUTW", 
                      "FRACNP", 
                      "NZL_NM", "NZL_MA")) %>% 
  ungroup() %>% 
  mutate(Source = "hmd",
         Code = case_when(Code == "GBR_NP" ~ "GBR",
                          Code == "NZL_NP" ~ "NZL",
                          Code == "DEUTNP" ~ "DEU",
                          Code == "FRATNP" ~ "FRA",
                          TRUE ~ Code))
  
sum_bs_hmd_1521 <- sum_bts_source(bs_hmd %>% 
                                    filter(Year >= 2015))
sum_bs_hmd_1014 <- sum_bts_source(bs_hmd %>% 
                                    filter(Year < 2015))


# from Eurostat
# ~~~~~~~~~~~~~
bs_eur <- 
  get_eurostat("demo_fmonth") %>% 
  mutate(year = year(TIME_PERIOD)) %>% 
  filter(str_length(geo) == 2,
         year %in% 2010:2021,
         month == "TOTAL") %>% 
  select(Code = geo, Year = year, Births = values) %>% 
  mutate(Country = suppressWarnings(countrycode(sourcevar = Code, 
                                                origin = "iso2c", 
                                                destination = "country.name")),
         Country = case_when(Code == "EL" ~ "Greece",
                             Code == "UK" ~ "United Kingdom",
                             TRUE ~ Country),
         Code = countryname(Country, destination = "iso3c"),
         Source = "eurostat") %>% 
  drop_na(Country) %>% 
  select(-Country)

sum_bs_eur_1521 <- sum_bts_source(bs_eur %>% 
                                    filter(Year >= 2015))

sum_bs_eur_1014 <- sum_bts_source(bs_eur %>% 
                                    filter(Year < 2015))

# from Country consultation data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bs_ccd <- 
  read_rds("data_inter/annual_births_ccd.rds")

sum_bs_ccd_1521 <- sum_bts_source(bs_ccd)


# from UNICEF annual data 
# ~~~~~~~~~~~~~~~~~~~~~~~
bs_ann <- 
  read_rds("data_inter/annual_births_ann.rds") %>% 
  select(Code = code,
        Year = year, 
        Births = bts,
        Source = source)

sum_bs_ann_1521 <- 
  sum_bts_source(bs_ann %>% 
                   filter(Year >= 2015))

sum_bs_ann_1014 <- 
  sum_bts_source(bs_ann %>% 
                   filter(Year < 2015))

# from WHO mortality database
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
who <-
  read_csv("data_input/WHO/mort_pop.zip")

ctry_names <- read_csv(file.path("data_input", "WHO", "mort_country_codes.zip")) %>% 
  rename(Country = country)

cts_who <- 
  who %>% 
  filter(Year >= 2020) %>% 
  pull(Country) %>% 
  unique

bs_who <- 
  who %>%
  filter(Year >= 2010,
         Country %in% cts_who) %>%
  select(Country, Year, Sex, Lb) %>% 
  left_join(ctry_names) %>% 
  drop_na() %>% 
  rename(Code = Country,
         Country = name,
         Births = Lb) %>% 
  mutate(Country = recode(Country,
                          "United Kingdom, England and Wales" = "England and Wales",
                          "United Kingdom, Scotland" = "Scotland",
                          "United Kingdom, Northern Ireland" = "Northern Ireland",
                          "Czech Republic" = "Czechia"),
         Code = countrycode(Country, origin = 'country.name', destination = 'iso3c'),
         Code = case_when(Country == "England and Wales" ~ "GBR-ENW",
                          Country == "Scotland" ~ "GBR-SCT",
                          Country == "Northern Ireland" ~ "GBR-NIR",
                          TRUE ~ Code)) %>% 
  # filter(!Country %in% cts_stff) %>% 
  group_by(Country, Code, Year) %>% 
  summarise(Births = sum(Births)) %>% 
  ungroup() %>%
  mutate(Source = "who_mort_db") %>% 
  group_by(Country) %>% 
  filter(max(Year) >= 2020) %>% 
  ungroup() %>% 
  select(-Country)

sum_bs_who_1521 <- sum_bts_source(bs_who %>% 
                                    filter(Year >= 2015))
sum_bs_who_1014 <- sum_bts_source(bs_who %>% 
                                    filter(Year < 2015))


# from Malaysia
# ~~~~~~~~~~~~~
db_mys <- read_xlsx("data_input/Malaysia.xlsx")

bs_mys <- 
  db_mys %>% 
  select(Year = year,
         Births = "Live births") %>% 
  mutate(Code = "MYS",
         Source = "country_public")

sum_bs_mys_1521 <- sum_bts_source(bs_mys)


# Colombia
# =========
files_names <- list.files("data_input/Colombia/births/annual/", 
                          pattern = "xls")
years <- 2010:2019
# i <- 1
bts <- list()
for(i in 1:(length(files_names)-2)){
  bts[[i]] <- 
    read_xls(here("Data", "Colombia", "births", "annual", files_names[[i]]),
             skip = 10) %>% 
    select(age_m = 1,
           bs = 2) %>% 
    filter(age_m == "TOTAL NACIONAL") %>% 
    mutate(year = years[i])
}

bts[[11]] <- 
  read_xlsx(here("data_input", "Colombia", "births", "annual", files_names[[11]]),
            sheet = "Cuadro1",
            skip = 8) %>% 
  select(age_m = 1,
         bs = 2) %>% 
  filter(age_m == "Total Nacional") %>% 
  mutate(year = 2020)

bts[[12]] <- 
  read_xlsx(here("data_input", "Colombia", "births", "annual", files_names[[12]]),
            sheet = "Cuadro1",
            skip = 8) %>% 
  select(age_m = 1,
         bs = 2) %>% 
  filter(age_m == "Total Nacional") %>% 
  mutate(year = 2021)

bs_col <- 
  bts %>% 
  bind_rows() %>% 
  select(-age_m, Year = year, Births = bs) %>% 
  mutate(Code = "COL",
         Source = "country_public")

sum_bs_col_1521 <- sum_bts_source(bs_col %>% 
                               filter(Year >= 2015))
sum_bs_col_1014 <- sum_bts_source(bs_col %>% 
                               filter(Year < 2015))

# Ecuador
# =======

names_grid <- 
  expand_grid(Measure = c("Deaths", "Births", "Rates"), Year = 2008:2020) %>% 
  mutate(names = paste(Measure, Year, sep = "_"))

var_names <- c("Region", names_grid$names, "noth")

db_ec_births <- 
  read_csv(here("data_input", "Ecuador", "1.2.3.csv"),
           skip = 4,
           col_names = var_names,
           col_types = cols(.default = "c"))

bs_ecu <- 
  db_ec_births %>% 
  filter(Region == "Total Nacional") %>% 
  select(-noth) %>% 
  gather(-Region, key = temp_names, value = Value) %>% 
  mutate(Value = str_replace(Value, ",", ""),
         Value = ifelse(Value == "-" | is.na(Value), "0", Value),
         Value = Value %>% as.integer()) %>% 
  separate(temp_names, c("Measure", "Year"), sep = "_") %>% 
  filter(Measure == "Births") %>% 
  select(Year, bts = Value) %>% 
  mutate(Year = Year %>% as.double()) %>% 
  filter(Year >= 2010) %>% 
  select(Year, Births = bts) %>% 
  mutate(Code = "ECU",
         Source = "country_public")

sum_bs_ecu_1521 <- sum_bts_source(bs_ecu %>% 
                                    filter(Year >= 2015))
sum_bs_ecu_1014 <- sum_bts_source(bs_ecu %>% 
                                    filter(Year < 2015))

# ======


# Brazil
# ======
# live births
Sys.setlocale( 'LC_ALL','C' )
bs20 <- read_csv2("data_input/Brazil/births/births_monthly_2020.csv")
bs21 <- read_csv2("data_input/Brazil/births/births_monthly_2021.csv")
# bs1519 <- read_csv2("Data/Brazil/births_monthly_2015_2019.csv", skip = 3)
bs1519 <- read.delim("data_input/Brazil/births/births_monthly_2015_2019.csv", skip = 3, 
                     encoding = "UTF-8", sep = ";")
# preparing births data
bs1519_2 <- 
  bs1519 %>% 
  drop_na() %>% 
  mutate(month = 1:n()) %>% 
  filter(month <= 12) %>% 
  select(-1, -Total) %>%
  gather(-month, key = Year, value = Births) %>% 
  group_by(Year) %>% 
  summarise(Births = sum(Births)) %>% 
  ungroup() %>% 
  mutate(Year = str_replace(Year, "X", ""),
         Year = Year %>% as.numeric(),
         Code = "BRA")
  
bs20_2 <- 
  bs20 %>% 
  select(Country = 50, Births = 63) %>% 
  filter(Country == "Brasil") %>% 
  mutate(Year = 2020,
         Code = "BRA") %>% 
  select(-Country)


bs21_2 <- 
  bs21 %>% 
  select(Country = 50, Births = 63) %>% 
  filter(Country == "Brasil") %>% 
  mutate(Year = 2021,
         Code = "BRA") %>% 
  select(-Country)

bs_bra <- 
  bind_rows(bs1519_2, bs20_2, bs21_2) %>% 
  mutate(Code = "BRA",
         Source = "country_public")

sum_bs_bra_1521 <- sum_bts_source(bs_bra %>% 
                                    filter(Year >= 2015))

# =======


# Mexico
# ==========
bs_mex <- 
  read_xlsx(here("data_input", "Mexico", "births", "Natalidad_01.xlsx"),
            skip = 4) %>% 
  select(Year = 1,
         Births = 2) %>% 
  drop_na() %>% 
  mutate(Year = Year %>% as.double(),
         Code = "MEX",
         Source = "country_public")

sum_bs_mex_1521 <- sum_bts_source(bs_mex)

# =======


# Uruguay
# =======
bs_ury <- 
  read_csv(here("data_input", "Uruguay", "uruguay_annual_births.csv")) %>% 
  rename(Year = 1,
         Births = 2) %>% 
  filter(Year >= 2010) %>% 
  mutate(Code = "URY",
         Source = "country_public")

sum_bs_ury_1521 <- sum_bts_source(bs_ury %>% 
                                    filter(Year >= 2015))
sum_bs_ury_1014 <- sum_bts_source(bs_ury %>% 
                                    filter(Year < 2015))

# ======

# Costa Rica
# ==========
bs_cri <- 
  read_xlsx(here("data_input", "Costa Rica", "repoblacevcygbmiisem2021.xlsx"),
            sheet = "Cuadro 1",
            skip = 6) %>% 
  select(Year = 1,
         Births = 2) %>% 
  drop_na() %>% 
  mutate(Year = str_remove(Year, "a/"),
         Year = Year %>% as.double()) %>% 
  filter(Year >= 2010) %>% 
  mutate(Code = "CRI",
         Source = "country_public")

sum_bs_cri_1521 <- sum_bts_source(bs_cri %>% 
                               filter(Year >= 2015))
sum_bs_cri_1014 <- sum_bts_source(bs_cri %>% 
                               filter(Year < 2015))

# ======

# Cuba
# ====
bs_cub <- 
  read_xlsx(here("data_input", "Cuba", "monthly_births.xlsx"),
            skip = 11) %>% 
  select(-1) %>%
  rename(Year = 1,
         Births = Total) %>% 
  mutate(Year = str_sub(Year, 5, 8),
         Year = Year %>% as.double()) %>% 
  drop_na() %>% 
  select(Year, Births) %>% 
  filter(Year >= 2010) %>% 
  mutate(Code = "CUB",
         Source = "country_public")

sum_bs_cub_1521 <- sum_bts_source(bs_cub %>% 
                               filter(Year >= 2015))
sum_bs_cub_1014 <- sum_bts_source(bs_cub %>% 
                               filter(Year < 2015))

# ======

# Northern Ireland
# ====
bs_nir <- 
  read_xlsx("data_input/Northern_Ireland/MonthlyBirths.xlsx",
            skip = 2) %>% 
  drop_na(`2006`) %>% 
  rename(mth = 1) %>% 
  gather(-mth, key = Year, value = Births) %>% 
  group_by(Year) %>% 
  summarise(Births = sum(Births)) %>% 
  ungroup() %>% 
  mutate(Year = recode(Year,
                       "2020*" = "2020",
                       "2021P" = "2021"),
         Year = Year %>% as.double()) %>% 
  drop_na() %>% 
  filter(Year >= 2010) %>% 
  mutate(Code = "NIR",
         Source = "nisra")

sum_bs_nir_1521 <- sum_bts_source(bs_nir %>% 
                                    filter(Year >= 2015))
sum_bs_nir_1014 <- sum_bts_source(bs_nir %>% 
                                    filter(Year < 2015))

# ======

# Australia
# ====
bs_aus <- 
  read_csv("data_input/Australia/Births registered, 1932 to 2020(a).csv",
           skip = 1) %>% 
  rename(Births = 2) %>% 
  mutate(Year = Year %>% as.double()) %>% 
  drop_na() %>% 
  filter(Year >= 2010) %>% 
  mutate(Code = "AUS",
         Source = "abs")

sum_bs_aus_1521 <- sum_bts_source(bs_aus %>% 
                                    filter(Year >= 2015))
sum_bs_aus_1014 <- sum_bts_source(bs_aus %>% 
                                    filter(Year < 2015))

# ======

# merging births from all sources
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
comp_bts_1521 <- 
  bind_rows(sum_bs_unpd_1521,
            sum_bs_stf_1521,
            sum_bs_hmd_1521,
            sum_bs_eur_1521,
            sum_bs_ccd_1521,
            sum_bs_ann_1521,
            sum_bs_who_1521,
            sum_bs_mys_1521,
            sum_bs_col_1521,
            sum_bs_ecu_1521,
            sum_bs_bra_1521,
            sum_bs_mex_1521,
            sum_bs_ury_1521,
            sum_bs_cri_1521,
            sum_bs_cub_1521,
            sum_bs_aus_1521,
            sum_bs_nir_1521) %>% 
  arrange(Code) %>% 
  # unique sources
  group_by(Code) %>% 
  filter(years == max(years)) %>% 
  filter(Births == max(Births)) %>% 
  filter((any(Source == "unicef_ccd") & Source == "unicef_ccd") | all(Source != "unicef_ccd")) %>%
  filter((any(Source == "unicef_ann") & Source == "unicef_ann") | all(Source != "unicef_ann")) %>%
  filter((any(Source == "country_public") & Source == "country_public") | all(Source != "country_public")) %>%
  filter((any(Source == "hmd") & Source == "hmd") | all(Source != "hmd")) %>%
  filter((any(Source == "stff") & Source == "stff") | all(Source != "stff")) %>%
  filter((any(Source == "who_mort_db") & Source == "who_mort_db") | all(Source != "who_mort_db")) %>%
  mutate(id = 1:n()) %>% 
  filter(id == n()) %>% 
  mutate(best = ifelse(n() == 1, Source, NA))

sel_bts_1521 <- 
  comp_bts_1521 %>% 
  select(Code, Source)

out_bts_1521 <- 
  bind_rows(bs_unpd,
            bs_stf,
            bs_hmd,
            bs_eur,
            bs_ccd,
            bs_ann,
            bs_who,
            bs_mys,
            bs_col,
            bs_ecu,
            bs_bra,
            bs_mex,
            bs_ury,
            bs_cri,
            bs_cub,
            bs_aus,
            bs_nir) %>% 
  filter(Year >= 2015) %>% 
  semi_join(sel_bts_1521) %>% 
  arrange(Code, Year)

# for years prior to 2015
comp_bts_1014 <- 
  bind_rows(sum_bs_unpd_1014,
            sum_bs_stf_1014,
            sum_bs_hmd_1014,
            sum_bs_eur_1014,
            sum_bs_ann_1014,
            # sum_bs_ccd_1014,
            sum_bs_who_1014,
            # sum_bs_mys_1014,
            sum_bs_col_1014,
            sum_bs_ecu_1014,
            # sum_bs_bra_1014,
            # sum_bs_mex_1014,
            sum_bs_ury_1014,
            sum_bs_cri_1014,
            sum_bs_cub_1014,
            sum_bs_aus_1014,
            sum_bs_nir_1014) %>% 
  arrange(Code) %>% 
  # unique sources
  group_by(Code) %>% 
  # max age definition
  filter(years == max(years)) %>% 
  filter(Births == max(Births)) %>% 
  filter((any(Source == "unicef_ccd") & Source == "unicef_ccd") | all(Source != "unicef_ccd")) %>%
  filter((any(Source == "country_public") & Source == "country_public") | all(Source != "country_public")) %>%
  filter((any(Source == "hmd") & Source == "hmd") | all(Source != "hmd")) %>%
  filter((any(Source == "stff") & Source == "stff") | all(Source != "stff")) %>%
  filter((any(Source == "who_mort_db") & Source == "who_mort_db") | all(Source != "who_mort_db")) %>%
  mutate(id = 1:n()) %>% 
  filter(id == n()) %>% 
  mutate(best = ifelse(n() == 1, Source, NA))

sel_bts_1014 <- 
  comp_bts_1014 %>% 
  select(Code, Source)

out_bts_1014 <- 
  bind_rows(bs_unpd,
            bs_stf,
            bs_hmd,
            bs_eur,
            bs_ccd,
            bs_who,
            bs_mys,
            bs_col,
            bs_ecu,
            bs_bra,
            bs_mex,
            bs_ury,
            bs_cri,
            bs_cub,
            bs_aus,
            bs_nir) %>% 
  filter(Year < 2015) %>% 
  semi_join(sel_bts_1014) %>% 
  arrange(Code, Year)

out_bts <- 
  bind_rows(out_bts_1014,
            out_bts_1521) %>% 
  arrange(Code, Year) %>% 
  unique()

unique(out_bts$Code)

write_rds(out_bts, "data_inter/annual_births.rds")

out_bts <- read_rds("data_inter/annual_births.rds")
