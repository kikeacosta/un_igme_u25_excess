rm (list = ls()); gc()
source("Code/00_functions.R")

# selected sources
srs_dts <- read_rds("data_inter/sources_unpd_dts.rds")
srs_bts <- read_rds("data_inter/sources_unpd_bts.rds")
srs_sbs <- read_rds("data_inter/sources_unpd_sbs.rds")
srs_neo <- read_rds("data_inter/sources_unpd_neo.rds")


# all sources
raw_dts <- read_rds("data_inter/unpd_deaths_raw.rds")
raw_bts <- read_rds("data_inter/unpd_births_raw.rds")
raw_sbs <- read_rds("data_inter/unpd_stillbirths_raw.rds")
raw_neo <- read_rds("data_inter/unpd_neonatal_raw.rds")

extract_srs <- function(raw){
  raw %>% 
    as_tibble() %>% 
    select(Country = LocName,
           DataSourceAuthor, 
           DataSourceName,
           DataSourceShortName,
           DataCatalogName,
           DataProcess,
           DataProcessType) %>% 
    unique() %>% 
    mutate(Source = DataSourceName)
}

all_srs_dts <- 
  extract_srs(raw_dts)

all_srs_bts <- 
  extract_srs(raw_bts)

all_srs_sbs <- 
  extract_srs(raw_sbs)

all_srs_neo <- 
  extract_srs(raw_neo)


# all source data ====
# ~~~~~~~~~~~~~~~~~~~~

# deaths
dts <- 
  srs_dts %>% 
  select(Country, Code, Source) %>% 
  unique() %>% 
  left_join(all_srs_dts) %>% 
  mutate(measure = "deaths")

# infant
inf <- 
  srs_dts %>% 
  filter(Age == 0 & age_up == 0, Sex == "t") %>% 
  select(Country, Code, Source) %>% 
  unique() %>% 
  left_join(all_srs_dts) %>% 
  mutate(measure = "infant")

# births
bts <- 
  srs_bts %>% 
  select(Country, Code, Source) %>% 
  unique() %>% 
  left_join(all_srs_bts) %>% 
  mutate(measure = "births")

# neonatal
neo <- 
  srs_neo %>% 
  select(Country = country, Code = code, Source = source) %>% 
  unique() %>% 
  left_join(all_srs_neo) %>% 
  mutate(measure = "neonatal")

# stillbirths
sbs <- 
  srs_sbs %>% 
  select(Country = country, Code = code, Source = source) %>% 
  unique() %>% 
  left_join(all_srs_sbs) %>% 
  mutate(measure = "stillbirths")

sources <- 
  bind_rows(dts,
            inf,
            bts,
            neo,
            sbs)

write_csv(sources, "data_inter/sources_unpd_details.csv")

