rm (list = ls()); gc()
source("code/00_functions.R")

# exclude populations that are subsets of other populations
cts_exclude <- c("GBRCENW", "GBRTENW", "GBR_SCO", "GBR_NIR",
                 "DEUTE", "DEUTW", 
                 "FRACNP", 
                 "NZL_NM", "NZL_MA")

# obtaining all country codes from the HMD 
cds_hmd <- getHMDcountries()[3] %>% pull()
cds_hmd2 <- cds_hmd[!cds_hmd %in% cts_exclude]
# HMD user and password
# ~~~~~~~~~~~~~~~~~~~~
hmd_us <- Sys.getenv("hmd_us")
hmd_pw <- Sys.getenv("hmd_pw")

# identifying those with data for 2020
hmd <- tibble()
for(ct in cds_hmd2){
  
  cat(paste0(ct, "\n"))
  chunk_d <- 
    readHMDweb(ct, "Deaths_1x1", hmd_us, hmd_pw) %>%
    filter(Year >= 2010) %>%
    as_tibble() %>%
    mutate(Code = ct)

  hmd <- 
    hmd %>%
    bind_rows(chunk_d)
}

hmd_out <- 
  hmd %>%
  # only countries with data in 2020
  group_by(Code) %>% 
  filter(max(Year) >= 2020) %>% 
  ungroup() %>% 
  select(-OpenInterval) %>% 
  gather(Female, Male, Total, key = Sex, value = Deaths) %>% 
  mutate(Sex = recode(Sex,
                      "Female" = "f",
                      "Male" = "m",
                      "Total" = "t"),
         Code = case_when(Code == "GBR_NP" ~ "GBR",
                          Code == "NZL_NP" ~ "NZL",
                          Code == "DEUTNP" ~ "DEU",
                          Code == "FRATNP" ~ "FRA",
                          TRUE ~ Code),         
         Country = countrycode(Code, origin = "iso3c",
                               destination = "country.name"),
         Country = case_when(Code == "GBR-ENW" ~ "England and Wales",
                             Code == "GBR-NIR" ~ "Northern Ireland",
                             Code == "GBR-SCO" ~ "Scotland",
                             Code == "USA" ~ "USA",
                             TRUE ~ Country)) %>% 
  filter(Age <= 24) %>% 
  mutate(Age = case_when(Age == 0 ~ 0,
                         Age %in% 1:4 ~ 1,
                         Age %in% 5:24 ~ Age - Age%%5)) %>% 
  group_by(Country, Code, Year, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Source = "hmd")

unique(hmd_out$Code)
unique(hmd_out$Country)

write_rds(hmd_out, "data_inter/hmd.rds")


