source("Code/00_functions.R")

cds_hmd <- getHMDcountries()

# cds_hmd <- c("AUS", "AUT")
# Extract deaths and exposures in 2020 from the HMD

# HMD user and password
# ~~~~~~~~~~~~~~~~~~~~
hmd_us <- "kikepaila@gmail.com"
hmd_pw <- "secreto"

# identifying those with data for 2020
hmd_2020 <- tibble()
for(ct in cds_hmd){
  chunk_d <- readHMDweb(ct, "Deaths_1x1", hmd_us, hmd_pw) %>%
    filter(Year == 2020) %>%
    as_tibble() %>%
    mutate(Code = ct)
  
  chunk_p <- readHMDweb(ct, "Exposures_1x1", hmd_us, hmd_pw) %>%
    filter(Year == 2020) %>%
    as_tibble() %>%
    mutate(Code = ct)
  
  hmd_2020 <- hmd_2020 %>%
    bind_rows(chunk_d, chunk_p)
}

cts_2020 <- unique(hmd_2020$Code)
hmd_long <- tibble()
for(ct in cts_2020){
  chunk_d <- readHMDweb(ct, "Deaths_1x1", hmd_us, hmd_pw) %>%
    filter(Year >= 2015) %>%
    as_tibble() %>%
    mutate(Code = ct,
           measure = "deaths")
  
  chunk_p <- readHMDweb(ct, "Exposures_1x1", hmd_us, hmd_pw) %>%
    filter(Year >= 2015) %>%
    as_tibble() %>%
    mutate(Code = ct,
           measure = "pop")
  
  hmd_long <- hmd_long %>%
    bind_rows(chunk_d, chunk_p)
}

hmd_long2 <- 
  hmd_long %>% 
  select(-OpenInterval) %>% 
  gather(Female, Male, Total, key = sex, value = value) %>% 
  mutate(sex = recode(sex,
                      "Female" = "f",
                      "Male" = "m",
                      "Total" = "t"),
         country = countrycode(Code, origin = "iso3c",
                              destination = "country.name")) %>% 
  rename(year = Year,
         age = Age,
         code = Code) %>% 
  spread(measure, value) %>% 
  mutate(source = "hmd") %>% 
  rename(population = pop)

unique(hmd_long2$country)

# reading deaths data from WHO and others
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dts_who <- 
  read_rds("Output/annual_deaths_pop.rds") %>% 
  rename(country = Country,
         code = Code,
         year = Year,
         sex = Sex,
         age = Age,
         deaths = Deaths,
         population = Population,
         source = Source) %>% 
  select(-mx) %>% 
  mutate(year = year %>% as.double()) %>% 
  filter(!code %in% cts_2020)

# merging previous who and hmd data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dts_out <- 
  bind_rows(dts_who,
            hmd_long2)

unique(dts_out$source)
