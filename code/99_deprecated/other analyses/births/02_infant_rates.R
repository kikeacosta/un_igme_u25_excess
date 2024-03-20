library(here)
source(here("Code", "00_functions.R"))

db_stff <- 
  read_rds("Output/annual_births_stff.rds")

db_ds <- 
  read_rds("Output/annual_deaths_pop.rds") %>% 
  filter(Sex == "t") %>% 
  mutate(Year = as.character(Year) %>% as.integer())

# Identifying countries with infant deaths data in 2020
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cts_inf <- 
  db_ds %>% 
  filter(Age == 1,
         Year == 2020) %>% 
  pull(Country) %>% 
  unique()

db_d_inf <- 
  db_ds %>% 
  filter(Age == 0,
         Country %in% cts_inf) %>% 
  select(Country, Year, Deaths)

db_inf <- 
  db_stff %>% 
  mutate(CountryCode = recode(CountryCode,
                          "DEUTNP" = "DEU",
                          "FRATNP" = "FRA"),
         Country = countrycode(CountryCode, 
                               origin = 'iso3c', 
                               destination = 'country.name'),
         Country = ifelse(Country == "United States", "USA", Country)) %>% 
  drop_na() %>% 
  filter(Country %in% cts_inf) %>% 
  left_join(db_d_inf) %>% 
  drop_na()
  
cts <- unique(db_inf$Country)
out <- list()
c <- "USA" 
for(c in cts){
  temp1 <- 
    db_inf %>% 
    filter(Country == c)
  
  temp2 <- pred_infant_deaths(temp1)
  
  out[[c]] <- 
    temp1 %>% 
    select(Year, Deaths) %>% 
    left_join(temp2) %>% 
    mutate(Country = c)
}

db_pred <- 
  out %>% 
  bind_rows()

db_exc <- 
  db_pred %>% 
  # filter(Year == 2020) %>% 
  mutate(p_score = Deaths / Baseline,
         up = Deaths / lp_baseline,
         lp = Deaths / up_baseline)

write_rds(db_exc, "Output/p_scores_excess_raw_data_infants_inc_births.rds")


