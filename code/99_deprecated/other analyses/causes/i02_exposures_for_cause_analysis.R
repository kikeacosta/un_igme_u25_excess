
# exposure data ====
# ~~~~~~~~~~~~~~~~~~
age5 <- read_rds("data_inter/exposures_5-year_groups.rds")
dt <- read_rds("data_inter/master_causes_who.rds")

# countries with mortality data for 2020
age5_2 <- 
  age5 %>% 
  filter(Country %in% cts_mort)

inf2 <- 
  inf %>% 
  filter(Country %in% cts_mort)





