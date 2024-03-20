
# exposure data ====
# ~~~~~~~~~~~~~~~~~~
age5 <- read_rds("Output/annual_exposure_5y_groups.rds")
inf <- read_rds("Output/annual_exposure_infant_child.rds")

# countries with mortality data for 2020
cts_mort <- 
  read_csv("Data/WHO/icd_raw.csv",
           col_types = cols(.default = "c")) %>% 
  filter(Year == 2020 & Frmat != "09") %>% 
  pull(name) %>% 
  unique() %>% 
  sort()


age5_2 <- 
  age5 %>% 
  filter(Country %in% cts_mort)

inf2 <- 
  inf %>% 
  filter(Country %in% cts_mort)





