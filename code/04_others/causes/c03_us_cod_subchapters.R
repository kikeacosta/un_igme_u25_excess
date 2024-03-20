source("Code/causes/00_functions_cause_analyses.R")


# exposures
dts <- 
  read_tsv("Data/USA/wonder_under_25_monthly_subchapters.txt",
           col_types = cols(.default = "c"))

dts2 <- 
  dts %>% 
  rename(age = 3,
         cod = 11,
         year = Year,
         dts = Deaths) %>% 
  mutate(dts = dts %>% as.double()) %>% 
  drop_na(year) %>% 
  group_by(year, age, cod) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup()
