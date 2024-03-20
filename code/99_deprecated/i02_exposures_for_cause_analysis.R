rm (list = ls())
source("Code/00_functions.R")

# exposure data ====
# ~~~~~~~~~~~~~~~~~~
pop <- read_rds("data_inter/exposures_5-year_groups.rds")
dt <- read_rds("data_inter/dts_causes_who.rds")

pop2 <- 
  pop %>% 
  rename_with(tolower) %>% 
  select(-source)
  
dt2 <- 
  dt %>% 
  filter(age <= 20) %>% 
  left_join(pop2)

write_rds(dt2, "data_inter/master_causes_who.rds")

