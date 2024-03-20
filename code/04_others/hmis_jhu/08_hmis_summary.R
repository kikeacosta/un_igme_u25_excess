source("Code/00_functions.R")

all_cts <- read_rds("Output/hmis_all_countries.rds")

unique(all_cts$measure)
dates <- 
  all_cts %>% 
  ungroup() %>% 
  filter(measure %in% c("sbs_r", "neo_r")) %>% 
  select(-measure, - value) %>% 
  unique() %>% 
  group_by(country) %>% 
  filter(date == min(date) | date == max(date)) %>% 
  mutate(type = ifelse(date == min(date), "ini", "end")) %>% 
  ungroup() %>% 
  spread(type, date)

write.excel(dates)

data <- 
  all_cts %>% 
  filter(!measure %in% c("dvs", "mat", "neo_ear", "neo_lat", 
                         "neo_r", "pos", "sbs_r")) %>% 
  select(country, measure) %>% 
  unique() %>% 
  mutate(pres = "X") %>% 
  spread(measure, pres)

write.excel(data)
