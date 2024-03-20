rm (list = ls())
source("Code/00_functions.R")

# loading excess estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~
dts_fit <- 
  read_rds("Output/p_scores_excess_deaths.rds") %>% 
  select(-age_up) %>% 
  mutate(Age = recode(Age,
                      "1-4" = "1_4", 
                      "0-4" = "0_4", 
                      "5-9" = "5_9", 
                      "10-14" = "10_14", 
                      "15-19" = "15_19", 
                      "20-24" = "20_24"),
         Age = factor(Age, 
                      levels = c("Infant", "1_4", "0_4", "5_9", 
                                 "10_14", "15_19", "20_24"))) %>% 
  arrange(Country, Code, Age, Sex, Year)

exc <- 
  dts_fit %>% 
  filter((is.na(psc) | psc == Inf | psc == 0) | 
           (is.na(lp) | lp == Inf) |
           (is.na(up) | up == Inf)) %>% 
  select(Country, Age) %>% 
  unique() %>% 
  mutate(exc = 1)
  

out <- 
  dts_fit %>% 
  left_join(exc) %>% 
  filter(is.na(exc)) %>% 
  select(-exc, -w)

write_csv(out, paste0("data_output/preliminary_pscores_", today(), ".csv"))
