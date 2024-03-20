rm (list = ls())
source("Code/00_functions.R")

# loading baselines estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

db_in <- read_rds("data_inter/sbs_neo_baselines.rds")

sbs_neo <- 
  db_in %>% 
  mutate(psc = value / bsn,
         psc_lp = value / up,
         psc_up = value / lp) %>%
  mutate(excess = case_when(year == 2020 & psc_up < 1 ~ "Negative",
                            year == 2020 & psc_lp > 1 ~ "Positive",
                            year < 2020 ~ NA_character_,
                            TRUE ~ "No-excess"),
         bts = ifelse(country == "South Africa", 899303, bts),
         exposure = ifelse(country == "South Africa", 899303, exposure),
         measure = recode(measure,
                          "neo_r" = "neo",
                          "sbs_r" = "sbs")) %>% 
  filter(measure != "sbs_neo")

write_csv(sbs_neo, "data_output/preliminary_stillbirths_neonatal_pscores_20220607.csv")
