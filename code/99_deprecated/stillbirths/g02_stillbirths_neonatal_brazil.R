rm (list = ls())
source("Code/00_functions.R")

# state codes
# ~~~~~~~~~~~
reg_codes <- 
  read_csv("Documents/bra_state_codes_names.csv")

bts <- 
  read_xlsx("Data/Brazil/births/br_annual_births_state_format.xlsx") 
  

# births
# ~~~~~~

bts <- 
  read_xlsx("Data/Brazil/births/br_annual_births_state_format.csv") %>% 
  rename(state_num = 1,
         state = 2) %>% 
  select(-Total) %>% 
  drop_na(state_num) %>% 
  gather(-state_num, -state, key = year, value = bts) %>% 
  mutate(year = str_replace(year, "X", ""),
         year = year %>% as.double(),
         state_num = state_num %>% as.double()) %>% 
  left_join(reg_codes) %>% 
  select(state_iso, state, year, bts) %>% 
  replace_na(list(state_iso = "BR"))


# stillbirths
# ~~~~~~~~~~~
sbs <- tibble()

for(y in 2015:2020){
  sbs_temp <- 
    read.csv(paste0("Data/Brazil/fetal_deaths/state_gestation_", y, "_format.csv")) %>% 
    rename(state_num = 1) %>% 
    drop_na(2) %>% 
    gather(-state_num, key = weeks, value = value) %>% 
    filter(weeks != "Ignorado") %>% 
    mutate(year = y,
           value = value %>% as.double(),
           state_num = str_sub(state_num, 1, 2))
  
  sbs <- 
    sbs %>% 
    bind_rows(sbs_temp)
}

unique(sbs$weeks)
unique(sbs$state_num)

sbs_tot <- 
  sbs %>% 
  filter(weeks == "Total") %>% 
  rename(sbs_tot = value) %>% 
  select(-weeks)

sbs2 <- 
  sbs %>% 
  filter(weeks != "Total") %>% 
  replace_na(list(value = 0)) %>% 
  group_by(state_num, year) %>% 
  mutate(fr = value / sum(value)) %>% 
  left_join(sbs_tot) %>% 
  mutate(value2 = fr * sbs_tot,
         state_num = state_num %>% as.double()) %>% 
  filter(!weeks %in% c("Menos.de.22.semanas", "X22.a.27.semanas")) %>% 
  group_by(state_num, year) %>% 
  summarise(value = round(sum(value2))) %>% 
  ungroup() %>% 
  left_join(reg_codes) %>% 
  select(state_iso, year, value) %>% 
  replace_na(list(state_iso = "BR")) %>% 
  mutate(measure = "sbs")


# neonatal deaths
# ~~~~~~~~~~~~~~~
inf <- tibble()
# y <- 2015
for(y in 2015:2020){
  inf_temp <- 
    read_csv2(paste0("Data/Brazil/infant_deaths/br_inf_deaths_state_", y, ".csv"),
              skip = 3) %>% 
    rename(measure = 1) %>% 
    drop_na(2) %>% 
    gather(-measure, key = state_iso, value = value) %>% 
    filter(measure != "Menor 1 ano (ign)") %>% 
    mutate(year = y,
           measure = case_when(measure == "0 a 6 dias" ~ "neo",
                           measure == "7 a 27 dias" ~ "neo",
                           measure == "28 a 364 dias" ~ "pos",
                           measure == "Total" ~ "inf"),
           value = value %>% as.double(),
           state_iso = ifelse(state_iso == "Total", "BR", state_iso)) %>% 
    group_by(state_iso, measure, year) %>% 
    summarise(value = sum(value)) %>% 
    ungroup()
  
  inf <- 
    inf %>% 
    bind_rows(inf_temp)
}

unique(inf$measure)
unique(inf$state_iso)

tot <- 
  inf %>% 
  filter(measure == "inf") %>% 
  rename(dts_tot = value) %>% 
  select(-measure)

inf2 <- 
  inf %>% 
  filter(measure != "inf") %>% 
  replace_na(list(value = 0)) %>% 
  group_by(state_iso, year) %>% 
  mutate(fr = value / sum(value)) %>% 
  left_join(tot) %>% 
  mutate(value2 = round(fr * dts_tot)) %>% 
  select(state_iso, measure, year, value = value2) %>% 
  bind_rows(inf %>% 
              filter(measure == "inf")) %>% 
  arrange(state_iso, measure, year)

# putting all together
all <- 
  bind_rows(sbs2,
            inf2) %>%
  left_join(bts)

brazil <- 
  all %>% 
  filter(state_iso == "BR") %>% 
  select(-state_iso) %>% 
  mutate(country = "Brazil",
         code = "BRA",
         source = "datasus")

write_rds(brazil, "data_inter/neo_sbs_brazil.rds")
write_rds(all, "data_inter/neo_sbs_brazil_state.rds")
