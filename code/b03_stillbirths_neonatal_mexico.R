rm (list = ls())
source("code/00_functions.R")


# births
# ~~~~~~
bts <- tibble()

for(y in 2015:2020){
  bts_temp <- 
    read_csv(paste0("data_input/Mexico/births/mx_annual_births_state_", y, ".csv"),
              skip = 9) %>% 
    rename(state_cod = 1,
           state = 2) %>% 
    drop_na(state) %>% 
    select(-Total, -`No especificado`) %>% 
    gather(-state, -state_cod, key = year, value = bts) %>% 
    mutate(year = year %>% as.double()) %>% 
    filter(year == y)
  
  bts <- 
    bts %>% 
    bind_rows(bts_temp)
}

bts2 <- 
  bts %>% 
  group_by(state_cod, state, year) %>% 
  summarise(bts = sum(bts)) %>% 
  ungroup() %>% 
  mutate(state = ifelse(state == "Total", "All", state))


# stillbirths
# ~~~~~~~~~~~
sbs <- tibble()
# y <- 2015
for(y in 2015:2020){
  sbs_temp <- 
    read_csv(paste0("data_input/Mexico/fetal_deaths/state_gestation_", y, ".csv"),
              skip = 9,
             locale = locale(encoding="latin1")) %>% 
    drop_na(Total) %>% 
    rename(weeks = 1) %>% 
    gather(-weeks, key = state, value = value) %>% 
    replace_na(list(value = 0)) %>% 
    mutate(year = y) %>% 
    filter(weeks != "No especificado")
  
  sbs <- 
    sbs %>% 
    bind_rows(sbs_temp)
}

unique(sbs$weeks)
unique(sbs$state)

tot <- 
  sbs %>% 
  filter(weeks == "Total") %>% 
  rename(sbs_tot = value) %>% 
  select(-weeks)

sbs2 <- 
  sbs %>% 
  filter(weeks != "Total") %>% 
  replace_na(list(value = 0)) %>% 
  group_by(state, year) %>% 
  mutate(fr = value / sum(value)) %>% 
  left_join(tot) %>% 
  mutate(value2 = round(fr * sbs_tot),
         weeks = str_sub(weeks, 4, 5) %>% as.double()) %>% 
  filter(weeks >= 28) %>% 
  group_by(state, year) %>% 
  summarise(value = sum(value2)) %>% 
  ungroup() %>% 
  mutate(measure = "sbs") %>% 
  mutate(state = ifelse(state == "Total", "All", state))


# neonatal
# ~~~~~~~~
inf <- tibble()
y <- 2015
for(y in 2015:2020){
  inf_temp <- 
    read_csv(paste0("data_input/Mexico/infant_deaths/mx_inf_deaths_state_", y, ".csv"),
             skip = 11,
             locale = locale(encoding="latin1")) %>% 
    rename(measure = 1) %>% 
    drop_na(2) %>% 
    gather(-measure, key = state, value = value) %>% 
    mutate(year = y,
           measure = case_when(measure == "Mortalidad neonatal precoz" ~ "neo",
                               measure == "Mortalidad neonatal" ~ "neo",
                               measure == "Mortalidad postneonatal" ~ "pos",
                               measure == "Total" ~ "inf"),
           value = value %>% as.double(),
           state = ifelse(state == "Total", "All", state)) %>% 
    group_by(state, measure, year) %>% 
    summarise(value = sum(value)) %>% 
    ungroup()
  
  inf <- 
    inf %>% 
    bind_rows(inf_temp)
}

unique(inf$measure)
unique(inf$state)

tot <- 
  inf %>% 
  filter(measure == "inf") %>% 
  rename(dts_tot = value) %>% 
  select(-measure)

inf2 <- 
  inf %>% 
  filter(measure != "inf") %>% 
  replace_na(list(value = 0)) %>% 
  group_by(state, year) %>% 
  mutate(fr = value / sum(value)) %>% 
  left_join(tot) %>% 
  mutate(value2 = round(fr * dts_tot)) %>% 
  select(state, measure, year, value = value2) %>% 
  bind_rows(inf %>% 
              filter(measure == "inf")) %>% 
  arrange(state, measure, year)


# putting all together
all <- 
  bind_rows(sbs2,
            inf2) %>%
  left_join(bts2)

mexico <- 
  all %>% 
  filter(state == "All") %>% 
  select(-state_cod, -state) %>% 
  mutate(country = "Mexico",
         code = "MEX",
         source = "inegi")



# saving
write_rds(mexico, "data_inter/neo_sbs_mexico.rds")
write_rds(all, "data_inter/neo_sbs_mexico_state.rds")



