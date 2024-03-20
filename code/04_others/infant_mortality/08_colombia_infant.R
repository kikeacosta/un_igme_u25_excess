library(here)
source(here("Code", "00_functions.R"))
library(haven)

# infant and child mortality in Colombia


# deaths by age
# =============
files_names <- list.files(here("Data", "Colombia", "non_fetal_annual"), pattern = "xls")
years <- 2015:2020

nfd <- list()
for(i in 1:length(files_names)){
  nfd[[i]] <- 
    read_xls(here("Data", "Colombia", "non_fetal_annual", files_names[[i]]),
             skip = 10) %>% 
    select(age = 1,
           ds = 2) %>% 
    drop_na() %>% 
    mutate(year = years[i])
}

nfd[[i]] <- 
  read_xlsx(here("Data", "Colombia", "non_fetal_annual", files_names[[i]]),
            sheet = "Cuadro1",
           skip = 8) %>% 
  select(age = 1,
         ds = 2) %>% 
  drop_na() %>% 
  mutate(year = years[i])

nfd2 <- 
  nfd %>% 
  bind_rows()

ages <- unique(nfd2$age)
ages[2:14]

nfd3 <- 
  nfd2 %>% 
  filter(age %in% ages[2:14]) %>% 
  mutate(age = case_when(age %in% ages[2:4] ~ "neo_ear",
                   age %in% ages[5:6] ~ "neo_lat",
                   age %in% ages[7:8] ~ "post_neo",
                   age %in% ages[9:10] ~ "1_4",
                   age %in% ages[11] ~ "5_9",
                   age %in% ages[12] ~ "10_14",
                   age %in% ages[13:14] ~ "15_24")) %>% 
  group_by(age, year) %>% 
  summarise(ds = sum(ds))

# ======

# births
# ======

files_names <- list.files(here("Data", "Colombia", "births", "annual"), pattern = "xls")
years <- 2015:2020

bts <- list()
for(i in 1:length(files_names)){
  bts[[i]] <- 
    read_xls(here("Data", "Colombia", "births", "annual", files_names[[i]]),
             skip = 10) %>% 
    select(age_m = 1,
           bs = 2) %>% 
    filter(age_m == "TOTAL NACIONAL") %>% 
    mutate(year = years[i])
}

bts[[i]] <- 
  read_xlsx(here("Data", "Colombia", "births", "annual", files_names[[i]]),
            sheet = "Cuadro1",
            skip = 8) %>% 
  select(age_m = 1,
         bs = 2) %>% 
  filter(age_m == "Total Nacional") %>% 
  mutate(year = years[i])

bts2 <- 
  bts %>% 
  bind_rows() %>% 
  select(-age_m)

# ========


# death counts and rates
db <- 
  nfd3 %>% 
  left_join(bts2) %>% 
  mutate(mx = ds / bs)

ages2 <- unique(db$age)

db_inf <- 
  db %>% 
  filter(age %in% ages2[5:7]) %>% 
  rename(deaths = ds,
         births = bs)

write_rds(db_inf, "Output/infant/colombia_annual_inf_deaths_births_2015_2020.rds")


db %>% 
  filter(age %in% ages2[5:7]) %>% 
  ggplot()+
  geom_point(aes(year, mx))+
  geom_line(aes(year, mx))+
  facet_wrap(~age, scales = "free")+
  theme_bw()
ggsave("Figures/infant/infant_mortality_colombia_year.png", dpi = 1000)


db %>% 
  filter(age %in% ages2[1:4]) %>% 
  ggplot()+
  geom_point(aes(year, ds))+
  geom_line(aes(year, ds))+
  facet_wrap(~age, scales = "free")+
  theme_bw()
# ggsave("Figures/infant/child_mortality_colombia_year.png", dpi = 1000)
