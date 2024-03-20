library(here)
source(here("Code", "00_functions.R"))
library(haven)


# deaths from microdata
# =====================

db_cr <- read_sav("Data/Costa Rica/Defu2015-2020.OPS.sav") %>% 
  as_tibble()

unique(db_cr$sexo)
unique(db_cr$codigo)
unique(db_cr$edad) %>% sort
unique(db_cr$causa) %>% sort

dts <- 
  db_cr %>% 
  mutate(edad = edad %>% as.double(),
         codigo = codigo %>% as.integer(),
         year = anodef %>% as.double(),
         age = case_when(
           codigo == 1 | (codigo == 2 & edad <= 7) ~ "neo_ear",
           (codigo == 2 & edad > 7) | codigo == 3 | (codigo == 4 & edad == 1) ~ "neo_lat",
           (codigo == 4 & edad > 1) ~ "post_neo",
           codigo == 5 & edad %in% 1:4 ~ "1_4",
           codigo == 5 & edad %in% 5:9 ~ "5_9",
           codigo == 5 & edad %in% 10:14 ~ "10_14",
           codigo == 5 & edad %in% 15:24 ~ "15_24",
           TRUE ~ "other")
  ) %>% 
  select(edad, codigo, age, year) %>% 
  group_by(year, age) %>% 
  summarise(dts = n()) %>% 
  filter(age != "other",
         year >= 2015)

bts <- 
  read_xlsx(here("Data", "Costa Rica", "repoblacevcygbmiisem2021.xlsx"),
            sheet = "Cuadro 1",
            skip = 6) %>% 
  select(year = 1,
         bts = 2,
         inf_dts = 3) %>% 
  drop_na() %>% 
  mutate(year = str_remove(year, "a/"),
         year = year %>% as.double())

ages <- unique(dts$age) %>% sort()

inf_dts <- 
  dts %>% 
  filter(age %in% ages[5:8]) %>% 
  left_join(bts) %>% 
  group_by(year) %>% 
  mutate(inf_dts_sum = sum(dts)) %>% 
  ungroup()


inf_dts2 <-  
  inf_dts %>% 
  mutate(mx = dts / bts)

inf_dts2 %>% 
  ggplot()+
  geom_line(aes(year, mx))+
  facet_wrap(~ age)

dts %>% 
  filter(age %in% ages[1:4]) %>% 
  ggplot()+
  geom_line(aes(year, dts))+
  facet_wrap(~ age, scales = "free")
# =====

# first semester as in the report
# ===============================
# "https://www.inec.cr/sites/default/files/documetos-biblioteca-virtual/repoblacevcygbmiisem2021.xlsx

dts_mth <- 
  db_cr %>% 
  mutate(edad = edad %>% as.double(),
         codigo = codigo %>% as.integer(),
         year = anodef %>% as.double(),
         month = mesdef %>% as.double(),
         age = case_when(
           (codigo == 1) | (codigo == 2 & edad <= 7) ~ "neo_ear",
           (codigo == 2 & edad > 7) | codigo == 3 | (codigo == 4 & edad == 1) ~ "neo_lat",
           (codigo == 4 & edad > 1) ~ "post_neo",
           codigo == 5 & edad %in% 1:4 ~ "1_4",
           codigo == 5 & edad %in% 5:9 ~ "5_9",
           codigo == 5 & edad %in% 10:14 ~ "10_14",
           codigo == 5 & edad %in% 15:24 ~ "15_24",
           TRUE ~ "other"
           ),
         date = make_date(d = 15, m = month, y = year)) %>% 
  select(edad, codigo, age, date) %>% 
  group_by(date, age) %>% 
  summarise(dts = n()) %>% 
  filter(age != "other",
         date >= "2015-01-01")

unique(dts_mth$age)

inf_sem_1 <- 
  dts_mth %>% 
  mutate(month = month(date),
         year = year(date),
         sem = ifelse(month <= 6, 1, 2),
         age2 = ifelse(age %in% c("neo_ear", "neo_lat", "post_neo"), "inf", "chd")) %>% 
  group_by(year, sem, age2) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup() %>% 
  filter(sem == 1,
         age2 == "inf") %>% 
  left_join(bts) %>% 
  mutate(mx = dts / bts)


inf_sem_1 %>% 
  ggplot()+
  geom_line(aes(year, mx))+
  geom_point(aes(year, mx))

bts2 <- 
  bts %>% 
  mutate(mx = inf_dts / bts)

bts2 %>% 
  ggplot()+
  geom_line(aes(year, mx))

# ======


# annual births
# =============

years <- 2015:2020.
files_names <- c("repoblacevnac2015.xls", 
                 "repoblacevnacdefinitivos2016_0.xlsx",
                 "repoblacevnac_2017.xls", 
                 "repoblacevnac2018.xls", 
                 "repoblacevnacdefinitivos2019.xls",
                 "repoblacnac2020-preliminares.xls")
i <- 4
bts <- list()
for(i in 1:length(files_names)){
  if(str_detect(files_names[[i]], "xlsx")){
    bts[[i]] <- 
      read_xlsx(here("Data", "Costa Rica", "births_annual", files_names[[i]]),
               sheet = "C1",
               skip = 5) %>% 
      select(place = 1,
             bts = 2) %>% 
      filter(place == "Costa Rica") %>% 
      mutate(year = years[i])
      
  }else{
    bts[[i]] <- 
      read_xls(here("Data", "Costa Rica", "births_annual", files_names[[i]]),
               sheet = "C1",
               skip = 5) %>% 
      select(place = 1,
             bts = 2) %>% 
      filter(place == "Costa Rica") %>% 
      mutate(year = years[i])
  }
}

bts2 <- 
  bts %>% 
  bind_rows() %>% 
  select(-place) %>% 
  mutate(bts = bts %>% as.double())

# =======================

# infant death rates 
# =======================

inf_dts <- 
  dts %>% 
  filter(age %in% ages[5:8]) %>% 
  left_join(bts2) %>% 
  mutate(mx = dts / bts)

db_inf <- 
  inf_dts %>% 
  rename(deaths = dts,
         births = bts)

write_rds(db_inf, "Output/infant/costa_rica_annual_inf_deaths_births_2015_2020.rds")


inf_dts %>% 
  ggplot()+
  geom_line(aes(year, mx))+
  geom_point(aes(year, mx))+
  facet_wrap(~ age, scales = "free")+
  theme_bw()
ggsave("Figures/infant/infant_mortality_costa_rica_year.png", dpi = 1000)


