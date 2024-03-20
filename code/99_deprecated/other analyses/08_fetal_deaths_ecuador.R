library(here)
source(here("Code", "00_functions.R"))
library(haven)

# fetal deaths in Ecuador
# microdata 2015-2019


# fetal deaths
# ============

fds_files <- c(paste0("Data/Ecuador/fetal_deaths/EDF_", c(2015, 2017:2020), ".sav"),
                  "Data/Ecuador/fetal_deaths/Defunciones Fetales 2016.sav")

test <- 
  read_sav(i)

unique(test$anio_insc)

fds <- list()
for(i in fds_files){
  fds[[i]] <- 
    read_sav(i) %>% 
    as_tibble() %>% 
    select(sem_gest,
           month = mes_fall,
           year = anio_fall,
           year_reg = anio_insc) %>% 
    mutate(weeks = case_when(sem_gest <= 27 ~ "<=27",
                             sem_gest >= 28 ~ ">=28",
                             TRUE ~ "other")) %>% 
    group_by(weeks, month, year) %>% 
    summarise(fds = n()) %>% 
    ungroup()
  
}

fds2 <- 
  fds %>% 
  bind_rows() %>% 
  mutate(date = make_date(d = 15, m = month, y = year)) %>% 
  group_by(date, weeks, year) %>% 
  summarise(fds = sum(fds)) %>% 
  ungroup() %>% 
  filter(year >= 2015)

fds3 <- 
  fds2 %>% 
  bind_rows(
    fds2 %>% 
      group_by(date, year) %>% 
      summarise(fds = sum(fds)) %>% 
      ungroup() %>% 
      mutate(weeks = "total")) 
    
unique(fds3$place)
unique(fds3$year_reg)

# ======


# births
# ======
births_files <- paste0("Data/Ecuador/births/ENV_", 2015:2020, ".sav")


bts <- list()
for(i in births_files){
  bts[[i]] <- 
    read_sav(i) %>% 
    as_tibble() %>% 
    group_by(anio_nac, mes_nac) %>% 
    summarise(bts = n()) %>% 
    ungroup()
}

bts2 <- 
  bts %>% 
  bind_rows() %>% 
  group_by(anio_nac, mes_nac) %>% 
  summarise(bts = sum(bts)) %>% 
  ungroup() %>% 
  mutate(date = make_date(d = 15, m = mes_nac, y = anio_nac)) %>% 
  filter(anio_nac >= 2015) %>% 
  select(date, bts)

bts2 %>% 
  ggplot()+
  geom_line(aes(date, bts))

# ====


# rates
# =====

rates <- 
  fds3 %>% 
  left_join(bts2) %>% 
  mutate(rate = fds / (bts + fds))


write_rds(rates, "Output/stillbirths/ecuador_monthly_stillbirths_births_2015_2020.rds")


rates %>% 
  ggplot()+
  geom_line(aes(date, rate))+
  facet_wrap(~ weeks, scales = "free")+
  theme_bw()
ggsave("Figures/fetal_deaths/ecuador_monthly_15_21.png", dpi = 1000)

# ==================



# trimester rates
# ===============
rates_qts <- 
  rates %>% 
  mutate(month = month(date),
         month = case_when(month %in% 1:3 ~ 2,
                           month %in% 4:6 ~ 5,
                        month %in% 7:9 ~ 8,
                        month %in% 10:12 ~ 11),
         date = make_date(d = 15, m = month, y = year)) %>% 
  group_by(date, weeks) %>% 
  summarise(fds = sum(fds),
            bts = sum(bts)) %>% 
  ungroup() %>% 
  mutate(rate = fds / (bts + fds))

rates_qts %>% 
  ggplot()+
  geom_line(aes(date, rate))+
  facet_wrap(~ weeks, scales = "free")+
  theme_bw()
ggsave("Figures/fetal_deaths/ecuador_quarters_15_21.png", dpi = 1000)
