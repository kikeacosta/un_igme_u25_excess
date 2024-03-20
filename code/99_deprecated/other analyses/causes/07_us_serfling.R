source("Code/causes/00_functions_cause_analyses.R")


# exposures
pop <- 
  read_rds("output/usa/exposures_young_ages_monthly.rds")

# deaths from CDC Wonder ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# infant mortality
dts_inf <- 
  read_tsv("Data/USA/wonder_infant_monthly.txt") %>% 
  select(age = 3,
         date = 7,
         dts = 8) %>% 
  drop_na(date) %>% 
  mutate(date = ymd(paste0(date, '/15'))) %>% 
  filter(date >= "2015-01-01")


# confirmed covid deaths ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
dts_covid_mth <- 
  read_tsv("Data/USA/wonder_under_25_monthly_covid.txt",
           col_types = cols(.default = "c")) %>% 
  select(age = 3,
         date = 7,
         dts = 10) %>% 
  drop_na(date) %>% 
  mutate(dts = dts %>% as.double(),
         date = ymd(paste0(date, '/15')),
         age = ifelse(age == "1", "0-1", age),
         age = factor(age, 
                      levels = c("0-1", "1-4", "5-9", 
                                 "10-14", "15-19", "20-24")),
         code_chp = "U00-U99",
         chapter = "Codes for special purposes",
         code_sub_chp = "U071",
         sub_chp = "COVID-19") %>% 
  filter(year(date) >= 2020) %>% 
  left_join(pop)

dts_covid_annual <- 
  dts_covid_mth %>% 
  group_by(age, code_sub_chp) %>% 
  summarise(dts = sum(dts),
            exposure = sum(exposure)) %>% 
  ungroup()


# all young ages analysis ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db_mth_causes <- 
  read_tsv("Data/USA/wonder_under_25_monthly_subchapters.txt",
           col_types = cols(.default = "c")) %>% 
  select(age = 3,
         date = 7,
         chapter = 8,
         code_chp = 9,
         sub_chp = 10,
         code_sub_chp = 11,
         dts = 12) %>% 
  drop_na(date) %>% 
  mutate(age = ifelse(age == "1", "0-1", age),
         age = factor(age, 
                      levels = c("0-1", "1-4", "5-9", 
                                 "10-14", "15-19", "20-24")),
         dts = dts %>% as.double(),
         date = ymd(paste0(date, '/15')))

dts_u_adjusted <- 
  db_mth_causes %>% 
  filter(code_chp == "U00-U99") %>% 
  left_join(dts_covid_mth %>% 
              select(age,
                     date, 
                     dts_covid = dts)) %>% 
  mutate(dts_covid = ifelse(is.na(dts_covid), 0, dts_covid),
         dts_adj = dts - dts_covid) %>% 
  select(-dts, -dts_covid, dts = dts_adj)


db_mth_causes2 <- 
  db_mth_causes %>% 
  filter(code_chp != "U00-U99") %>% 
  bind_rows(dts_u_adjusted,
            dts_covid_mth %>% select(-exposure))

ags <- db_mth_causes2$age %>% unique

codes <- 
  db_mth_causes2 %>% 
  select(code_chp, chapter, code_sub_chp, sub_chp) %>% 
  unique() 

write.excel(codes)

dts_causes <- 
  db_mth_causes2 %>% 
  filter(code_sub_chp != "U071") %>% 
  select(-sub_chp, -code_chp, -chapter) %>% 
  complete(age, date, code_sub_chp, fill = list(dts = 0)) %>% 
  group_by(age, date) %>% 
  mutate(prop = dts / sum(dts)) %>% 
  group_by(age, code_sub_chp) %>% 
  mutate(avg_prop = mean(prop)) %>% 
  ungroup() %>% 
  filter(avg_prop > 0.007) %>% 
  select(-prop, -avg_prop) 

db_young <- 
  read_tsv("Data/USA/wonder_under_25_monthly.txt") %>% 
  select(age = 3,
         date = 7,
         dts = 8) %>% 
  drop_na(date) %>% 
  mutate(date = ymd(paste0(date, '/15')),
         age = ifelse(age == "1", "0-1", age)) %>% 
  left_join(pop) %>% 
  group_by(age) %>% 
  mutate(t = 1:n(),
         age = factor(age, 
                      levels = c("0-1", "1-4", "5-9", 
                                 "10-14", "15-19", "20-24")))

resid <- 
  dts_causes %>% 
  bind_rows(dts_covid_mth %>% 
              select(age,
                     date, 
                     code_sub_chp,
                     dts)) %>% 
  group_by(age, date) %>% 
  summarise(dts_cod = sum(dts)) %>% 
  left_join(db_young %>% 
              select(age, date, dts_all = dts)) %>% 
  mutate(dts = dts_all - dts_cod,
         code_sub_chp = "Z_residual") %>% 
  select(age, date, code_sub_chp, dts)

# adding residuals by month age
dts_causes2 <- 
  dts_causes %>% 
  bind_rows(resid) %>% 
  arrange(age, date, code_sub_chp) %>% 
  left_join(pop) %>% 
  group_by(age, code_sub_chp) %>% 
  mutate(t = 1:n()) %>% 
  ungroup() %>% 
  mutate(mth = month(date),
         w = ifelse(date <= "2020-03-15", 1, 0))
  
# contribution of selected causes
conts <- 
  dts_causes2 %>% 
  group_by(age, code_sub_chp) %>% 
  mutate(dts_all_sub_chp = sum(dts)) %>% 
  ungroup() %>% 
  group_by(age) %>% 
  mutate(cont = dts_all_sub_chp / sum(dts)) %>% 
  ungroup() %>% 
  select(age, code_sub_chp, cont) %>% 
  unique()


# predicting baselines ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~
bsn_causes <- 
  dts_causes2 %>% 
  group_by(age, code_sub_chp) %>% 
  do(fit_monthly(chunk = .)) %>% 
  ungroup() %>% 
  select(-t, -mth, -w)

# adding covid deaths 
bsn_covid <- 
  dts_covid_mth %>% 
  select(age, date, code_sub_chp, dts, exposure) %>% 
  mutate(bsn = 0,
         ll = 0, 
         ul = 0)
  
bsn_causes2 <- 
  bsn_causes %>% 
  bind_rows(bsn_covid) %>% 
  mutate(age = factor(age, 
                      levels = c("0-1", "1-4", "5-9", 
                                 "10-14", "15-19", "20-24")))

bsn_causes2 %>% 
  filter(age == "0-1",
         code_sub_chp == "P05-P08") %>% 
  ggplot()+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.3)+
  geom_line(aes(date, bsn))+
  geom_point(aes(date, dts))+
  facet_wrap(~age, scales = "free")+
  theme_bw()

excs <- 
  bsn_causes2 %>% 
  filter(date >= "2020-01-01") %>% 
  group_by(age, code_sub_chp) %>% 
  summarise(dts = sum(dts),
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(exc = dts - bsn,
         exc_pos = ifelse(exc > 0, exc, 0),
         exc_neg = ifelse(exc < 0, exc, 0)) %>% 
  group_by(age) %>% 
  mutate(exc_pos = sum(exc_pos),
         exc_neg = sum(exc_neg)) %>% 
  ungroup() %>% 
  mutate(cont = ifelse(exc > 0, exc / exc_pos, exc / exc_neg),
         cont = round(cont * 100, 2),
         exc_sign = ifelse(exc > 0, "positive", "negative"))

# in absolute term
excs %>% 
  ggplot()+
  geom_bar(aes(x = exc, y = code_sub_chp), 
           stat = "identity", 
           position = "stack")+
  facet_grid(age~., scales = "free")

# in relative term (contribution to positive and negative changes)
excs %>% 
  ggplot()+
  geom_bar(aes(x = cont, y = code_sub_chp), 
           stat = "identity", 
           position = "stack")+
  facet_grid(age~exc_sign, scales = "free")



# all cause excess by age ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bsn_yng <- 
  db_young %>% 
  mutate(mth = month(date),
         w = ifelse(date <= "2020-03-15", 1, 0)) %>% 
  group_by(age) %>% 
  do(fit_monthly(chunk = .)) %>% 
  ungroup()

bsn_yng %>% 
  ggplot()+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.3)+
  geom_line(aes(date, bsn))+
  geom_point(aes(date, dts))+
  facet_wrap(~age, scales = "free")+
  theme_bw()

bsn_yng_annual <- 
  bsn_yng %>% 
  filter(date >= "2020-01-01") %>% 
  group_by(age) %>% 
  summarise(dts = sum(dts),
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(exc = dts - bsn)

excs %>% 
  group_by(age) %>% 
  summarise(dts = sum(dts),
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(exc = dts - bsn)





# infant mortality

dts_inf2 <- 
  dts_inf %>% 
  left_join(pop %>% 
              filter(age == "0-1") %>% 
              select(-age)) %>% 
  mutate(mx = dts / exposure,
         mth = month(date)) %>% 
  group_by(age) %>% 
  mutate(t = 1:n(),
         w = ifelse(date <= "2020-03-15", 1, 0))


dts_inf2 %>% 
  ggplot()+
  geom_point(aes(date, mx))+
  facet_wrap(~age, scales = "free")+
  theme_bw()

dts_inf3 <- 
  dts_inf2 %>% 
  group_by(age) %>% 
  do(fit_monthly(chunk = .))

dts_inf3 %>% 
  ggplot()+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.3)+
  geom_line(aes(date, bsn))+
  geom_point(aes(date, dts))+
  facet_wrap(~age, scales = "free")+
  theme_bw()


