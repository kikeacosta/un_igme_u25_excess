rm (list = ls())
source("code/i00_functions.R")

dt <- read_rds("data_inter/baselines_2015_2021_sex_age_cause.rds")

unique(dt$cause)

dt2 <- 
  dt %>% 
  filter(year %in% 2020:2020) %>% 
  mutate(exc = dts - bsn) 

dt3 <- 
  dt2 %>% 
  filter(cause != "total") %>% 
  group_by(country, year, sex, age) %>% 
  mutate(bsn_sum = sum(bsn),
         exc_sum = sum(exc)) %>% 
  select(country, year, sex, age, cause, dts, 
         bsn, bsn_sum, exc_sum, lp, up) %>% 
  arrange(country, year, sex, age, cause)

dt4 <- 
  dt3 %>% 
  left_join(dt2 %>% 
              filter(cause == "total") %>% 
              select(country, year, sex, age, 
                     bsn_tot = bsn, exc_tot = exc)) %>% 
  mutate(
    diff_rel = exc_tot / exc_sum,
    diff_abs = exc_tot - exc_sum,
    bsn2 = bsn * bsn_tot / bsn_sum,
    lp2 = lp * bsn_tot / bsn_sum,
    up2 =  up * bsn_tot / bsn_sum) %>% 
  mutate(exc2 = dts - bsn2) %>% 
  group_by(country, year, sex, age) %>% 
  mutate(bsn_sum2 = sum(bsn2),
         exc_sum2 = sum(exc2))

unique(dt4$country)

comp_spec_all <- 
  dt4 %>% 
  select(country, sex, age, diff_rel, diff_abs) %>% 
  unique() 
# %>% 
#   group_by(sex, age, )
  

dt4 %>% 
  select(country, year, sex, age, diff_rel, diff_abs) %>% 
  filter(country == "USA") %>% 
  unique() %>% 
  ggplot()+
  geom_point(aes(diff_rel, age))+
  # scale_x_log10(limits  = c(0.8, 1.2))+
  scale_x_log10()+
  geom_vline(xintercept = 1, linetype = "dashed")+
  facet_grid(year~sex, scale = "free_x", space = "free")


dt4 %>% 
  filter(country == "USA") %>% 
  select(country, year, sex, age, exc_sum, exc_tot) %>% 
  unique() %>% 
  gather(exc_sum, exc_tot, key = source, value = exc) %>% 
  ggplot()+
  geom_bar(aes(fill=source, y=source, x=exc),
           position="dodge", stat="identity")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  facet_grid(age~sex, scale = "free_x", space = "free")





ct <- "USA"
dt4 %>% 
  filter(sex == "t") %>% 
  filter(country == ct) %>% 
  ggplot()+
  geom_bar(aes(fill=cause, y=cause, x=exc2),
           position="dodge", stat="identity")+
  geom_point(data = dt2 %>% 
               filter(cause == "total", sex == "t", country == ct),
             aes(exc, cause))+
  facet_grid(age~year, scale = "free_y", space = "free")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_bw()

dt5 <- 
  dt4 %>% 
  select(country, year, sex, age, cause, dts, 
         bsn = bsn2, lp = lp2, up = up2, 
         exc = exc2) %>% 
  bind_rows(dt2 %>% 
              filter(cause == "total") %>% 
              select(country, year, sex, age, cause, 
                     dts, bsn, lp, up, exc)) %>% 
  arrange(country, year, sex, age, cause)

write_rds(dt5, "data_inter/baselines_2015_2021_sex_age_cause_adj.rds")


dt4 %>% 
  filter(sex == "t") %>% 
  # mutate(exc_r = exc2 /)
  # filter(country == ct) %>% 
  ggplot()+
  geom_bar(aes(fill=cause, y=cause, x=exc2),
           position="dodge", stat="identity")+
  geom_point(data = dt2 %>% 
               filter(cause == "total", sex == "t"),
             aes(exc, cause))+
  facet_grid(age~country, scale = "free", space = "free")+
  geom_vline(xintercept = 0, linetype = "dashed")+
  theme_bw()
