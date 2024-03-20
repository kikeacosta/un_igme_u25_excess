# Analysis of stillbirths in Brazil
# Sources of data
# For monthly fetal deaths between 2015 and 2019 
# http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sim/cnv/fet10uf.def
# For monthly fetal deaths in 2020 and 2021 
# https://dados.gov.br/dataset/sistema-de-informacao-sobre-mortalidade
# monthly live births between 2015 and 2019
# http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sinasc/cnv/nvuf.def
# monthly live births between 2020 and 2021
# http://svs.aids.gov.br/dantps/centrais-de-conteudos/paineis-de-monitoramento/natalidade/nascidos-vivos/

library(here)
source(here("Code", "00_functions.R"))
library(mgcv)
library(foreign)

# fetal deaths
d20 <- read.dbf("Data/Brazil/DOBR20DA.DBF")
d21 <- read.dbf("Data/Brazil/DOBR21DA.DBF")
fd <- read_csv("Data/Brazil/fetal_deaths_monthly_2015_2019.csv")

# live births
bs20 <- read_csv2("Data/Brazil/births_monthly_2020.csv")
bs21 <- read_csv2("Data/Brazil/births_monthly_2021.csv")
bs1519 <- read_csv2("Data/Brazil/births_monthly_2015_2019.csv", skip = 3)

# ==== preparing data ====

# preparing data on fetal deaths
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fd2 <- 
  fd %>% 
  drop_na() %>% 
  gather(-Month, -month_n, key = year, value = fetal_deaths) %>% 
  filter(Month != "Total") %>% 
  mutate(date = make_date(d = 15, m = month_n, y = year),
         month = month_n %>% as.double()) %>% 
  select(date, month, fetal_deaths)
  
d20_2 <- 
  d20 %>% 
  as_tibble() %>% 
  filter(TIPOBITO == 1) %>% 
  mutate(date_e = dmy(DTOBITO),
         date = make_date(d = 15, m = month(date_e), y = year(date_e))) %>% 
  group_by(date) %>% 
  summarise(fetal_deaths = n()) %>% 
  ungroup() %>% 
  mutate(month = month(date))

d21_2 <- 
  d21 %>% 
  as_tibble() %>% 
  filter(TIPOBITO == 1) %>% 
  mutate(date_e = dmy(DTOBITO),
         date = make_date(d = 15, m = month(date_e), y = year(date_e))) %>% 
  group_by(date) %>% 
  summarise(fetal_deaths = n()) %>% 
  ungroup() %>% 
  mutate(month = month(date))

fd_all <- 
  bind_rows(fd2,
            d20_2,
            d21_2)

fd_all %>% 
  ggplot()+
  geom_point(aes(date, fetal_deaths))+
  geom_line(aes(date, fetal_deaths), alpha = 0.4)+
  theme_bw()

ggsave("Figures/fetal_deaths/brazil_fetal_deaths.png", 
       dpi = 1000,
       width = 6,
       height = 2) 



# preparing births data
# ~~~~~~~~~~~~~~~~~~~~~
bs1519_2 <- 
  bs1519 %>% 
  drop_na() %>% 
  mutate(month = 1:n()) %>% 
  filter(month != 13) %>% 
  select(-1, -Total) %>%
  gather(-month, key = year, value = births) %>% 
  mutate(date = make_date(d = 15, m = month, y = year)) %>% 
  select(date, births)

bs20_2 <- 
  bs20 %>% 
  select(loc = 50, 51:63) %>% 
  filter(loc == "Brasil") %>% 
  gather(-loc, key = month_l, value = births) %>% 
  mutate(month = 1:n()) %>% 
  filter(month != 13) %>% 
  mutate(date = make_date(d = 15, m = month, y = 2020)) %>% 
  select(date, births)

bs21_2 <- 
  bs21 %>% 
  select(loc = 50, 51:63) %>% 
  filter(loc == "Brasil") %>% 
  gather(-loc, key = month_l, value = births) %>% 
  mutate(month = 1:n()) %>% 
  filter(month <= 5) %>% 
  mutate(date = make_date(d = 15, m = month, y = 2021)) %>% 
  select(date, births)

bs_all <- 
  bind_rows(bs1519_2, bs20_2, bs21_2)

bs_all %>% 
  ggplot()+
  geom_point(aes(date, births))+
  geom_line(aes(date, births), alpha = 0.4)+
  theme_bw()
ggsave("Figures/fetal_deaths/brazil_births.png", 
       dpi = 1000,
       width = 6,
       height = 2) 


# merging births and fetal deaths
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db <- 
  bs_all %>% 
  left_join(fd_all) %>% 
  arrange(date) %>% 
  mutate(exposure = fetal_deaths + births,
         t = 1:n(),
         fd_rate = fetal_deaths / exposure) %>% 
  filter(date <= "2021-03-15")

write_rds(db, "Output/stillbirths/brazil_monthly_stillbirths_births_2015_2021.rds")




db_plot <- 
  db %>% 
  select(date, births, fetal_deaths, fd_rate) %>% 
  gather(-date, key = measure, value = value) %>% 
  mutate(measure = factor(measure, levels = c("births", "fetal_deaths", "fd_rate")))

db_plot %>% 
  ggplot()+
  geom_point(aes(date, value))+
  geom_line(aes(date, value), alpha = 0.4)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  facet_wrap(~measure, scales = "free_y", ncol = 1)+
  theme_bw()
ggsave("Figures/fetal_deaths/brazil_births_fetal_deaths.png", 
       dpi = 1000,
       width = 6,
       height = 6) 



# ==== estimating baseline ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

to_train <- 
  db %>% 
  filter(date < "2020-03-15")
  
# gam model specification
base_gam <-
  gam(fetal_deaths ~
        t +
        s(month, bs = 'cp', k = 8) +
        offset(log(exposure)),
      data = to_train,
      family = quasipoisson(link = "log"))
  
resp <- predict(base_gam, newdata = db, type = "response", se.fit = TRUE)
  
db_baseline <- 
  db %>% 
  left_join(tibble(t = db$t, 
                   baseline = resp$fit,
                   se = resp$se.fit,
                   ul = baseline + 1.96 * se,
                   ll = baseline - 1.96 * se)) %>% 
  mutate(p_score = fetal_deaths / baseline,
         fd_r = fetal_deaths / exposure,
         bs_r = baseline / exposure,
         ll_r = ll / exposure,
         ul_r = ul / exposure)


db_baseline %>% 
  ggplot()+
  geom_point(aes(date, fetal_deaths), size = 0.3)+
  geom_line(aes(date, baseline), size = 0.5, col = "#023e8a")+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.4, fill = "#48cae4")+
  geom_vline(xintercept = ymd("2020-03-15"), 
             col = "red", alpha = 0.5)+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 5) 
  )

ggsave("Figures/fetal_deaths/brazil_fetal_deaths_baselines.png", 
       dpi = 1000,
       width = 6,
       height = 2) 

db_baseline %>% 
  ggplot()+
  geom_point(aes(date, fd_r), size = 0.3)+
  geom_line(aes(date, bs_r), size = 0.5, col = "#023e8a")+
  geom_ribbon(aes(date, ymin = ll_r, ymax = ul_r), alpha = 0.4, fill = "#48cae4")+
  geom_vline(xintercept = ymd("2020-03-15"), 
             col = "red", alpha = 0.5)+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 5) 
  )

ggsave("Figures/fetal_deaths/brazil_fetal_deaths_rates_baselines.png", 
       dpi = 1000,
       width = 6,
       height = 2) 

pers <- seq(ymd('2020-03-01'),ymd('2020-12-31'), by = '1 day')

# p-scores
p_scores_stb <- 
  db_baseline %>% 
  mutate(fetal_deaths_unc = ifelse(fetal_deaths > ul | fetal_deaths < ll, fetal_deaths, baseline),
         year = year(date)) %>% 
  group_by(year) %>% 
  summarise(fetal_deaths = sum(fetal_deaths),
            fetal_deaths_unc = sum(fetal_deaths_unc),
            baseline = sum(baseline)) %>% 
  ungroup() %>% 
  mutate(w_uncertainty = fetal_deaths_unc / baseline,
         total = fetal_deaths / baseline)

