library(here)
source(here("Code", "00_functions.R"))

ecu <- 
  read_rds("Output/stillbirths/ecuador_monthly_stillbirths_births_2015_2020.rds")

col <- 
  read_rds("Output/stillbirths/colombia_trim_stillbirths_births_2015_2021.rds")

bra <- 
  read_rds("Output/stillbirths/brazil_monthly_stillbirths_births_2015_2021.rds")

ecu2 <- 
  ecu %>% 
  mutate(country = "ecuador") %>% 
  select(-year)

col2 <- 
  col %>% 
  select(date, weeks, fds = stbs, bts = births) %>% 
  mutate(country = "colombia",
         rate = fds / (fds + bts),
         weeks = recode(weeks,
                        "0_27" = "<=27",
                        "28+" = ">=28"))

bra2 <- 
  bra %>% 
  mutate(weeks = "total",
         country = "brazil") %>% 
  select(country, date, weeks, fds = fetal_deaths, bts = births, rate = fd_rate)

monthly <- 
  bind_rows(ecu2,
            bra2)

to_trimesters <- function(db){
  db2 <- 
    db %>% 
    mutate(month = month(date),
           month = case_when(month %in% 1:3 ~ 2,
                             month %in% 4:6 ~ 5,
                             month %in% 7:9 ~ 8,
                             month %in% 10:12 ~ 11),
           year = year(date),
           date = make_date(d = 15, m = month, y = year)) %>% 
    group_by(country, date, weeks) %>% 
    summarise(fds = sum(fds),
              bts = sum(bts)) %>% 
    ungroup() %>% 
    mutate(rate = fds / (bts + fds))
}

ecu_qt <- to_trimesters(ecu2)
bra_qt <- to_trimesters(bra2)


trims <- 
  bind_rows(ecu_qt,
            bra_qt,
            col2) %>% 
  group_by(country, weeks) %>% 
  mutate(r_std = rate / mean(rate))


trims %>% 
  ggplot()+
  geom_line(aes(date, r_std), alpha = 0.8)+
  geom_point(aes(date, r_std), size = 0.5)+
  geom_vline(xintercept = ymd("2020-03-15"), col = "red")+
  facet_grid(weeks ~ country, scales = "free_y")+
  labs(y = "stb rates")+
  theme_bw()+
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 10) 
  )

ggsave("Figures/fetal_deaths/fetal_death_rates_several_countries_trimesters.png", 
       dpi = 600,
       width = 6,
       height = 3)


to_annual <- function(db){
  db2 <- 
    db %>% 
    mutate(year = year(date)) %>% 
    filter(year <= 2020) %>% 
    group_by(country, year, weeks) %>% 
    summarise(date = mean(date),
              fds = sum(fds),
              bts = sum(bts)) %>% 
    ungroup() %>% 
    mutate(rate = fds / (bts + fds))
}

ecu_an <- to_annual(ecu2)
bra_an <- to_annual(bra2)
col_an <- to_annual(col2)


col_an %>% 
  ggplot()+
  geom_line(aes(date, rate))+
  geom_point(aes(date, rate), size = 0.3)+
  geom_vline(xintercept = ymd("2020-03-15"), 
             col = "red", alpha = 0.5)+
  facet_wrap(~ weeks, scales = "free", ncol = 3)+
  theme_bw()+
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 11) 
  )
ggsave("Figures/fetal_deaths/stillbirths_colombia_annual.png", 
       dpi = 600,
       width = 6,
       height = 3)


ecu_an %>% 
  ggplot()+
  geom_line(aes(date, rate))+
  geom_point(aes(date, rate), size = 0.3)+
  geom_vline(xintercept = ymd("2020-03-15"), 
             col = "red", alpha = 0.5)+
  facet_wrap(~ weeks, scales = "free", ncol = 3)+
  theme_bw()+
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 11) 
  )
ggsave("Figures/fetal_deaths/stillbirths_ecuador_annual.png", 
       dpi = 600,
       width = 6,
       height = 3)
