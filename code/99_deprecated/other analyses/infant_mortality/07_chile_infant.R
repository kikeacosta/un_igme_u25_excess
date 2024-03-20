source("code/00_functions.R")
library(mgcv)

ch <- 
  read_delim("Data/Chile/DEFUNCIONES_FUENTE_DEIS_2016_2021_26082021.csv", 
             delim = ";",
             col_names = F)

ch_b <- 
  read_csv("Data/Births/CHLstffout.csv") %>% 
  select(year = Year, births = TOT)

ch_b <- 
  read_csv("Data/Births/CHLstffout.csv") %>% 
  select(-CountryCode, -Area) %>% 
  gather(-Year, key = "month", value = births) %>% 
  mutate(m = match(month, month.name)) %>% 
  drop_na(m) %>% 
  filter(births != ".",
         Year >= 2016) %>% 
  mutate(date = make_date(d = 15, m = m, y = Year),
         births = births %>% as.double()) %>%
  select(date, births) %>% 
  arrange(date)
           
  
ch2 <- 
  ch %>% 
  select(year = 1,
         date = 2,
         sex = 3,
         cod_age = 4,
         age = 5,
         place = 27)

unique(ch2$year)

ch3 <- 
  ch2 %>% 
  mutate(age2 = case_when(cod_age == 4 ~ "neo_ear",
                          cod_age == 3 & age <= 7 ~ "neo_lat",
                          cod_age == 2 ~ "post_neo",
                          cod_age == 1 & age <= 4 ~ "1_4",
                          cod_age == 1 & age >= 5 & age <= 9 ~ "5_9",
                          cod_age == 1 & age >= 10 & age <= 14 ~ "10_14",
                          cod_age == 1 & age >= 15 & age <= 24 ~ "15_24",
                          TRUE ~ "other"),
         month = month(date),
         date = make_date(d = 15, m = month, y = year)) 

ch4 <- 
  ch3 %>% 
  group_by(date, age2) %>% 
  summarise(deaths = n()) %>% 
  ungroup()

table(ch4$age2)

db_inf <- 
  ch4 %>% 
  filter(age2 %in% c("neo_ear", "neo_lat", "post_neo")) %>% 
  left_join(ch_b) %>% 
  mutate(mx = deaths / births) %>% 
  rename(age = age2)


db_inf %>% 
  filter(age %in% c("neo_ear", "neo_lat")) %>% 
  filter(date <= "2021-03-15") %>% 
  ggplot()+
  geom_line(aes(date, mx))+
  geom_point(aes(date, mx), size = 0.3)+
  geom_vline(xintercept = ymd("2020-03-15"), 
             col = "red", alpha = 0.5)+
  facet_wrap(~ age, scales = "free", ncol = 1)+
  theme_bw()+
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 11) 
  )
ggsave("Figures/infant/neonatal_mortality_chile_monthly.png", 
       dpi = 600,
       width = 6,
       height = 3)

write_rds(db_inf, "Output/infant/chile_monthly_inf_deaths_births_2016_2021.rds")


mx_monthly <- 
  ch4 %>% 
  filter(age2 %in% c("neo_ear", "neo_lat", "post_neo")) %>% 
  left_join(ch_b) %>% 
  # filter(year <= 2020) %>% 
  mutate(mx = deaths / bts,
         month = month(date)) %>% 
  group_by(age2) %>% 
  mutate(t = 1:n()) %>% 
  ungroup()




mx_annual <- 
  mx_monthly %>%
  mutate(year = year(date)) %>% 
  group_by(year, age2) %>% 
  summarise(deaths = sum(deaths),
            bts = sum(bts)) %>% 
  ungroup() %>% 
  drop_na() %>% 
  mutate(mx = deaths / bts)


mx_monthly %>% 
  ggplot()+
  geom_line(aes(date, mx))+
  facet_wrap(~age2, scales = "free")+
  theme_bw()

mx_annual %>% 
  ggplot()+
  geom_line(aes(year, mx))+
  facet_wrap(~age2, scales = "free")+
  theme_bw()


chunk <- 
  mx_monthly %>% 
  filter(age2 == "neo_tard") %>% 
  mutate(w = ifelse(date <= "2020-03-15", 1, 0))

model <- gam(deaths ~ t + s(month, bs = 'cp') +
               offset(log(bts)),
             data = chunk,
             family = quasipoisson(link = "log"))

resp <- predict(model, newdata = chunk, type = "response", se.fit = TRUE)

db_baseline <- 
  chunk %>%
  mutate(baseline = resp$fit,
         ul = baseline + 1.96 * resp$se,
         ll = baseline - 1.96 * resp$se,
         p_score = deaths / baseline,
         bs_r = baseline / bts,
         ll_r = ll / bts,
         ul_r = ul / bts)


db_baseline %>% 
  ggplot()+
  geom_point(aes(date, mx), size = 0.3)+
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

filter(Population == "Actual",
       Year %in% c(2007, 2018))