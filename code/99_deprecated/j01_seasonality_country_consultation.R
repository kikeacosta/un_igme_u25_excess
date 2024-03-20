rm (list = ls())
source("Code/00_functions.R")

ccd_file_name <- "Data/unicef/220830_Covid analysis data.xlsx"

db_ccd <- 
  read_xlsx(ccd_file_name,
            sheet = 1) 

db2 <- 
  db_ccd %>% 
  filter(sex == "b") %>% 
  mutate(date = ymd(date)) %>% 
  group_by(whoname) %>% 
  filter(date == max(date)) %>% 
  select(country = whoname, year, month, neo = ndth, inf = d0, chd = d0_4) %>% 
  gather(neo, inf, chd, key = age, value = dts) %>% 
  drop_na() %>% 
  group_by(country, year, age) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n > 4) %>% 
  mutate(date = make_date(d = 15, m = month, y = year))

cts_inc <- 
  db2 %>% 
  select(country, year, age) %>% 
  unique() %>% 
  group_by(country, age) %>% 
  filter(max(year) >= 2020) %>% 
  ungroup() %>% 
  filter(year < 2020) %>% 
  group_by(country, age) %>% 
  summarise(ys = n()) %>% 
  ungroup() %>% 
  filter(ys >= 3) %>% 
  mutate(inc = 1) %>% 
  select(-ys)

db3 <- 
  db2 %>% 
  left_join(cts_inc) %>% 
  filter(!is.na(inc)) %>% 
  select(-inc)

est_baseline_wo_exp <- 
  function(chunk){
    
    chunk2 <- 
      chunk %>% 
      arrange(date) %>% 
      mutate(t = 1:n(),
             w = ifelse(date <= "2020-03-15", 1, 0))
    
    gam_model <- 
      gam(dts ~ 
            t + 
            s(month, bs = 'cp'),
          weights = w,
          data = chunk2,
          family = "quasipoisson")
    
    gam_pred <- 
      predict(gam_model, 
              newdata = chunk2, 
              type = "response")
    
    bsn_ests <- 
      chunk2 %>% 
      mutate(bsn = gam_pred) 

    return(bsn_ests)
  }


bsn <- 
  db3 %>% 
  group_by(country, age) %>% 
  do(est_baseline_wo_exp(chunk = .data)) %>% 
  ungroup() 

# ages 0-4
# ~~~~~~~~
bsn %>% 
  filter(age == "chd") %>% 
  ggplot()+
  geom_point(aes(date, dts))+
  geom_line(aes(date, bsn))+
  facet_wrap(~country, scales = "free_y")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed")+
  theme_bw()+
  theme(strip.text = element_text(size = 15))
ggsave("Figures/last version/seasonality/children_all.png", 
       dpi = 600,
       height = 10,
       width = 20)



bsn %>% 
  filter(age == "chd", 
         country %in% c("Colombia", "Argentina", "Panama", "Philippines", "Malaysia")) %>% 
  ggplot()+
  geom_point(aes(date, dts))+
  geom_line(aes(date, bsn))+
  facet_wrap(~country, scales = "free_y")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed")+
  theme_bw()+
  theme(strip.text = element_text(size = 15))
ggsave("Figures/last version/seasonality/children_sel.png", 
       dpi = 600,
       height = 10,
       width = 20)


# ages infant
# ~~~~~~~~
bsn %>% 
  filter(age == "inf") %>% 
  ggplot()+
  geom_point(aes(date, dts))+
  geom_line(aes(date, bsn))+
  facet_wrap(~country, scales = "free_y")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed")+
  theme_bw()+
  theme(strip.text = element_text(size = 15))
ggsave("Figures/last version/seasonality/infant_all.png", 
       dpi = 600,
       height = 10,
       width = 20)


bsn %>% 
  filter(age == "inf", 
         country %in% c("Colombia", "Argentina", "Panama", "Philippines", "Malaysia")) %>% 
  ggplot()+
  geom_point(aes(date, dts))+
  geom_line(aes(date, bsn))+
  facet_wrap(~country, scales = "free_y")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed")+
  theme_bw()+
  theme(strip.text = element_text(size = 15))
ggsave("Figures/last version/seasonality/infant_sel.png", 
       dpi = 600,
       height = 10,
       width = 20)



# ages neonatal
# ~~~~~~~~~~~~~
bsn %>% 
  filter(age == "neo") %>% 
  ggplot()+
  geom_point(aes(date, dts))+
  geom_line(aes(date, bsn))+
  facet_wrap(~country, scales = "free_y")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed")+
  theme_bw()


bsn %>% 
  filter(age == "neo", 
         country %in% c("Colombia", "Argentina", "Panama", "Philippines", "Malaysia")) %>% 
  ggplot()+
  geom_point(aes(date, dts))+
  geom_line(aes(date, bsn))+
  facet_wrap(~country, scales = "free_y")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed")+
  theme_bw()









# cts <- unique(db2$country)
# 
# 
# 
# for(ct in cts){
#   temp <- 
#     db2 %>% 
#     filter(country == ct)
#   ags <- unique(db2$age)
#   for(ag in ags){
#     
#     chunk2 <- 
#       temp %>% 
#       filter(age == ag) %>% 
#       arrange(date) %>% 
#       mutate(t = 1:n(),
#              w = ifelse(date <= "2020-03-15", 1, 0))
#     
#     gam_model <- 
#       gam(dts ~ 
#             t + 
#             s(month, bs = 'cp'),
#           weights = w,
#           data = chunk2,
#           family = "quasipoisson")
#     
#     gam_pred <- 
#       predict(gam_model, 
#               newdata = chunk2, 
#               type = "response")
#     
#     chunk3 <- 
#       chunk2 %>% 
#       mutate(bsn = gam_pred)
# 
#   }
#   
# }


