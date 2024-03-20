library(tidyverse)
library(readxl)
library(lubridate)

bts <- read_xlsx("Data/JHU/COMSA CSA data_2021 09 11-Shared.xlsx",
                 sheet = 1)

sbs_mths <- read_xlsx("Data/JHU/COMSA CSA data_2021 09 11-Shared.xlsx",
                 sheet = 2,
                 skip = 1)

dts <- read_xlsx("Data/JHU/COMSA CSA data_2021 09 11-Shared.xlsx",
                 sheet = 3)

bts2 <- 
  bts %>% 
  filter(dm != "Total") %>% 
  separate(dm, c("year", "month"), sep = "m") %>% 
  mutate(date = make_date(d = 15, m = as.double(month), y = as.double(year))) %>% 
  select(date, everything(), -year, -month)
  


sbs <- 
  bts2 %>% 
  mutate(sbs_r = SB / LBSB)

# stillbirth counts
sbs %>% 
  ggplot()+
  geom_point(aes(date, SB))+
  theme_bw()

# birth counts
sbs %>% 
  ggplot()+
  geom_point(aes(date, LB))+
  theme_bw()

# stillbirth rates
sbs %>% 
  ggplot()+
  geom_point(aes(date, sbs_r))+
  theme_bw()

sbs_plot <- 
  sbs %>% 
  gather(-date, key = measure, value = count)

sbs_plot %>% 
  filter(measure != "LBSB") %>% 
  ggplot()+
  geom_point(aes(date, count))+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed")+
  scale_x_date(breaks = "4 months")+
  facet_wrap(~measure, scales = "free", ncol = 1)+
  theme_bw()
ggsave("Figures/JHU/mozambique_descriptive.png", dpi = 1000, 
       width = 8, height = 4)




library(mgcv)

sbs2 <- 
  sbs %>% 
  rename(lbs = LB,
         sbs = SB,
         dvs = LBSB) %>% 
  mutate(ws = ifelse(date <= "2020-05-15", 1, 0),
         t = 1:n(),
         t2 = t^2,
         t3 = t^3,
         log_t = log(t))

sbs_fit <- 
  sbs2 %>% 
  filter(date <= "2020-04-15") 


sbs <- sbs_fit$sbs
dvs <- sbs_fit$dvs
t <- sbs_fit$t

glm(sbs ~ t + offset(dvs), 
    family = "quasipoisson")

sbs_model <- 
  glm(sbs ~ t + offset(dvs), 
      data = sbs_fit,
      weights = ws,
      maxit = 1000,
      family = "quasipoisson")

sbs_model_lm <- 
  lm(sbs_r ~ t,
     data = sbs_fit)

sbs_model_log <- 
  lm(sbs_r ~ log_t,
     data = sbs_fit)

pred_lm <- predict(sbs_model_lm, se.fit = TRUE, newdata = sbs2)
pred_log <- predict(sbs_model_log, se.fit = TRUE, newdata = sbs2)

sbs3 <- 
  sbs2 %>% 
  mutate(bsn = predict(sbs_model, newdata = sbs2, type = "response"),
         bsn_lm = pred_lm$fit,
         bsn_log = pred_log$fit,
         ul = bsn_lm + 1.96 * pred_lm$se.fit,
         ll = bsn_lm - 1.96 * pred_lm$se.fit,
         ul_log = bsn_log + 1.96 * pred_log$se.fit,
         ll_log = bsn_log - 1.96 * pred_log$se.fit)

sbs3 %>% 
  ggplot()+
  geom_point(aes(date, sbs))+
  geom_line(aes(date, bsn))

sbs3 %>% 
  ggplot()+
  geom_point(aes(date, sbs_r))+
  geom_line(aes(date, bsn_lm))+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.3)+
  geom_line(aes(date, bsn_log), col = "blue")+
  geom_ribbon(aes(date, ymin = ll_log, ymax = ul_log), fill = "blue", alpha = 0.3)+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 10)
  )
ggsave("Figures/JHU/mozambique_fetal_deaths.png", dpi = 1000, 
       width = 8, height = 4)

sbs4 <- 
  sbs3 %>% 
  mutate(p_score_lm = sbs_r / bsn_lm,
         p_score_log = sbs_r / bsn_log)





dts2 <- 
  dts %>% 
  filter(YYMM != "Total") %>% 
  separate(YYMM, c("year", "month"), sep = "m") %>% 
  mutate(date = make_date(d = 15, m = as.double(month), y = as.double(year))) %>% 
  select(date, everything(), -year, -month)

neo <- 
  dts2 %>% 
  select(date, neo = '<1M') %>% 
  left_join(bts2 %>% 
              select(date, bts = LB)) %>% 
  mutate(neo_r = neo / bts)

neo %>% 
  ggplot()+
  geom_point(aes(date, neo_r))+
  theme_bw()

unique(inf_chd$age)

inf_chd <-
  dts2 %>% 
  gather(-date, key = age, value = dts) %>% 
  mutate(age = factor(age, 
                      levels = c("<1M", "1_11M", "12_59M", "5_9Y", "10_14Y", "15_19Y", "20_24Y")),
         ws = ifelse(date <= "2020-04-15", 1, 0)) %>% 
  group_by(age) %>% 
  mutate(t = 1:n(),
         log_t = log(t)) %>% 
  ungroup()


fit_linear <- function(chunk){
  md_lm <- lm(dts ~ log_t, weights = ws, data = chunk)
  pred <- predict(md_lm, se.fit = TRUE, type = "response", newdata = chunk)
  chunk %>% 
    mutate(bsn = pred$fit,
           ul = bsn + 1.96 * pred$se.fit,
           ll = bsn - 1.96 * pred$se.fit)
}


inf_chd_bln <- 
  inf_chd %>% 
  group_by(age) %>% 
  do(fit_linear(chunk = .data))
  

inf_chd %>% 
  ggplot()+
  geom_point(aes(date, dts))+
  facet_grid(age~., scales = "free")+
  theme_bw()


inf_chd_bln %>% 
  mutate(out = ifelse(dts > ul | dts < ll, "1", "0")) %>% 
  ggplot()+
  geom_point(aes(date, dts, col = out))+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.3)+
  geom_line(aes(date, bsn))+
  geom_vline(xintercept = ymd("2020-04-15"), linetype = "dashed")+
  scale_color_manual(values = c("black", "#ef476f"))+
  facet_grid(age~., scales = "free")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 10)
  )
ggsave("Figures/JHU/mozambique_infant_young_deaths.png", dpi = 1000, 
       width = 8, height = 4)














sbs_mths2 <- 
  sbs_mths %>% 
  filter(dm != "Total") %>% 
  separate(dm, c("year", "month"), sep = "m") %>% 
  mutate(date = make_date(d = 15, m = as.double(month), y = as.double(year))) %>% 
  select(date, everything(), -year, -month) %>% 
  gather(-date, key = mths, value = sbs) %>% 
  left_join(bts2) %>% 
  mutate(sbs_r = sbs / (LBSB),
         mths2 = str_sub(mths, 1, 2) %>% str_trim())

sbs_mths2 %>% 
  ggplot()+
  geom_line(aes(date, sbs_r))+
  facet_wrap(~ mths2, scales = "free")
