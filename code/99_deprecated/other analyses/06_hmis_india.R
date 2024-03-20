library(here)
source(here("Code", "00_functions.R"))
library(readxl)
library(mgcv)

# Health Information System (HMIS) data for 38 states in India
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db <- read_csv(here("Data", "HMIS", "India_HMIS_byState_2015_2020.csv"))
  
unique(db$Indicator)
unique(db$Region)
unique(db$Year) %>% sort

good_cov <- c("Maharashtra", "Tamil Nadu", "Kerala")
examps <- c("All India", "Maharashtra", "Tamil Nadu", "Kerala", "Delhi", 
            "Madhya Pradesh", "Uttar Pradesh", "West Bengal")


# Dadra & Nagar Haveli and Daman & Diu were integrated somewhere in 2020 
# Ladakh only has information for 2020

db2 <- 
  db %>% 
  mutate(Indicator = recode(Indicator,
                            "Infant deaths" = "infant",
                            "Neonatal deaths" = "neonatal", 
                            "Stillbirths" = "stillbirths",
                            "Facility deliveries" = "deliveries",
                            "Live birth" = "live_births",
                            "Postneonatal deaths" = "postneonatal",
                            "Child deaths age 1 to 4" = "child_1_4",
                            "Maternal deaths" = "maternal_deaths",
                            "Under-five deaths" = "child",
                            "Deaths 1 month to 5 years" = "child_1m_4y"),
         date = make_date(d = 15, 
                          month = match(Month, month.abb), 
                          year = Year),
         Region = ifelse(Region %in% c("Dadra & Nagar Haveli",
                                       "Daman & Diu", 
                                       "The Dadra And Nagar Haveli And Daman And Diu"), 
                         "D&NH&D&D",
                         Region)) %>% 
  filter(Region != 	"Ladakh") %>% 
  group_by(Region, date, Indicator) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()
  
unique(db2$Region)

db_annual <- 
  db %>% 
  group_by(Indicator, Year, Region) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  filter(Year <= 2020)

table(db$Indicator)
unique(db$Indicator)
unique(db$Region)
unique(db$Year)

# births
db_births <- 
  db_annual %>% 
  filter(Indicator == "Facility deliveries")

db_births %>% 
  ggplot()+
  geom_point(aes(Year, value))+
  facet_wrap(~ Region, scales = "free")

# infant deaths
db_infant <- 
  db_annual %>% 
  filter(Indicator == "Infant deaths")

db_infant %>% 
  ggplot()+
  geom_point(aes(Year, value))+
  facet_wrap(~ Region, scales = "free")

# Neonatal deaths
db_neonatal <- 
  db_annual %>% 
  filter(Indicator == "Neonatal deaths")

db_neonatal %>% 
  ggplot()+
  geom_point(aes(Year, value))+
  facet_wrap(~ Region, scales = "free")

# rates
db_rates <- 
  db_annual %>% 
  filter(Indicator %in% c("Infant deaths", 
                          "Neonatal deaths", 
                          "Stillbirths",
                          "Facility deliveries",
                          "Live birth",
                          "Postneonatal deaths")) %>% 
  mutate(Indicator = recode(Indicator,
                            "Infant deaths" = "infant",
                            "Neonatal deaths" = "neonatal", 
                            "Stillbirths" = "stillbirths",
                            "Facility deliveries" = "deliveries",
                            "Live birth" = "live_births",
                            "Postneonatal deaths" = "postneonatal")) %>% 
  spread(Indicator, value) %>% 
  mutate(neonatal_rate = neonatal / live_births,
         still_rate = stillbirths / (stillbirths + live_births),
         # deliveries are different from stillbirths + live_births!!
         test = stillbirths + live_births,
         is_2020 = ifelse(Year == 2020, "y", "n"),
         y2 = str_sub(Year, 3, 4))

db_rates %>% 
  ggplot()+
  geom_point(aes(y2, neonatal_rate, col = is_2020))+
  facet_wrap(~ Region, scales = "free")+
  scale_color_manual(values = c("black", "red"))

db_rates %>% 
  ggplot()+
  geom_point(aes(y2, still_rate, col = is_2020))+
  facet_wrap(~ Region, scales = "free")+
  scale_color_manual(values = c("black", "red"))

# maternal mortality
db_annual %>% 
  filter(Indicator == "Maternal deaths") %>% 
  ggplot()+
  geom_point(aes(Year, value))+
  facet_wrap(~ Region, scales = "free")


# stillbirths trends
# ~~~~~~~~~~~~~~~~~~

# Increase in stillbirths after lockdown measures in place
# In India, the lockdown started on 24 March 2020, and it was sustained
# until the end of May

db_still <- 
  db2 %>% 
  filter(Indicator %in% c("stillbirths",
                          "live_births")) %>% 
  spread(Indicator, value) %>% 
  mutate(exposure = stillbirths + live_births,
         still_rate = stillbirths / exposure,
         lockdown = ifelse(date >= ymd("2020-03-24") &
                             date <= ymd("2020-05-31"), "y", "n"),
         month = month(date)) %>% 
  arrange(date, Region) %>% 
  group_by(Region) %>% 
  mutate(t = 1:n(),
         t2 = t^2) %>% 
  ungroup()

db_still %>% 
  ggplot()+
  geom_line(aes(date, still_rate), col = "black", alpha = 0.3)+
  geom_point(aes(date, still_rate, col = lockdown), size = 0.3)+
  facet_wrap(~ Region, scales = "free")+
  geom_vline(xintercept = c(ymd("2020-03-24"), ymd("2020-05-31")), 
             col = "red", alpha = 0.5)+
  scale_color_manual(values = c("black", "red"))+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 5) 
  )
ggsave("Figures/HMIS/india_monthly_stillbirths.png", dpi = 600)


bslns <- tibble()
rgs <- unique(db_still$Region)

for(r in rgs){
  
  chunk <- 
    db_still %>% 
    filter(Region == r) %>% 
    select(Region, date, month, t, t2, date, stillbirths, exposure)
  
  to_train <- 
    chunk %>% 
    filter(date < "2020-04-15")
  
  # gam model specification
  base_gam <- 
    gam(stillbirths ~ 
          t +
          t2 +
          s(month, bs = 'cp', k = 4) + 
          offset(log(exposure)), 
        data = to_train, 
        family = quasipoisson(link = "log"))
  
  resp <- predict(base_gam, newdata = chunk, type = "response", se.fit = TRUE)
  
  out <- 
    chunk %>% 
    left_join(tibble(t = chunk$t, 
                     baseline = resp$fit,
                     se = resp$se.fit,
                     ul = baseline + 1.96 * se,
                     ll = baseline - 1.96 * se)) %>% 
    mutate(p_score = stillbirths / baseline)
  
  bslns <- 
    bslns %>% 
    bind_rows(out)

}

bslns %>% 
  ggplot()+
  geom_point(aes(date, stillbirths), size = 0.3)+
  geom_line(aes(date, baseline), size = 0.5, col = "#023e8a")+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.4, fill = "#48cae4")+
  facet_wrap(~ Region, scales = "free")+
  geom_vline(xintercept = c(ymd("2020-03-24"), ymd("2020-05-31")), 
             col = "red", alpha = 0.5)+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 5) 
  )
ggsave("Figures/HMIS/india_stillbirths_baselines.png", 
       dpi = 600,
       width = 8,
       height = 4) 

bslns %>% 
  mutate(sbr = stillbirths / exposure,
         sbr_bsln = baseline / exposure,
         ul = ul / exposure,
         ll = ll / exposure) %>% 
  ggplot()+
  geom_point(aes(date, sbr), size = 0.3)+
  geom_line(aes(date, sbr_bsln), size = 0.5, col = "#023e8a")+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.4, fill = "#48cae4")+
  facet_wrap(~ Region, scales = "free")+
  geom_vline(xintercept = c(ymd("2020-03-24"), ymd("2020-05-31")), 
             col = "red", alpha = 0.5)+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 5) 
  )
ggsave("Figures/HMIS/india_stillbirths_baselines_rates.png", 
       dpi = 1000,
       width = 8,
       height = 4) 


bslns %>% 
  filter(Region %in% examps) %>% 
  mutate(sbr = stillbirths / exposure,
         sbr_bsln = baseline / exposure,
         ul = ul / exposure,
         ll = ll / exposure) %>% 
  ggplot()+
  geom_point(aes(date, sbr), size = 0.3)+
  geom_line(aes(date, sbr_bsln), size = 0.5, col = "#023e8a")+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.4, fill = "#48cae4")+
  facet_wrap(~ Region, scales = "free", ncol = 2)+
  geom_vline(xintercept = c(ymd("2020-03-24"), ymd("2020-05-31")), 
             col = "red", alpha = 0.5)+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 8) 
  )
ggsave("Figures/HMIS/india_examples_stillbirths_baselines_rates.png", 
       dpi = 600,
       width = 4,
       height = 4) 

pers <- seq(ymd('2020-03-24'),ymd('2020-12-31'), by = '1 day')

bslns %>% 
  filter(Region %in% good_cov) %>% 
  mutate(sbr = stillbirths / exposure,
         sbr_bsln = baseline / exposure,
         ul = ul / exposure,
         ll = ll / exposure) %>% 
  ggplot()+
  geom_point(aes(date, sbr), size = 0.3)+
  geom_line(aes(date, sbr_bsln), size = 0.5, col = "#023e8a")+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.4, fill = "#48cae4")+
  facet_wrap(~ Region, scales = "free")+
  geom_vline(xintercept = c(ymd("2020-03-24"), ymd("2020-05-31")), 
             col = "red", alpha = 0.5)+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 5) 
  )
ggsave("Figures/HMIS/india_stillbirths_baselines_rates_good_cov.png", 
       dpi = 600,
       width = 8,
       height = 4) 

# p-scores
p_scores_stb <- 
  bslns %>% 
  mutate(stillbirths_unc = ifelse(stillbirths > ul | stillbirths < ll, stillbirths, baseline)) %>% 
  filter(date %in% pers) %>% 
  group_by(Region) %>% 
  summarise(stillbirths = sum(stillbirths),
            stillbirths_unc = sum(stillbirths_unc),
            baseline = sum(baseline)) %>% 
  ungroup() %>% 
  mutate(w_uncertainty = stillbirths_unc / baseline,
         total = stillbirths / baseline,
         good_cov = ifelse(Region %in% good_cov, "1", "0")) %>% 
  select(Region, total, w_uncertainty, good_cov) %>% 
  gather(total, w_uncertainty, key = estimate, value = p_score) %>% 
  mutate(age = "stillbirths")


p_scores_stb %>% 
  ggplot()+
  geom_point(aes(p_score, reorder(Region, p_score), col = good_cov, shape = estimate))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10(breaks = c(0.8, 0.9, 1, 1.1, 1.2, 1.5, 1.8), limits = c(0.8, 2))+
  scale_color_manual(values = c("black", "red"), guide = "none")+
  scale_shape_manual(values = c(1, 16))+
  theme_bw()+
  theme(axis.title.y = element_blank())
ggsave("Figures/HMIS/india_stillbirths_pscores.png", 
       dpi = 1000,
       width = 10,
       height = 5) 
       




# infant and child mortality trends
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

unique(db2$Indicator)

db_inf <- 
  db2 %>% 
  filter(Indicator %in% c("live_births",
                          "infant",
                          "neonatal",
                          "postneonatal",
                          "child_1_4")) %>% 
  spread(Indicator, value) %>% 
  mutate(neo_rate = neonatal / live_births,
         postneo_rate = postneonatal / live_births,
         lockdown = ifelse(date >= ymd("2020-03-24") &
                             date <= ymd("2020-05-31"), "y", "n")) %>% 
  filter(date >= "2017-04-15")

db_inf %>% 
  ggplot()+
  geom_line(aes(date, neo_rate), col = "black", alpha = 0.3)+
  geom_point(aes(date, neo_rate, col = lockdown), size = 0.3)+
  facet_wrap(~ Region, scales = "free")+
  geom_vline(xintercept = c(ymd("2020-03-24"), ymd("2020-05-31")), 
             col = "red", alpha = 0.5)+
  scale_color_manual(values = c("black", "red"))+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 5) 
  )
ggsave("Figures/HMIS/india_monthly_neonatal.png", 
       dpi = 600,
       width = 8,
       height = 4)


db_inf %>% 
  ggplot()+
  geom_line(aes(date, postneo_rate), col = "black", alpha = 0.3)+
  geom_point(aes(date, postneo_rate, col = lockdown), size = 0.3)+
  facet_wrap(~ Region, scales = "free")+
  geom_vline(xintercept = c(ymd("2020-03-24"), ymd("2020-05-31")), 
             col = "red", alpha = 0.5)+
  scale_color_manual(values = c("black", "red"))+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 5) 
  )
ggsave("Figures/HMIS/india_monthly_postneonatal.png", 
       dpi = 600,
       width = 8,
       height = 4)

db_inf %>% 
  ggplot()+
  geom_line(aes(date, child_1_4), col = "black", alpha = 0.3)+
  geom_point(aes(date, child_1_4, col = lockdown), size = 0.3)+
  facet_wrap(~ Region, scales = "free")+
  geom_vline(xintercept = c(ymd("2020-03-24"), ymd("2020-05-31")), 
             col = "red", alpha = 0.5)+
  scale_color_manual(values = c("black", "red"))+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 5) 
  )
ggsave("Figures/HMIS/india_monthly_child_1_4.png", 
       dpi = 600,
       width = 8,
       height = 4)




# ===== neonatal mortality ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db_inf <- 
  db2 %>% 
  filter(Indicator %in% c("live_births",
                          "infant",
                          "neonatal",
                          "postneonatal",
                          "child_1_4")) %>% 
  spread(Indicator, value) %>% 
  mutate(
    # adjusting outlier in Uttar Pradesh
    neonatal = ifelse(Region == "Uttar Pradesh" & date == "2017-07-15", 462, neonatal),
    neo_rate = neonatal / live_births,
    postneo_rate = postneonatal / live_births,
    lockdown = ifelse(date >= ymd("2020-03-24") &
                        date <= ymd("2020-05-31"), "y", "n"),
    month = month(date)
    ) %>% 
  arrange(date, Region) %>% 
  group_by(Region) %>% 
  mutate(t = 1:n(),
         t2 = t^2,
         t3 = t^3) %>% 
  ungroup() %>% 
  filter(date >= "2017-07-15")

bslns_neo <- tibble()
rgs <- unique(db_inf$Region)

for(r in rgs){
  
  chunk <- 
    db_inf %>% 
    filter(Region == r) %>% 
    select(Region, date, month, t, t2, t3, date, neonatal, live_births)
  
  to_train <- 
    chunk %>% 
    filter(date < "2020-04-15")
  
  # gam model specification
  base_gam <- 
    gam(neonatal ~ 
          t +
          t2 +
          # t3 +
          s(month, bs = 'cp', k = 4) + 
          offset(log(live_births)), 
        data = to_train, 
        family = quasipoisson(link = "log"))
  
  resp <- predict(base_gam, newdata = chunk, type = "response", se.fit = TRUE)
  
  out <- 
    chunk %>% 
    left_join(tibble(t = chunk$t, 
                     baseline = resp$fit,
                     se = resp$se.fit,
                     ul = baseline + 1.96 * se,
                     ll = baseline - 1.96 * se)) %>% 
    mutate(p_score = neonatal / baseline)
  
  bslns_neo <- 
    bslns_neo %>% 
    bind_rows(out)
  
}


bslns_neo %>% 
  ggplot()+
  geom_point(aes(date, neonatal), size = 0.3)+
  geom_line(aes(date, baseline), size = 0.5, col = "#023e8a")+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.4, fill = "#48cae4")+
  facet_wrap(~ Region, scales = "free")+
  geom_vline(xintercept = c(ymd("2020-03-24"), ymd("2020-05-31")), 
             col = "red", alpha = 0.5)+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 5) 
  )
ggsave("Figures/HMIS/india_neonatal_baselines.png", 
       dpi = 600,
       width = 8,
       height = 4) 

bslns_neo %>% 
  mutate(ner = neonatal / live_births,
         ner_bsln = baseline / live_births,
         ul = ul / live_births,
         ll = ll / live_births) %>% 
  ggplot()+
  geom_point(aes(date, ner), size = 0.3)+
  geom_line(aes(date, ner_bsln), size = 0.5, col = "#023e8a")+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.4, fill = "#48cae4")+
  facet_wrap(~ Region, scales = "free")+
  geom_vline(xintercept = c(ymd("2020-03-24"), ymd("2020-05-31")), 
             col = "red", alpha = 0.5)+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 5) 
  )
ggsave("Figures/HMIS/india_neonatal_baselines_rates.png", 
       dpi = 1000,
       width = 8,
       height = 4) 


bslns_neo %>% 
  filter(Region %in% examps) %>% 
  mutate(ner = neonatal / live_births,
         ner_bsln = baseline / live_births,
         ul = ul / live_births,
         ll = ll / live_births) %>% 
  ggplot()+
  geom_point(aes(date, ner), size = 0.3)+
  geom_line(aes(date, ner_bsln), size = 0.5, col = "#023e8a")+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.4, fill = "#48cae4")+
  facet_wrap(~ Region, scales = "free", ncol = 2)+
  geom_vline(xintercept = c(ymd("2020-03-24"), ymd("2020-05-31")), 
             col = "red", alpha = 0.5)+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 8) 
  )
ggsave("Figures/HMIS/india_examples_neonatal_baselines_rates.png", 
       dpi = 1000,
       width = 4,
       height = 4) 


p_scores_neo <- 
  bslns_neo %>% 
  mutate(neonatal_unc = ifelse(neonatal > ul | neonatal < ll, neonatal, baseline)) %>% 
  filter(date %in% pers) %>% 
  group_by(Region) %>% 
  summarise(neonatal = sum(neonatal),
            neonatal_unc = sum(neonatal_unc),
            baseline = sum(baseline)) %>% 
  ungroup() %>% 
  mutate(total = neonatal / baseline,
         w_uncertainty = neonatal_unc / baseline,
         good_cov = ifelse(Region %in% good_cov, "1", "0")) %>% 
  select(Region, total, w_uncertainty, good_cov) %>% 
  gather(total, w_uncertainty, key = estimate, value = p_score) %>% 
  mutate(age = "neonatal")

p_scores_neo %>% 
  ggplot()+
  geom_point(aes(p_score, reorder(Region, p_score), col = good_cov, shape = estimate))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10(breaks = c(0.2, 0.5, 1, 1.5, 2, 4, 6), limits = c(1/8, 8))+
  scale_color_manual(values = c("black", "red"), guide = "none")+
  scale_shape_manual(values = c(1, 16))+
  theme_bw()+
  theme(axis.title.y = element_blank())
ggsave("Figures/HMIS/india_neonatal_pscores.png", 
       dpi = 600,
       width = 9,
       height = 5) 


# ===== Postneonatal mortality ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db_inf <- 
  db2 %>% 
  filter(Indicator %in% c("live_births",
                          "infant",
                          "neonatal",
                          "postneonatal",
                          "child_1_4")) %>% 
  spread(Indicator, value) %>% 
  mutate(
    # adjusting outlier in Uttar Pradesh
    neonatal = ifelse(Region == "Uttar Pradesh" & date == "2017-07-15", 462, neonatal),
    neo_rate = neonatal / live_births,
    postneo_rate = postneonatal / live_births,
    lockdown = ifelse(date >= ymd("2020-03-24") &
                        date <= ymd("2020-05-31"), "y", "n"),
    month = month(date)
  ) %>% 
  arrange(date, Region) %>% 
  group_by(Region) %>% 
  mutate(t = 1:n(),
         t2 = t^2,
         t3 = t^3) %>% 
  ungroup() %>% 
  filter(date >= "2017-07-15")

bslns_post <- tibble()
rgs <- unique(db_inf$Region)

for(r in rgs){
  
  chunk <- 
    db_inf %>% 
    filter(Region == r) %>% 
    select(Region, date, month, t, t2, t3, date, postneonatal)
  
  to_train <- 
    chunk %>% 
    filter(date < "2020-04-15")
  
  # gam model specification
  base_gam <- 
    gam(postneonatal ~ 
          t +
          # t2 +
          # t3 +
          s(month, bs = 'cp', k = 4), 
        data = to_train, 
        family = quasipoisson(link = "log"))
  
  resp <- predict(base_gam, newdata = chunk, type = "response", se.fit = TRUE)
  
  out <- 
    chunk %>% 
    left_join(tibble(t = chunk$t, 
                     baseline = resp$fit,
                     se = resp$se.fit,
                     ul = baseline + 1.96 * se,
                     ll = baseline - 1.96 * se)) %>% 
    mutate(p_score = postneonatal / baseline)
  
  bslns_post <- 
    bslns_post %>% 
    bind_rows(out)
  
}


bslns_post %>% 
  ggplot()+
  geom_point(aes(date, postneonatal), size = 0.3)+
  geom_line(aes(date, baseline), size = 0.5, col = "#023e8a")+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.4, fill = "#48cae4")+
  facet_wrap(~ Region, scales = "free")+
  geom_vline(xintercept = c(ymd("2020-03-24"), ymd("2020-05-31")), 
             col = "red", alpha = 0.5)+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 5) 
  )
ggsave("Figures/HMIS/india_postneonatal_baselines.png", 
       dpi = 1000,
       width = 8,
       height = 4) 

bslns_post %>% 
  filter(Region %in% examps) %>% 
  ggplot()+
  geom_point(aes(date, postneonatal), size = 0.3)+
  geom_line(aes(date, baseline), size = 0.5, col = "#023e8a")+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.4, fill = "#48cae4")+
  facet_wrap(~ Region, scales = "free", ncol = 2)+
  geom_vline(xintercept = c(ymd("2020-03-24"), ymd("2020-05-31")), 
             col = "red", alpha = 0.5)+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 8) 
  )
ggsave("Figures/HMIS/india_examples_postneonatal_baselines_rates.png", 
       dpi = 1000,
       width = 4,
       height = 4) 


p_scores_post <- 
  bslns_post %>% 
  mutate(postneonatal_unc = ifelse(postneonatal > ul | postneonatal < ll, postneonatal, baseline)) %>% 
  filter(date %in% pers) %>% 
  group_by(Region) %>% 
  summarise(postneonatal = sum(postneonatal),
            postneonatal_unc = sum(postneonatal_unc),
            baseline = sum(baseline)) %>% 
  ungroup() %>% 
  mutate(total = postneonatal / baseline,
         w_uncertainty = postneonatal_unc / baseline,
         good_cov = ifelse(Region %in% good_cov, "1", "0")) %>% 
  select(Region, total, w_uncertainty, good_cov) %>% 
  gather(total, w_uncertainty, key = estimate, value = p_score) %>% 
  mutate(age = "postneonatal")

p_scores_post %>% 
  ggplot()+
  geom_point(aes(p_score, reorder(Region, p_score), col = good_cov, shape = estimate))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10(breaks = c(0.2, 0.5, 1, 1.5, 2, 4, 6), limits = c(1/3, 3))+
  scale_color_manual(values = c("black", "red"), guide = "none")+
  scale_shape_manual(values = c(1, 16))+
  theme_bw()+
  theme(axis.title.y = element_blank())
ggsave("Figures/HMIS/india_postneonatal_pscores.png", 
       dpi = 600,
       width = 9,
       height = 5) 


# ===== child mortality ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~

bslns_child <- tibble()
rgs <- unique(db_inf$Region)

for(r in rgs){
  
  chunk <- 
    db_inf %>% 
    filter(Region == r) %>% 
    select(Region, date, month, t, t2, t3, date, child_1_4)
  
  to_train <- 
    chunk %>% 
    filter(date < "2020-04-15")
  
  # gam model specification
  base_gam <- 
    gam(child_1_4 ~ 
          t +
          t2 +
          # t3 +
          s(month, bs = 'cp', k = 4), 
        data = to_train, 
        family = quasipoisson(link = "log"))
  
  resp <- predict(base_gam, newdata = chunk, type = "response", se.fit = TRUE)
  
  out <- 
    chunk %>% 
    left_join(tibble(t = chunk$t, 
                     baseline = resp$fit,
                     se = resp$se.fit,
                     ul = baseline + 1.96 * se,
                     ll = baseline - 1.96 * se)) %>% 
    mutate(p_score = child_1_4 / baseline)
  
  bslns_child <- 
    bslns_child %>% 
    bind_rows(out)
  
}


bslns_child %>% 
  ggplot()+
  geom_point(aes(date, child_1_4), size = 0.3)+
  geom_line(aes(date, baseline), size = 0.5, col = "#023e8a")+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.4, fill = "#48cae4")+
  facet_wrap(~ Region, scales = "free")+
  geom_vline(xintercept = c(ymd("2020-03-24"), ymd("2020-05-31")), 
             col = "red", alpha = 0.5)+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 5) 
  )
ggsave("Figures/HMIS/india_child_baselines.png", 
       dpi = 1000,
       width = 8,
       height = 4) 

bslns_child %>% 
  filter(Region %in% examps) %>% 
  ggplot()+
  geom_point(aes(date, child_1_4), size = 0.3)+
  geom_line(aes(date, baseline), size = 0.5, col = "#023e8a")+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.4, fill = "#48cae4")+
  facet_wrap(~ Region, scales = "free", ncol = 2)+
  geom_vline(xintercept = c(ymd("2020-03-24"), ymd("2020-05-31")), 
             col = "red", alpha = 0.5)+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 8) 
  )
ggsave("Figures/HMIS/india_examples_child_baselines_rates.png", 
       dpi = 1000,
       width = 4,
       height = 4) 


p_scores_ch <- 
  bslns_child %>% 
  mutate(child_1_4_unc = ifelse(child_1_4 > ul | child_1_4 < ll, child_1_4, baseline)) %>% 
  filter(date %in% pers) %>% 
  group_by(Region) %>% 
  summarise(child_1_4 = sum(child_1_4),
            child_1_4_unc = sum(child_1_4_unc),
            baseline = sum(baseline)) %>% 
  ungroup() %>% 
  mutate(total = child_1_4 / baseline,
         w_uncertainty = child_1_4_unc / baseline,
         good_cov = ifelse(Region %in% good_cov, "1", "0")) %>% 
  select(Region, total, w_uncertainty, good_cov) %>% 
  gather(total, w_uncertainty, key = estimate, value = p_score) %>% 
  mutate(age = "child_1_4")

p_scores_ch %>% 
  ggplot()+
  geom_point(aes(p_score, reorder(Region, p_score), col = good_cov, shape = estimate))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10(breaks = c(0.2, 0.5, 1, 1.5, 2, 4, 6), limits = c(1/3, 3))+
  scale_color_manual(values = c("black", "red"), guide = "none")+
  scale_shape_manual(values = c(1, 16))+
  theme_bw()+
  theme(axis.title.y = element_blank())
ggsave("Figures/HMIS/india_child_pscores.png", 
       dpi = 1000,
       width = 9,
       height = 5) 


# === All p-scores together ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rank_sbr <- 
  p_scores_stb %>% 
  filter(estimate == "w_uncertainty") %>% 
  arrange(-p_score) %>% 
  mutate(order = 1:n()) %>% 
  select(Region, order)

p_scores_all <- 
  bind_rows(p_scores_stb,
            p_scores_neo,
            p_scores_post,
            p_scores_ch) %>% 
  left_join(rank_sbr) %>% 
  mutate(age = factor(age, levels = c("stillbirths", "neonatal", "postneonatal", "child_1_4")),
         regs_examps = case_when(good_cov == "0" & Region != "All India" ~ "0",
                                 good_cov == "1" ~ "1",
                                 Region == "All India" ~ "2"))

p_scores_all %>% 
  filter(Region != "Lakshadweep") %>% 
  ggplot()+
  geom_point(aes(p_score, reorder(Region, order), col = regs_examps, shape = estimate))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10()+
  scale_color_manual(values = c("black", "red", "blue"), guide = "none")+
  scale_shape_manual(values = c(1, 16))+
  facet_wrap(~age, ncol = 4, scale = "free_x")+
  theme_bw()+
  theme(axis.title.y = element_blank())
ggsave("Figures/HMIS/india_all_pscores.png", 
       dpi = 1000,
       width = 10,
       height = 5) 

# 
# possibility of evaluate correlations.... in progress
p_scores_all2 <- 
  p_scores_all %>% 
  filter(estimate == "w_uncertainty")
