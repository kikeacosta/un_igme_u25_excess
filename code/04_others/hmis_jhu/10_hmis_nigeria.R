source("Code/00_functions.R")
library(readxl)



# Nigeria
# =======

nga <- 
  read_xlsx("Data/HMIS/HMIS_data_Nigeria_2015_2021_all_data.xlsx")


unique(nga$Indicator)

indics <- c("Stillbirths" = "sbs",
            "Live births" = "bts",
            # several deliveries types
            "Total deliveries" = "dvs_t",
            "Deliveries" = "dvs",
            "Deliveries - Normal" = "dvs_n",
            "Deliveries - Assisted" = "dvs_a",
            "Deliveries - Caesarian Section" = "dvs_c",
            # not clear the difference between "neonatal" and "0-28d"
            # what if "Neonatal" refers to "Infant"?
            # - there is no infant deaths category)
            # - it doubles the counts in 0_28d, which seems plausible as 
            #   the proportion of neonatal within infant mortality  
            "Neonatal deaths" = "neo1",
            "Deaths (by age) 0-28d" = "neo2",
            "Child deaths 1 to 59m" = "1m_4y",
            "Under-five deaths" = "0_4",
            "Deaths (by age) 29d-11m" = "pos",
            "Deaths (by age) 12-59 m" = "1_4",
            "Deaths (by age) 5-9yrs" = "5_9",
            "Deaths (by age) 10-19yrs" = "10_19",
            "Deaths (by age) 20+ yrs" = "20+",
            "Deaths (by age)" = "age_unk")

nga2 <-
  nga %>% 
  rename(indic = Indicator) %>% 
  mutate(Month = match(Month, month.abb),
         Date = make_date(d = 15, m = Month, y = Year),
         indic = recode(indic,
                        !!!indics)) %>% 
  select(country = Country, date = Date, measure = indic, value = value)

unique(nga2$measure)

# testing internal consistency
test <- 
  nga2 %>% 
  spread(measure, value) %>% 
  mutate(
    neo_s1 = `0_4` - `1m_4y`, # alternative estimation of neonatal mortality
    neo_s2 = `0_4` - `1_4` - pos, # alternative estimation of neonatal mortality
    diff = neo_s2 - neo_s1,
    inf = `0_4` - `1_4`,
    `1_4_s` = `1m_4y` - pos # alternative estimation of deaths 1-4 years
    ) %>% 
  select(neo1 , neo2, neo_s1, neo_s2, diff, pos, `1_4`, `1_4_s`, `1m_4y`, `0_4`, inf)


# there is consistency between "Neonatal deaths" and 
# ("Under-five deaths" - "Child deaths 1 to 59m")

nga3 <- 
  nga2 %>% 
  filter(measure %in% c("sbs", "bts", "neo1", "neo2", "0_4", "1_4")) %>% 
  spread(measure, value) %>% 
  mutate(inf = `0_4` - `1_4`)

neo1 <- 
  nga3 %>% 
  select(date, neo1, bts) %>% 
  rename(exposure = bts,
         value = neo1) %>% 
  mutate(measure = "neo1")

neo2 <- 
  nga3 %>% 
  select(date, neo2, bts) %>% 
  rename(exposure = bts,
         value = neo2) %>% 
  mutate(measure = "neo2")

sbs <- 
  nga3 %>% 
  select(date, bts, sbs) %>% 
  rename(value = sbs) %>% 
  mutate(measure = "sbs",
         exposure = bts + value) %>% 
  select(-bts)



# regression of neonatal deaths and stillbirths including exposures
# ==================================================================
est_baseline_expo <- 
  function(chunk){
    
    gam_model <- 
      gam(value ~ 
            t + 
            s(month, bs = 'cp') + 
            offset(log(exposure)),
          weights = w,
          data = chunk,
          family = "quasipoisson")
    
    gam_pred <- 
      predict(gam_model, 
              newdata = chunk, 
              type = "response")
    
    bsn_ests <- 
      chunk %>% 
      mutate(bsn = gam_pred) %>% 
      left_join(simul_intvals(gam_model, 
                              "gam", 
                              chunk, 
                              2000, 
                              0.9) %>% 
                  rename(bsn_u = up,
                         bsn_l = lp),
                by = "t") 
    
    return(bsn_ests)
  }

# merge stillbirths and neonatal
sbs_neo <- 
  bind_rows(sbs, neo1, neo2) %>% 
  group_by(measure) %>%
  arrange(date) %>% 
  mutate(t = 1:n(),
         w = ifelse(date <= "2020-03-15", 1, 0),
         month = month(date)) %>% 
  ungroup() %>% 
  # exclude ouliers from regression
  mutate(w = ifelse(measure == "neo1" & date == "2015-12-15", 0, w),
         w = ifelse(measure == "sbs" & date == "2015-07-15", 0, w))

# estimating baseline
bsn <- 
  sbs_neo %>% 
  group_by(measure) %>% 
  do(est_baseline_expo(chunk = .data)) %>% 
  ungroup()

bsn2 <- 
  bsn %>% 
  filter(!(measure == "neo1" & date == "2015-12-15")) %>% 
  filter(!(measure == "sbs" & date == "2015-07-15")) %>% 
  mutate(type_excess = case_when(
    date >= "2020-03-15" & value < bsn_l ~ "Negative",
    date >= "2020-03-15" & value > bsn_u ~ "Positive",
    TRUE ~ "No-excess"),
    measure = factor(measure, levels = c("sbs", "neo1", "neo2")))

bsn2 %>% 
  ggplot()+
  geom_point(aes(date, value, col = type_excess), size = 1)+
  geom_line(aes(date, bsn), alpha = 0.5, col = "#118ab2")+
  geom_ribbon(aes(date, ymin = bsn_l, ymax = bsn_u), alpha = 0.3,
              fill = "#118ab2")+
  facet_wrap(~measure, scales = "free")+
  theme_bw()+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed",
             col = "red")+
  # scale_x_date(date_breaks = "4 months", date_labels = "%b%y")+
  scale_color_manual(values = c("blue", "black", "red"))+
  labs(y = "counts")+
  theme_bw()+
  theme(
    legend.position = "right",
    plot.subtitle = element_text(size = 9),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0.4, t = 0.4),
                              size = 8) 
  )

ggsave(paste0("Figures/last version/hmis/nigeria.png"), 
       dpi = 600,
       height = 5,
       width = 9)

bsn_r <- 
  bsn2 %>% 
  mutate(obs_r = value/exposure,
         bsn_r = bsn / exposure,
         bsn_u_r = bsn_u / exposure,
         bsn_l_r = bsn_l / exposure)

bsn_r %>% 
  filter(measure %in% c("sbs")) %>% 
  ggplot()+
  geom_point(aes(date, obs_r, col = type_excess), size = 1)+
  geom_line(aes(date, bsn_r), alpha = 0.5, col = "#118ab2")+
  geom_ribbon(aes(date, ymin = bsn_l_r, ymax = bsn_u_r), alpha = 0.3,
              fill = "#118ab2")+
  facet_wrap(~measure, scales = "free")+
  theme_bw()+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed",
             col = "red")+
  # scale_x_date(date_breaks = "4 months", date_labels = "%b%y")+
  scale_color_manual(values = c("blue", "black", "red"))+
  labs(y = "rate")+
  theme_bw()+
  theme(
    legend.position = "right",
    plot.subtitle = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    strip.text = element_text(margin = margin(b = 0.4, t = 0.4),
                              size = 8) 
  )

ggsave(paste0("Figures/last version/hmis/nigeria_sbs_rates.png"), 
       dpi = 600,
       height = 5,
       width = 9)


# =====
bsn_r %>% 
  filter(measure %in% c("neo1", "neo2")) %>% 
  ggplot()+
  geom_point(aes(date, obs_r, col = type_excess), size = 1)+
  geom_line(aes(date, bsn_r), alpha = 0.5, col = "#118ab2")+
  geom_ribbon(aes(date, ymin = bsn_l_r, ymax = bsn_u_r), alpha = 0.3,
              fill = "#118ab2")+
  facet_wrap(~measure, scales = "free")+
  theme_bw()+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed",
             col = "red")+
  # scale_x_date(date_breaks = "4 months", date_labels = "%b%y")+
  scale_color_manual(values = c("blue", "black", "red"))+
  labs(y = "rate")+
  theme_bw()+
  theme(
    legend.position = "right",
    plot.subtitle = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    strip.text = element_text(margin = margin(b = 0.4, t = 0.4),
                              size = 8) 
  )

ggsave(paste0("Figures/last version/hmis/nigeria_neo_rates.png"), 
       dpi = 600,
       height = 5,
       width = 9)


# =====







# infant mortality =====
# ~~~~~~~~~~~~~~~~~~~~~~
est_baseline <- 
  function(chunk){
    
    model <- 
      gam(value ~ 
            t + t2 +
            s(month, bs = 'cp'),
          weights = w,
          data = chunk,
          family = "quasipoisson")
    
    pred <- 
      predict(model, 
              newdata = chunk, 
              type = "response", 
              se.fit = TRUE)
    
    bsn_ests <- 
      chunk %>% 
      mutate(bsn = pred$fit,
             bsn_u = bsn + 1.96*pred$se.fit,
             bsn_l = bsn - 1.96*pred$se.fit) 
    
    return(bsn_ests)
  }


inf <- 
  nga2 %>% 
  spread(measure, value) %>% 
  mutate(value = `0_4` - `1_4`) %>% 
  select(country, date, value) %>% 
  arrange(date) %>% 
  mutate(t = 1:n(),
         t2 = t^2,
         w = ifelse(date <= "2020-03-15", 1, 0),
         month = month(date)) %>% 
  ungroup() %>%
  # exclude ouliers from regression
  mutate(w = ifelse(date == "2015-12-15", 0, w),
         w = ifelse(date == "2019-12-15", 0, w))

inf_bsn <- 
  inf %>% 
  do(est_baseline(chunk = .data))

inf_bsn2 <- 
  inf_bsn %>% 
  # filter(!(date == "2015-12-15"),
  #        !(date == "2019-12-15")) %>%
  # filter(!(measure == "sbs" & date == "2015-07-15")) %>% 
  mutate(type_excess = case_when(
    date >= "2020-03-15" & value < bsn_gam_l ~ "Negative",
    date >= "2020-03-15" & value > bsn_gam_u ~ "Positive",
    TRUE ~ "No-excess"),
    # measure = factor(measure, levels = c("sbs", "neo1", "neo2"))
    )

inf_bsn2 %>% 
  ggplot()+
  geom_point(aes(date, value, col = type_excess), size = 1)+
  geom_line(aes(date, bsn_gam), alpha = 0.5, col = "#118ab2")+
  geom_ribbon(aes(date, ymin = bsn_gam_l, ymax = bsn_gam_u), alpha = 0.3,
              fill = "#118ab2")+
  theme_bw()+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed",
             col = "red")+
  scale_y_continuous(limits = c(0, 6500))+
  # scale_x_date(date_breaks = "4 months", date_labels = "%b%y")+
  scale_color_manual(values = c("blue", "black", "red"))+
  labs(y = "counts")+
  theme_bw()+
  theme(
    legend.position = "right",
    plot.subtitle = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    strip.text = element_text(margin = margin(b = 0.4, t = 0.4),
                              size = 8) 
  )

ggsave(paste0("Figures/last version/hmis/nigeria_infant.png"), 
       dpi = 600,
       height = 5,
       width = 9)

