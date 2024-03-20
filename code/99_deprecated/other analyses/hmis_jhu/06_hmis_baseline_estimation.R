source("Code/00_functions.R")

all_cts <- read_rds("Output/hmis_all_countries.rds")

# baseline estimation without exposures
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 
# test <- try(res <- 
#               predict(glm(Deaths ~ Year + offset(log(Population)), 
#                           weights = w,
#                           family = "quasipoisson",
#                           data = chunk2), 
#                       newdata = chunk2,
#                       type = "response", 
#                       se.fit = TRUE))
# 
# try(chunk3 <- 
#       chunk2 %>% 
#       bind_cols(tibble(bsn = res$fit,
#                        bsn_lp = bsn - 1.96 * res$se.fit,
#                        bsn_up = bsn + 1.96 * res$se.fit)))
# 
# if(class(test) == "try-error"){
#   chunk3 <- 
#     chunk2 %>% 
#     bind_cols(tibble(bsn = NA,
#                      bsn_lp = NA,
#                      bsn_up = NA))
  


est_baseline <- 
  function(chunk){
    
    chunk2 <- 
      chunk %>% 
      arrange(date) %>% 
      mutate(t = 1:n(),
             w = ifelse(date <= "2020-03-15", 1, 0),
             month = month(date))
    
    gam_model <- 
      gam(value ~ 
            t + 
            s(month, bs = 'cp'),
          weights = w,
          data = chunk2,
          family = "quasipoisson")
    
    glm_model <- 
      glm(value ~ 
            t,
          weights = w,
          data = chunk2,
          family = "quasipoisson")
    
    gam_pred <- 
      predict(gam_model, 
              newdata = chunk2, 
              type = "response", 
              se.fit = TRUE)
    
    glm_pred <- 
      predict(glm_model, 
              newdata = chunk2, 
              type = "response", 
              se.fit = TRUE)
    
    chunk %>% 
      bind_cols(tibble(bsn_glm = glm_pred$fit,
                       bsn_glm_u = bsn_glm + 1.96*glm_pred$se.fit,
                       bsn_glm_l = bsn_glm - 1.96*glm_pred$se.fit,
                       bsn_gam = gam_pred$fit,
                       bsn_gam_u = bsn_gam + 1.96*gam_pred$se.fit,
                       bsn_gam_l = bsn_gam - 1.96*gam_pred$se.fit))
  }

cts <- unique(all_cts$country)
mrs <- unique(all_cts$measure)

for(ct in cts){
  chunk1 <- 
    all_cts %>% 
    filter(country == ct) %>% 
    filter(measure %in% c("neo_r", "sbs_r", "inf_r", "0_4")) 
    
  
  mrs <- unique(chunk1$measure)
  
  for(mr in mrs){
    chunk2 <-  
      chunk1 %>% 
      filter(measure == mr)
    
    cat(paste0(dim(chunk2), "\n"))
    
    chunk3 <- 
      chunk2 %>% 
      arrange(date) %>% 
      mutate(t = 1:n(),
             w = ifelse(date <= "2020-03-15", 1, 0),
             month = month(date))
    
    test_gam <- 
      try(
        gam_model <- 
          gam(value ~ 
                t + 
                s(month, bs = 'cp'),
              weights = w,
              data = chunk3,
              family = "quasipoisson")
        )
        
    test_gam_p <- 
      try(
        gam_pred <- 
          predict(gam_model, 
                  newdata = chunk3, 
                  type = "response", 
                  se.fit = TRUE)
      )
    
    test_glm <- 
      try(
        glm_model <- 
          glm(value ~ 
                t,
              weights = w,
              data = chunk3,
              family = "quasipoisson")
        
        glm_pred <- 
          predict(glm_model, 
                  newdata = chunk3, 
                  type = "response", 
                  se.fit = TRUE)
      )
    
    
    
    chunk2 %>% 
      bind_cols(tibble(bsn_glm = glm_pred$fit,
                       bsn_glm_u = bsn_glm + 1.96*glm_pred$se.fit,
                       bsn_glm_l = bsn_glm - 1.96*glm_pred$se.fit,
                       bsn_gam = gam_pred$fit,
                       bsn_gam_u = bsn_gam + 1.96*gam_pred$se.fit,
                       bsn_gam_l = bsn_gam - 1.96*gam_pred$se.fit))
    
    
  }
}


all_cts2 <- 
  all_cts %>% 
  filter(measure %in% c("neo_r", "sbs_r", "inf_r", "0_4")) %>% 
  group_by(country, measure) %>% 
  do(est_baseline(chunk = .data)) %>% 
  ungroup()

ct <- "India"
ms <- "sbs_r"
all_cts2 %>% 
  filter(country == ct,
         measure == ms) %>% 
  mutate(type_excess = case_when(
    date >= "2020-03-15" & value < bsn_gam_l ~ "Negative",
    date >= "2020-03-15" & value > bsn_gam_u ~ "Positive",
    TRUE ~ "No-excess"),
    type_excess = factor(type_excess, 
                         levels = c("Negative", "No-excess", "Positive"))) %>% 
  ggplot()+
  geom_point(aes(date, value, col = type_excess))+
  geom_line(aes(date, bsn_gam), col = "#118ab2")+
  geom_ribbon(aes(date, ymin = bsn_gam_l, ymax = bsn_gam_u), 
              alpha = 0.3, fill = "#118ab2")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed",
             col = "red")+
  # scale_y_continuous(limits = c(0, 0.016))+
  scale_x_date(date_breaks = "1 year", date_labels = "%b%y")+
  scale_color_manual(values = c("blue", "black", "red"),
                     guide = "none")+
  labs(y = "Stillbirth rate")+
  theme_bw()
ggsave("Figures/last version/india_example_excess_sbs.png", dpi = 600,
       height = 2.5,
       width = 5)


all_cts2 %>% 
  filter(country == ct,
         measure == ms) %>% 
  mutate(type_excess = case_when(
    date >= "2020-03-15" & value < bsn_gam_l ~ "Negative",
    date >= "2020-03-15" & value > bsn_gam_u ~ "Positive",
    TRUE ~ "No-excess"),
    type_excess = factor(type_excess, 
                         levels = c("Negative", "No-excess", "Positive"))) %>% 
  ggplot()+
  geom_point(aes(date, value, col = type_excess))+
  geom_line(aes(date, bsn_gam), col = "#118ab2")+
  geom_ribbon(aes(date, ymin = bsn_gam_l, ymax = bsn_gam_u), 
              alpha = 0.3, fill = "#118ab2")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed",
             col = "red")+
  scale_y_continuous(limits = c(0, 0.016))+
  scale_x_date(date_breaks = "1 year", date_labels = "%b%y")+
  scale_color_manual(values = c("blue", "black", "red"),
                     guide = "none")+
  labs(y = "Stillbirth rate")+
  theme_bw()
ggsave("Figures/last version//india_example_excess_sbs_zoomout.png", dpi = 600,
       height = 2.5,
       width = 5)


# write_rds(all_cts2, "Output/hmis_all_countries.rds")



# regression of neonatal deaths and stillbirths including exposures
# ==================================================================
est_baseline_expo <- 
  function(chunk){
    
    chunk2 <- 
      chunk %>% 
      arrange(date) %>% 
      mutate(t = 1:n(),
             w = ifelse(date <= "2020-03-15", 1, 0),
             month = month(date))
    
    gam_model <- gam(value ~ 
                       t + 
                       s(month, bs = 'cp') + 
                       offset(log(exposure)),
                     weights = w,
                     data = chunk2,
                     family = "quasipoisson")
    
    glm_model <- glm(value ~ 
                       t + 
                       offset(log(exposure)),
                     weights = w,
                     data = chunk2,
                     family = "quasipoisson")
    
    gam_pred <- predict(gam_model, 
                        newdata = chunk2, 
                        type = "response", 
                        se.fit = TRUE)
    
    glm_pred <- predict(glm_model, 
                        newdata = chunk2, 
                        type = "response", 
                        se.fit = TRUE)
    
    
    pred1 <- predict(model1, type = "response", newdata = db_to_pred)
    
    # identifying outliers
    outlrs <- 
      db_to_pred %>% 
      mutate(baseline = pred1) %>% 
      # bootstrapping prediction intervals at specified threshold, 
      # with 2000 iterations
      left_join(simul_intvals(model1, 
                              model_type, 
                              db_to_pred, 
                              2000, 
                              outlrs_thld),
                by = "date") %>% 
      mutate(outlier = ifelse(deaths > up & !is.na(deaths), 1, 0)) %>% 
      select(date, outlier)
    
    
    
    chunk %>% 
      bind_cols(tibble(bsn_glm = glm_pred$fit,
                       bsn_glm_u = bsn_glm + 1.96*glm_pred$se.fit,
                       bsn_glm_l = bsn_glm - 1.96*glm_pred$se.fit,
                       bsn_gam = gam_pred$fit,
                       bsn_gam_u = bsn_gam + 1.96*gam_pred$se.fit,
                       bsn_gam_l = bsn_gam - 1.96*gam_pred$se.fit))
  }




to_fit_exps <- 
  all_cts %>% 
  filter(measure %in% c("sbs", "neo")) %>% 
  left_join(all_cts %>% 
              filter(measure == "bts") %>% 
              rename(bts = value) %>% 
              select(-measure)) %>% 
  mutate(exposure = ifelse(measure == "sbs", value + bts, bts)) %>% 
  select(-bts)


chunk <- 
  to_fit_exps %>% 
  filter(country == "Ethiopia",
         measure == "sbs")

# regression of neonatal deaths and stillbirths including exposures
# ==================================================================
est_baseline_expo <- 
  function(chunk){
    
    chunk2 <- 
      chunk %>% 
      arrange(date) %>% 
      mutate(t = 1:n(),
             w = ifelse(date <= "2020-03-15", 1, 0),
             month = month(date))
    
    gam_model <- 
      gam(value ~ 
            t + 
            s(month, bs = 'cp') + 
            offset(log(exposure)),
          weights = w,
          data = chunk2,
          family = "quasipoisson")
    
    glm_model <- 
      glm(value ~ 
            t + 
            offset(log(exposure)),
          weights = w,
          data = chunk2,
          family = "quasipoisson")
    
    gam_pred <- 
      predict(gam_model, 
              newdata = chunk2, 
              type = "response")
    
    glm_pred <- 
      predict(glm_model, 
              newdata = chunk2, 
              type = "response")

    bsn_ests <- 
      chunk2 %>% 
      mutate(bsn_gam = gam_pred) %>% 
      left_join(simul_intvals(gam_model, 
                              "gam", 
                              chunk2, 
                              2000, 
                              0.9) %>% 
                  rename(bsn_gam_u = up,
                         bsn_gam_l = lp),
                by = "t") %>% 
      mutate(bsn_glm = glm_pred) %>% 
      left_join(simul_intvals(glm_model, 
                              "glm", 
                              chunk2, 
                              2000, 
                              0.9) %>% 
                  rename(bsn_glm_u = up,
                         bsn_glm_l = lp),
                by = "t")

    return(bsn_ests)
  }


bsn_expo_sbs_neo <-
  all_cts %>% 
  filter(measure %in% c("sbs", "neo")) %>% 
  left_join(all_cts %>% 
              filter(measure == "bts") %>% 
              rename(bts = value) %>% 
              select(-measure)) %>% 
  mutate(exposure = ifelse(measure == "sbs", value + bts, bts)) %>% 
  select(-bts) %>% 
  group_by(country, measure) %>% 
  do(est_baseline_expo(chunk = .data)) %>% 
  ungroup()

bsn_expo_sbs_neo %>% 
  filter(country == "Uganda",
         measure == "neo") %>% 
  ggplot()+
  geom_point(aes(date, value))+
  geom_line(aes(date, bsn_gam))+
  geom_ribbon(aes(date, ymin = bsn_gam_l, ymax = bsn_gam_u), alpha = 0.3)+
  theme_bw()

bsn_expo_sbs_neo_r <- 
  bsn_expo_sbs_neo %>% 
  mutate(obs_r = value/exposure,
         bsn_r = bsn_gam / exposure,
         bsn_r_u = bsn_gam_u / exposure,
         bsn_r_l = bsn_gam_l / exposure)

bsn_expo_sbs_neo_r %>% 
  filter(country == "Eswatini",
         measure == "neo") %>% 
  ggplot()+
  geom_point(aes(date, obs_r))+
  geom_line(aes(date, bsn_r))+
  geom_ribbon(aes(date, ymin = bsn_r_l, ymax = bsn_r_u), alpha = 0.3)+
  theme_bw()


# neonatal rates
bsn_expo_sbs_neo_r %>% 
  filter(measure == "neo") %>% 
  filter(date >= "2018-01-01" & date <= "2020-12-31") %>% 
  mutate(type_excess = case_when(
    date >= "2020-03-15" & obs_r < bsn_r_l ~ "Negative",
    date >= "2020-03-15" & obs_r > bsn_r_u ~ "Positive",
    TRUE ~ "No-excess")) %>% 
  ggplot()+
  geom_line(aes(date, bsn_r), alpha = 0.7, col = "#118ab2")+
  geom_ribbon(aes(date, ymin = bsn_r_l, ymax = bsn_r_u), alpha = 0.3,
              fill = "#118ab2")+
  geom_point(aes(date, obs_r, col = type_excess), size = 1)+
  facet_grid(country~., scales = "free")+
  # facet_grid(country~.)+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed",
             col = "red")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b%y")+
  scale_color_manual(values = c("blue", "black", "red"))+
  labs(y = "rates")+
  theme_bw()+
  theme(
    legend.position = "right",
    plot.subtitle = element_text(size = 9),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0.4, t = 0.4),
                              size = 7) 
  )
# ggsave("Figures/last version/hmis/all_neo_r_expos.png", dpi = 600,
#        height = 5,
#        width = 9)


# stillbirth rates
bsn_expo_sbs_neo_r %>% 
  filter(measure == "sbs") %>% 
  filter(date >= "2018-01-01" & date <= "2020-12-31") %>% 
  mutate(type_excess = case_when(
    date >= "2020-03-15" & obs_r < bsn_r_l ~ "Negative",
    date >= "2020-03-15" & obs_r > bsn_r_u ~ "Positive",
    TRUE ~ "No-excess")) %>% 
  ggplot()+
  geom_line(aes(date, bsn_r), alpha = 0.7, col = "#118ab2")+
  geom_ribbon(aes(date, ymin = bsn_r_l, ymax = bsn_r_u), alpha = 0.3,
              fill = "#118ab2")+
  geom_point(aes(date, obs_r, col = type_excess), size = 1)+
  facet_grid(country~., scales = "free")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed",
             col = "red")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b%y")+
  scale_color_manual(values = c("blue", "black", "red"))+
  labs(y = "rates")+
  theme_bw()+
  theme(
    legend.position = "right",
    plot.subtitle = element_text(size = 9),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0.4, t = 0.4),
                              size = 7) 
  )
# ggsave("Figures/last version/hmis/all_sbs_r_expos.png", dpi = 600,
#        height = 5,
#        width = 9)

# =======



# merging baselines with and without exposures ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bsn_wout_exp <- 
  all_cts2 %>% 
  filter(!measure %in% c("neo_r", "sbs_r"))

bsn_with_exp <- 
  bsn_expo_sbs_neo %>% 
  mutate(measure = recode(measure,
                          "neo" = "neo_r",
                          "sbs" = "sbs_r"),
         value = value/exposure,
         bsn_glm = bsn_glm / exposure,
         bsn_glm_u = bsn_glm_u / exposure,
         bsn_glm_l = bsn_glm_l / exposure,
         bsn_gam = bsn_gam / exposure,
         bsn_gam_u = bsn_gam_u / exposure,
         bsn_gam_l = bsn_gam_l / exposure) %>% 
  select(-exposure, -t, -w, -month)

all_bsn <- 
  bind_rows(bsn_wout_exp,
            bsn_with_exp)


write_rds(all_bsn, "Output/hmis_baselines_all_countries.rds")


# ====




# comparison with and without exposures ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

unique(all_cts2$measure)

wout_exp <- 
  all_cts2 %>% 
  filter(measure %in% c("neo_r", "sbs_r")) %>% 
  mutate(estimation = "wout exposures")


unique(bsn_expo_sbs_neo$measure)
with_exp <- 
  bsn_expo_sbs_neo %>% 
  mutate(measure = recode(measure,
                          "neo" = "neo_r",
                          "sbs" = "sbs_r"),
         value = value/exposure,
         bsn_glm = bsn_glm / exposure,
         bsn_glm_u = bsn_glm_u / exposure,
         bsn_glm_l = bsn_glm_l / exposure,
         bsn_gam = bsn_gam / exposure,
         bsn_gam_u = bsn_gam_u / exposure,
         bsn_gam_l = bsn_gam_l / exposure,
         estimation = "with exposures")

bsn_ww <- 
  bind_rows(wout_exp,
            with_exp)


bsn_ww %>% 
  filter(date >= "2018-01-01" & date <= "2020-12-31") %>% 
  filter(measure == "neo_r") %>% 
  ggplot()+
  geom_line(aes(date, bsn_gam, col = estimation), alpha = 0.7)+
  geom_ribbon(aes(date, ymin = bsn_gam_l, ymax = bsn_gam_u, fill = estimation), 
              alpha = 0.2)+
  geom_point(aes(date, value, col = estimation), size = 1)+
  facet_grid(country~., scales = "free")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed",
             col = "red")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b%y")

bsn_ww %>% 
  filter(date >= "2018-01-01" & date <= "2020-12-31") %>% 
  filter(measure == "sbs_r") %>% 
  ggplot()+
  geom_line(aes(date, bsn_gam, col = estimation), alpha = 0.7)+
  geom_ribbon(aes(date, ymin = bsn_gam_l, ymax = bsn_gam_u, fill = estimation), 
              alpha = 0.2)+
  geom_point(aes(date, value, col = estimation), size = 1)+
  facet_grid(country~., scales = "free")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed",
             col = "red")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b%y")




# Madagascar
# ~~~~~~~~~~

# neonatal rates
bsn_expo_sbs_neo_r %>% 
  filter(measure %in% c("neo", "sbs"),
         country == "Madagascar") %>% 
  # filter(date >= "2018-01-01" & date <= "2020-12-31") %>% 
  mutate(type_excess = case_when(
    date >= "2020-03-15" & obs_r < bsn_r_l ~ "Negative",
    date >= "2020-03-15" & obs_r > bsn_r_u ~ "Positive",
    TRUE ~ "No-excess")) %>% 
  ggplot()+
  geom_line(aes(date, bsn_r), alpha = 0.7, col = "#118ab2")+
  geom_ribbon(aes(date, ymin = bsn_r_l, ymax = bsn_r_u), alpha = 0.3,
              fill = "#118ab2")+
  geom_point(aes(date, obs_r, col = type_excess), size = 1)+
  facet_wrap(~measure, scales = "free_y", ncol = 1)+
  # facet_grid(country~.)+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed",
             col = "red")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b%y")+
  scale_color_manual(values = c("blue", "black", "red"))+
  labs(y = "rates")+
  theme_bw()+
  theme(
    legend.position = "right",
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 6),
    plot.subtitle = element_text(size = 9),
    axis.title = element_text(size = 8),
    axis.text.x = element_text(size = 5, angle = 60, hjust = 1),
    axis.text.y = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0.4, t = 0.4),
                              size = 7) 
  )
ggsave("Figures/last version/hmis/madagascar_hmis_neo_sbs.png", dpi = 600,
       height = 3,
       width = 5)



