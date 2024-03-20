source("Code/00_functions.R")

hmis_bsn <- read_rds("Output/hmis_baselines_all_countries.rds")
hmis <- read_rds("Output/hmis_all_countries.rds")

sbs <- 
  hmis %>% 
  filter(measure %in% c("sbs", "bts")) %>% 
  spread(measure, value) %>% 
  mutate(outcome = sbs,
         exposure = bts + sbs) %>% 
  drop_na(outcome, exposure)

neo <- 
  hmis %>% 
  filter(measure %in% c("neo", "bts")) %>% 
  spread(measure, value) %>% 
  mutate(outcome = neo,
         exposure = bts) %>% 
  drop_na(outcome, exposure)

inf <- 
  hmis %>% 
  filter(measure %in% c("inf", "bts")) %>% 
  spread(measure, value) %>% 
  mutate(outcome = inf,
         exposure = bts) %>% 
  drop_na(outcome, exposure)

chd <- 
  hmis %>% 
  filter(measure == "0_4") %>% 
  mutate(outcome = value)

unique(inf$country)
unique(chd$country)


bsn_sbs <- 
  sbs %>% 
  group_by(country) %>% 
  do(est_baseline_w_exp(chunk = .data)) %>% 
  ungroup()

bsn_neo <- 
  neo %>% 
  group_by(country) %>% 
  do(est_baseline_w_exp(chunk = .data)) %>% 
  ungroup()

bsn_inf <- 
  inf %>% 
  group_by(country) %>% 
  do(est_baseline_w_exp(chunk = .data)) %>% 
  ungroup()

bsn_chd <- 
  chd %>% 
  group_by(country) %>% 
  do(est_baseline_wo_exp(chunk = .data)) %>% 
  ungroup()


psc <- 
  bind_rows(
    bsn_sbs %>% 
      select(country, year, outcome, bsn, lp, up) %>% 
      mutate(measure = "Stillbirths"),
    bsn_neo %>% 
      select(country, year, outcome, bsn, lp, up) %>% 
      mutate(measure = "Neonatal"),
    bsn_inf %>% 
      select(country, year, outcome, bsn, lp, up) %>% 
      mutate(measure = "Infant"),
    bsn_chd %>% 
      select(country, year, outcome, bsn, lp, up) %>% 
      mutate(measure = "Child (0-4)")
    ) %>% 
  filter(year %in% 2020:2021) %>% 
  mutate(psc = outcome / bsn,
         psc_lp = outcome / up,
         psc_up = outcome / lp,
         excess = case_when(psc_lp > 1 ~ "Positive",
                            psc_up < 1 ~ "Negative",
                            TRUE ~ "No-excess"),
         out = ifelse(excess == "No-excess", 0, 1),
         year = year %>% as.character,
         measure = factor(measure,
                          levels = c("Stillbirths",
                                     "Neonatal",
                                     "Infant",
                                     "Child (0-4)"))) %>% 
  group_by(year, measure) %>% 
  mutate(w = outcome / sum(outcome),
         psc_w = sum(psc * w)) %>% 
  ungroup()

write_rds(psc, "data_inter/hmis_pscores.rds")

psc <- read_rds("data_inter/hmis_pscores.rds")

cols <- 
  c("Positive" = "#b7094c",
    "No-excess" = "#5c4d7d",
    "Negative" = "#0091ad")

tx <- 8
psc %>% 
  ggplot()+
  geom_point(aes(psc, country, shape = year, col = excess, 
                 alpha = out))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10()+
  scale_color_manual(values = cols)+
  scale_alpha(range = c(0.7, 1), guide = "none")+
  facet_grid(~measure, scales = "free_y")+
  labs(shape = "Year",
       col = "Excess",
       x= "P-score")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = tx),
        legend.title = element_text(size = tx + 1),
        strip.text.y = element_text(size = tx - 2),
        axis.title.x = element_text(size = tx),
        axis.text.x = element_text(size = tx),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = tx))

ggsave("Figures/last version/hmis_excess.png",
       dpi = 600,
       w = 7,
       h = 3.5)

tx <- 8

psc %>% 
  group_by(year, measure) %>% 
  summarise(psc_w = sum(psc * (outcome / sum(outcome))),
            psc_lp_w = sum(psc_lp * (outcome / sum(outcome))),
            psc_up_w = sum(psc_up * (outcome / sum(outcome)))) %>% 
  replace_na(list(psc_w = 0, psc_lp_w = 0, psc_up_w = 0)) %>% 
  arrange(measure, year)


psc %>% 
  filter(measure %in% c("Stillbirths", "Neonatal")) %>% 
  ggplot()+
  geom_point(aes(psc, country, shape = year, col = excess, 
                 alpha = out))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10(breaks = c(0.5, 0.7, 0.8, 1, 1.2, 1.5, 2))+
  scale_color_manual(values = cols)+
  scale_alpha(range = c(0.7, 1), guide = "none")+
  facet_wrap(~measure, ncol = 1)+
  labs(shape = "Year",
       col = "Excess",
       x= "P-score")+
  theme_bw()+
  theme(legend.position = "right",
        legend.text = element_text(size = tx),
        legend.title = element_text(size = tx + 1),
        strip.text.y = element_text(size = tx - 2),
        axis.title.x = element_text(size = tx),
        axis.text.x = element_text(size = tx),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = tx))

ggsave("Figures/last version/hmis_sbs_neo_excess.png",
       dpi = 600,
       w = 5,
       h = 3.5)


sbs_neo_corr <- 
  psc %>% 
  filter(measure %in% c("Stillbirths", "Neonatal")) %>% 
  select(country, year, measure, psc) %>%
  spread(measure, psc) %>% 
  drop_na(Stillbirths, Neonatal)

library("ggpubr")
ggscatter(sbs_neo_corr, x = "Stillbirths", y = "Neonatal", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Stillbirths p-scores", ylab = "Neonatal p-scores")+
  scale_y_log10(breaks = seq(0.6, 2, 0.2))+
  scale_x_log10(breaks = seq(0.6, 2, 0.2))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  theme(axis.text = element_text(size = 8))
ggsave("Figures/last version/hmis_stillbirths_neonatal_correlation.png", 
       dpi = 600, width = 5, height = 3)



# Quadrant count ratio
# ~~~~~~~~~~~~~~~~~~~~
qcr <- 
  sbs_neo_corr %>% 
  # filter(country == "India") %>% 
  mutate(q = case_when(Stillbirths > 1 & Neonatal > 1 ~ "I",
                       Stillbirths < 1 & Neonatal > 1 ~ "II",
                       Stillbirths < 1 & Neonatal < 1 ~ "III",
                       Stillbirths > 1 & Neonatal < 1 ~ "IV")) %>% 
  group_by(q) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  spread(q, n) %>% 
  replace_na(list(I=0, II=0, III=0, IV=0)) %>%
  mutate(qcr = (I + III - II - IV) / (I + III + II + IV))

qcr











# periods
sbs %>% 
  group_by(country) %>% 
  filter(date == min(date) | date == max(date)) %>% 
  select(country, date) %>% 
  arrange(country, date) %>% 
  ggplot()+
  geom_line(aes(date, country))+
  geom_point(aes(date, country))+
  geom_vline(xintercept = ymd("2020-03-01"), linetype = "dashed")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme_bw()+
  theme(axis.title.y = element_blank())
ggsave("Figures/last version/hmis_stillbirths_availability.png", 
       dpi = 600, width = 5, height = 2)








est_baseline_w_exp <- 
  function(chunk){
    
    chunk2 <- 
      chunk %>% 
      arrange(date) %>% 
      mutate(t = 1:n(),
             w = ifelse(date <= "2020-03-15", 1, 0),
             month = month(date),
             year = year(date))
    
    gam_model <- 
      gam(outcome ~ 
            t + 
            s(month, bs = 'cp') + 
            offset(log(exposure)),
          weights = w,
          data = chunk2,
          family = "quasipoisson")
    
    gam_pred <- 
      predict(gam_model, 
              newdata = chunk2, 
              type = "response")
    
    bsn_ests <- 
      chunk2 %>% 
      mutate(bsn = gam_pred) %>% 
      group_by(year) %>%
      summarise(outcome = sum(outcome),
                bsn = sum(bsn),
                .groups = 'drop') %>%
      left_join(pred_intvals_annual(gam_model, 
                                    "gam", 
                                    chunk2, 
                                    2000, 
                                    0.95),
                by = "year")
    # test <- 
    #   bsn_ests %>% 
    #   group_by(year) %>%
    #   summarise(outcome = sum(outcome),
    #             bsn = sum(bsn),
    #             bsn_sim = sum(bsn_sim),
    #             lp = sum(lp),
    #             up = sum(up)) %>%
    #   ungroup()
      
    return(bsn_ests)
  }

pred_intvals_annual <- 
  function(
    # fitted model 
    model, 
    # either GLM or GAM (needed for model matrix extraction step)
    model_type, 
    # prediction data
    db, 
    # number of iterations
    nsim, 
    # prediction intervals' uncertainty level (between 0 and 1)
    p
  ){
    
    # defining upper and lower prediction quantiles
    lp <- (1 - p) / 2
    up <- 1 - lp
    
    # matrix model extraction
    if(model_type == "glm"){
      X_prd <- model.matrix(model, data = db, na.action = na.pass)
    }
    if(model_type == "gam"){
      X_prd <- predict(model, newdata = db, type = 'lpmatrix')
    }
    
    # estimated coefficients
    beta <- coef(model)
    
    # offsets extracted directly from the prediction data
    offset_prd <- matrix(log(db$exposure))
    
    # extracting variance covariance matrix
    beta_sim <- MASS::mvrnorm(nsim, 
                              coef(model), 
                              suppressWarnings(vcov(model)))
    
    # simulation process
    Ey_sim <- apply(beta_sim, 1, FUN = function (b) exp(X_prd %*% b + offset_prd))
    
    y_sim <- apply(Ey_sim, 2, FUN = function (Ey) {
      y <- mu <- Ey
      # NA's can't be passed to the simulation functions, so keep them out
      idx_na <- is.na(mu) 
      mu_ <- mu[!idx_na] 
      N <- length(mu_)
      phi <- suppressWarnings(summary(model)$dispersion)
      # in case of under-dispersion, sample from Poisson
      if (phi < 1) { phi = 1 }
      y[!idx_na] <- rnbinom(n = N, mu = mu_, size = mu_/(phi-1))      
      return(y)
    })
    
    # from wide to tidy format
    ints_simul <- 
      db %>% 
      select(date)
    
    colnames_y_sim <- paste0('deaths_sim', 1:nsim)
    
    ints_simul[,colnames_y_sim] <- y_sim
    
    # prediction intervals output
    ints_simul <-
      ints_simul %>%
      pivot_longer(cols = starts_with('deaths_sim'),
                   names_to = 'sim_id', values_to = 'deaths_sim') %>%
      mutate(year = year(date)) %>% 
      group_by(year, sim_id) %>% 
      summarise(deaths_sim = sum(deaths_sim),
                .groups = 'drop') %>% 
      group_by(year) %>%
      summarise(
        lp = quantile(deaths_sim, lp, na.rm = TRUE),
        up = quantile(deaths_sim, up, na.rm = TRUE), 
        bsn_sim = median(deaths_sim, na.rm = TRUE), 
        .groups = 'drop'
      ) 
    
    return(ints_simul)
  }

est_baseline_wo_exp <- 
  function(chunk){
    
    chunk2 <- 
      chunk %>% 
      arrange(date) %>% 
      mutate(t = 1:n(),
             w = ifelse(date <= "2020-03-15", 1, 0),
             month = month(date),
             year = year(date))
    
    gam_model <- 
      gam(outcome ~ 
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
      mutate(bsn = gam_pred) %>% 
      group_by(year) %>%
      summarise(outcome = sum(outcome),
                bsn = sum(bsn),
                .groups = 'drop') %>%
      left_join(pred_intvals_annual_wo_exp(gam_model, 
                                    "gam", 
                                    chunk2, 
                                    2000, 
                                    0.95),
                by = "year")

    
    return(bsn_ests)
  }

pred_intvals_annual_wo_exp <- 
  function(
    # fitted model 
    model, 
    # either GLM or GAM (needed for model matrix extraction step)
    model_type, 
    # prediction data
    db, 
    # number of iterations
    nsim, 
    # prediction intervals' uncertainty level (between 0 and 1)
    p
  ){
    
    # defining upper and lower prediction quantiles
    lp <- (1 - p) / 2
    up <- 1 - lp
    
    # matrix model extraction
    if(model_type == "glm"){
      X_prd <- model.matrix(model, data = db, na.action = na.pass)
    }
    if(model_type == "gam"){
      X_prd <- predict(model, newdata = db, type = 'lpmatrix')
    }
    
    # estimated coefficients
    beta <- coef(model)
    
    # offsets extracted directly from the prediction data
    # offset_prd <- matrix(log(db$exposure))
    
    # extracting variance covariance matrix
    beta_sim <- MASS::mvrnorm(nsim, 
                              coef(model), 
                              suppressWarnings(vcov(model)))
    
    # simulation process
    Ey_sim <- apply(beta_sim, 1, FUN = function (b) exp(X_prd %*% b))
    
    y_sim <- apply(Ey_sim, 2, FUN = function (Ey) {
      y <- mu <- Ey
      # NA's can't be passed to the simulation functions, so keep them out
      idx_na <- is.na(mu) 
      mu_ <- mu[!idx_na] 
      N <- length(mu_)
      phi <- suppressWarnings(summary(model)$dispersion)
      # in case of under-dispersion, sample from Poisson
      if (phi < 1) { phi = 1 }
      y[!idx_na] <- rnbinom(n = N, mu = mu_, size = mu_/(phi-1))      
      return(y)
    })
    
    # from wide to tidy format
    ints_simul <- 
      db %>% 
      select(date)
    
    colnames_y_sim <- paste0('deaths_sim', 1:nsim)
    
    ints_simul[,colnames_y_sim] <- y_sim
    
    # prediction intervals output
    ints_simul <-
      ints_simul %>%
      pivot_longer(cols = starts_with('deaths_sim'),
                   names_to = 'sim_id', values_to = 'deaths_sim') %>%
      mutate(year = year(date)) %>% 
      group_by(year, sim_id) %>% 
      summarise(deaths_sim = sum(deaths_sim),
                .groups = 'drop') %>% 
      group_by(year) %>%
      summarise(
        lp = quantile(deaths_sim, lp, na.rm = TRUE),
        up = quantile(deaths_sim, up, na.rm = TRUE), 
        bsn_sim = median(deaths_sim, na.rm = TRUE), 
        .groups = 'drop'
      ) 
    
    return(ints_simul)
  }
