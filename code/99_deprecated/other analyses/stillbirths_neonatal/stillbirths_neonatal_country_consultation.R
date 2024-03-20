source("Code/00_functions.R")

db_ccs <- 
  read_xlsx("Data/unicef/211102_Covid analysis data.xlsx",
            sheet = 1)

unique(db_ccs$whoname) %>% sort

db_ccs2 <- 
  db_ccs %>% 
  select(country = whoname,
         year, month, quarter,
         ndth, d0, d0_4, d5, d10, d15, d20, lb, sb_x28wks) %>% 
  gather(ndth, d0, d0_4, d5, d10, d15, d20, lb, sb_x28wks,
         key = measure,
         value = value) %>% 
  drop_na() %>% 
  mutate(value = ifelse(value == "-", "0", value),
         value = value %>% as.double())

lbs <- 
  db_ccs2 %>% 
  filter(measure == "lb") %>% 
  rename(lbs = value) %>% 
  select(-measure)

sbs <- 
  db_ccs2 %>% 
  filter(measure == "sb_x28wks") %>% 
  left_join(lbs) %>% 
  mutate(exposure = value + lbs,
         measure = "sbs") %>% 
  select(-lbs, -quarter)

neo <- 
  db_ccs2 %>% 
  filter(measure == "ndth") %>% 
  left_join(lbs) %>% 
  rename(exposure = lbs) %>% 
  mutate(measure = "neo") %>% 
  select(-quarter)

sbs_neo <- 
  bind_rows(sbs, neo)

write_rds(sbs_neo, "Output/monthly_stillbirths_neonataldths_country_consultation.rds")

sbs_neo2 <- 
  sbs_neo %>% 
  mutate(date = make_date(d = 15, m = month, y = year)) %>% 
  rename(obs = value) %>% 
  drop_na() %>% 
  group_by(country, measure, year) %>% 
  mutate(mts = n()) %>% 
  ungroup() %>% 
  filter(mts > 1) %>% 
  arrange(country, measure, date) %>% 
  group_by(country, measure) %>% 
  mutate(t = 1:n(),
         w = ifelse(date <= "2020-02-15", 1, 0)) %>% 
  ungroup()

# selecting countries with enough data for fitting baselines
cts_sbs <- 
  sbs_neo2 %>% 
  filter(measure == "sbs",
         year <= 2020) %>% 
  select(country, year) %>% 
  mutate(d2020 = ifelse(year == 2020, 1, 0)) %>% 
  unique() %>% 
  group_by(country) %>% 
  summarise(d2020 = sum(d2020),
            n = n()) %>% 
  filter(n > 1 & d2020 == 1) %>% 
  pull(country)

cts_neo <- 
  sbs_neo2 %>% 
  filter(measure == "neo",
         year <= 2020) %>% 
  select(country, year) %>% 
  mutate(d2020 = ifelse(year == 2020, 1, 0)) %>% 
  unique() %>% 
  group_by(country) %>% 
  summarise(d2020 = sum(d2020),
            n = n()) %>% 
  filter(n > 1 & d2020 == 1) %>% 
  pull(country)


# filtering countries with enough data for fitting stillbirths and 
# neonatal mortality baselines
sbs_neo3 <- 
  sbs_neo2 %>% 
  filter((measure == "sbs" & country %in% cts_sbs) |
           (measure == "neo" & country %in% cts_neo))
  



# fitting baselines
# =================

est_baseline <- function(db){
  
  test <- try(base_gam <-
                gam(obs ~
                      t +
                      s(month, bs = 'cp', k = 12) +
                      offset(log(exposure)),
                    weights = w,
                    data = db,
                    family = quasipoisson(link = "log")))
              
  test2 <- 
    try(resp <- predict(base_gam, newdata = db, type = "response", se.fit = TRUE))
  
  if(class(test)[1] == "try-error" | class(test2)[1] == "try-error"){
    out <- db %>% 
      mutate(bsn = NA,
             p_score = NA,
             obs_r = obs / exposure,
             bsn_r = NA,
             lp = NA,
             up = NA,
             lp_r = NA,
             up_r = NA)
  }else{
   
    out <- db %>% 
      mutate(bsn = resp$fit,
             p_score = obs / bsn,
             obs_r = obs / exposure,
             bsn_r = bsn / exposure) %>% 
      left_join(simul_intvals(base_gam, db, 500),
                by = "date") %>% 
      mutate(lp_r = lp / exposure,
             up_r = up / exposure)
  }
  return(out)
}


# bootstrapping using Jonas' method 
simul_intvals <- function(model, db, nsim){
  # matrix model
  X_prd <- predict(model, newdata = db, type = 'lpmatrix')
  # estimated coefficients
  beta <- coef(model)
  # offsets
  offset_prd <- matrix(log(db$exposure))
  
  # applying Huber-White adjustment for robust estimators 
  # beta_sim <- MASS::mvrnorm(nsim, beta, sandwich::vcovHAC(model))
  beta_sim <- MASS::mvrnorm(nsim, coef(model), vcov(model))
  Ey_sim <- apply(beta_sim, 1, FUN = function (b) exp(X_prd%*%b + offset_prd))
  
  y_sim <- apply(Ey_sim, 2, FUN = function (Ey) {
    y <- mu <- Ey
    # NA's can't be passed to the simulation functions, so keep them out
    idx_na <- is.na(mu) 
    mu_ <- mu[!idx_na] 
    N <- length(mu_)
    phi <- summary(model)$dispersion
    # in case of under-dispersion, sample from Poisson
    if (phi < 1) { phi = 1 }
    y[!idx_na] <- rnbinom(n = N, mu = mu_, size = mu_/(phi-1))      
    return(y)
  })
  
  ints_simul <- 
    db %>% 
    select(date)
  
  colnames_y_sim <- paste0('deaths_sim', 1:nsim)
  
  ints_simul[,colnames_y_sim] <- y_sim
  
  ints_simul <-
    ints_simul %>%
    pivot_longer(cols = starts_with('deaths_sim'),
                 names_to = 'sim_id', values_to = 'deaths_sim') %>%
    group_by(date) %>%
    summarise(
      lp = quantile(deaths_sim, 0.05, na.rm = TRUE),
      up = quantile(deaths_sim, 0.95, na.rm = TRUE)
    ) %>%
    ungroup()
  
  return(ints_simul)
}

sbs_neo_bsln <- 
  sbs_neo3 %>% 
  group_by(country, measure) %>% 
  do(est_baseline(db = .data)) %>% 
  mutate(type_excess = case_when(obs > up ~ "Positive",
                                 obs < lp ~ "Negative", 
                                 TRUE ~ "None"))

# ====


# plottiong baseline estimates
# ============================

sbs_neo_bsln %>% 
  filter(measure == "sbs") %>% 
  ggplot()+
  geom_ribbon(aes(date, ymin = ll_r, ymax = ul_r), alpha = 0.3,
              fill = "#118ab2")+
  geom_point(aes(date, obs_r, col = type_excess), size = 1)+
  geom_line(aes(date, bsn_r), col = "#118ab2", alpha = 0.7)+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed")+
  scale_color_manual(values = c("blue", "black", "red"))+
  facet_wrap(~ country, scales = "free")+
  theme_bw()+
  labs(title = "Stillbirth rates")+
  theme(
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 10)
  )
ggsave("Figures/last version/ccd_monthly_baseline_stillbirth.png", dpi = 1000, 
       width = 10, height = 4)


sbs_neo_bsln %>% 
  filter(measure == "neo") %>% 
  ggplot()+
  geom_ribbon(aes(date, ymin = ll_r, ymax = ul_r), alpha = 0.3,
              fill = "#118ab2")+
  geom_point(aes(date, obs_r, col = type_excess), size = 1)+
  geom_line(aes(date, bsn_r), col = "#118ab2", alpha = 0.7)+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed")+
  scale_color_manual(values = c("blue", "black", "red"))+
  facet_wrap(~ country, scales = "free")+
  theme_bw()+
  labs(title = "Neonatal mortality rates")+
  theme(
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 10)
  )
ggsave("Figures/last version/ccd_monthly_baseline_neonatal_mortality.png", dpi = 1000, 
       width = 10, height = 4)


# annual baseline
# ===============

annual <- 
  sbs_neo3 %>% 
  filter(year <= 2020) %>% 
  group_by(country, year, measure) %>% 
  summarise(obs = sum(obs),
            exposure = sum(exposure)) %>% 
  ungroup() %>% 
  group_by(country, measure) %>% 
  mutate(t = 1:n()) %>% 
  ungroup() %>% 
  mutate(w = ifelse(year < 2020, 1, 0))


est_annual_baseline <- function(chunk){
  
  res <- 
    predict(glm(obs ~ t + offset(log(exposure)), 
                weights = w,
                family = "quasipoisson",
                data = chunk), 
            newdata = chunk,
            type = "response", 
            se.fit = TRUE)
  chunk %>% 
    bind_cols(tibble(bsn = res$fit,
                     lp = bsn - 1.96 * res$se.fit,
                     up = bsn + 1.96 * res$se.fit))
}

annual_bsn <- 
  annual %>% 
  group_by(country, measure) %>% 
  do(est_annual_baseline(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(obs_r = obs / exposure,
         bsn_r = bsn / exposure, 
         lp_r = lp / exposure,
         up_r = up / exposure,
         type_excess = case_when(obs > up ~ "Positive",
                                 obs < lp ~ "Negative", 
                                 TRUE ~ "None"))


annual_bsn %>% 
  filter(measure == "sbs") %>% 
  ggplot()+
  geom_ribbon(aes(year, ymin = lp_r, ymax = up_r), alpha = 0.3,
              fill = "#118ab2")+
  geom_point(aes(year, obs_r, col = type_excess), size = 1)+
  geom_line(aes(year, bsn_r), col = "#118ab2", alpha = 0.7)+
  geom_vline(xintercept = 2020, linetype = "dashed")+
  scale_color_manual(values = c("blue", "black", "red"))+
  facet_wrap(~ country, scales = "free")+
  theme_bw()+
  labs(title = "Stillbirth rates")+
  theme(
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 10)
  )
ggsave("Figures/last version/ccd_annual_baseline_stillbirth.png", dpi = 1000, 
       width = 10, height = 4)

annual_bsn %>% 
  filter(measure == "neo") %>% 
  ggplot()+
  geom_ribbon(aes(year, ymin = lp_r, ymax = up_r), alpha = 0.3,
              fill = "#118ab2")+
  geom_point(aes(year, obs_r, col = type_excess), size = 1)+
  geom_line(aes(year, bsn_r), col = "#118ab2", alpha = 0.7)+
  geom_vline(xintercept = 2020, linetype = "dashed")+
  scale_color_manual(values = c("blue", "black", "red"))+
  facet_wrap(~ country, scales = "free")+
  theme_bw()+
  labs(title = "Neonatal mortality rates")+
  theme(
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 10)
  )
ggsave("Figures/last version/ccd_annual_baseline_neonatal_mortality.png", dpi = 1000, 
       width = 10, height = 4)


# ===================


pscore_monthly <- 
  sbs_neo_bsln %>% 
  filter(year == 2020) %>% 
  group_by(country, measure) %>% 
  summarise(obs = sum(obs),
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(pscore = obs / bsn,
         method = "monthly")

pscore_annual <- 
  annual_bsn %>% 
  filter(year == 2020) %>% 
  mutate(pscore = obs / bsn,
         method = "annual") %>% 
  select(country, measure, obs, bsn, pscore, method)

pscores <- 
  bind_rows(pscore_monthly,
            pscore_annual) %>% 
  mutate(measure = factor(measure, levels = c("sbs", "neo")))

pscores %>% 
  ggplot()+
  geom_point(aes(pscore, country, col = method))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  facet_grid(~measure)+
  scale_x_log10(breaks = seq(0.1, 2, 0.1))+
  theme_bw()

ggsave("Figures/last version/ccd_pscores_sbs_neo.png", dpi = 1000, 
       width = 10, height = 4)



