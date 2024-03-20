library(here)
source(here("Code", "00_functions.R"))
library(read.dbc)

# codes
# =====
reg_codes <- 
  read_csv("Documents/bra_state_codes_names.csv")

muncodes <- 
  read_csv("Data/brazil/br-city-codes.csv")


# births 
# ======
bts_10_20 <- 
  read_rds("Output/births/brazil_monthly_births_state.rds")

# ============

# Fetal deaths
# ============
# identify all files .dbc in directory
files <- list.files("Data/Brazil/fetal_deaths/", "dbc|DBC")
file_paths <- paste0("Data/Brazil/fetal_deaths/", files)
# read them all
fds <- list()
for(i in file_paths){
  fds[[i]] <- 
    read.dbc(i)
}

fds2 <- 
  fds %>% 
  bind_rows() %>% 
  as_tibble() %>% 
  mutate(date = dmy(DTOBITO),
         year = year(date))

# gestation weeks per year, including NAs
gest_time_year <- 
  table(fds2$year, fds2$GESTACAO, useNA = c("ifany"))

write.excel(gest_time_year)



# monthly by state
# =================

muncodes2 <- 
  muncodes %>% 
  select(name, state_iso = state, muncode = idIBGE) %>% 
  mutate(muncode = str_sub(muncode, 1, 6) %>% as.double()) %>% 
  left_join(reg_codes)

fds_municip <- 
  fds2 %>% 
  mutate(weeks = case_when(GESTACAO %in% 1:2 ~ "<27",
                           GESTACAO %in% 3:6 ~ "28+",
                           TRUE ~ NA_character_),
         date_d = dmy(DTOBITO),
         year_d = year(date_d),
         month_d = month(date_d),
         muncode = as.double(as.character(CODMUNOCOR)),
         date = make_date(d = 15, m = month_d, y = year_d)) %>% 
  select(date, 
         muncode,
         weeks) %>% 
  group_by(date, muncode, weeks) %>% 
  summarise(fds = n()) %>% 
  ungroup() %>% 
  left_join(muncodes2)

ref_state <- 
  read_csv2("Data/Brazil/fetal_deaths/annual_fetal_deaths_by_state.csv",
           skip = 4) %>%
  drop_na(Total) %>% 
  select(-Total) %>% 
  rename(state_raw = 1) %>% 
  filter(state_raw != "Total") %>% 
  gather(-state_raw, key = year, value = fds_ref) %>% 
  mutate(state_num = str_sub(state_raw, 1, 2) %>% as.double(),
         year = year %>% as.double()) %>% 
  left_join(reg_codes) %>% 
  select(year, state_iso, fds_ref)

fds_state_compare <- 
  fds_municip %>% 
  mutate(year = year(date)) %>% 
  group_by(year, state_iso) %>% 
  summarise(fds = sum(fds)) %>% 
  ungroup() %>% 
  left_join(ref_state) %>% 
  mutate(diff = fds - fds_ref)

# almost identical match!!!!!!!!!!!!!!!!!!

fds_state_mth <- 
  fds_municip %>% 
  group_by(date, state_iso, weeks) %>% 
  summarise(fds = sum(fds)) %>% 
  ungroup() %>% 
  left_join(reg_codes) %>% 
  select(date, state_iso, state_name, region, weeks, fds)

assign_unk_weeks <- 
  function(chunk){
    tot <- 
      chunk %>% 
      summarise(tot = sum(fds)) %>% 
      pull(tot)
    
    chunk %>% 
      filter(!is.na(weeks)) %>% 
      mutate(fds = round(tot * fds / sum(fds), 0))
  }

fds_state_mth2 <- 
  fds_state_mth %>% 
  group_by(date, state_iso) %>% 
  do(assign_unk_weeks(chunk = .data)) %>% 
  drop_na(state_iso)

fds_state_mth2_all_wks <- 
  fds_state_mth2 %>% 
  group_by(date, state_iso, state_name, region) %>% 
  summarise(fds = sum(fds)) %>% 
  ungroup() %>% 
  mutate(weeks = "total")

fds_state_mth3 <- 
  fds_state_mth2 %>% 
  bind_rows(fds_state_mth2_all_wks)


# adding birth counts by state
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

fds_state_mth4 <- 
  fds_state_mth3 %>% 
  left_join(bts_10_20) %>% 
  filter(date >= "2015-01-01") %>% 
  arrange(date) %>% 
  group_by(state_iso, weeks) %>% 
  mutate(t = 1:n(),
         ws = ifelse(date <= "2020-03-15", 1, 0)) %>% 
  ungroup()

fds_region_mth <- 
  fds_state_mth4 %>% 
  group_by(date, region, weeks) %>% 
  summarise(fds = sum(fds),
            bts = sum(bts)) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  group_by(region, weeks) %>% 
  mutate(t = 1:n(),
         ws = ifelse(date <= "2020-03-15", 1, 0)) %>% 
  ungroup() 


# ==================

est_baseline <- function(db){
  
  base_gam <-
    gam(fds ~
          t +
          s(month, bs = 'cp', k = 12) +
          offset(log(exposure)),
        weights = ws,
        data = db,
        family = quasipoisson(link = "log"))
  
  resp <- predict(base_gam, newdata = db, type = "response", se.fit = TRUE)
  
  db %>% 
    mutate(bsn = resp$fit,
           # ul = bsn + 1.96 * resp$se.fit,
           # ll = bsn - 1.96 * resp$se.fit,
           p_score = fds / bsn,
           fds_r = fds / exposure,
           bsn_r = bsn / exposure) %>% 
    left_join(simul_intvals(base_gam, db, 200)) %>% 
    mutate(ll_r = ll / exposure,
           ul_r = ul / exposure)
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
      ll = quantile(deaths_sim, 0.05, na.rm = TRUE),
      ul = quantile(deaths_sim, 0.95, na.rm = TRUE)
    ) %>%
    ungroup()
  
  return(ints_simul)
}

region_bsln <- 
  fds_region_mth %>% 
  arrange(region, date) %>% 
  mutate(exposure = fds + bts,
         month = month(date)) %>% 
  group_by(region, weeks) %>% 
  do(est_baseline(db = .data)) %>% 
  mutate(p_score_un = ifelse(fds > ul | fds < ll, p_score, 1))

state_bsln <- 
  fds_state_mth4 %>% 
  arrange(state_iso, date) %>% 
  mutate(exposure = fds + bts,
         month = month(date)) %>% 
  group_by(state_iso, weeks) %>% 
  do(est_baseline(db = .data)) %>% 
  mutate(p_score_un = ifelse(fds > ul | fds < ll, p_score, 1))

# =======

# saving estimates
write_rds(region_bsln, "Output/excess/brazil_monthly_excess_fetal_deaths_region.rds")
write_rds(state_bsln, "Output/excess/brazil_monthly_excess_fetal_deaths_state.rds")

region_bsln <- read_rds("Output/excess/brazil_monthly_excess_fetal_deaths_region.rds")
state_bsln <- read_rds("Output/excess/brazil_monthly_excess_fetal_deaths_state.rds")

# plots
# =====

# regions
# ~~~~~~~

# counts
region_bsln %>% 
  ggplot()+
  geom_point(aes(date, fds))+
  geom_line(aes(date, bsn))+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.3)+
  facet_grid(weeks ~ region, scales = "free")+
  theme_bw()

# rates
region_bsln %>% 
  mutate(out = ifelse(p_score_un != 1, "1", "0")) %>% 
  ggplot()+
  geom_point(aes(date, fds_r, col = out), size = 1)+
  geom_ribbon(aes(date, ymin = ll_r, ymax = ul_r), alpha = 0.3)+
  geom_line(aes(date, bsn_r), col = "#118ab2", alpha = 0.7)+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed", 
             col = "black",
             alpha = 0.5)+
  facet_grid(weeks ~ region, scales = "free")+
  scale_color_manual(values = c("black", "#ef476f"))+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 10)
  )
ggsave("Figures/excess/excess_regional_fetal_deaths.png", dpi = 1000, 
       width = 8, height = 4)

region_bsln %>% 
  filter(weeks == "28+") %>% 
  mutate(out = ifelse(p_score_un != 1, "1", "0")) %>% 
  ggplot()+
  geom_point(aes(date, fds_r, col = out), size = 1)+
  geom_ribbon(aes(date, ymin = ll_r, ymax = ul_r), alpha = 0.3)+
  geom_line(aes(date, bsn_r), col = "#118ab2", alpha = 0.7)+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed", 
             col = "black",
             alpha = 0.5)+
  scale_color_manual(values = c("black", "#ef476f"))+
  facet_wrap( ~ region, ncol = 5)+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 10)
  )
ggsave("Figures/excess/excess_regional_stillbirths.png", dpi = 1000, 
       width = 8, height = 2.5)


# p-scores
region_bsln %>% 
  filter(weeks == "total") %>% 
  ggplot()+
  geom_point(aes(date, p_score), alpha = 0.5)+
  geom_point(aes(date, p_score_un), col = "red", alpha = 0.5)+
  facet_wrap(~ region)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  theme_bw()

# p-scores
region_bsln %>% 
  mutate(out = ifelse(p_score_un != 1, "1", "0")) %>% 
  filter(weeks == "28+") %>% 
  ggplot()+
  geom_point(aes(date, p_score, col = out), alpha = 0.5)+
  scale_color_manual(values = c("black", "#ef476f"))+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed", 
             col = "black",
             alpha = 0.5)+
  facet_wrap(~ region, ncol = 5)+
  geom_hline(yintercept = 1, linetype = "dashed", col = "#118ab2")+
  scale_y_log10(breaks = c(0.8, 0.9, 1, 1.2, 1.3))+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 10)
  )
ggsave("Figures/excess/p_score_region_stillbirths.png", dpi = 1000, 
       width = 8, height = 2.5)


# p-scores
region_bsln %>% 
  filter(weeks == "28+") %>% 
  ggplot()+
  geom_point(aes(date, p_score), alpha = 0.5)+
  geom_point(aes(date, p_score_un), col = "red", alpha = 0.5)+
  facet_grid(weeks ~ region)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  theme_bw()


# states
# ~~~~~~

# counts
state_bsln %>% 
  mutate(out = ifelse(p_score_un != 1, 1, 0)) %>% 
  filter(weeks == "total") %>% 
  ggplot()+
  geom_point(aes(date, fds, col = out), size = 1)+
  geom_line(aes(date, bsn))+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.3)+
  facet_wrap(~ state_iso, scales = "free")+
  theme_bw()

# rates
state_bsln %>% 
  mutate(out = ifelse(p_score_un != 1, 1, 0)) %>% 
  filter(weeks == "total") %>% 
  ggplot()+
  geom_point(aes(date, fds_r, col = out), size = 1)+
  geom_line(aes(date, bsn_r))+
  geom_ribbon(aes(date, ymin = ll_r, ymax = ul_r), alpha = 0.3)+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed", 
             col = "red",
             alpha = 0.5)+
  facet_wrap(region ~ state_iso, scales = "free")+
  theme_bw()

# rates
state_bsln %>% 
  mutate(out = ifelse(p_score_un != 1, "1", "0")) %>% 
  filter(weeks == "total") %>% 
  ggplot()+
  geom_point(aes(date, fds_r, col = out), size = 0.5, alpha = 0.7)+
  geom_ribbon(aes(date, ymin = ll_r, ymax = ul_r), alpha = 0.3)+
  geom_line(aes(date, bsn_r), col = "#118ab2", alpha = 0.7)+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed", 
             col = "black",
             alpha = 0.5)+
  scale_color_manual(values = c("black", "#ef476f"))+
  facet_wrap(region ~ state_iso, scales = "free", ncol = 7)+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 6)
  )
ggsave("Figures/excess/excess_state_stillbirths.png", dpi = 1000, 
       width = 8, height = 4)


# ==============







