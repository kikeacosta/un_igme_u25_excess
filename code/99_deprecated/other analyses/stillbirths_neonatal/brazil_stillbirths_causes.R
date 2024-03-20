library(here)
source(here("Code", "00_functions.R"))
library(read.dbc)

# codes
# =====
reg_codes <- 
  read_csv("Documents/bra_state_codes_names.csv")

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


fds_state <- 
  fds2 %>% 
  mutate(weeks = case_when(GESTACAO %in% 1:2 ~ "<27",
                           GESTACAO %in% 3:6 ~ "28+",
                           TRUE ~ NA_character_),
         date_d = dmy(DTOBITO),
         year_d = year(date_d),
         month_d = month(date_d),
         muncode = as.double(as.character(CODMUNOCOR)),
         state_num = str_sub(as.character(CODMUNOCOR), 1, 2),
         date = make_date(d = 15, m = month_d, y = year_d)) %>% 
  select(date, 
         state_num,
         weeks,
         LINHAA) %>% 
  group_by(date, state_num, weeks, LINHAA) %>% 
  summarise(fds = n()) %>% 
  ungroup() 

fds_cause <- 
  fds_state %>% 
  filter(weeks == "28+") %>% 
  group_by(date, LINHAA) %>% 
  summarise(fds = sum(fds)) %>% 
  ungroup()

causes <- 
  fds_cause %>% 
  mutate(year = year(date),
         LINHAA = as.character(LINHAA)) %>% 
  group_by(year, LINHAA) %>% 
  summarise(fds = sum(fds)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  arrange(-fds) %>% 
  mutate(id = 1:n()) %>% 
  ungroup()

top_10_causes <- 
  causes %>% 
  filter(id <= 10) %>% 
  drop_na() %>% 
  pull(LINHAA) %>% 
  unique() %>% 
  as.character()

top_10_cause_inc_2020 <- 
  causes %>% 
  arrange(LINHAA, year) %>% 
  group_by(LINHAA) %>% 
  mutate(incr = fds / lag(fds)) %>% 
  filter(year == 2020) %>% 
  arrange(-incr)


fds_main_causes <- 
  fds_state %>% 
  mutate(LINHAA = as.character(LINHAA),
         cause = ifelse(LINHAA %in% top_10_causes, LINHAA, "other")) %>% 
  group_by(date, cause) %>% 
  summarise(fds = sum(fds)) %>% 
  ungroup()

fds_main_causes %>% 
  ggplot()+
  geom_line(aes(date, fds, col = cause))



# causes related to maternal health/death
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# (P00-P04) Foetus and newborn affected by maternal factors and by complications 
# of pregnancy, labour and delivery

mat_death_causes <- 
  fds_cause %>% 
  ungroup() %>% 
  mutate(cause3 = str_sub(LINHAA, 2, 5)) %>% 
  filter(cause3 %in% "P016") %>% 
  group_by(date) %>% 
  summarise(fds = sum(fds)) %>% 
  ungroup()


mat_causes <- 
  fds_cause %>% 
  ungroup() %>% 
  mutate(cause3 = str_sub(LINHAA, 2, 4)) %>% 
  filter(cause3 %in% paste0("P0", 0:4)) %>% 
  group_by(date) %>% 
  summarise(fds = sum(fds)) %>% 
  ungroup()

mat_causes %>% 
  filter(date >= "2015-01-01") %>% 
  ggplot()+
  geom_line(aes(date, fds))+
  theme_bw()

ggsave("Figures/fetal_deaths/brazil_stillbirths_causes_maternal.png", dpi = 1000, 
       width = 6, height = 3)


# broad cause groups
# ~~~~~~~~~~~~~~~~~~
broad_causes <- 
  fds_cause %>% 
  mutate(year = year(date),
         LINHAA = as.character(LINHAA),
         cause3 = str_sub(LINHAA, 2, 4)) %>% 
  group_by(year, cause3) %>% 
  summarise(fds = sum(fds)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  arrange(-fds) %>% 
  mutate(id = 1:n()) %>% 
  ungroup()

top_15_broad_causes <- 
  broad_causes %>% 
  filter(id <= 15) %>% 
  drop_na() %>% 
  pull(cause3) %>% 
  unique()

fds_broad_causes <- 
  fds_state %>% 
  mutate(LINHAA = as.character(LINHAA),
         cause3 = str_sub(LINHAA, 2, 4),
         cause3b = ifelse(cause3 %in% top_15_broad_causes, cause3, "other")) %>% 
  group_by(date, cause3b) %>% 
  summarise(fds = sum(fds)) %>% 
  ungroup()

fds_broad_causes %>% 
  ggplot()+
  geom_line(aes(date, fds, col = cause3b))

# =====


# fit a baseline for each cause, including exposures
# ==================================================
monthly_bts <- 
  bts_10_20 %>% 
  group_by(date) %>% 
  summarise(bts = sum(bts)) %>% 
  ungroup()

#
cause_for_fit <- 
  fds_broad_causes %>% 
  left_join(monthly_bts) %>% 
  arrange(cause3b, date) %>% 
  group_by(cause3b) %>% 
  mutate(t = 1:n()) %>% 
  ungroup() %>% 
  rename(exposure = bts) %>% 
  mutate(w = ifelse(date <= "2020-03-01", 1, 0),
         month = month(date))

est_baseline <- function(db){
  
  base_gam <-
    gam(fds ~
          t +
          s(month, bs = 'cp', k = 12) +
          offset(log(exposure)),
        weights = w,
        data = db,
        family = quasipoisson(link = "log"))
  
  resp <- predict(base_gam, newdata = db, type = "response", se.fit = TRUE)
  
  db %>% 
    mutate(bsn = resp$fit,
           p_score = fds / bsn,
           fds_r = fds / exposure,
           bsn_r = bsn / exposure) %>% 
    left_join(simul_intvals(base_gam, db, 500)) %>% 
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

cause_bsln <- 
  cause_for_fit %>% 
  group_by(cause3b) %>% 
  do(est_baseline(db = .data)) %>% 
  mutate(p_score_un = ifelse(fds > ul | fds < ll, p_score, 1))

  
cause_bsln2020 <- 
  cause_bsln %>% 
  mutate(year = year(date)) %>% 
  filter(year >= 2020)

cause_bsln2020 %>% 
  ggplot()+
  geom_point(aes(date, p_score, col = cause3b))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  theme_bw()


# highest p-score by cause
# ========================
cause_pscores <- 
  cause_bsln %>% 
  mutate(excess = fds - bsn) %>% 
  filter(date >= "2020-03-01") %>% 
  group_by(cause3b) %>% 
  summarise(excess = sum(excess),
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(p_score = (excess + bsn)/bsn) %>% 
  arrange(-p_score) 


# leading relative increases (p-scores) 
# P07: 75%
# P05: 31%
# P29: 36%
# P95: 24%

# "P07   Disorders related to short gestation and low birth weight, not elsewhere classified"
# "P05   Slow foetal growth and foetal malnutrition"
# "P29   Cardiovascular disorders originating in the perinatal period"
# "P95   Foetal death of unspecified cause"

cause_pscores %>% 
  ggplot()+
  geom_point(aes(p_score, reorder(cause3b, p_score)))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()+
  labs(y = "ICD-10 code")

ggsave("Figures/fetal_deaths/brazil_stillbirths_cause_p_scores.png", dpi = 1000, 
       width = 6, height = 3)




# ======================



# analysis of stillbirths weights
# ===============================

fds_weight <- 
  fds2 %>% 
  mutate(weeks = case_when(GESTACAO %in% 1:2 ~ "<27",
                           GESTACAO %in% 3:6 ~ "28+",
                           TRUE ~ NA_character_),
         date_d = dmy(DTOBITO),
         year_d = year(date_d),
         month_d = month(date_d),
         muncode = as.double(as.character(CODMUNOCOR)),
         state_num = str_sub(as.character(CODMUNOCOR), 1, 2),
         date = make_date(d = 15, m = month_d, y = year_d),
         weight = as.double(PESO)) %>% 
  select(date, 
         state_num,
         weeks,
         weight) %>% 
  filter(!is.na(weight),
         !is.na(weeks)) %>% 
  group_by(date, weeks) %>% 
  summarise(fds = n(),
            weight = mean(weight)) %>% 
  ungroup() 


fds_weight %>% 
  filter(date >= "2015-01-01") %>% 
  filter(weeks == "28+") %>% 
  ggplot()+
  geom_line(aes(date, weight))+
  theme_bw()

ggsave("Figures/fetal_deaths/brazil_stillbirths_weight.png", dpi = 1000, 
       width = 6, height = 3)

# ======================



# analysis of stillbirths weeks
# ===============================
fds_weeks <- 
  fds2 %>% 
  mutate(weeks = case_when(GESTACAO == 1 ~ (22)/2,
                           GESTACAO == 2 ~ (22+27)/2,
                           GESTACAO == 3 ~ (28+31)/2,
                           GESTACAO == 4 ~ (32+36)/2,
                           GESTACAO == 5 ~ (37+41)/2,
                           GESTACAO == 6 ~ 43,
                           TRUE ~ NA_real_),
         date_d = dmy(DTOBITO),
         year_d = year(date_d),
         month_d = month(date_d),
         muncode = as.double(as.character(CODMUNOCOR)),
         state_num = str_sub(as.character(CODMUNOCOR), 1, 2),
         date = make_date(d = 15, m = month_d, y = year_d),
         weight = as.double(PESO)) %>% 
  select(date, 
         weeks) %>% 
  filter(!is.na(weeks)) %>% 
  group_by(date) %>% 
  summarise(fds = n(),
            weeks = mean(weeks)) %>% 
  ungroup() 


fds_weeks %>% 
  filter(date >= "2015-01-01") %>% 
  ggplot()+
  geom_line(aes(date, weeks))+
  theme_bw()

ggsave("Figures/fetal_deaths/brazil_stillbirths_weeks.png", dpi = 1000, 
       width = 6, height = 3)


