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



# by place of occurrence
# ======================

fds3 <-
  fds2 %>%
  mutate(weeks = case_when(GESTACAO %in% 1:2 ~ "<27",
                           GESTACAO %in% 3:6 ~ "28+",
                           TRUE ~ NA_character_),
         muncode = as.double(as.character(CODMUNOCOR)),
         state_num = as.double(str_sub(muncode, 1, 2)),
         date_d = dmy(DTOBITO),
         year_d = year(date_d),
         month_d = month(date_d),
         date = make_date(d = 15, m = month_d, y = year_d)) %>%
  select(date,
         place_raw = LOCOCOR,
         weeks,
         state_num) %>%
  group_by(date, place_raw, state_num, weeks) %>%
  summarise(fds = n()) %>%
  ungroup() %>% 
  left_join(reg_codes)

place_state <-
  fds3 %>%
  mutate(place = case_when(place_raw %in% 1:2 ~ "hosp",
                           place_raw %in% 3:6 ~ "other",
                           TRUE ~ NA_character_)) %>%
  group_by(date, state_iso, region, place, weeks) %>%
  summarise(fds = sum(fds)) %>% 
  ungroup()

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

assign_unk_place <- 
  function(chunk){
    tot <- 
      chunk %>% 
      summarise(tot = sum(fds)) %>% 
      pull(tot)
    
    chunk %>% 
      filter(!is.na(place)) %>% 
      mutate(fds = round(tot * fds / sum(fds), 0))
  }


place_state2 <- 
  place_state %>% 
  group_by(date, state_iso, place) %>% 
  do(assign_unk_weeks(chunk = .data)) %>% 
  drop_na(state_iso) %>% 
  ungroup() %>% 
  group_by(date, state_iso, weeks) %>%
  do(assign_unk_place(chunk = .data)) %>% 
  ungroup()
  
place_state3 <- 
  place_state2 %>% 
  group_by(date, state_iso, region, place) %>% 
  summarise(fds = sum(fds)) %>% 
  ungroup() %>% 
  mutate(weeks = "total") %>% 
  bind_rows(place_state2)

place_state4 <- 
  place_state3 %>% 
  left_join(bts_10_20) %>% 
  filter(date >= "2015-01-01")

unique(place_state4$state_iso)

place_region <- 
  place_state4 %>% 
  group_by(date, region, place, weeks) %>% 
  summarise(fds = sum(fds),
            bts = sum(bts)) %>% 
  ungroup() %>% 
  mutate(fds_r = fds / (bts + fds))

place_nal <- 
  place_region %>% 
  group_by(date, place, weeks) %>% 
  summarise(fds = sum(fds),
            bts = sum(bts)) %>% 
  ungroup() %>% 
  mutate(fds_r = fds / (bts + fds))




place_nal %>% 
  filter(date >= "2015-01-01") %>% 
  ggplot()+
  geom_line(aes(date, fds_r))+
  facet_wrap(place ~ weeks, scales = "free")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 10)
  )
ggsave("Figures/fetal_deaths/brazil_fetal_deaths_place.png", dpi = 1000, 
       width = 8, height = 4)




place_region %>% 
  filter(weeks == "28+") %>% 
  filter(date >= "2015-01-01") %>% 
  ggplot()+
  geom_line(aes(date, fds_r))+
  facet_wrap(region ~ place, scales = "free")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 10)
  )



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
  place_region %>% 
  arrange(region, weeks, place, date) %>% 
  group_by(region, weeks, place) %>% 
  mutate(t = 1:n(),
         ws = ifelse(date <= "2020-03-15", 1, 0)) %>% 
  ungroup() %>% 
  mutate(exposure = fds + bts,
         month = month(date)) %>% 
  group_by(region, weeks, place) %>% 
  do(est_baseline(db = .data)) %>% 
  mutate(p_score_un = ifelse(fds > ul | fds < ll, p_score, 1))


region_bsln %>% 
  filter(weeks == "total") %>% 
  mutate(out = ifelse(p_score_un != 1, "1", "0")) %>% 
  ggplot()+
  geom_point(aes(date, fds_r, col = out), size = 1)+
  geom_ribbon(aes(date, ymin = ll_r, ymax = ul_r), alpha = 0.3)+
  geom_line(aes(date, bsn_r), col = "#118ab2", alpha = 0.7)+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed", 
             col = "black",
             alpha = 0.5)+
  facet_wrap(place ~ region, scales = "free", ncol = 5)+
  scale_color_manual(values = c("black", "#ef476f"))+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 10)
  )
ggsave("Figures/excess/brazil_excess_fetal_deaths_place.png", dpi = 1000, 
       width = 8, height = 4)
