rm (list = ls())
source("Code/00_functions.R")

ccd <- read_rds("data_inter/neo_sbs_unicef.rds")
brazil <- read_rds("data_inter/neo_sbs_brazil.rds")
mexico <- read_rds("data_inter/neo_sbs_mexico.rds")
usa <- read_rds("data_inter/neo_sbs_usa.rds")
zaf <- read_rds("data_inter/neo_sbs_zaf.rds")

cts_exc <- c("Nauru", "Andorra")
cts_exc <- c()

all <- 
  bind_rows(ccd,
            brazil,
            mexico,
            usa,
            zaf) %>% 
  filter(!country %in% cts_exc) %>% 
  mutate(exposure = ifelse(measure == "sbs", bts + value, bts))

write_rds(all, "data_inter/neonatal_and_stillbirths_vital_reg.rds")

# summary of available data
all %>% 
  filter(year >= 2020,
         measure %in% c("neo", "sbs")) %>% 
  select(country, year, measure) %>% 
  unique() %>%
  mutate(n = 1) %>% 
  spread(year, n) %>% 
  replace_na(list(`2020` = 0, `2021` = 0)) %>% 
  group_by(measure) %>% 
  summarise(`2020` = sum(`2020`),
            `2021` = sum(`2021`))

# fitting baselines
# ~~~~~~~~~~~~~~~~~

# chunk <-
#   all %>%
#   filter(country == "Argentina",
#          measure == "sbs") %>%
#   mutate(t = 1:n(),
#          w = ifelse(year < 2020, 1, 0))

fit_sbs_neo <- function(chunk){
  
  model <- glm(value ~ t + offset(log(exposure)),
               data = chunk,
               weights = w,
               family = "quasipoisson")
  
  test <- try(pred <- predict(model, 
                              se.fit = TRUE,
                              type = "response"))
  
  try(out <- 
        chunk %>%
        mutate(bsn = pred$fit,
               bsn_lc = (pred$fit - 1.96 * pred$se.fit),
               bsn_uc = (pred$fit + 1.96 * pred$se.fit)) %>% 
        left_join(simul_intvals(model, 
                                model_type = "glm", 
                                db = chunk, 
                                nsim = 1000,
                                p = 0.95),
                  by = "t"))
  
  if(class(test) == "try-error"){
    out <- 
      chunk %>% 
      mutate(bsn = NA,
             lc = NA,
             uc = NA,
             lp = NA,
             up = NA)
    
  }
  
  return(out)
}
simul_intvals <- 
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
    # model.offset(x)
    
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
      select(t)
    
    colnames_y_sim <- paste0('deaths_sim', 1:nsim)
    
    ints_simul[,colnames_y_sim] <- y_sim
    
    # prediction intervals output
    ints_simul <-
      ints_simul %>%
      pivot_longer(cols = starts_with('deaths_sim'),
                   names_to = 'sim_id', values_to = 'deaths_sim') %>%
      group_by(t) %>%
      summarise(
        bsn_lp = quantile(deaths_sim, lp, na.rm = TRUE),
        bsn_up = quantile(deaths_sim, up, na.rm = TRUE), 
        .groups = 'drop'
      ) 
    
    return(ints_simul)
  }


# fitting baselines 
bsn <- 
  all %>% 
  filter(measure %in% c('neo', 'sbs')) %>% 
  group_by(country, measure) %>% 
  arrange(year) %>% 
  mutate(t = 1:n(),
         w = ifelse(year < 2020, 1, 0)) %>% 
  do(fit_sbs_neo(chunk = .data)) %>% 
  ungroup()


# fitting rates for South Africa
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fit_sbs_neo_r <- function(chunk){
  
  model <- glm(value ~ t,
               data = chunk,
               weights = w,
               family = "quasipoisson")
  
  test <- try(pred <- predict(model, 
                              se.fit = TRUE,
                              type = "response"))
  
  try(out <- 
        chunk %>%
        mutate(bsn = pred$fit,
               bsn_lp = (pred$fit - 1.96 * pred$se.fit),
               bsn_up = (pred$fit + 1.96 * pred$se.fit)))
  
  if(class(test) == "try-error"){
    out <- 
      chunk %>% 
      mutate(bsn = NA,
             bsn_lp = NA,
             bsn_up = NA)
    
  }
  
  return(out)
}

zaf2 <- 
  zaf %>% 
  filter(year >= 2015) %>% 
  group_by(measure) %>% 
  mutate(t = 1:n(),
         w = ifelse(year < 2020, 1, 0)) %>% 
  group_by(measure) %>% 
  do(fit_sbs_neo_r(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(bts = 899303,
         exposure = 899303)

out <- 
  bsn %>% 
  bind_rows(zaf2) %>%
  mutate(psc = value / bsn,
         up = value / bsn_lp,
         lp = value / bsn_up)

write_rds(out, "data_inter/sbs_neo_baselines.rds")

