library(tidyverse)
library(readxl)
library(here)
library(lubridate)
# library(MortalitySmooth)
library(ungroup)
library(countrycode)
library(scales)
library(mgcv)
library(countrycode)
library(HMDHFDplus)
library(ggh4x)
library(cowplot)

options(scipen=999)
options(scipen=3)

# rescale deaths by age to total age
rescale_age <- function(chunk){
  TOT <- chunk %>% dplyr::filter(Age == "TOT") %>% dplyr::pull(Deaths)
  chunk %>% 
    dplyr::filter(Age != "TOT") %>% 
    mutate(Deaths = (Deaths / sum(Deaths)) * TOT)
}

# rescale deaths by sex to total sexes
rescale_sex <- function(chunk){
  TOT <- chunk %>% dplyr::filter(Sex == "t") %>% dplyr::pull(Deaths)
  chunk %>% 
    dplyr::filter(Sex != "t") %>% 
    mutate(Deaths = (Deaths / sum(Deaths)) * TOT) %>% 
    bind_rows(chunk %>% 
                dplyr::filter(Sex == "t"))
}

# standard data from UNICEF
std_db <- function(db){
  db2 <- db %>% 
    select(Country, Year = YearOccurrence, Sex, Age = AgeStart, Deaths)
}

# Function for grouping population age groups in same categories as deaths 
assign_age_intervals <- function(db, pop, ct, yr){
  
  int <- db %>% 
    filter(Country == ct,
           Year == yr) %>% 
    pull(Age) %>% 
    unique()
  
  if(max(int) < 110){
    int <- c(int, 110)
  }
  
  labs <- int[1:length(int)-1]
  
  pop %>% 
    filter(Country == ct,
           Year == yr) %>% 
    mutate(Age_int = cut(Age, 
                         breaks = int, 
                         include.lowest = TRUE, 
                         right = FALSE, 
                         labels = labs),
           Age_int = as.numeric(as.character(Age_int)))
  
}

smooth_mx <- function(chunk = temp2, s = "f", c = "Colombia", y = 2020, lambda = 1e5){
  
  ages <- seq(0, 105, 0.5)
  
  db3 <- 
    tibble(Age_mid = ages) %>% 
    left_join(chunk %>% 
                select(Age_mid, 
                       mx,
                       Deaths,
                       Population)) %>% 
    mutate(w = ifelse(is.na(mx), 0, 1))
  
  # PCLM 1 with exposures in single-years 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ages <- chunk$Age %>% as.integer()
  deaths <- chunk$Deaths
  nlast <- 101 - max(ages)
  
  exposure_s <- 
    exps %>% 
    filter(Sex == s,
           Country == c,
           Year == y) %>% 
    arrange(Age) %>% 
    pull(Population)
  
  V1 <- pclm(x = ages, 
             y = deaths, 
             nlast = nlast, 
             offset = exposure_s,
             control = list(lambda = lambda, deg = 3))$fitted
  
  pclm_expos <- 
    tibble(Age = seq(0, 100, 1), 
           mx = V1 * 100000,
           type = "mx_pclm_exp")
  
  # PCLM 1 without exposures 
  # ~~~~~~~~~~~~~~~~~~~~~~~
  
  nlast <- 110 - max(ages)
  V2 <- pclm(x = ages, 
             y = deaths, 
             nlast = nlast, 
             control = list(lambda = lambda, deg = 3))$fitted
  
  pclm_paramt <- 
    tibble(Age = seq(0, 109, 1), Deaths = V2) %>% 
    mutate(Age = ifelse(Age > 100, 100, Age)) %>% 
    group_by(Age) %>% 
    summarise(Deaths = sum(Deaths)) %>% 
    ungroup() %>% 
    mutate(Population = exposure_s,
           mx = 100000 * Deaths / Population,
           type = "mx_pclm_par") %>% 
    select(Age, mx, type)
  
  db_all <-
    chunk %>% 
    mutate(type = "observed") %>% 
    select(Age, type, mx) %>% 
    # bind_rows(mx_cubic) %>%
    bind_rows(pclm_expos) %>%
    bind_rows(pclm_paramt) %>% 
    mutate(Country = c,
           Code = unique(chunk$Code),
           Year = y,
           Sex = s) 
  
}

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,file = paste0("clipboard-", object.size(x)),sep="\t",row.names=row.names,col.names=col.names,...)
}


# function to predict mortality by cause
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ext_deaths <- function(db){
  xs <- db %>% drop_na() %>% pull(Year)
  ds <- db %>% drop_na() %>% pull(Deaths)
  new_xs <- min(xs):2020
  # cubic interpolation
  md2 <- splinefun(x = xs, y = ds, method="fmm",  ties = mean)
  pred_deaths <- tibble(Year = new_xs,
                      Deaths = md2(new_xs))
  return(pred_deaths)
}

spline_this <- function(db, l){
  
  xs <- db %>% drop_na() %>% pull(Year)
  ds <- db %>% drop_na() %>% pull(Deaths)
  new_xs <- min(xs):2020
  
  # smoothing remaining life exp
  md <- smooth.spline(x = xs, y = ds, lambda = l)
  res <- tibble(Year = new_xs,
                Deaths = predict(md, new_xs)$y)
  return(res)
}

pred_this <- function(db){
  Year <- db %>% drop_na() %>% pull(Year)
  Deaths <- db %>% drop_na() %>% pull(Deaths)
  new <- tibble(Year = min(Year):2020)
  res <- predict(lm(Deaths ~ Year), new)
  out <- tibble(Year = min(Year):2020, 
                Deaths = as.numeric(res))
  return(out)
}


# Poisson model for baseline mortality
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pred_deaths <- function(chunk2){
  Year <- chunk2 %>% drop_na() %>% pull(Year)
  Deaths <- chunk2 %>% drop_na() %>% pull(Deaths) %>% as.integer()
  new <- tibble(Year = min(Year):2020)
  
  res <- 
    predict(glm(Deaths ~ Year, family = "poisson"), 
            newdata = new,
            type = "response", 
            se.fit = TRUE) %>% 
    as_tibble() %>% 
    mutate(upr = fit + 1.96 * se.fit,
           lwr = fit - 1.96 * se.fit)
  
  out <- tibble(Year = min(Year):2020, 
                Baseline = res$fit,
                up_baseline = res$upr,
                lp_baseline = res$lwr)
  return(out)
}


# Poisson model for baseline mortality (natural splines)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pred_deaths_spl <- function(chunk2){
  Year <- chunk2 %>% drop_na() %>% pull(Year)
  Deaths <- chunk2 %>% drop_na() %>% pull(Deaths) %>% as.integer()
  new <- tibble(Year = min(Year):2020)
  
  res <- 
    predict(glm(Deaths ~ splines::ns(Year, df = 2), family = "poisson"), 
            newdata = new,
            type = "response", 
            se.fit = TRUE) %>% 
    as_tibble() %>% 
    mutate(upr = fit + 1.96 * se.fit,
           lwr = fit - 1.96 * se.fit)
  
  out <- tibble(Year = min(Year):2020, 
                Baseline_s = res$fit,
                up_baseline_s = res$upr,
                lp_baseline_s = res$lwr)
  return(out)
}


# function to fit the Poisson model
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# chunk <- db_inf
# c <- "Brazil"
# s <- "f"

est_exc <- function(chunk = db_inf, ages = "Infant"){
  
  cts <- unique(chunk$Country)
  out <- list()
  for(c in cts){
    temp1 <- 
      chunk %>% 
      filter(Country == c)
    sxs <- unique(temp1$Sex)
    for(s in sxs){
      temp2 <- 
        temp1 %>% 
        filter(Sex == s)
      
      # fit the Poisson model
      temp3 <- 
        est_deaths_exps(temp2)
      
      cs <- paste0(c, "_", s)
      
      out[[cs]] <- 
        chunk %>% 
        filter(Country == c,
               Sex == s) %>% 
        select(Year, Sex, Deaths, Population) %>% 
        left_join(temp3)  %>% 
        mutate(Country = c,
               Sex = s)
      
    }
  }
  
  db_pred <- 
    out %>% 
    bind_rows()
  
  db_exc <- 
    db_pred %>% 
    mutate(p_score = Deaths / Baseline,
           up = Deaths / lp_baseline,
           lp = Deaths / up_baseline,
           Age = ages)
  
  return(db_exc)
}


# function to fit the Poisson model including exposures
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
est_deaths_exps <- function(chunk2){
  
  chunk3 <- chunk2 %>% drop_na()
  
  Year <- 
    chunk3 %>% 
    filter(Year < 2020) %>% 
    pull(Year)
  
  Deaths <- 
    chunk3 %>% 
    filter(Year < 2020) %>% 
    pull(Deaths) %>% 
    as.integer()
  
  Population <- 
    chunk3 %>% 
    filter(Year < 2020) %>% 
    pull(Population) %>% 
    as.integer()
  
  new <- tibble(Year = min(Year):2020,
                Population = chunk3 %>% 
                  pull(Population) %>% 
                  as.integer())
  
  res <- 
    predict(glm(Deaths ~ Year + offset(log(Population)), 
                family = "poisson"), 
            newdata = new,
            type = "response", 
            se.fit = TRUE) %>% 
    as_tibble() %>% 
    mutate(lwr = fit - 1.96 * se.fit,
           upr = fit + 1.96 * se.fit)
  
  out <- tibble(Year = min(Year):2020, 
                Baseline = res$fit,
                lp_baseline = res$lwr,
                up_baseline = res$upr)
  
  return(out)
  
}

# chunk <- 
#   db_inf %>% 
#   filter(Country == "Colombia",
#          Sex == "t")

est_baseline <- function(chunk){
  
  chunk2 <- 
    chunk %>% 
    mutate(w = ifelse(Year >= 2020, 0, 1))
  
  test <- try(res <- 
    predict(glm(Deaths ~ Year + offset(log(Population)), 
                weights = w,
                family = "quasipoisson",
                data = chunk2), 
            newdata = chunk2,
            type = "response", 
            se.fit = TRUE))

  try(chunk3 <- 
        chunk2 %>% 
        bind_cols(tibble(bsn = res$fit,
                         bsn_lp = bsn - 1.96 * res$se.fit,
                         bsn_up = bsn + 1.96 * res$se.fit)))

  if(class(test) == "try-error"){
    chunk3 <- 
      chunk2 %>% 
      bind_cols(tibble(bsn = NA,
                       bsn_lp = NA,
                       bsn_up = NA))
  }
  
  return(chunk3)
}

# estimation of baseline with bootstrapped prediction intervals
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
est_baseline_pi <- function(chunk){
  
  chunk2 <- 
    chunk %>% 
    arrange(Year) %>% 
    mutate(w = ifelse(Year >= 2020, 0, 1),
           t = 1:n())
  
  model <- glm(Deaths ~ Year + offset(log(Population)), 
               weights = w,
               family = "quasipoisson",
               data = chunk2)
  
  test <- 
    try(
      res <- 
        predict(model, 
                newdata = chunk2,
                type = "response", 
                se.fit = TRUE)
    )
  
  try(
    chunk3 <- 
      chunk2 %>% 
      mutate(bsn = res$fit,
             bsn_lc = bsn - 1.96 * res$se.fit,
             bsn_uc = bsn + 1.96 * res$se.fit) %>% 
      left_join(simul_intvals(model, 
                              model_type = "glm", 
                              db = chunk2, 
                              nsim = 1000,
                              p = 0.95),
                by = "t")
  )
  
  if(class(test) == "try-error"){
    chunk3 <- 
      chunk2 %>% 
      bind_cols(tibble(bsn = NA,
                       bsn_lp = NA,
                       bsn_up = NA,
                       bsn_lc = NA,
                       bsn_uc = NA))
  }
  
  return(chunk3)
}


# sensitivity analyses
# ~~~~~~~~~~~~~~~~~~~~
est_bsn_sens_analysis <- function(chunk){
  test <- try(res <- 
                predict(glm(Deaths ~ Year + offset(log(Population)), 
                            weights = w,
                            family = "quasipoisson",
                            data = chunk), 
                        newdata = chunk,
                        type = "response", 
                        se.fit = TRUE))
  try(chunk3 <- 
        chunk %>% 
        bind_cols(tibble(bsn = res$fit,
                         bsn_lc = bsn - 1.96 * res$se.fit,
                         bsn_uc = bsn + 1.96 * res$se.fit)))
      
  if(class(test) == "try-error"){
    chunk3 <- 
      chunk %>% 
      bind_cols(tibble(bsn = NA,
                       bsn_lc = NA,
                       bsn_uc = NA))
  }
  return(chunk3)
}

# sensitivity including prediction intervals
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
est_bsn_sens_analysis_pi <- 
  function(chunk){
    
    model <- glm(Deaths ~ Year + offset(log(Population)), 
                 weights = w,
                 family = "quasipoisson",
                 data = chunk)
    
    test <- 
      try(
        res <- 
          predict(model, 
                  newdata = chunk,
                  type = "response", 
                  se.fit = TRUE)
      )
    
    try(chunk2 <- 
          chunk %>% 
          mutate(bsn = res$fit,
                 bsn_lc = bsn - 1.96 * res$se.fit,
                 bsn_uc = bsn + 1.96 * res$se.fit) %>% 
          left_join(simul_intvals(model, 
                                  model_type = "glm", 
                                  db = chunk, 
                                  nsim = 1000,
                                  p = 0.95),
                    by = "t"))
    
    if(class(test) == "try-error"){
      chunk2 <- 
        chunk %>% 
        bind_cols(tibble(bsn = NA,
                         bsn_lc = NA,
                         bsn_uc = NA,
                         bsn_lp = NA,
                         bsn_up = NA))
  }
  return(chunk2)
}
# =====




# function for bootstrapping ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# bootstrapping process, adapted from Jonas schoeley's method 
# https://github.com/jschoeley/rbx2020
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
    offset_prd <- matrix(log(db$Population))
    
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
