# setup code for estimating disturbances in mortality for stillbirths and 
# ages under 25

# author: Enrique Acosta

# installing and loading required packages ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# install pacman to streamline further package installation
if(!require("pacman", character.only = TRUE)) {
  install.packages("pacman", dep = TRUE)
  if (!require("pacman", character.only = TRUE))
    stop("Package pacman not found")
}
library(pacman)

packages_CRAN <- c("tidyverse","lubridate","ungroup","HMDHFDplus", "readxl",
                   "scales", "mgcv", "countrycode", "cowplot", "eurostat",
                   "spatstat.geom", "ggpubr", "wpp2022", "here",
                   "cartography", "tmap", "sf", "ggh4x", "data.table")

# Install required CRAN packages if not available yet
if(sum(!p_isinstalled(packages_CRAN))>0) {
  install_these <-  packages_CRAN[!p_isinstalled(packages_CRAN)]
  for (i in 1:length(install_these)){
    install.packages(install_these[i],
                     dependencies = "Depends")
  }
}

# Load the required CRAN/github packages
p_load(packages_CRAN, character.only = TRUE)

# avoiding scientific notation
options(scipen=999)
# let's keep the same seed for reproducibility of results
set.seed(2019) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# functions ====
# ~~~~~~~~~~~~~~~

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
    mutate(Deaths = ifelse(TOT != 0 & Deaths != 0 & sum(Deaths) != 0, 
                           (Deaths / sum(Deaths)) * TOT, 
                           0)) %>% 
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


write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,file = paste0("clipboard-", object.size(x)),sep="\t",row.names=row.names,col.names=col.names,...)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# estimation of baseline deaths with bootstrapped prediction intervals
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
est_baseline_pi <- function(chunk){
  
  ct <- unique(chunk$Country)
  sx <- unique(chunk$Sex)
  ag <- unique(chunk$Age)
  cat(paste0(ct, "_", sx, "_", ag, "\n"))
  
  chunk2 <- 
    chunk %>% 
    arrange(Year) %>% 
    mutate(w = ifelse(Year >= 2020, 0, 1),
           t = 1:n())
  
  model <- glm(Deaths ~ Year + offset(log(Exposure)), 
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
      mutate(bsn = NA,
             bsn_lp = NA,
             bsn_up = NA,
             bsn_lc = NA,
             bsn_uc = NA)
  }
  
  return(chunk3)
}

# estimation of rates baseline with bootstrapped prediction intervals
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
est_baseline_rates_pi <- function(chunk){
  
  chunk2 <-
    chunk %>%
    arrange(Year) %>%
    mutate(Rate = Rate * 10,
           w = ifelse(Year >= 2020, 0, 1),
           t = 1:n())
  
  model <- glm(Rate ~ Year,
               weights = w,
               family = "quasipoisson",
               data = chunk2)
  
  test <-
    try(
      res <-
        predict(model,
                newdata = chunk2,
                type = "response")
    )
  
  try(
    chunk3 <-
      chunk2 %>%
      mutate(bsn = res) %>% 
      left_join(simul_intvals_no_off(model,
                                     model_type = "glm",
                                     db = chunk2,
                                     nsim = 1000,
                                     p = 0.95),
                by = "t") %>% 
      mutate(Rate = Rate / 10,
             bsn = bsn / 10,
             bsn_lp = bsn_lp / 10,
             bsn_up = bsn_up / 10)
  )
  
  if(class(test) == "try-error"){
    chunk3 <-
      chunk2 %>%
      mutate(bsn = NA,
             bsn_lp = NA,
             bsn_up = NA,
             bsn_lc = NA,
             bsn_uc = NA)
  }
  
  return(chunk3)
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# sensitivity analyses =========================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# sensitivity baseline deaths including prediction intervals
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
est_bsn_sens_analysis_pi <- 
  function(chunk){
    
    chunk2 <- 
      chunk %>% 
      arrange(Year) %>% 
      mutate(t = 1:n())
    
    test_mod <- 
      try(
        model <- glm(Deaths ~ Year + offset(log(Exposure)), 
                     weights = w,
                     family = "quasipoisson",
                     data = chunk2)
      )
    
    test_pre <- 
      try(
        res <- 
          predict(model, 
                  newdata = chunk2,
                  type = "response", 
                  se.fit = TRUE)
      )
    
    if(class(test_mod)[1] != "try-error" & class(test_pre)[1] != "try-error"){
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
    }else{
      chunk2 %>% 
        mutate(bsn = NA,
               bsn_lc = NA,
               bsn_uc = NA,
               bsn_lp = NA,
               bsn_up = NA)
      
    }
  }

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fit_log_lm_sensit_pi <- function(chunk){
  
  chunk2 <-
    chunk %>%
    arrange(Year) %>%
    mutate(Rate = Rate * 10,
           t = 1:n())
  
  test_mod <-
    try(model <- glm(Rate ~ Year,
               weights = w,
               family = "quasipoisson",
               data = chunk2)
  )
  
  test_pre <-
    try(
      res <-
        predict(model,
                newdata = chunk2,
                type = "response")
    )
  
  if(class(test_mod)[1] == "try-error" | class(test_pre)[1] == "try-error"){
    chunk3 <-
      chunk2 %>%
      mutate(bsn = NA,
                       bsn_lp = NA,
                       bsn_up = NA,
                       bsn_lc = NA,
                       bsn_uc = NA)
  }else{
    chunk3 <-
      chunk2 %>%
      mutate(bsn = res) %>% 
      left_join(simul_intvals_no_off(model,
                                     model_type = "glm",
                                     db = chunk2,
                                     nsim = 1000,
                                     p = 0.95),
                by = "t") %>% 
      mutate(Rate = Rate / 10,
             bsn = bsn / 10,
             bsn_lp = bsn_lp / 10,
             bsn_up = bsn_up / 10)
  }
  
  return(chunk3)
}



# =====

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    offset_prd <- matrix(log(db$Exposure))
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



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bootstrapping for regression of rates (no offsets)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
simul_intvals_no_off <-
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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# sensitivity analysis on time resolution
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# monthly baselines 
# ~~~~~~~~~~~~~~~~~
est_mth_baseline_pi <- function(chunk){
  
  model <- 
    gam(dts ~ t + s(week, bs = 'cp') + offset(log(exposure)), 
        weights = w,
        data = chunk, 
        family = quasipoisson(link = "log"))
  
  test <- 
    try(
      res <- 
        predict(model, 
                newdata = chunk,
                type = "response", 
                se.fit = TRUE)
    )
  
  try(
    chunk2 <- 
      chunk %>% 
      mutate(bsn = res$fit,
             bsn_lc = bsn - 1.96 * res$se.fit,
             bsn_uc = bsn + 1.96 * res$se.fit) %>% 
      left_join(simul_mth_intvals(model, 
                              model_type = "gam", 
                              db = chunk, 
                              nsim = 1000,
                              p = 0.95),
                by = "t")
  )
  
  if(class(test) == "try-error"){
    chunk2 <- 
      chunk %>% 
      mutate(bsn = NA,
             bsn_lp = NA,
             bsn_up = NA,
             bsn_lc = NA,
             bsn_uc = NA)
  }
  
  return(chunk2)
}

simul_mth_intvals <- 
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

    



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# functions for annual baseline estimation from monthly data (HMIS) ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
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
}


