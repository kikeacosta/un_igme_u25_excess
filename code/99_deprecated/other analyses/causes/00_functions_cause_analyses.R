library(tidyverse)
library(readxl)
library(lubridate)
library(mgcv)
library(countrycode)
library(scales)
library(ggh4x)
options(scipen=3)

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,file = paste0("clipboard-", object.size(x)),sep="\t",row.names=row.names,col.names=col.names,...)
}

est_baseline_inf <- function(chunk){
  
  chunk2 <- 
    chunk %>% 
    mutate(w = ifelse(year >= 2020, 0, 1))
  
  test <- try(res <- 
                predict(glm(dts ~ year + offset(log(bts)), 
                            weights = w,
                            family = "quasipoisson",
                            data = chunk2), 
                        newdata = chunk2,
                        type = "response", 
                        se.fit = TRUE))
  
  try(chunk3 <- 
        chunk2 %>% 
        bind_cols(tibble(bsn = res$fit,
                         lp_bsn = bsn - 1.96 * res$se.fit,
                         up_bsn = bsn + 1.96 * res$se.fit)))
  
  if(class(test) == "try-error"){
    chunk3 <- 
      chunk2 %>% 
      bind_cols(tibble(bsn = NA,
                       lp_bsn = NA,
                       up_bsn = NA))
  }
  
  return(chunk3)
}


# function for weekly population interpolation 
# ============================================
interpop <- function(db)
{
  xs <- db %>% drop_na() %>% pull(t)
  ys <- db %>% drop_na() %>% pull(pop)
  # smoothing using cubic splines
  ts <- db %>% pull(t)
  inter_pop <- 
    tibble(t = ts,
           pop2 = spline(xs, ys, xout = ts)$y)
  return(inter_pop)
}
# ====


# fitting annual deaths
# ~~~~~~~~~~~~~~~~~~~~~
fit_annual <- function(chunk){
  
  model <- glm(log(dts) ~ t + t^2 + offset(log(exposure)),
               data = chunk %>% mutate(dts = ifelse(dts == 0, 1, dts)),
               weights = w)
  
  pred <- predict(model, se.fit = TRUE)
  
  chunk %>%
    mutate(bsn = pred$fit %>% exp(),
           ll = (pred$fit - 1.96 * pred$se.fit) %>% exp(),
           ul = (pred$fit + 1.96 * pred$se.fit) %>% exp())
}

fit_annual <- function(chunk){
  
  model <- glm(dts ~ t + offset(log(exposure)),
               data = chunk %>% mutate(dts = dts + 1),
               weights = w,
               family = "quasipoisson")
  
  pred <- predict(model, 
                  se.fit = TRUE,
                  type = "response")
  
  chunk %>%
    mutate(bsn = pred$fit,
           ll = (pred$fit - 1.96 * pred$se.fit),
           ul = (pred$fit + 1.96 * pred$se.fit)) 
}
# %>%
  #   ) %>% 
  #   left_join(simul_intvals(model, 
  #                           "gam", 
  #                           chunk, 
  #                           1000, 
  #                           0.95),
  #             by = "date")


# fit_annual <- function(chunk){
#   model <- glm(dts ~ t + offset(log(exposure)),
#                data = chunk,
#                weights = w)
#   pred <- predict(model, se.fit = TRUE)
#   chunk %>% 
#     mutate(bsn = pred$fit,
#            ll = bsn - 1.96 * pred$se.fit,
#            ul = bsn + 1.96 * pred$se.fit) 
# }


# fitting monthly deaths
# ~~~~~~~~~~~~~~~~~~~~~
fit_monthly <- function(chunk){
  model <- gam(dts ~ t + s(mth, bs = 'cp') + offset(log(exposure)),
               data = chunk,
               weights = w)
  pred <- predict(model, se.fit = TRUE)
  chunk %>% 
    mutate(bsn = pred$fit,
           ll = bsn - 1.96 * pred$se.fit,
           ul = bsn + 1.96 * pred$se.fit) 
  # %>%
  #   ) %>% 
  #   left_join(simul_intvals(model, 
  #                           "gam", 
  #                           chunk, 
  #                           1000, 
  #                           0.95),
  #             by = "date")
}

# function for bootstrapping 
# ==========================

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
      group_by(date) %>%
      summarise(
        lp = quantile(deaths_sim, lp, na.rm = TRUE),
        up = quantile(deaths_sim, up, na.rm = TRUE), 
        .groups = 'drop'
      ) 
    
    return(ints_simul)
  }
