rm (list = ls())
source("code/00_functions.R")

rts_all <- 
  read_rds("data_inter/annual_deaths_rates_2010_2022.rds") %>% 
  filter(type_data == "rates") 

unique(rts_all$Country)
unique(rts_all$Age)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fitting the linear model in each age group and sex
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# chunk <- 
#   rts_all %>% filter(Country == "Germany", Sex == "t", Age == "Stillbirths")

est_baseline_rates_pi <- function(chunk){

  inf_fct <- 1000
  
  chunk2 <-
    chunk %>%
    arrange(Year) %>%
    mutate(Rate = Rate * inf_fct,
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
      mutate(Rate = Rate / inf_fct,
             bsn = bsn / inf_fct,
             bsn_lp = bsn_lp / inf_fct,
             bsn_up = bsn_up / inf_fct)
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

bsn_all <-
  rts_all %>%
  filter(Year >= 2015) %>%
  group_by(Country, Code, Sex, Age) %>%
  do(est_baseline_rates_pi(chunk = .data)) %>%
  ungroup()%>% 
  mutate(psc = Rate / bsn,
         up = Rate / bsn_lp,
         lp = Rate / bsn_up)


write_rds(bsn_all, "data_output/p_scores_excess_rates.rds")
