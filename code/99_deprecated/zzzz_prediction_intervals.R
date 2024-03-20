rm (list = ls())
source("Code/00_functions.R")

# there is one package that estimates prediction intervals, but does not work yet with offsets!!!
# https://cran.r-project.org/web/packages/trending/vignettes/prediction_intervals.html

dts_all <- 
  read_rds("data_inter/annual_deaths_rates_2010_2021.rds") %>% 
  filter(type_data == "counts") %>% 
  mutate(Deaths = round(Deaths, 0),
         Population = round(Population, 0))

chunk <- 
  dts_all %>% 
  filter(Year >= 2015, 
         Country == "Argentina",
         Sex == "t",
         Age == "Infant") %>% 
  arrange(Year) 


dts_all_fit <- 
  dts_all %>% 
  filter(Year >= 2015) %>% 
  filter(Country %in% c("Brazil", "Colombia", "USA", "Switzerland", "Sweden",
                        "Poland", "Spain")) %>% 
  group_by(Country, Sex, Age) %>% 
  do(est_baseline_pi(chunk = .data)) %>% 
  ungroup()


dts_all_fit %>% 
  filter(Age == "Infant") %>% 
  ggplot()+
  geom_ribbon(aes(x = Year, ymin = lp, ymax = up), alpha = 0.3, fill = "blue")+
  geom_ribbon(aes(x = Year, ymin = lc, ymax = uc), alpha = 0.3, fill = "green")+
  geom_point(aes(Year, Deaths))+
  geom_line(aes(Year, bsn))+
  facet_wrap(~Country, scales = "free_y")+
  theme_bw()
  
ggsave("Figures/last version/uncertainty/methods_comparison.png",
       w = 8, h = 4)

# %>% 
#   mutate(
#     psc = Deaths / bsn,
#     bsn_lp = ifelse(bsn_lp < 0, 0, bsn_lp),
#     bsn_up = ifelse(bsn_up < 0, 0, bsn_up),
#     up = Deaths / bsn_lp,
#     lp = Deaths / bsn_up)


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
             lc = bsn - 1.96 * res$se.fit,
             uc = bsn + 1.96 * res$se.fit) %>% 
      left_join(simul_intvals(model, 
                              model_type = "glm", 
                              db = chunk2, 
                              nsim = 1000,
                              p = 0.95),
                by = "t")
  )
  
  return(chunk3)
}


model_type <- "glm"
db <- chunk2
nsim <- 2000
p <- 0.95

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
        lp = quantile(deaths_sim, lp, na.rm = TRUE),
        up = quantile(deaths_sim, up, na.rm = TRUE), 
        .groups = 'drop'
      ) 
    
    return(ints_simul)
  }


