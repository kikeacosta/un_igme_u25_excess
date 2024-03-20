library(here)
source(here("Code", "00_functions.R"))

db <- read_csv("Output/annual_deaths_latam.csv")
exps <- read_rds("Output/exposures_single-years.rds")

clist <- unique(db$Country) %>% sort()
slist <- unique(db$Sex) %>% sort()
ylist <- unique(db$Year) %>% sort()


s = "f"; c = "Colombia"; y = 2020
s = "f"; c = "Peru"; y = 2019
smooth_mx <- function(s = "f", c = "Colombia", y = 2020){

  db2 <- db %>% 
    filter(Sex == s,
           Country == c,
           Year == y) %>% 
    arrange(Age) %>% 
    mutate(Age_mid = (Age + lead(Age))/2,
           Age_mid = ifelse(is.na(Age_mid), (Age + 105)/2, Age_mid))
  
  
  
  # p-splines using interpolation 
  # ~~~~~~~~~~~~~~~~~~~~~~~
  
  ages <- seq(0, 103, 0.5)

  db3 <- 
    tibble(Age_mid = ages) %>% 
    left_join(db2 %>% 
                select(Age_mid, 
                       mx,
                       Deaths,
                       Population)) %>% 
    mutate(w = ifelse(is.na(mx), 0, 1))
  
  
  deaths <- db3$Deaths
  exposure <- db3$Population
  W <- db3$w
  
  fit1D <- Mort1Dsmooth(x = ages, 
                        y = deaths, 
                        offset = log(exposure), 
                        w = W,
                        overdispersion = T)
  
  pspl_interp <- 
    tibble(Age = ages,
           pspl = 100000 * exp(fit1D$logmortality))
  
  # p-splines using predict 
  # ~~~~~~~~~~~~~~~~~~~~~~~
  
  # deaths <- db2$Deaths %>% as.integer()
  # exposure <- db2$Population
  # ages <- db2$Age_mid
  # 
  # fit1D <- Mort1Dsmooth(x = ages, y = deaths, offset = log(exposure))
  # 
  # mx_sol <- predict(fit1D, newdata = seq(0, 100, 0.5), se.fit = TRUE)
  # 
  # mx_sol2 <- tibble(Age = 0:100, mx_pspl = exp(mx_sol$fit)*100000)

  
  # PCLM 1 with exposures in single-years 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ages <- db2$Age %>% as.integer()
  deaths <- db2$Deaths
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
             offset = exposure_s)$fitted
  
  pclm_expos <- 
    tibble(Age = seq(0, 100, 1), 
           mx_pclm_exp = V1 * 100000)
  
  
  # PCLM 1 without exposures 
  # ~~~~~~~~~~~~~~~~~~~~~~~
  
  nlast <- 110 - max(ages)
  lambda = 100
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
           mx_pclm_par = 100000 * Deaths / Population) %>% 
    select(Age, mx_pclm_par)
  
  
  # cubic splines
  # ~~~~~~~~~~~~~
  
  ages <- 
    db3 %>%
    drop_na() %>% 
    pull(Age_mid)
  
  log_mxs <- 
    log(db3 %>%
          drop_na() %>% 
          pull(mx))
  
  # smoothing in single-years of age
  new_x <- seq(0, 100)
  md2 <- smooth.spline(x = ages, y = log_mxs)
  mx_cubic <- tibble(Age = new_x,
                      mx_cub = exp(predict(md2, new_x)$y))
  
  test <- 
    tibble(Age = seq(0, 100, 0.5)) %>% 
    left_join(mx_cubic) %>% 
    left_join(pspl_interp) %>%
    left_join(pclm_expos) %>% 
    left_join(pclm_paramt) %>% 
    left_join(db2 %>% mutate(Age = Age_mid) %>% select(Age, mx)) %>% 
    gather(-Age, key = type, value = mx)
  
  cols <- c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51")
  cols <- c("#073b4c", "#ef476f", "#ffd166", "#06d6a0", "#118ab2")
  test %>% 
    ggplot()+
    geom_point(aes(Age, mx, col = type, size = type, 
                   alpha = type))+
    scale_y_log10()+
    scale_size_manual(values = c(1.5, 1, 1, 1, 1))+
    scale_alpha_manual(values = c(0.7, 0.6, 0.6, 0.6, 0.6))+
    scale_color_manual(values = cols)+
    theme_bw()
  ggsave(paste0("Figures/smoothing_comparison", c, "_", s, ".png"), dpi = 600)

}

smooth_mx(s = "f", c = "Brazil", y = 2020)
