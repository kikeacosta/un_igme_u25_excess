source("code/00_functions.R")

# Loading STMF data
# ~~~~~~~~~~~~~~~~~

# downloading the last version of STMF Mortality input data zip 
# this version as of 25 May 2021
# download.file("https://www.mortality.org/File/GetDocument/Public/STMF/Inputs/STMFinput.zip", 
#               "data_input/STMFinput.zip")
# "https://www.mortality.org/File/GetDocument/Public/STMF/Inputs/STMFinput.zip"
# list of country codes in STMF
zipdf <- unzip(here("data_input", "STMFinput.zip"), list = TRUE)

# loading all cause deaths from all countries in STMF
db_d <- tibble()
for(i in 1:length(zipdf$Name)){
  csv_file <- zipdf$Name[i]
  print(csv_file)
  temp <- 
    read_csv(unz(here("data_input", "STMFinput.zip"), csv_file)) %>% 
    mutate(Week = as.double(Week))
  db_d <- db_d %>% 
    bind_rows(temp)
}

unique(db_d$PopCode)

# countries with full 2020
cts_2020 <- 
  db_d %>% 
  drop_na(Week) %>% 
  filter(Year == 2020) %>% 
  group_by(PopCode) %>% 
  filter(max(Week) >= 52) %>% 
  pull(PopCode) %>% unique()

# countries with full 2021
cts_2021 <- 
  db_d %>% 
  filter(Year == 2021) %>% 
  group_by(PopCode) %>% 
  filter(max(Week) == 52) %>% 
  pull(PopCode) %>% unique()

# filtering periods 2015-2021 with full annual information
dts <- 
  db_d %>% 
  mutate(PopCode = ifelse(PopCode == "a", "NOR", PopCode)) %>% 
  filter(Year %in% 2010:2021) %>% 
  filter(PopCode %in% cts_2020) %>% 
  filter(Year <= 2020 | PopCode %in% cts_2021) %>% 
  select(-Access, -Type, -Area)

unique(dts$PopCode)

# countries with changes in age groups
unique_ages_year <- 
  dts %>% 
  select(PopCode, Age, AgeInterval) %>% 
  unique() %>% 
  group_by(PopCode, Age) %>% 
  summarise(n = n()) %>% 
  filter(n >= 2)

# countries with changes in sex
unique_sex_year <- 
  dts %>% 
  select(PopCode, Sex, Week, Year) %>% 
  unique() %>% 
  group_by(PopCode, Week, Year) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  select(PopCode, Year, n) %>% 
  unique() %>% 
  filter(n < 3)



exc <- c("USA", "GBR_NIR", "GBRTENW", "GBR_SCO")

dts2 <- 
  dts %>% 
  filter(!PopCode %in% exc,
         Age == "0" & AgeInterval %in% c("1", "5"),
         Sex == "b",
         Year %in% 2015:2021) %>% 
  group_by(code = PopCode, year = Year, week = Week) %>% 
  summarise(dts = sum(Deaths)) %>% 
  ungroup() 


# populations

# looking for countries in the HMD
hmd_codes <- 
  read_csv("data_input/country_codes_hmd.csv") 

cts_hmd <- 
  dts2 %>% 
  pull(code) %>% 
  unique()

hmd_us <- Sys.getenv("hmd_us")
hmd_pw <- Sys.getenv("hmd_pw")

hmd_exps <- tibble()
for(ct in cts_hmd){
  chunk_p <- readHMDweb(ct, "Exposures_1x1", hmd_us, hmd_pw) %>%
    filter(Year >= 2010) %>%
    as_tibble() %>%
    mutate(Code = ct)
  
  hmd_exps <- hmd_exps %>%
    bind_rows(chunk_p)
}

pop_hmd0 <- 
  hmd_exps %>% 
  group_by(Code) %>% 
  filter(max(Year) >= 2020,
         !Code %in% c("GBRCENW", "DEUTE", "DEUTW", "FRACNP", "NZL_NM", "NZL_NM",
                      "USA", "GBR_NIR", "GBRTENW", "GBR_SCO")) %>% 
  ungroup() %>% 
  select(year = Year, code = Code, age = Age, pop = Total) %>% 
  filter(age <= 4) %>% 
  group_by(code, year) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(code = case_when(code == "GBR_NP" ~ "GBR",
                          code == "NZL_NP" ~ "NZL",
                          code == "FRATNP" ~ "FRA",
                          code == "DEUTNP" ~ "DEU",
                          TRUE ~ code))

# from annual to weekly population
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# interpolation 
library(ISOweek)
# years with 53 weeks 2000-2025
leap_yrs <- 
  tibble(year = 2010:2025) %>% 
  mutate(isoweek = paste0(year, "-W53-7"),
         date = ISOweek2date(isoweek),
         isoweek2 = date2ISOweek(date),
         equal = ifelse(isoweek == isoweek2, "y", "n")) %>% 
  filter(equal == "y") %>% 
  pull(year)

# locate week at the mid-year 2000-2025 
wk_midyear <- 
  tibble(year = 2010:2025) %>% 
  mutate(wks = ifelse(year %in% leap_yrs, 53, 52),
         hlf = wks/2,
         t = cumsum(wks) - 26) %>% 
  select(year, t)


pop3 <- 
  pop_hmd0 %>% 
  left_join(wk_midyear) %>% 
  mutate(t = round(t))

  
# Interpolating exposures to weeks  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
interpop <- function(db)
{
  xs <- db %>% drop_na() %>% pull(t)
  ys <- db %>% drop_na() %>% pull(pop)
  # smoothing using cubic splines
  ts <- db %>% pull(t) %>% sort()
  db %>% 
    mutate(pop2 = spline(xs, ys, xout = ts)$y)
}

pop_interpol <- 
  expand_grid(year = 2010:2022, week = 1:52, code = pop3$code %>% unique()) %>% 
  bind_rows(expand_grid(year = leap_yrs, week = 53, code = pop3$code %>% unique())) %>% 
  arrange(code, year, week) %>% 
  group_by(code) %>% 
  mutate(t = 1:n()) %>% 
  ungroup() %>% 
  left_join(pop3) %>%  
  group_by(code) %>% 
  do(interpop(db = .data)) %>% 
  ungroup() %>% 
  mutate(isoweek = paste0(year, "-W", sprintf("%02d",week), "-1"),
         date = ISOweek2date(isoweek))

pop_interpol %>%
  filter(code == "BEL") %>%
  ggplot()+
  geom_line(aes(t, pop2))+
  geom_point(aes(t, pop))

pop_int2 <- 
  pop_interpol %>% 
  select(code, year, week, date, exposure = pop2)

# 
dts_pop <- 
  dts2 %>% 
  left_join(pop_int2) %>% 
  mutate(exposure = exposure / 52) %>% 
  drop_na() %>% 
  group_by(code) %>% 
  mutate(t = 1:n(),
         w = ifelse(date <= "2020-01-01", 1, 0)) %>% 
  ungroup()

unique(dts_pop$code)


chunk <- 
  dts_pop %>% 
  filter(code == "ESP")

# date_ini <- "2020-01-01"

# function for fitting period aggregated baseline of any measure ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fit_any_aggr <- function(chunk, date_ini){
  
  model <- 
    gam(dts ~ t + s(week, bs = 'cp') + offset(log(exposure)),
        weights = w,
        data = chunk,
        family = 'quasipoisson')
  
  aggr_out <-
    chunk %>%
    mutate(bsn_pred = predict(model, type = "response", newdata = chunk)) %>% 
    filter(date >= date_ini) %>% 
    group_by(year) %>%
    summarise(exposure = sum(exposure),
              dts = sum(dts),
              bsn_pred = sum(bsn_pred)) %>%
    ungroup()
  
  
  # Prediction intervals: from monthly to annual time resolution
  model_type <- "gam"
  nsim <- 500
  p <- 0.95
  
  lp <- (1 - p) / 2
  up <- 1 - lp
  
  # matrix model extraction
  if(model_type == "glm"){
    X_prd <- model.matrix(model, data = chunk, na.action = na.pass)
  }
  if(model_type == "gam"){
    X_prd <- predict(model, newdata = chunk, type = 'lpmatrix')
  }
  
  # estimated coefficients
  beta <- coef(model)
  
  # offsets extracted directly from the prediction data
  offset_prd <- matrix(log(chunk$exposure))
  
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
    chunk %>% 
    select(date)
  
  colnames_y_sim <- paste0('out_sim', 1:nsim)
  
  ints_simul[,colnames_y_sim] <- y_sim
  
  # aggregated prediction intervals
  ints_simul_ann <-
    ints_simul %>%
    pivot_longer(cols = starts_with('out_sim'),
                 names_to = 'sim_id', values_to = 'out_sim') %>%
    filter(date > date_ini) %>% 
    mutate(year = year(date)) %>% 
    group_by(sim_id, year) %>% 
    summarise(out_sim = sum(out_sim), 
              .groups = 'drop') %>% 
    ungroup() %>% 
    group_by(year) %>%
    summarise(
      bsn = mean(out_sim),
      lp = quantile(out_sim, lp, na.rm = TRUE),
      up = quantile(out_sim, up, na.rm = TRUE), 
      .groups = 'drop'
    )
  
  out <- 
    aggr_out %>% 
    left_join(ints_simul_ann, by = "year")
  
  return(out)
  
}


dts_bsn <- 
  dts_pop %>% 
  group_by(code) %>% 
  do(fit_any_aggr(chunk = .data,
                  date_ini = "2020-01-01")) %>% 
  ungroup() %>% 
  mutate(exc = dts - bsn,
         psc = dts/bsn)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~
# annual resolution
# ~~~~~~~~~~~~~~~~~


est_baseline_pi_test <- function(chunk){
  
  model <- 
    gam(dts ~ year + offset(log(exposure)), 
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
  
  try(
    chunk2 <- 
      chunk %>% 
      mutate(bsn = res$fit,
             bsn_lc = bsn - 1.96 * res$se.fit,
             bsn_uc = bsn + 1.96 * res$se.fit) %>% 
      left_join(simul_intvals(model, 
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

simul_intvals <- function(
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

dts_ann <- 
  dts_pop %>% 
  group_by(code, year) %>% 
  summarise(dts = sum(dts),
            exposure = sum(exposure)) %>% 
  ungroup()

dts_ann_bsn <- 
  dts_ann %>% 
  group_by(code) %>% 
  mutate(w = ifelse(year >= 2020, 0, 1),
         t = 1:n()) %>% 
  do(est_baseline_pi_test(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(exc = dts - bsn,
         psc = dts/bsn)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# comparing estimates from annual and monthly fitting
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dts_bsn

# comp <- 
#   dts_ann_bsn %>% 
#   filter(year >= 2020) %>% 
#   select(code, year, bsn_an = bsn, 
#          dts_an = dts, exc_an = exc, psc_an = psc) %>% 
#   left_join(dts_bsn %>% 
#               select(code, year, bsn_mt = bsn, 
#                      dts_mt = dts, exc_mt = exc, psc_mt = psc))
# 
# pscs <- 
#   comp %>% 
#   select(code, year, psc_an, psc_mt) %>% 
#   gather(psc_an, psc_mt, key = resol, value = psc)

pscs_ann <- 
  dts_ann_bsn %>% 
  filter(year >= 2020) %>% 
  mutate(lp = dts / bsn_up,
         up = dts / bsn_lp,
         type = "annual") %>% 
  select(code, year, psc, lp, up, type)

pscs_mth <- 
  dts_bsn %>% 
  rename(bsn_up = up, 
         bsn_lp = lp) %>% 
  mutate(lp = dts / bsn_up,
         up = dts / bsn_lp,
         type = "weekly") %>% 
  select(code, year, psc, lp, up, type)

pscs <- 
  bind_rows(pscs_ann,
            pscs_mth) %>% 
  mutate(country = countrycode(code, origin = "iso3c",
                               destination = "country.name"))

unique(pscs$code)
exc <- c("ISL", "LUX")
bks <- c(0.6, 0.8, 1, 1.2, 1.6)
lbs <- paste0((bks-1)*100, " %")
cols <- c("#2b2d42", "#d90429")
tx <- 12
pscs %>% 
  filter(!code %in% exc) %>% 
  mutate(type = str_to_title(type)) %>% 
  ggplot()+
  geom_errorbar(aes(xmin = lp, xmax = up, y = country, col = type),
                linewidth = 1)+
  geom_point(aes(psc, country, shape = type, col = type), 
             alpha = 0.9, size = 1.5, stroke = 1)+
  scale_x_log10(breaks = bks, labels = lbs)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_color_manual(values = cols)+
  scale_shape_manual(values = c(1, 16))+
  facet_grid(~year)+
  labs(col = "Data\nconfiguration", shape = "Data\nconfiguration", x = "P-score")+
  theme_bw()+
  theme(strip.text = element_text(size = tx + 2, face = "bold"),
        strip.background = element_blank(),
        legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.text = element_text(size = tx),
        legend.text = element_text(size = tx-1))
  
ggsave("figures/manuscript/figS14_pscores_weekly_annual.png",
       w = 10,
       h = 5)
