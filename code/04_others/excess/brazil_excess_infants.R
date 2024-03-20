library(here)
source(here("Code", "00_functions.R"))
# Source of data
# https://opendatasus.saude.gov.br/dataset/sistema-de-informacao-sobre-mortalidade-sim-1979-a-2019
# dictionnary
# Source: https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/Estrutura_SIM.pdf

# Variable (Age) IDADE:
# ~~~~~~~~~~~~~~~~~~~
# Idade do falecido em minutos, horas, dias, meses ou anos. (Idade:
# composto de dois subcampos. - O primeiro, de 1 dígito, indica a
# unidade da idade (se 1 = minuto, se 2 = hora, se 3 = mês, se 4 = ano,
# se = 5 idade maior que 100 anos). - O segundo, de dois dígitos, indica a
# quantidade de unidades: 
# Idade menor de 1 hora: subcampo varia de 01 e 59 (minutos); 
# De 1 a 23 Horas: subcampo varia de 01 a 23 (horas);
# De 24 horas e 29 dias: subcampo varia de 01 a 29 (dias); 
# De 1 a menos de 12 meses completos: subcampo varia de 01 a 11 (meses); Anos -
# subcampo varia de 00 a 99; - 9 - ignorado)

# Variable: LINHAA (ICD Code)
# ~~~~~~~~~~~~~~~~
# CIDs informados na Linha A da DO referente ao diagnóstico na Linha A
# da DO (causa terminal - doença ou estado mórbido que causou
# diretamente a morte). (Códigos CID 10)

# Date variables
# ~~~~~~~~~~~~~~
# DTOBITO: Data em que occoreu o óbito.(Data no padrão ddmmaaaa) 
# DTRECEBIM: Data do recebimento. (Data no padrão ddmmaaaa)
# DTRECORIG: Data do recebimento original. (Data no padrão ddmmaaaa)

# data loading
# ============
# State codes
reg_codes <- 
  read_csv("Documents/bra_state_codes_names.csv")

# births 
bts_10_20 <- 
  read_rds("Output/births/brazil_monthly_births_state.rds")

# ======
getwd()

# loading infant deaths from microdata
# ====================================
time_units <- 
  c("0" = "mins",
    "1" = "hrs", 
    "2" = "days", 
    "3" = "mts", 
    "4" = "yrs",
    "5" = ">100")

links <- paste0(here("Data", "Brazil", "deaths"), "/Mortalidade_Geral_", 2015:2021, ".csv")
i <- links[1]
out <- list()
for (i in links){
  cat(i)
  out[[i]] <- read_delim(i, 
                         delim = ";",
                         col_types = cols(.default = "c")) %>%  
    filter(TIPOBITO == "2") %>% 
    select(date_e = DTOBITO, 
           mun_code = CODMUNOCOR,
           age_val = IDADE) %>% 
    mutate(date_e = dmy(date_e),
           mth = month(date_e),
           year = year(date_e),
           date = make_date(d = 15, m = mth, y = year),
           time_unit = str_sub(age_val, 1, 1),
           time_unit = recode(time_unit,
                              !!!time_units),
           age_val = str_sub(age_val, 2, 3) %>% as.integer(),
           state_num = str_sub(mun_code, 1, 2)) %>% 
    mutate(age = case_when(time_unit %in% c("mins", "hrs") ~ "neo_day",
                           time_unit %in% c("days") & age_val <= 7 ~ "neo_week",
                           time_unit %in% c("days") & age_val >= 8 ~ "neo_month",
                           time_unit %in% c("mts") ~ "post_neo",
                           time_unit == "yrs" & age_val < 5 ~ "0_4",
                           time_unit == "yrs" & age_val >= 5  & age_val <= 9 ~ "5_9",
                           time_unit == "yrs" & age_val >= 10  & age_val <= 14 ~ "10_14",
                           time_unit == "yrs" & age_val >= 15  & age_val <= 24 ~ "15_24",
                           TRUE ~ "adult")) %>% 
    group_by(date, age, state_num) %>% 
    summarise(dts = n()) %>% 
    ungroup()
}

dts <- 
  out %>% 
  bind_rows() %>% 
  mutate(state_num = state_num %>% as.double()) %>% 
  left_join(reg_codes)

unique(dts$age)

dts_neo <- 
  dts %>% 
  filter(age %in% c("neo_day", "neo_week", "neo_month")) %>% 
  left_join(bts_10_20)

# by state
dts_neo_state <- 
  dts_neo %>% 
  group_by(date, state_num, state_name, state_iso, region, bts) %>% 
  summarise(dts = sum(dts)) %>% 
  ungroup() %>% 
  mutate(age = "neo") %>% 
  bind_rows(dts_neo) %>% 
  mutate(w = ifelse(date <= "2020-03-15", 1, 0),
         month = month(date)) %>% 
  group_by(state_iso) %>% 
  mutate(t = 1:n()) %>% 
  ungroup() %>% 
  filter(date <= "2021-03-15") %>% 
  rename(exposure = bts)

# by region
dts_neo_region <- 
  dts_neo_state %>% 
  group_by(date, region, age, w, month, t) %>% 
  summarise(dts = sum(dts),
            exposure = sum(exposure)) %>% 
  ungroup() 


# estimating mortality baseline
# ==============================

# baseline fitting function
# ~~~~~~~~~~~~~~~~~~~~~~~~~
est_baseline <- function(db){
  
  base_gam <-
    gam(dts ~
          t +
          s(month, bs = 'cp') +
          offset(log(exposure)),
        weights = w,
        data = db,
        family = quasipoisson(link = "log"))
  
  resp <- predict(base_gam, newdata = db, type = "response", se.fit = TRUE)
  
  db %>% 
    mutate(bsn = resp$fit,
           # ul = bsn + 1.96 * resp$se.fit,
           # ll = bsn - 1.96 * resp$se.fit,
           p_score = dts / bsn,
           dts_r = dts / exposure,
           bsn_r = bsn / exposure) %>% 
    left_join(simul_intvals(base_gam, db, 200)) %>% 
    mutate(ll_r = ll / exposure,
           ul_r = ul / exposure)
}

# bootstrapping using Jonas' method 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

state_bsln <- 
  dts_neo_state %>% 
  group_by(state_iso, age) %>% 
  do(est_baseline(db = .data)) %>% 
  mutate(p_score_un = ifelse(dts > ul | dts < ll, p_score, 1))

region_bsln <- 
  dts_neo_region %>% 
  group_by(region, age) %>% 
  do(est_baseline(db = .data)) %>% 
  mutate(p_score_un = ifelse(dts > ul | dts < ll, p_score, 1))

# ==========

# saving estimates
# ~~~~~~~~~~~~~~~~
write_rds(region_bsln, "Output/excess/brazil_monthly_excess_neonatal_region.rds")
write_rds(state_bsln, "Output/excess/brazil_monthly_excess_neonatal_state.rds")

region_bsln <- read_rds("Output/excess/brazil_monthly_excess_neonatal_region.rds")
state_bsln <- read_rds("Output/excess/brazil_monthly_excess_neonatal_state.rds")


# plots
# =====

# states
# ~~~~~~

# counts
state_bsln %>% 
  ggplot()+
  geom_point(aes(date, dts))+
  geom_line(aes(date, bsn))+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.3)+
  facet_wrap(age ~ state_iso, scales = "free")+
  theme_bw()

# rates
state_bsln %>%
  mutate(out = ifelse(p_score_un != 1, "1", "0")) %>% 
  filter(age == "neo") %>% 
  ggplot()+
  geom_point(aes(date, dts_r, col = out), size = 0.5, alpha = 0.7)+
  geom_ribbon(aes(date, ymin = ll_r, ymax = ul_r), alpha = 0.3)+
  geom_line(aes(date, bsn_r), col = "#118ab2", alpha = 0.7)+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed", 
             col = "black",
             alpha = 0.5)+
  scale_color_manual(values = c("black", "#ef476f"))+
  facet_wrap(region ~ state_iso, scales = "free", ncol = 7)+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 7)
  )
ggsave("Figures/excess/excess_state_neonatal.png", dpi = 1000, 
       width = 8, height = 4)

# p-scores
state_bsln %>% 
  mutate(out = ifelse(p_score_un != 1, "1", "0"),
         age = factor(age, levels = c("neo_day", "neo_week", "neo_month", "neo"))) %>% 
  filter(age == "neo") %>% 
  ggplot()+
  geom_point(aes(date, p_score, col = out), size = 1, alpha = 1)+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed", 
             col = "black",
             alpha = 0.5)+
  scale_color_manual(values = c("black", "#ef476f"))+
  facet_wrap(region ~ state_iso, scales = "free", ncol = 7)+
  geom_hline(yintercept = 1, linetype = "dashed", col = "#118ab2")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 7)
  )

ggsave("Figures/excess/excess_state_neonatal_p_scores.png", dpi = 1000, 
       width = 8, height = 4)

# regions
# ~~~~~~~

# counts
region_bsln %>% 
  ggplot()+
  geom_point(aes(date, dts))+
  geom_line(aes(date, bsn))+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.3)+
  facet_wrap(age ~ region, scales = "free")+
  theme_bw()

# rates
region_bsln %>%
  mutate(out = ifelse(p_score_un != 1, "1", "0"),
         age = factor(age, levels = c("neo_day", "neo_week", "neo_month", "neo"))) %>% 
  filter(age == "neo") %>%
  ggplot()+
  geom_point(aes(date, dts_r, col = out), size = 1)+
  geom_ribbon(aes(date, ymin = ll_r, ymax = ul_r), alpha = 0.3)+
  geom_line(aes(date, bsn_r), col = "#118ab2", alpha = 0.7)+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed", 
             col = "black",
             alpha = 0.5)+
  scale_color_manual(values = c("black", "#ef476f"))+
  facet_grid(age ~ region, scales = "free")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 10)
  )
ggsave("Figures/excess/excess_regional_neonatal_deaths.png", dpi = 1000, 
       width = 8, height = 2.5)

# p-scores
region_bsln %>% 
  filter(age == "neo") %>% 
  mutate(out = ifelse(p_score_un != 1, "1", "0"),
         age = factor(age, levels = c("neo_day", "neo_week", "neo_month", "neo"))) %>% 
  ggplot()+
  geom_point(aes(date, p_score, col = out), size = 1, alpha = 0.8)+
  facet_grid(age ~ region, scales = "free")+
  geom_hline(yintercept = 1, linetype = "dashed", col = "#118ab2")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed", 
             col = "black",
             alpha = 0.5)+
  scale_color_manual(values = c("black", "#ef476f"))+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 10)
  )

ggsave("Figures/excess/excess_regional_neonatal_p_scores.png", dpi = 1000, 
       width = 8, height = 2.5)
