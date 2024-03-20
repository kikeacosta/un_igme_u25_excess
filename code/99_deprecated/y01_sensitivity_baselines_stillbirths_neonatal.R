rm (list = ls())
source("Code/00_functions.R")

all <- read_rds("data_inter/neonatal_and_stillbirths_vital_reg.rds")
zaf <- read_rds("data_inter/neo_sbs_zaf.rds")

# summary of available data
all %>% 
  filter(year == 2019,
         measure %in% c("neo", "sbs")) %>% 
  select(country, year, measure) %>% 
  unique() %>%
  mutate(n = 1) %>% 
  group_by(measure) %>% 
  summarise(`2019` = sum(n))

# fitting baselines
# ~~~~~~~~~~~~~~~~~
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
               lp = (pred$fit - 1.96 * pred$se.fit),
               up = (pred$fit + 1.96 * pred$se.fit)))
  
  if(class(test) == "try-error"){
    out <- 
      chunk %>% 
      mutate(bsn = NA,
             lp = NA,
             up = NA)
    
  }
  
  return(out)
}

# stillbirths and neonatal deaths combined
sbs_neo <- 
  all %>% 
  filter(measure %in% c("neo", 'sbs')) %>% 
  select(-exposure) %>% 
  spread(measure, value) %>% 
  mutate(value = sbs + neo, 
         exposure = bts + sbs,
         measure = "sbs_neo") %>% 
  select(-neo, -sbs) %>% 
  drop_na(value)

all2 <- 
  all %>% 
  bind_rows(sbs_neo)

# fitting baselines 
bsn <- 
  all2 %>% 
  filter(measure %in% c('neo', 'sbs', 'sbs_neo')) %>% 
  group_by(country, measure) %>% 
  mutate(t = 1:n(),
         w = ifelse(year < 2019, 1, 0)) %>% 
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
               lp = (pred$fit - 1.96 * pred$se.fit),
               up = (pred$fit + 1.96 * pred$se.fit)))
  
  if(class(test) == "try-error"){
    out <- 
      chunk %>% 
      mutate(bsn = NA,
             lp = NA,
             up = NA)
    
  }
  
  return(out)
}

zaf2 <- 
  zaf %>% 
  filter(year >= 2015) %>% 
  group_by(measure) %>% 
  mutate(t = 1:n(),
         w = ifelse(year < 2019, 1, 0)) %>% 
  group_by(measure) %>% 
  do(fit_sbs_neo_r(chunk = .data)) %>% 
  ungroup()

out <- 
  bsn %>% 
  bind_rows(zaf2)

write_rds(out, "data_inter/sens_analysis_sbs_neo_baselines.rds")


# example
bsn %>% 
  filter(country == "USA",
         measure == "sbs") %>% 
  mutate(rate = value / exposure,
         bsn_r = bsn / exposure,
         lp_r = lp / exposure,
         up_r = up / exposure,
         is_2020 = ifelse(year >= 2019, "y", "n")) %>% 
  ggplot()+
  geom_point(aes(year, rate, col = is_2020), size = 3)+
  geom_ribbon(aes(year, ymin = lp_r, ymax = up_r), alpha = 0.3)+
  geom_line(aes(year, bsn_r))+
  geom_vline(xintercept = 2019.5, linetype = "dashed")+
  scale_color_manual(values = c("black", "red"), guide = "none")+
  # scale_y_continuous(limits = c(0, 0.004))+
  theme_bw()+
  theme()
ggsave("Figures/last version/sens_analysis/stillbirth_fitting_example.png",
       dpi = 300,
       w = 5,
       h = 3)

# zoom out
bsn %>% 
  filter(country == "USA",
         measure == "sbs") %>% 
  mutate(rate = value / exposure,
         bsn_r = bsn / exposure,
         lp_r = lp / exposure,
         up_r = up / exposure,
         is_2020 = ifelse(year >= 2019, "y", "n")) %>% 
  ggplot()+
  geom_point(aes(year, rate, col = is_2020), size = 2)+
  geom_ribbon(aes(year, ymin = lp_r, ymax = up_r), alpha = 0.3)+
  geom_line(aes(year, bsn_r))+
  geom_vline(xintercept = 2019.5, linetype = "dashed")+
  scale_color_manual(values = c("black", "red"), guide = "none")+
  scale_y_continuous(limits = c(0, 0.004))+
  theme_bw()+
  theme()
ggsave("Figures/last version/sens_analysis/stillbirth_fitting_example_zoomout.png",
       dpi = 300,
       w = 5,
       h = 3)

bsn %>% 
  filter(country == "USA",
         measure == "sbs",
         year == 2020) %>% 
  mutate(psc = value / bsn,
         psc_lp = value / up,
         psc_up = value / lp) %>% 
  select(psc, psc_lp, psc_up)

