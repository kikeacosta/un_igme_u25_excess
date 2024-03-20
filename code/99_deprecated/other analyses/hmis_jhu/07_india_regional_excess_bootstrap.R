
library(here)
source(here("Code", "00_functions.R"))

# India
# =====

db <- 
  read_csv(here("Data", "HMIS", "HMIS_data_India_byState_2015_2021.csv")) %>% 
  rename(State = Region) %>% 
  mutate(State = recode(State,
                        "Andaman & Nicobar Islands" = "Andaman and Nicobar Islands",
                        "Dadra & Nagar Haveli" = "Dadra and Nagar Haveli and Daman and Diu",
                        "Daman & Diu" = "Dadra and Nagar Haveli and Daman and Diu",
                        "The Dadra And Nagar Haveli And Daman And Diu" = "Dadra and Nagar Haveli and Daman and Diu",
                        "Jammu And Kashmir" = "Jammu and Kashmir")) %>% 
  group_by(Indicator, Year, Month, State) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()

db_regs <- read_csv("Data/india_regions_states.csv") %>% 
  rename(State = State_Territory)

unique(db$State) %>% sort
unique(db_regs$State) %>% sort


db2 <- 
  db %>% 
  left_join(db_regs) %>% 
  mutate(Region = ifelse(State == "All India", "All India", Region)) %>% 
  group_by(Indicator, Year, Month, Region) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  filter(!Region %in% c("Arabian Sea", "Bay of Bengal"))


unique(db2$Indicator)
unique(db2$Region)
unique(db2$Year) %>% sort

db3 <- 
  db2 %>% 
  mutate(Indicator = recode(Indicator,
                            "Infant deaths" = "inf",
                            "Neonatal deaths" = "neo", 
                            "Stillbirths" = "sbs",
                            "Facility deliveries" = "dvs_fct",
                            "Home deliveries" = "dvs_home",
                            "Live birth" = "bts",
                            "Postneonatal deaths" = "pos",
                            "Child deaths age 1 to 4" = "1_4",
                            "Maternal deaths" = "mat",
                            "Under-five deaths" = "0_4",
                            "Deaths 1 month to 5 years" = "1m_4y"),
         date = make_date(d = 15, 
                          month = match(Month, month.abb), 
                          year = Year))

db4 <- 
  db3 %>%
  filter(Indicator %in% c("bts",
                          "sbs",
                          "neo")) %>% 
  spread(Indicator, value) %>% 
  drop_na()


db_neo <- 
  db4 %>% 
  rename(value = neo,
         exposure = bts) %>% 
  mutate(measure = "neo") %>% 
  select(-sbs)

db_sbs <- 
  db4 %>% 
  mutate(exposure = bts + sbs,
         measure = "sbs") %>% 
  rename(value = sbs) %>% 
  select(-bts, -neo)

db_to_fit <- 
  bind_rows(db_neo,
            db_sbs) %>% 
  mutate(month = match(Month, month.abb)) %>% 
  select(-Year, -Month) %>% 
  arrange(Region, measure, date) %>% 
  group_by(Region, measure) %>% 
  mutate(t = 1:n(),
         w = ifelse(date <= "2020-03-15", 1, 0)) %>% 
  ungroup() %>% 
  mutate(Region = recode(Region,
                         "All India" = "India",
                         "Central" = "C",
                         "Eastern" = "E",
                         "Northeastern" = "NE",
                         "Northern" = "N",
                         "Southern" = "S", 
                         "Western" = "W"))

unique(db_to_fit$Region)
  

est_baseline <- 
  function(chunk){
    chunk2 <- 
      chunk %>% 
      arrange(date) %>% 
      mutate(t = 1:n(),
             w = ifelse(date <= "2020-03-15", 1, 0),
             month = month(date))
    
    gam_model <- gam(value ~ t + s(month, bs = 'cp') + offset(log(exposure)),
                     weights = w,
                     data = chunk2,
                     family = "quasipoisson")
    
    gam_pred <- predict(gam_model, newdata = chunk2, type = "response", se.fit = TRUE)

    chunk %>% 
      bind_cols(tibble(bsn = gam_pred$fit)) %>%
                  left_join(simul_intvals(gam_model, chunk, 200))
                
  }
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
      bsn_l = quantile(deaths_sim, 0.05, na.rm = TRUE),
      bsn_u = quantile(deaths_sim, 0.95, na.rm = TRUE)
    ) %>%
    ungroup()
  
  return(ints_simul)
}

bslns_reg <- 
  db_to_fit %>% 
  group_by(Region, measure) %>% 
  do(est_baseline(chunk = .data)) %>% 
  mutate(type_excess = case_when(
    date >= "2020-03-15" & value < bsn_l ~ "Negative",
    date >= "2020-03-15" & value > bsn_u ~ "Positive",
    TRUE ~ "No-excess"),
    type_excess = factor(type_excess, 
                         levels = c("Negative", "No-excess", "Positive")))


# India example
bslns_reg %>% 
  filter(measure == "sbs",
         Region == "India") %>% 
  ggplot()+
  geom_line(aes(date, bsn), alpha = 0.5, col = "#118ab2")+
  geom_ribbon(aes(date, ymin = bsn_l, ymax = bsn_u), alpha = 0.3,
              fill = "#118ab2")+
  geom_point(aes(date, value, col = type_excess), size = 1)+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed",
             col = "red")+
  scale_x_date(date_breaks = "12 months", date_labels = "%b%y")+
  scale_color_manual(values = c("blue", "black", "red"))+
  labs(y = "counts")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(margin = margin(b = 0.4, t = 0.4),
                              size = 8) 
  )
ggsave("Figures/last version//india_example_excess_sbs.png", dpi = 600,
       height = 2.5,
       width = 5)



bslns_reg %>% 
  filter(measure == "sbs") %>% 
  ggplot()+
  geom_line(aes(date, bsn), alpha = 0.5, col = "#118ab2")+
  geom_ribbon(aes(date, ymin = bsn_l, ymax = bsn_u), alpha = 0.3,
              fill = "#118ab2")+
  geom_point(aes(date, value, col = type_excess), size = 1)+
  facet_grid(Region~., scales = "free")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed",
             col = "red")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b%y")+
  scale_color_manual(values = c("blue", "black", "red"))+
  labs(y = "counts", subtitle = "Stillbirths")+
  theme_bw()+
  theme(
    plot.subtitle = element_text(size = 9),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0.4, t = 0.4),
                              size = 8) 
  )
ggsave("Figures/TAG_pres/hmis/india_excess_sbs.png", dpi = 600,
       height = 5,
       width = 10)




bslns_reg %>% 
  filter(measure == "neo") %>% 
  ggplot()+
  geom_line(aes(date, bsn), alpha = 0.5, col = "#118ab2")+
  geom_ribbon(aes(date, ymin = bsn_l, ymax = bsn_u), alpha = 0.3,
              fill = "#118ab2")+
  geom_point(aes(date, value, col = type_excess), size = 1)+
  facet_grid(Region~., scales = "free")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed",
             col = "red")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b%y")+
  scale_x_date(limits = c(ymd("2015-01-01"), ymd("2020-12-31")))+
  scale_color_manual(values = c("blue", "black", "red"))+
  labs(y = "counts", subtitle = "Neonatal deaths")+
  theme_bw()+
  theme(
    # legend.position = "none",
    plot.subtitle = element_text(size = 9),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0.4, t = 0.4),
                              size = 8) 
  )
ggsave("Figures/TAG_pres/hmis/india_excess_neo.png", dpi = 600,
       height = 5,
       width = 10)



all_p_scores <- 
  bslns_reg %>% 
  filter(date >= "2020-01-01" & date <= "2020-12-31") %>% 
  group_by(Region, measure) %>% 
  summarize(value = sum(value),
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(all_p_score = value / bsn) %>% 
  arrange(measure, Region)
