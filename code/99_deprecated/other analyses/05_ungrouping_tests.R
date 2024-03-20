library(here)
source(here("Code", "00_functions.R"))

# Testing ungrouping methods
# Using Chile as it has single years of death rates and it is also in the HMD
# Testing of smoothing using single-year of age and also ungrouping
# Methods to be tested:
# 1) PCLM using exposures
# 2) PCLM using parameters
# 3) Cubic splines
# 4) P-splines
# 5) 3-component p-splines (working only in single-year of age)
# 6) D-splines (pending)

s <- "f"; c <- "Chile"; y <- 2017

db <- read_rds("Output/annual_deaths_pop.rds")
exps <- read_rds("Output/exposures_single-years.rds")

# HMD mortality rates
# ~~~~~~~~~~~~~~~~~~~
# HMD data Last modified: 20 Jul 2020;  Methods Protocol: v6 (2017)
hmd <- read_csv("Data/CHL_HMD_mx.csv")
hmd2 <- 
  hmd %>% 
  rename(f = Female,
         m = Male,
         t = Total) %>% 
  gather(f, m, t, key = Sex, value = mx)

mx_hmd <- 
  hmd2 %>% 
  mutate(Age = as.integer(Age),
         mx = 100000 * mx) %>% 
  filter(Sex == s,
         Year == y, 
         Age <= 100) %>% 
  select(-Year, -Sex) 

db2 <-
  db %>%
  filter(Sex == s,
         Country == c,
         Year == y) %>%
  arrange(Age) %>%
  mutate(mx = 100000 * Deaths / Population) %>% 
  select(Country, Age, Deaths, Population, mx)
  

gr <- 5
smooth_test <- function(db2, gr){
  
  db3 <- 
    db2 %>% 
    mutate(Age = floor(Age/gr)*gr) %>% 
    group_by(Age) %>% 
    summarise(Deaths = sum(Deaths),
              Population = sum(Population),
              mx = 100000 * Deaths / Population) %>% 
    ungroup()
  
  # PCLM 1 with exposures in single-years 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ages <- db3$Age %>% as.integer()
  deaths <- db3$Deaths
  nlast <- 101 - max(ages)
  
  exposure_s <- 
    exps %>% 
    filter(Sex == s,
           Country == c,
           Year == y) %>% 
    arrange(Age) %>% 
    pull(Population)
  
  lambda <- 1e6 
  
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
  db_cub <- 
    tibble(Age_mid = seq(0, 105, 0.5)) %>% 
    left_join(db3 %>% 
                mutate(Age_mid = Age + (lead(Age) - 1 - Age)/2,
                       Age_mid = ifelse(is.na(Age_mid), Age + (104 - Age)/2, Age_mid)) %>% 
                select(Age_mid, 
                       mx,
                       Deaths,
                       Population)) %>% 
    mutate(w = ifelse(is.na(mx), 0, 1))
  
  ages <- 
    db_cub %>%
    drop_na() %>% 
    pull(Age_mid)
  
  log_mxs <- 
    log(db_cub %>%
          drop_na() %>% 
          pull(mx))
  
  # smoothing in single-years of age
  new_x <- seq(0, 100)
  md2 <- smooth.spline(x = ages, y = log_mxs)
  mx_cubic <- 
    tibble(Age = new_x,
           mx_cub = exp(predict(md2, new_x)$y))
  
  # merging all estimates
  # ~~~~~~~~~~~~~~~~~~~~~
  test <- 
    tibble(Age = seq(0, 100, 0.5)) %>% 
    left_join(mx_cubic) %>% 
    left_join(pclm_expos) %>% 
    left_join(pclm_paramt) %>% 
    gather(-Age, key = type, value = mx) %>% 
    drop_na() %>% 
    mutate(AgeGr = gr)
  
  return(test)
  
}

ungr_01 <- smooth_test(db2, 1)
ungr_05 <- smooth_test(db2, 5)
ungr_10 <- smooth_test(db2, 10)

obs <- 
  db2 %>% 
  select(Age, mx) %>% 
  mutate(type = "Observed",
         AgeGr = "1")

ungr_all <- 
  bind_rows(ungr_01,
            ungr_05,
            ungr_10) %>% 
  mutate(AgeGr = factor(AgeGr))

unique(ungr_all$type)

cols <- c("#073b4c", "#ef476f", "#ffd166", "#06d6a0", "#118ab2", "#5e60ce")
cols <- c("#073b4c", "#ef476f", "#06d6a0", "#118ab2", "#5e60ce", "#ffd166")

ungr_all %>%
  filter(type %in% c("Observed", "mx_pclm_par")) %>% 
  ggplot()+
  geom_point(data = obs, aes(Age, mx))+
  geom_line(aes(Age, mx, col = AgeGr))+
  scale_y_log10()+
  scale_size_manual(values = c(1.5, 1, 1, 1, 1))+
  scale_alpha_manual(values = c(0.7, 0.6, 0.6, 0.6, 0.6))+
  scale_color_manual(values = cols)+
  theme_bw()

ggsave("Figures/ungroup_test_pclm.png")

ungr_all %>%
  filter(type %in% c("Observed", "mx_pclm_par"),
         Age <= 25) %>% 
  ggplot()+
  geom_point(data = obs %>% filter(Age <= 25), aes(Age, mx))+
  geom_line(aes(Age, mx, col = AgeGr))+
  scale_y_log10()+
  scale_size_manual(values = c(1.5, 1, 1, 1, 1))+
  scale_alpha_manual(values = c(0.7, 0.6, 0.6, 0.6, 0.6))+
  scale_color_manual(values = cols)+
  theme_bw()

ggsave("Figures/ungroup_test_pclm_young.png")
