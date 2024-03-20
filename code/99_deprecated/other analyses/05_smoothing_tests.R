library(here)
source(here("Code", "00_functions.R"))
# install.packages("MortHump")
# remotes::install_github("cran/MortHump")

# Testing smoothing methods
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

# HMD data Last modified: 20 Jul 2020;  Methods Protocol: v6 (2017)

hmd <- read_csv("Data/CHL_HMD_mx.csv")

clist <- unique(db$Country) %>% sort()
slist <- unique(db$Sex) %>% sort()
ylist <- unique(db$Year) %>% sort()

db2 <- db %>% 
  filter(Sex == s,
         Country == c,
         Year == y) %>% 
  arrange(Age)

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

lambda <- 1e6 

V1 <- pclm(x = ages, 
           y = deaths, 
           nlast = nlast, 
           offset = exposure_s, 
           control = list(lambda = lambda, deg = 2))$fitted

V1 <- pclm(x = ages, 
           y = deaths, 
           nlast = nlast, 
           offset = exposure_s)$fitted

pclm_expos <- 
  tibble(Age = seq(0, 100, 1), 
         mx_pclm_exp = V1 * 100000)

pclm_expos %>% 
  ggplot()+
  geom_line(aes(Age, mx_pclm_exp))+
  geom_point(data = db2, aes(Age, mx))+
  scale_y_log10()

# PCLM 1 without exposures 
# ~~~~~~~~~~~~~~~~~~~~~~~

nlast <- 105 - max(ages)
lambda = 100
V2 <- pclm(x = ages, 
           y = deaths, 
           nlast = nlast, 
           control = list(lambda = lambda, deg = 3))$fitted

pclm_paramt <- 
  tibble(Age = seq(0, 104, 1), Deaths = V2) %>% 
  mutate(Age = ifelse(Age > 100, 100, Age)) %>% 
  group_by(Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Population = exposure_s,
         mx_pclm_par = 100000 * Deaths / Population) %>% 
  select(Age, mx_pclm_par)

pclm_paramt %>% 
  ggplot()+
  geom_line(aes(Age, mx_pclm_par))+
  geom_point(data = db2, aes(Age, mx))+
  scale_y_log10()

# cubic splines
# ~~~~~~~~~~~~~

db3 <- 
  tibble(Age_mid = seq(0, 105, 0.5)) %>% 
  left_join(db2 %>% 
              mutate(Age_mid = Age + (lead(Age) - 1 - Age)/2,
                     Age_mid = ifelse(is.na(Age_mid), Age + (104 - Age)/2, Age_mid)) %>% 
              select(Age_mid, 
                     mx,
                     Deaths,
                     Population)) %>% 
  mutate(w = ifelse(is.na(mx), 0, 1))

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
mx_cubic <- 
  tibble(Age = new_x,
         mx_cub = exp(predict(md2, new_x)$y))

# 3-component p-splines
# ~~~~~~~~~~~~~~~~~~~~~
source("Code_examples/MortHump/mort_hump_functions.r")
chile_h <- 
  db2 %>% 
  select(Age, Deaths, Population, mx) %>% 
  mutate(m = mx / 100000) %>% 
  select(x = Age,
         d = Deaths,
         n = Population,
         m)

data = chile_h
model = "sse"
method = "port"
w = 1/data$m
start = NULL
maxit = 500
x1 = 30
x2 = 50
# x.hump = x1;
# x.sen = x2;
lambda.sen = 100
lambda.hump = 1

fit <- sse.fit(data  = chile_h, maxit = maxit, x.hump = x1, x.sen = x2, lambda.sen = lambda.sen, lambda.hump = lambda.hump)
sse_manual <- log(fit$mhat$mhat1 + fit$mhat$mhat2 + fit$mhat$mhat3)

mx_sse_splines <- 
  tibble(Age = chile_h$x,
         mx_sse = 100000 * exp(sse_manual))

# HMD mortality rates
# ~~~~~~~~~~~~~~~~~~~
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
  rename(mx_hmd = mx) %>% 
  select(-Year, -Sex) 

test <- 
  tibble(Age = seq(0, 100, 0.5)) %>% 
  left_join(mx_hmd) %>% 
  left_join(mx_cubic) %>% 
  left_join(mx_sse_splines) %>%
  left_join(pclm_expos) %>% 
  left_join(pclm_paramt) %>% 
  gather(-Age, key = type, value = mx)

unique(test$type)

cols <- c("#073b4c", "#ef476f", "#ffd166", "#06d6a0", "#118ab2", "#5e60ce", "red")

test %>%
  drop_na() %>% 
  ggplot()+
  geom_point(data = db2, aes(Age, mx))+
  geom_line(aes(Age, mx, col = type))+
  scale_y_log10()+
  scale_size_manual(values = c(1.5, 1, 1, 1, 1))+
  scale_alpha_manual(values = c(0.7, 0.6, 0.6, 0.6, 0.6))+
  scale_color_manual(values = cols)+
  theme_bw()

ggsave(paste0("Figures/smoothing_comparison", c, "_", s, ".png"), dpi = 600)


cols <- c("#073b4c", "#ef476f", "#06d6a0", "#118ab2", "#5e60ce", "red", "#ffd166")

test %>%
  drop_na() %>% 
  filter(type %in% c("mx_cub", "mx_sse", "mx_pclm_par")) %>% 
  ggplot()+
  geom_point(data = db2, aes(Age, mx), size = 0.7, alpha = 0.6)+
  geom_line(aes(Age, mx, col = type), alpha = 0.8)+
  scale_y_log10()+
  scale_color_manual(values = cols)+
  theme_bw()

ggsave(paste0("Figures/smoothing_comparison_example_chile.png"), dpi = 600)


smooth_mx(s = "f", c = "Brazil", y = 2020)
