library(here)
library(tidyverse)
library(countrycode)

# Loading STMF data
# ~~~~~~~~~~~~~~~~~

# downloading the last version of STMF Mortality input data zip 
# this version as of 25 May 2021
# download.file("https://www.mortality.org/Public/STMF/Inputs/STMFinput.zip", here("Data/STMFinput.zip"))

# list of country codes in STMF
zipdf <- unzip(here("Data", "STMFinput.zip"), list = TRUE)

# loading all cause deaths from all countries in STMF
db_d <- tibble()
for(i in 1:length(zipdf$Name)){
  csv_file <- zipdf$Name[i]
  print(csv_file)
  temp <- read_csv(unz(here("Data", "STMFinput.zip"), csv_file))
  db_d <- db_d %>% 
    bind_rows(temp)
}

# only countries with complete data in 2021
cts_2021 <- 
  db_d %>% 
  filter(Year == 2021, 
         Week == 52,
         Age == "TOT",
         Sex == "b") %>% 
  pull(PopCode) %>% 
  unique()


unique(db_d$PopCode)
# a PopCode "a"
test <- 
  db_d %>% 
  mutate(id = 1:n())

# it is Norway in week 18, age 90, sex "b"

db_d2 <- 
  db_d %>% 
  filter(PopCode %in% cts_2021) %>% 
  select(-Access, -Type, -AgeInterval, -Area) %>% 
  mutate(PopCode = ifelse(PopCode == "a", "NOR", PopCode)) 

unique(db_d2$PopCode)

db_stmf <- 
  db_d2 %>% 
  mutate(PopCode = recode(PopCode,
                          "AUS2" = "AUS",
                          "DEUTNP" = "DEU",
                          "FRATNP" = "FRA",
                          "NZL_NP" = "NZL"),
         Sex = recode(Sex,
                      "b" = "t"), 
         Week = as.character(Week)) %>%
  rename(Code = PopCode) %>% 
  drop_na(Deaths) %>% 
  group_by(Code, Year, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  filter(Age != "UNK",
         Year >= 2015,
         Year <= 2021) %>% 
  mutate(Source = "stmf",
         Country = countrycode(sourcevar = Code, 
                               origin = "iso3c", 
                               destination = "country.name"),
         Country = case_when(Code == "GBR_NIR" ~ "Northern Ireland", 
                             Code == "GBR_SCO" ~ "Scotland", 
                             Code == "GBRTENW" ~ "England and Wales",
                             Code == "USA" ~ "USA", 
                             TRUE ~ Country)) %>% 
  select(Country, Code, Year, Sex, Age, Deaths, Source)


# write_csv(db_stmf, "Output/stmf.csv")

unique(db_stmf$Sex)

# imputing unknown ages and sexes
rescale_age <- function(chunk){
  TOT <- chunk %>% dplyr::filter(Age == "TOT") %>% dplyr::pull(Deaths)
  chunk %>% 
    dplyr::filter(Age != "TOT") %>% 
    mutate(Deaths = (Deaths / sum(Deaths)) * TOT)
}

# rescale deaths by sex to total sexes
rescale_sex <- function(chunk){
  TOT <- chunk %>% dplyr::filter(Sex == "t") %>% dplyr::pull(Deaths)
  temp1 <- 
    chunk %>% 
    dplyr::filter(Sex != "t") %>% 
    mutate(Deaths = (Deaths / sum(Deaths)) * TOT) %>% 
    bind_rows(chunk %>% 
                dplyr::filter(Sex == "t"))
  
}

db2 <- 
  db_stmf %>% 
  group_by(Country, Sex, Year, Source) %>% 
  do(rescale_age(chunk = .data)) %>% 
  ungroup() %>% 
  group_by(Country, Age, Year, Source) %>% 
  do(rescale_sex(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(Age = as.double(Age)) %>% 
  arrange(Source, Country, Year, Sex, Age) %>% 
  replace_na(list(Deaths = 0))

source_selec <- 
  db2 %>% 
  filter(Sex == "t",
         Age <= 24) %>% 
  group_by(Country, Source, Year) %>% 
  # identifying amount of age groups
  summarise(Deaths = sum(Deaths),
            age_groups = n()) %>% 
  ungroup()


age_grps <- 
  source_selec %>% 
  select(Country, age_groups) %>% 
  unique() %>% 
  group_by(Country) %>% 
  summarise(n = n())

cts_unique_ages <- 
  age_grps %>% 
  filter(n == 1) %>% 
  pull(Country)

db3 <- 
  db2 %>% 
  

fit_log_lm <- function(db){
  chunk <- 
    db %>% 
    mutate(log_deaths = log(Deaths),
           w = ifelse(Year >= 2020, 0, 1)) %>% 
    arrange(Year)
  
  md <- lm(log_deaths ~ Year, 
           weights = w,
           data = chunk)
  pred <- 
    predict(md, newdata = chunk, 
            se.fit = T,
            interval = c("prediction"))$fit %>% 
    as_tibble()
  
  out <- 
    chunk %>% 
    mutate(log_bsn = pred$fit,
           log_up = pred$upr,
           log_lp = pred$lwr,
           bsn = exp(log_bsn),
           up = exp(log_up),
           lp = exp(log_lp))
} 

horrible_ages <- 
  c("Germany", "Israel", "New Zealand")

db3 <- 
  db2 %>% 
  filter(Country %in% cts_unique_ages,
         !Country %in% horrible_ages) %>% 
  filter(Age <= 24,
         Year >= 2014) %>% 
  mutate(Age = ifelse(Age == 1, 0, Age)) %>% 
  group_by(Country, Code, Year, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Deaths = Deaths + 1) %>% 
  group_by(Country, Sex, Age) %>% 
  do(fit_log_lm(db = .data)) %>% 
  ungroup() 


db3 %>% 
  filter(Sex == "t") %>% 
  ggplot()+
  geom_ribbon(aes(x = Year, ymin = lp, ymax = up), alpha = 0.2)+
  geom_point(aes(Year, Deaths))+
  geom_line(aes(Year, bsn))+
  theme_bw()+
  facet_grid(Age ~ Country, scales = "free_y")

pscores <- 
  db3 %>%
  filter(Year >= 2020,
         Sex == "t") %>% 
  mutate(pscore = Deaths / bsn,
         pscore_up = Deaths / lp,
         pscore_lp = Deaths / up)


pscores %>% 
  mutate(Year = Year %>% as.character()) %>% 
  ggplot()+
  geom_point(aes(pscore, Country, col = Year))+
  # geom_errorbar(aes(xmin = pscore_lp, xmax = pscore_up, y = Country))+
  facet_grid(~Age)+
  scale_x_log10()+
  geom_vline(xintercept = 1, linetype = "dashed")+
  theme_bw()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# including extrapolated exposures
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data2020 <- read_rds("Output/annual_deaths_pop.rds")

pops <- 
  data2020 %>% 
  filter(Country %in% cts_unique_ages,
         !Country %in% horrible_ages) %>% 
  mutate(Age = Age - Age %% 5) %>% 
  group_by(Country, Sex, Age, Year) %>% 
  summarise(Population = sum(Population)) %>% 
  ungroup()


chunk <- 
  pops %>% 
  filter(Country == "Austria",
         Sex == "t",
         Age == 0)

pred_pop <- function(chunk){
  
  chunk2 <- 
    chunk %>% 
    select(Year, Population) %>% 
    mutate(w = 1) %>% 
    bind_rows(tibble(Year = 2021,
                     Population = NA,
                     w = 0))

  ct <- chunk$Country %>% unique()
  sx <- chunk$Sex %>% unique()
  ag <- chunk$Age %>% unique()
  
  md <- lm(Population ~ Year,
           weights = w,
           data = chunk2)
  pred <- 
    predict(md, 
            newdata = chunk2)
  
  out <- 
    chunk2 %>% 
    mutate(Country = ct,
           Sex = sx,
           Age = ag,
           pop_pred = pred)
} 

pops_pred <- 
  pops %>% 
  group_by(Country, Sex, Age) %>% 
  do(pred_pop(chunk = .data)) %>% 
  ungroup()


# pops_pred %>% 
#   ggplot()+
#   geom_point(aes(Year, rate, col = is_2020, size = is_2020))+
#   geom_ribbon(aes(Year, ymin = bsn_r_l, ymax = bsn_r_u), 
#               fill = "grey70", alpha = 0.4)+
#   geom_line(aes(Year, bsn_r))+
#   scale_x_continuous(breaks = 2015:2021)+
#   scale_color_manual(values = c("black", "red"))+
#   scale_size_manual(values = c(1, 1.5))+
#   facet_grid(Age~Country, scales = "free")+
#   theme_bw()+
#   theme(legend.position = "none",
#         strip.text.x = element_text(margin = margin(b = 0.1, t = 0.1),
#                                     size = tx + 3),
#         axis.text = element_text(size = tx),
#         strip.background = element_rect(fill = "transparent"))
# 
# ggsave("Figures/last version/y_2021/example_fitting_Austria_Estonia.png",
#        w = 8,
#        h = 5)
# 

pops2 <- 
  pops_pred %>%  
  mutate(Pop = ifelse(Year == 2021, pop_pred, Population)) %>% 
  select(Year, Country, Sex, Age, Pop)

unique(pops2$Country)

db4 <- 
  db2 %>% 
  filter(Country %in% cts_unique_ages,
         !Country %in% horrible_ages) %>% 
  filter(Age <= 24,
         Year >= 2014) %>% 
  mutate(Age = ifelse(Age == 1, 0, Age)) %>% 
  group_by(Country, Code, Year, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  left_join(pops2) %>% 
  filter(Sex == "t") %>% 
  drop_na() %>% 
  rename(Population = Pop)

db_all_fit <- 
  db4 %>% 
  group_by(Country, Sex, Age) %>% 
  do(est_baseline(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(
    p_score = Deaths / Baseline,
    lp_baseline = ifelse(lp_baseline < 0, 0, lp_baseline),
    up_baseline = ifelse(up_baseline < 0, 0, up_baseline),
    up = Deaths / lp_baseline,
    lp = Deaths / up_baseline)

lvs_age <- 
  c("Infant", "1_4", "0_4", "5_9", "10_14", "15_19", "20_24")

lvs_cts3 <- 
  db_all_fit %>% 
  filter(Year == 2020,
         Age == 0,
         Sex == "t") %>% 
  arrange(-p_score) %>% 
  pull(Country) %>% 
  unique()

pscores <- 
  db_all_fit %>% 
  filter(Country != "Iceland") %>% 
  mutate(Age2 = case_when(Age == 0 ~ "0_4",
                          Age == 5 ~ "5_9",
                          Age == 10 ~ "10_14",
                          Age == 15 ~ "15_19",
                          Age == 20 ~ "20_24")) %>% 
  select(-Age) %>% 
  rename(Age = Age2) %>% 
  mutate(exc = case_when(up > 1 & lp > 1 ~ "Positive",
                         up < 1 & lp < 1 ~ "Negative",
                         TRUE ~ "No-excess"),
         out = ifelse(exc == "No-excess", 0.5, 1),
         ins = ifelse(exc == "No-excess", 0.6, 0.4),
         Age = factor(Age, 
                      levels = lvs_age),
         Country = factor(Country, levels = lvs_cts3))


tx <- 10
cols <- c("Positive" = "#b7094c",
          "Negative" = "#0091ad",
          "No-excess" = "#5c4d7d")

pscores %>% 
  filter(Year == 2021) %>% 
  # filter(Country != "Armenia") %>% 
  ggplot()+
  # geom_errorbar(aes(xmin = lp, xmax = up, y = Country, 
  #                   alpha = ins), 
  #               col = "black", size = 0.6, width = 0)+
  geom_point(aes(p_score, Country, 
                 alpha = out, 
                 col = exc,
                 shape = Sex))+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  facet_wrap(~ Age, scales = "free_x", nrow = 1)+
  scale_x_log10()+
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  labs(y = "Country")+
  theme_bw()+
  theme(legend.position = "left",
        legend.text = element_text(size = tx),
        legend.title = element_text(size = tx),
        legend.key.size = unit(3, "mm"),
        strip.text.x = element_text(size = tx),
        axis.title.y = element_blank())



pscores %>% 
  filter(Year >= 2020) %>%
  mutate(Year = Year %>% as.character()) %>% 
  # filter(Country != "Armenia") %>% 
  ggplot()+
  # geom_errorbar(aes(xmin = lp, xmax = up, y = Country, 
  #                   alpha = ins), 
  #               col = "black", size = 0.6, width = 0)+
  geom_point(aes(p_score, Country, 
                 alpha = out, 
                 col = exc,
                 shape = Year))+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  facet_wrap(~ Age, scales = "free_x", nrow = 1)+
  scale_x_log10()+
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  labs(y = "Country")+
  theme_bw()+
  theme(legend.position = "left",
        legend.text = element_text(size = tx),
        legend.title = element_text(size = tx),
        legend.key.size = unit(3, "mm"),
        strip.text.x = element_text(size = tx),
        axis.title.y = element_blank())

ggsave(paste0("Figures/last version/y_2021/excess_2020_2021.png"), 
       dpi = 600, width = 8, height = 4)


# Austria\
db_all_fit2 <- 
  db_all_fit %>% 
  mutate(Age = case_when(Age == 0 ~ "0_4",
                        Age == 5 ~ "5_9",
                        Age == 10 ~ "10_14",
                        Age == 15 ~ "15_19",
                        Age == 20 ~ "20_24"))

cts <- c("Austria", "Estonia")
chunk_plot <- 
  db_all_fit2 %>% 
  filter(Country %in% cts,
         Sex == "t") %>% 
  mutate(is_2020 = ifelse(Year >= 2020, "y", "n"),
         Age = factor(Age, levels = lvs_age))

chunk_plot %>% 
  mutate(rate = Deaths/Population,
         bsn_r = Baseline/Population,
         bsn_r_l = lp_baseline/Population,
         bsn_r_u = up_baseline/Population) %>% 
  ggplot()+
  geom_point(aes(Year, rate, col = is_2020, size = is_2020))+
  geom_ribbon(aes(Year, ymin = bsn_r_l, ymax = bsn_r_u), 
              fill = "grey70", alpha = 0.4)+
  geom_line(aes(Year, bsn_r))+
  scale_x_continuous(breaks = 2015:2021)+
  scale_color_manual(values = c("black", "red"))+
  scale_size_manual(values = c(1, 1.5))+
  facet_grid(Age~Country, scales = "free")+
  theme_bw()+
  theme(legend.position = "none",
        strip.text.x = element_text(margin = margin(b = 0.1, t = 0.1),
                                    size = tx + 3),
        axis.text = element_text(size = tx),
        strip.background = element_rect(fill = "transparent"))

ggsave("Figures/last version/y_2021/example_fitting_Austria_Estonia.png",
       w = 8,
       h = 5)
