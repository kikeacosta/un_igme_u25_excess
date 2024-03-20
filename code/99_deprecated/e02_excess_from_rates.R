rm (list = ls())
library(readxl)
source("Code/00_functions.R")

# all <- read_rds("Output/annual_rates_pop_infant_child.rds")

# rts_inf <- read_rds("data_inter/annual_rates_infant_child.rds")
# rts_yng <- read_rds("data_inter/annual_rates_5y_groups.rds")

rts_all <- 
  read_rds("data_inter/annual_deaths_rates_2010_2021.rds") %>% 
  filter(type_data == "rates") %>% 
  mutate(Deaths = round(Deaths, 0),
         Exposure = round(Exposure, 0))






fit_log_lm <- function(db){
  chunk <- 
    db %>% 
    mutate(log_rate = log(Rate),
           w = ifelse(Year %in% 2015:2019, 1, 0)) %>% 
    arrange(Year)
  
  md <- lm(log_rate ~ Year, 
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
           bsn_up = exp(log_up),
           bsn_lp = exp(log_lp))
} 

rts_inf2 <- 
  rts_inf %>% 
  mutate(Age = recode(Age,
                      "0" = "Infant",
                      "1" = "1-4"))

rts_yng2 <- 
  rts_yng %>% 
  mutate(Age = recode(Age,
                      "0" = "0-4",
                      "5" = "5-9",
                      "10" = "10-14",
                      "15" = "15-19",
                      "20" = "20-24"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fitting the linear model in each age group and sex
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bsn_all <- 
  bind_rows(rts_inf2,
            rts_yng2) %>% 
  filter(Year >= 2015) %>% 
  group_by(Country, Code, Sex, Age) %>% 
  do(fit_log_lm(db = .data)) %>% 
  ungroup() %>% 
  mutate(excess = case_when(Year >= 2020 & Rate > bsn_up ~ "pos",
                            Year >= 2020 & Rate < bsn_lp ~ "neg",
                            TRUE ~ "none"),
         excess = factor(excess, levels = c("pos", "none", "neg")))

# bsn_all <- 
#   all %>% 
#   filter(Year >= 2015) %>% 
#   separate(measure, c("Age", "Region")) %>% 
#   
#   group_by(country, measure, sex) %>% 
#   do(fit_log_lm(db = .data)) %>% 
#   ungroup() %>% 
#   separate(measure, c("measure", "location")) %>% 
#   mutate(excess = case_when(Year == 2020 & rate > bsn_up ~ "pos",
#                             Year == 2020 & rate < bsn_lp ~ "neg",
#                             TRUE ~ "none"),
#          measure = recode(measure,
#                           "chd" = "0_4",
#                           "chd2" = "1_4",
#                           "1" = "1_4",
#                           "5" = "5_9",
#                           "10" = "10_14",
#                           "15" = "15_19",
#                           "20" = "20_24"),
#          measure = factor(measure, levels = c("neo", "pos", "inf", "0_4", "1_4",
#                                               "5_9", "10_14", "15_19", "20_24")),
#          location = factor(location, levels = c("urb", "rur", "tot")),
#          excess = factor(excess, levels = c("pos", "none", "neg")))

col_exc <- 
  c("pos" = "red",
    "neg" = "blue",
    "none" = "black")

bsn_all %>% 
  filter(Country == "China") %>% 
  ggplot()+
  geom_ribbon(aes(x = Year, ymin = bsn_lp, ymax = bsn_up), alpha = 0.2)+
  geom_point(aes(Year, Rate, col = excess))+
  geom_line(aes(Year, bsn))+
  facet_grid( ~ Age, scales = "free_y")+
  scale_color_manual(values = col_exc)+
  theme_bw()+
  theme(axis.text = element_text(size = 6))

bsn_all %>% 
  filter(Country == "South Africa",
         Sex == "t",
         Year >= 2015) %>% 
  ggplot()+
  geom_ribbon(aes(x = Year, ymin = bsn_lp, ymax = bsn_up), alpha = 0.2)+
  geom_point(aes(Year, Rate, col = excess))+
  geom_line(aes(Year, bsn))+
  facet_grid( ~ Age, scales = "free_y")+
  scale_color_manual(values = col_exc)+
  theme_bw()+
  theme(axis.text = element_text(size = 6))

bsn_all %>% 
  filter(Country == "Bangladesh",
         Year >= 2015) %>% 
  ggplot()+
  geom_ribbon(aes(x = Year, ymin = bsn_lp, ymax = bsn_up), alpha = 0.2)+
  geom_point(aes(Year, Rate, col = excess))+
  geom_line(aes(Year, bsn))+
  facet_wrap(~ Age, scales = "free_y", nrow = 5)+
  scale_color_manual(values = col_exc)+
  theme_bw()+
  theme(axis.text = element_text(size = 6))


pscores <- 
  bsn_all %>% 
  mutate(p_score = Rate / bsn,
         up = Rate / bsn_lp,
         lp = Rate / bsn_up,
         exc = case_when(up > 1 & lp > 1 ~ "Positive",
                         up < 1 & lp < 1 ~ "Negative",
                         TRUE ~ "No-excess"),
         out = ifelse(exc == "No-excess", 0.5, 1),
         ins = ifelse(exc == "No-excess", 0.6, 0.4))

tx <- 8
cols <- c("Positive" = "#b7094c",
          "Negative" = "#0091ad",
          "No-excess" = "#5c4d7d")

# China
pscores %>% 
  filter(Year == 2020) %>% 
  filter(Country == "China") %>%
  ggplot()+
  # geom_errorbar(aes(xmin = lp, xmax = up, y = Country, 
  #                   alpha = ins), 
  #               col = "black", size = 0.6, width = 0)+
  geom_point(aes(p_score, Age, 
                 alpha = out, 
                 col = exc))+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  # facet_wrap( ~ location, scales = "free_x", nrow = 1)+
  # facet_wrap( ~ Age, nrow = 1)+
  scale_x_log10()+
  scale_y_discrete(limits = rev)+
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 4),
        axis.title = element_blank())
# ggsave(paste0("Figures/last version/china_bangladesh/pscores_china.png"), 
#        dpi = 600,
#        width = 2, height = 3)


# Bangladesh
pscores %>% 
  filter(Year == 2020) %>% 
  filter(country == "Bangladesh") %>%
  ggplot()+
  # geom_errorbar(aes(xmin = lp, xmax = up, y = Country, 
  #                   alpha = ins), 
  #               col = "black", size = 0.6, width = 0)+
  geom_point(aes(p_score, measure, 
                 alpha = out, 
                 col = exc))+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  # facet_wrap( ~ location, scales = "free_x", nrow = 1)+
  scale_x_log10()+
  scale_y_discrete(limits = rev)+
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  labs(y = "Country")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        axis.title = element_blank())

# ggsave(paste0("Figures/last version/china_bangladesh/pscores_bangladesh.png"), 
#        dpi = 600,
#        width = 1.5, height = 3)

unique(pscores$measure)
pscores2 <- 
  pscores %>%
  filter(!measure %in% c("neo", "pos"),
         location %in% "tot") %>% 
  select(Country = country, Year, Sex = sex, Age = measure, p_score, up, lp) %>% 
  mutate(Code = countrycode(Country, origin = "country.name",
                               destination = "iso3c"),
         Source = "country_public",
         Age = case_when(Age == "inf" ~ "Infant",
                         TRUE ~ str_replace(Age, "_", "-")))

write_rds(pscores2, "Output/p_scores_excess_rates_data.rds")

pscores2 <- read_rds("Output/p_scores_excess_rates_data.rds")

# raw_rates <- 
#   pscores2 %>% 
#   filter(!(Country == "Bangladesh" & Age == "0-4")) %>% 
#   select(Country, Code, Year, Sex, Age, Source) %>% 
#   mutate(Deaths = 0,
#          age_up = case_when(Age == "0-4" ~ 4,
#                             Age == "1-4" ~ 4,
#                             Age == "Infant" ~ 0),
#          Age = case_when(Age == "0-4" ~ 0,
#                          Age == "1-4" ~ 1,
#                          Age == "Infant" ~ 0),
#          Population = 0)
#   
# write_rds(raw_rates, "Output/annual_rates_pop_infant_child.rds")








