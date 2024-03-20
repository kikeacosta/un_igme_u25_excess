rm (list = ls())
library(readxl)
source("Code/00_functions.R")


fit_log_lm_sensit <- function(db){
  chunk <- 
    db %>% 
    mutate(log_rate = log(Rate)) %>% 
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
           log_up_bsn = pred$upr,
           log_lp_bsn = pred$lwr,
           bsn = exp(log_bsn),
           up_bsn = exp(log_up_bsn),
           lp_bsn = exp(log_lp_bsn))
} 

rts_all <- 
  read_rds("data_inter/annual_deaths_rates_2010_2021.rds") %>% 
  filter(type_data == "rate") %>% 
  mutate(Deaths = round(Deaths, 0),
         Population = round(Population, 0))

unique(rts_all$Country)

# ypre = 2020

fit_sensitivity_rts <- 
  function(ypre = 2019,
           ymin = 2014,
           ymax = 2018){
    
    cts_noenough_data <- 
      rts_all %>% 
      filter(Year %in% ymin:ymax) %>% 
      group_by(Country, Sex, Age) %>% 
      filter(n() < 3) %>% 
      select(Country, Sex, Age) %>% 
      unique() %>% 
      mutate(exc = 1)
    
    rts_all_fit <- 
      rts_all %>% 
      filter(Year %in% ymin:ypre) %>% 
      left_join(cts_noenough_data, by = c("Age", "Country", "Sex")) %>% 
      filter(is.na(exc)) %>% 
      select(-exc) %>% 
      mutate(w = ifelse(Year == ymax, 0, 1)) %>% 
      group_by(Country, Sex, Age) %>% 
      do(fit_log_lm_sensit(db = .data)) %>% 
      ungroup() %>% 
      mutate(
        psc = Rate / bsn,
        lp_bsn = ifelse(lp_bsn < 0, 0, lp_bsn),
        up_bsn = ifelse(up_bsn < 0, 0, up_bsn),
        lp = Rate / up_bsn,
        up = Rate / lp_bsn) %>% 
      select(Country, Code, Year, Age, Sex, Population, Deaths, Rate, Income,
             bsn, lp_bsn, up_bsn, psc, up, lp) %>% 
      mutate(year_pred = ypre)
    
  }

sensitivity_2017 <- fit_sensitivity_rts(ypre = 2017, ymin = 2012, ymax = 2016)
sensitivity_2018 <- fit_sensitivity_rts(ypre = 2018, ymin = 2013, ymax = 2017)
sensitivity_2019 <- fit_sensitivity_rts(ypre = 2019, ymin = 2014, ymax = 2018)
sensitivity_2020 <- fit_sensitivity_rts(ypre = 2020, ymin = 2015, ymax = 2019)
sensitivity_2021 <- fit_sensitivity_rts(ypre = 2021, ymin = 2015, ymax = 2019)

sensit_2017_2021 <- 
  bind_rows(sensitivity_2017 %>% filter(Year == 2017),
            sensitivity_2018 %>% filter(Year == 2018),
            sensitivity_2019 %>% filter(Year == 2019),
            sensitivity_2020 %>% filter(Year == 2020),
            sensitivity_2021 %>% filter(Year == 2021))

write_rds(sensit_2017_2021, "Output/sens_analysis_p_scores_excess_rates.rds")



# visual inspection of fitting examples
# 
# bsn_all <- 
#   all %>% 
#   group_by(country, measure,) %>% 
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
# 
# col_exc <- 
#   c("pos" = "red",
#     "neg" = "blue",
#     "none" = "black")
# 
# bsn_all %>% 
#   filter(country == "China") %>% 
#   ggplot()+
#   geom_ribbon(aes(x = Year, ymin = bsn_lp, ymax = bsn_up), alpha = 0.2)+
#   geom_point(aes(Year, rate, col = excess))+
#   geom_line(aes(Year, bsn))+
#   facet_grid(measure ~ location, scales = "free_y")+
#   scale_color_manual(values = col_exc)+
#   theme_bw()+
#   theme(axis.text = element_text(size = 6))
# 
# bsn_all %>% 
#   filter(country == "South Africa",
#          sex == "t") %>% 
#   ggplot()+
#   geom_ribbon(aes(x = Year, ymin = bsn_lp, ymax = bsn_up), alpha = 0.2)+
#   geom_point(aes(Year, rate, col = excess))+
#   geom_line(aes(Year, bsn))+
#   facet_grid(measure ~ location, scales = "free_y")+
#   scale_color_manual(values = col_exc)+
#   theme_bw()+
#   theme(axis.text = element_text(size = 6))
# 
# bsn_all %>% 
#   filter(country == "Bangladesh") %>% 
#   ggplot()+
#   geom_ribbon(aes(x = Year, ymin = bsn_lp, ymax = bsn_up), alpha = 0.2)+
#   geom_point(aes(Year, rate, col = excess))+
#   geom_line(aes(Year, bsn))+
#   facet_wrap(~ measure, scales = "free_y", nrow = 5)+
#   scale_color_manual(values = col_exc)+
#   theme_bw()+
#   theme(axis.text = element_text(size = 6))
# 
# 
# pscores <- 
#   bsn_all %>% 
#   mutate(p_score = rate / bsn,
#          up = rate / bsn_lp,
#          lp = rate / bsn_up,
#          exc = case_when(up > 1 & lp > 1 ~ "Positive",
#                          up < 1 & lp < 1 ~ "Negative",
#                          TRUE ~ "No-excess"),
#          out = ifelse(exc == "No-excess", 0.5, 1),
#          ins = ifelse(exc == "No-excess", 0.6, 0.4))
# 
# tx <- 8
# cols <- c("Positive" = "#b7094c",
#           "Negative" = "#0091ad",
#           "No-excess" = "#5c4d7d")
# 
# # China
# pscores %>% 
#   filter(Year == 2020) %>% 
#   filter(country == "China") %>%
#   ggplot()+
#   # geom_errorbar(aes(xmin = lp, xmax = up, y = Country, 
#   #                   alpha = ins), 
#   #               col = "black", size = 0.6, width = 0)+
#   geom_point(aes(p_score, measure, 
#                  alpha = out, 
#                  col = exc))+
#   geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
#   # facet_wrap( ~ location, scales = "free_x", nrow = 1)+
#   facet_wrap( ~ location, nrow = 1)+
#   scale_x_log10()+
#   scale_y_discrete(limits = rev)+
#   scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
#   scale_color_manual(values = cols, guide = "none")+
#   theme_bw()+
#   theme(axis.text.y = element_text(size = 8),
#         axis.text.x = element_text(size = 4),
#         axis.title = element_blank())
# # ggsave(paste0("Figures/last version/china_bangladesh/pscores_china.png"), 
# #        dpi = 600,
# #        width = 2, height = 3)
# 
# 
# # Bangladesh
# pscores %>% 
#   filter(Year == 2020) %>% 
#   filter(country == "Bangladesh") %>%
#   ggplot()+
#   # geom_errorbar(aes(xmin = lp, xmax = up, y = Country, 
#   #                   alpha = ins), 
#   #               col = "black", size = 0.6, width = 0)+
#   geom_point(aes(p_score, measure, 
#                  alpha = out, 
#                  col = exc))+
#   geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
#   # facet_wrap( ~ location, scales = "free_x", nrow = 1)+
#   scale_x_log10()+
#   scale_y_discrete(limits = rev)+
#   scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
#   scale_color_manual(values = cols, guide = "none")+
#   labs(y = "Country")+
#   theme_bw()+
#   theme(axis.text.y = element_text(size = 8),
#         axis.text.x = element_text(size = 6),
#         axis.title = element_blank())
# 
# # ggsave(paste0("Figures/last version/china_bangladesh/pscores_bangladesh.png"), 
# #        dpi = 600,
# #        width = 1.5, height = 3)
# 
# unique(pscores$measure)
# pscores2 <- 
#   pscores %>%
#   filter(!measure %in% c("neo", "pos"),
#          location %in% "tot") %>% 
#   select(Country = country, Year, Sex = sex, Age = measure, p_score, up, lp) %>% 
#   mutate(Code = countrycode(Country, origin = "country.name",
#                                destination = "iso3c"),
#          Source = "country_public",
#          Age = case_when(Age == "inf" ~ "Infant",
#                          TRUE ~ str_replace(Age, "_", "-")))
# 
# write_rds(pscores2, "Output/sens_analysis_p_scores_excess_rates_data.rds")
