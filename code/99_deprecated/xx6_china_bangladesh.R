library(readxl)
library(tidyverse)

cn <- read_xlsx("Data/unicef/china_xlsx.xlsx")
bd <- read_xlsx("Data/unicef/SRS 2020 Table 4.21.xlsx",
                skip = 1)

cn2 <- 
  cn %>% 
  gather(-Year, key = measure, value = rate) %>% 
  filter(Year >= 2015) %>% 
  # separate(measure, c("measure", "location")) %>% 
  # mutate(measure = recode(measure,
  #                         "chd" = "0_4")) %>% 
  mutate(country = "China")

unique(cn2$measure)

  
bd2 <- 
  bd %>% 
  rename(inf = 2,
         neo = 3,
         pos = 4,
         chd = 5,
         chd2 = 6) %>% 
  gather(-Year, key = measure, value = rate) %>% 
  filter(Year >= 2015) %>% 
  # mutate(measure = recode(measure,
  #                         "chd" = "0_4",
  #                         "chd2" = "1_4")) %>% 
  mutate(country = "Bangladesh",
         measure = paste(measure, "tot", sep = "_"))

cn_bd <- 
  bind_rows(cn2,
            bd2)

unique(cn_bd$measure)


fit_log_lm <- function(db){
  chunk <- 
    db %>% 
    mutate(log_rate = log(rate),
           w = ifelse(Year == 2020, 0, 1)) %>% 
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

test <- 
  fit_log_lm(bd2, "neo")


test %>% 
  ggplot()+
  geom_ribbon(aes(x = Year, ymin = bsn_lp, ymax = bsn_up), alpha = 0.2)+
  geom_point(aes(Year, rate))+
  geom_line(aes(Year, bsn))+
  theme_bw()
  
bsn_cn_bd <- 
  cn_bd %>% 
  group_by(country, measure) %>% 
  do(fit_log_lm(db = .data)) %>% 
  ungroup() %>% 
  separate(measure, c("measure", "location")) %>% 
  mutate(excess = case_when(Year == 2020 & rate > bsn_up ~ "pos",
                            Year == 2020 & rate < bsn_lp ~ "neg",
                            TRUE ~ "none"),
         measure = recode(measure,
                          "chd" = "0_4",
                          "chd2" = "1_4"),
         measure = factor(measure, levels = c("neo", "pos", "inf", "0_4", "1_4")),
         location = factor(location, levels = c("urb", "rur", "tot")),
         excess = factor(excess, levels = c("pos", "none", "neg")))

col_exc <- 
  c("pos" = "red",
    "neg" = "blue",
    "none" = "black")

bsn_cn_bd %>% 
  filter(country == "China") %>% 
  ggplot()+
  geom_ribbon(aes(x = Year, ymin = bsn_lp, ymax = bsn_up), alpha = 0.2)+
  geom_point(aes(Year, rate, col = excess))+
  geom_line(aes(Year, bsn))+
  facet_grid(measure ~ location, scales = "free_y")+
  scale_color_manual(values = col_exc)+
  theme_bw()+
  theme(axis.text = element_text(size = 6))

ggsave(paste0("Figures/last version/china_bangladesh/baselines_china.png"), 
       dpi = 600,
       width = 6, height = 4)

bsn_cn_bd %>% 
  filter(country == "Bangladesh") %>% 
  ggplot()+
  geom_ribbon(aes(x = Year, ymin = bsn_lp, ymax = bsn_up), alpha = 0.2)+
  geom_point(aes(Year, rate, col = excess))+
  geom_line(aes(Year, bsn))+
  facet_wrap(~ measure, scales = "free_y", nrow = 5)+
  scale_color_manual(values = col_exc)+
  theme_bw()+
  theme(axis.text = element_text(size = 6))

ggsave(paste0("Figures/last version/china_bangladesh/baselines_bangladesh.png"), 
       dpi = 600,
       width = 4, height = 4)

pscores <- 
  bsn_cn_bd %>% 
  mutate(p_score = rate / bsn,
         up = rate / bsn_lp,
         lp = rate / bsn_up,
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
  filter(country == "China") %>%
  ggplot()+
  # geom_errorbar(aes(xmin = lp, xmax = up, y = Country, 
  #                   alpha = ins), 
  #               col = "black", size = 0.6, width = 0)+
  geom_point(aes(p_score, measure, 
                 alpha = out, 
                 col = exc))+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  # facet_wrap( ~ location, scales = "free_x", nrow = 1)+
  facet_wrap( ~ location, nrow = 1)+
  scale_x_log10()+
  scale_y_discrete(limits = rev)+
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  theme_bw()+
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 4),
        axis.title = element_blank())
ggsave(paste0("Figures/last version/china_bangladesh/pscores_china.png"), 
       dpi = 600,
       width = 2, height = 3)


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

ggsave(paste0("Figures/last version/china_bangladesh/pscores_bangladesh.png"), 
       dpi = 600,
       width = 1.5, height = 3)















