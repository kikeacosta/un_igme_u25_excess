library(readxl)

# William excess estimates 
# wm <- 
#   read_xlsx("Data/WM_EstimatesBySexAge.xlsx",
#             sheet = 2,
#             skip = 5,
#             col_types = c(rep("text", 5), rep("numeric", 2), "text", rep("numeric", 3)))


# gt <- read_csv("Data/age_sex_compare2.csv")
tag_ests <- read_csv("Data/age_sex_deaths_comparison.csv")

# Own estimates
own_est <- 
  read_rds("Output/p_scores_excess_tag_format.rds")


unique(tag_ests$measure)

wm_deaths <-
  wm %>%
  select(Country, source = 4, sex, age, measure, mean) %>% 
  filter(source %in% c("Observed 2020", "Expected 2020", "Predicted 2020"),
         measure %in% c("deaths")) %>% 
  spread(source, mean) %>% 
  rename(baseline = 5,
         observed = 6,
         predicted = 7) %>% 
  mutate(p_score_tag1_pred = predicted / baseline,
         p_score_tag1_obsv = observed / baseline) %>% 
  mutate(Country = case_when(Country == "Russian Federation" ~ "Russia",
                             Country == "United States of America" ~ "USA",
                             Country == "The United Kingdom" ~ "United Kingdom",
                             TRUE ~ Country)) %>% 
  select(Country, sex, age, p_score_tag1 = p_score_tag1_pred)


wm_mx <- 
  wm %>% 
  select(Country, source = 4, sex, age, measure, mean) %>% 
  filter(source %in% c("Expected 2020"),
         measure %in% c("mx")) %>% 
  select(Country, sex, age, baseline_mx = mean)
  

unique(gt$variant)
gt_ps <- 
  gt %>% 
  filter(variant %in% c("mx_pred_qm")) %>% 
  # spread(variant, mx) %>% 
  left_join(wm_mx) %>%   
  mutate(p_score_tag2 = mx / baseline_mx) %>% 
  select(Country, sex, age, p_score_tag2)

comparison <- 
  own_est %>% 
  filter(Year == 2020,
         Sex != "t") %>% 
  select(Country, sex = Sex, age = Age, p_score) %>% 
  mutate(sex = recode(sex,
                      "f" = "Female",
                      "m" = "Male")) %>% 
  left_join(wm_deaths) %>% 
  left_join(gt_ps) %>% 
  drop_na() %>% 
  gather(starts_with("p_score"), key = source, value = p_score) %>% 
  mutate(excess = ifelse(p_score > 1, "Positive", "Negative"))

comparison %>% 
  filter(Country != "Armenia") %>%
  ggplot()+
  geom_point(aes(p_score, Country, col = source, shape = excess), 
             alpha = 0.6, size = 1, stroke = 1)+
  facet_grid(sex~age, scales = "free")+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10()+
  scale_shape_manual(values = c(17, 16))+
  theme_bw()+
  theme(
    axis.text = element_text(size = 5)
  )
ggsave("Figures/TAG_pres/tag_p_score_comparison.png",
       width = 10,
       height = 6)

averages <- 
  comparison %>% 
  filter(Country != "Armenia") %>% 
  group_by(sex, age, source) %>% 
  summarise(p_score_av = mean(p_score, na.rm = T))

averages %>% 
  ggplot()+
  geom_point(aes(p_score_av, source), 
             alpha = 0.5)+
  facet_grid(sex~age)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10()+
  theme_bw()+
  theme(
    axis.text = element_text(size = 8)
  )
ggsave("Figures/TAG_pres/tag_p_score_average_comparison.png")


comparison_sign <- 
  comparison %>% 
  group_by(age, sex, source, excess) %>% 
  summarise(n_cts = n()) %>% 
  group_by(age, sex, source) %>% 
  mutate(prop = n_cts / sum(n_cts)) %>% 
  ungroup()

cols <- c("Positive" = "#b7094c",
          "Negative" = "#0091ad")

comparison_sign %>% 
  ggplot(aes(x = prop, y = source))+
  geom_bar(aes(fill = excess),
           stat = "identity", 
           position = "fill",
           alpha = 0.8)+
  geom_text(aes(label = paste0(round(prop*100)), group = excess), 
            position = position_stack(vjust = 0.5, reverse = F), size = 3)+
  scale_x_continuous(breaks = c())+
  facet_grid(sex ~ age)+
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE))+
  labs(fill = "Excess")+
  coord_cartesian(expand = 0)+
  # expand_limits(x = 0, y = 0)+
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        panel.spacing = unit(2, "lines"))

ggsave("Figures/TAG_pres/tag_p_score_excess_sign_comparison.png", width = 8, height = 4)


# ==================


# observed / baseline
wm_baseline <-
  wm %>%
  select(Country, source = 4, sex, age, measure, mean) %>% 
  filter(source %in% c("Observed 2020", "Expected 2020", "Predicted 2020"),
         measure %in% c("deaths")) %>% 
  spread(source, mean) %>% 
  rename(baseline = 5,
         observed = 6,
         predicted = 7) %>% 
  mutate(p_score_tag1_pred = predicted / baseline,
         p_score_tag1_obsv = observed / baseline) %>% 
  mutate(Country = case_when(Country == "Russian Federation" ~ "Russia",
                             Country == "United States of America" ~ "USA",
                             Country == "The United Kingdom" ~ "United Kingdom",
                             TRUE ~ Country)) %>% 
  select(Country, sex, age, p_score_tag1 = p_score_tag1_obsv)


comparison_baseline <- 
  own_est %>% 
  filter(Year == 2020,
         Sex != "t") %>% 
  select(Country, sex = Sex, age = Age, p_score_own = p_score) %>% 
  mutate(sex = recode(sex,
                      "f" = "Female",
                      "m" = "Male")) %>% 
  left_join(wm_baseline) %>% 
  drop_na() %>% 
  gather(starts_with("p_score"), key = source, value = p_score) %>% 
  mutate(excess = ifelse(p_score > 1, "Positive", "Negative"))

comparison_baseline %>% 
  filter(Country != "Armenia") %>%
  ggplot()+
  geom_point(aes(p_score, Country, col = source, shape = excess), 
             alpha = 0.6, size = 1, stroke = 1)+
  facet_grid(sex~age, scales = "free")+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10()+
  scale_shape_manual(values = c(17, 16))+
  theme_bw()+
  theme(
    axis.text = element_text(size = 5)
  )
ggsave("Figures/TAG_pres/tag_p_score_comparison_baselines.png",
       width = 10,
       height = 6)


comparison_bsn_sign <- 
  comparison_baseline %>% 
  group_by(age, sex, source, excess) %>% 
  summarise(n_cts = n()) %>% 
  group_by(age, sex, source) %>% 
  mutate(prop = n_cts / sum(n_cts)) %>% 
  ungroup()

cols <- c("Positive" = "#b7094c",
          "Negative" = "#0091ad")

comparison_bsn_sign %>% 
  ggplot(aes(x = prop, y = source))+
  geom_bar(aes(fill = excess),
           stat = "identity", 
           position = "fill",
           alpha = 0.8)+
  geom_text(aes(label = paste0(round(prop*100)), group = excess), 
            position = position_stack(vjust = 0.5, reverse = F), size = 3)+
  scale_x_continuous(breaks = c())+
  facet_grid(sex ~ age)+
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE))+
  labs(fill = "Excess")+
  coord_cartesian(expand = 0)+
  # expand_limits(x = 0, y = 0)+
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        panel.spacing = unit(2, "lines"))

ggsave("Figures/TAG_pres/tag_p_score_excess_sign_comparison_bln.png", width = 8, height = 4)
