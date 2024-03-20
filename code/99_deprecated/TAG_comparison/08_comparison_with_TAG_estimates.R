library(readxl)

tag_ests <- read_csv("Data/age_sex_deaths_comparison.csv")

# Own estimates
own_est <- 
  read_rds("Output/p_scores_excess_tag_format.rds")

unique(own_est$Age)

tag_ests2 <-
  tag_ests %>%
  mutate(p_score_tag1_pred = predicted_dx_tag1 / baseline_dx_tag1,
         p_score_tag1_obsv = observed / baseline_dx_tag1,
         p_score_tag2_pred = predicted_dx_tag2 / baseline_dx_tag1) %>% 
  mutate(Country = case_when(Country == "Russian Federation" ~ "Russia",
                             Country == "United States of America" ~ "USA",
                             Country == "The United Kingdom" ~ "United Kingdom",
                             TRUE ~ Country)) %>% 
  select(Country, sex, age, p_score_tag1_pred, p_score_tag1_obsv, p_score_tag2_pred)


comparison <- 
  own_est %>% 
  filter(Year == 2020,
         Sex != "t") %>% 
  select(Country, sex = Sex, age = Age, p_score) %>% 
  mutate(sex = recode(sex,
                      "f" = "Female",
                      "m" = "Male")) %>% 
  left_join(tag_ests2) 

comparison2 <- 
  comparison %>% 
  drop_na() %>% 
  gather(starts_with("p_score"), key = source, value = p_score) %>% 
  mutate(excess = ifelse(p_score > 1, "Positive", "Negative"))

comparison2 %>% 
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
# ggsave("Figures/TAG_pres/tag_p_score_comparison2.png",
#        width = 10,
#        height = 6)

averages <- 
  comparison2 %>% 
  filter(Country != "Armenia") %>% 
  group_by(sex, age, source) %>% 
  summarise(p_score_av = mean(p_score, na.rm = T))

averages %>% 
  ggplot()+
  geom_point(aes(p_score_av, source), 
             alpha = 0.5)+
  facet_grid(sex~age, scales = "free")+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10()+
  theme_bw()+
  theme(
    axis.text = element_text(size = 8)
  )
# ggsave("Figures/TAG_pres/tag_p_score_average_comparison.png")


comparison_sign <- 
  comparison2 %>% 
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

# ggsave("Figures/TAG_pres/tag_p_score_excess_sign_comparison.png", width = 8, height = 4)


# ==================

unique(comparison_sign$source)
comparison_sign2 <- 
  comparison_sign %>% 
  mutate(source = recode(source,
                         "p_score" = "Own (obs/exp)", 
                         "p_score_tag1_obsv" = "TAG-WG1 (obs/exp)", 
                         "p_score_tag1_pred" = "TAG-WG1 (pred/exp)", 
                         "p_score_tag2_pred" = "TAG-Spin-Off (pred/exp)"),
         source = factor(source, levels = c("Own (obs/exp)",
                                            "TAG-WG1 (obs/exp)", 
                                            "TAG-WG1 (pred/exp)", 
                                            "TAG-Spin-Off (pred/exp)")),
         Age = recode(age,
                      "0" = "0-4",
                      "5" = "5-9",
                      "10" = "10-14",
                      "15" = "15-19",
                      "20" = "20-24"),
         Age = factor(Age, levels = rev(c("0-4", "5-9", "10-14", "15-19", "20-24"))))


comparison_sign2 %>% 
  ggplot(aes(x = prop, y = Age))+
  geom_bar(aes(fill = excess),
           stat = "identity", 
           position = "fill",
           alpha = 0.8)+
  geom_text(aes(label = paste0(round(prop*100)), group = excess), 
            position = position_stack(vjust = 0.5, reverse = F), size = 3)+
  # scale_x_continuous(breaks = c())+
  facet_grid(sex ~ source)+
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE))+
  labs(fill = "Excess")+
  coord_cartesian(expand = 0)+
  # expand_limits(x = 0, y = 0)+
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        panel.spacing = unit(2, "lines"))


comparison_sign2 %>% 
  filter(source %in% c("Own (obs/exp)", "TAG-WG1 (obs/exp)")) %>% 
  ggplot(aes(x = prop, y = Age))+
  geom_bar(aes(fill = excess),
           stat = "identity", 
           position = "fill",
           alpha = 0.8)+
  geom_text(aes(label = paste0(round(prop*100)), group = excess), 
            position = position_stack(vjust = 0.5, reverse = F), size = 3)+
  # scale_x_continuous(breaks = c())+
  facet_grid(sex ~ source)+
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE))+
  labs(fill = "Excess")+
  coord_cartesian(expand = 0)+
  # expand_limits(x = 0, y = 0)+
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        panel.spacing = unit(2, "lines"))
# ggsave("Figures/TAG_pres/comparison_tag_obs.png", width = 8, height = 4)


comparison_sign2 %>% 
  filter(source %in% c("Own (obs/exp)", "TAG-WG1 (pred/exp)", "TAG-Spin-Off (pred/exp)")) %>% 
  ggplot(aes(x = prop, y = Age))+
  geom_bar(aes(fill = excess),
           stat = "identity", 
           position = "fill",
           alpha = 0.8)+
  geom_text(aes(label = paste0(round(prop*100)), group = excess), 
            position = position_stack(vjust = 0.5, reverse = F), size = 3)+
  # scale_x_continuous(breaks = c())+
  facet_grid(sex ~ source)+
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE))+
  labs(fill = "Excess")+
  coord_cartesian(expand = 0)+
  # expand_limits(x = 0, y = 0)+
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        panel.spacing = unit(2, "lines"))
# ggsave("Figures/TAG_pres/comparison_tag_pred.png", width = 8, height = 4)


