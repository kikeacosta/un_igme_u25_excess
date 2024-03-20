library(here)
source(here("Code", "00_functions.R"))
tx <- 10
cols <- c("Positive" = "#b7094c",
          "Negative" = "#0091ad",
          "No-excess" = "#5c4d7d")

tag_est <- read_csv("Data/age_sex_compare2.csv")
own_est <- read_rds("Output/p_scores_excess_tag_format.rds")

unique(tag_est$variant)
unique(tag_est$Cluster)

tag_est2 <- 
  tag_est %>% 
  mutate(Country = case_when(Country == "Russian Federation" ~ "Russia",
                             Country == "United States of America" ~ "USA",
                             Country == "The United Kingdom" ~ "United Kingdom",
                             TRUE ~ Country))

test <- 
  tag_est %>% 
  filter(variant == "observed")


# p-scores
# ~~~~~~~~

# observed
obs <- 
  tag_est2 %>% 
  filter(variant == "observed") %>% 
  select(Country, sex, age, mx_obs = mx) %>% 
  unique()

p_scores <- 
  tag_est2 %>% 
  filter(variant != "observed") %>% 
  select(Country, sex, age, variant, mx) %>% 
  unique() %>% 
  left_join(obs) %>% 
  mutate(p_score = mx_obs / mx)

p_scores_young <- 
  p_scores %>% 
  filter(age < 25,
         variant %in% c("mx_pred_qm", "mx_WM")) %>% 
  drop_na() %>% 
  mutate(sex = recode(sex,
                      "Female" = "f",
                      "Male" = "m"))

mx_tag <- 
  p_scores_young %>% 
  select(Country, Sex = sex, Age = age, mx_tag = mx) %>% 
  unique()

p_scores_tag <- 
  p_scores_young %>% 
  select(Country, Sex = sex, Age = age, variant, p_score) %>% 
  spread(variant, p_score) %>% 
  rename(p_score_tag2 = mx_pred_qm,
         p_score_tag1 = mx_WM)
  
  
unique(own_est$Country) %>% sort()
unique(p_scores_young$Country) %>% sort()

comparison <- 
  own_est %>% 
  filter(Year == 2020,
         Sex != "t") %>% 
  mutate(mx = Deaths / Population) %>% 
  select(Country, Sex, Age, Deaths, Population, mx, p_score) %>% 
  left_join(p_scores_tag) %>% 
  drop_na() %>% 
  gather(starts_with("p_score"), key = source, value = p_score) %>% 
  mutate(source = factor(source, levels = c("p_score", 
                                            "p_score_tag1",
                                            "p_score_tag2")),
         Sex = recode(Sex,
                      "f" = "females",
                      "m" = "males"),
         excess = ifelse(p_score > 1, "Positive", "Negative"))

  

comparison %>% 
  filter(Country != "Armenia") %>% 
  ggplot()+
  geom_point(aes(p_score, Country, col = source, shape = excess), 
             alpha = 0.6, size = 1, stroke = 1)+
  facet_grid(Sex~Age, scales = "free")+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10()+
  scale_shape_manual(values = c(1, 16))+
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
  group_by(Sex, Age, source) %>% 
  summarise(p_score_av = mean(p_score, na.rm = T))

averages %>% 
  ggplot()+
  geom_point(aes(p_score_av, source), 
             alpha = 0.5)+
  facet_grid(Sex~Age)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10()+
  theme_bw()+
  theme(
    axis.text = element_text(size = 8)
  )
ggsave("Figures/TAG_pres/tag_p_score_average_comparison.png")



comparison_sign <- 
  comparison %>% 
  mutate(excess = ifelse(p_score > 1, "Positive", "Negative")) %>% 
  group_by(Age, Sex, source, excess) %>% 
  summarise(n_cts = n()) %>% 
  group_by(Age, Sex, source) %>% 
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
  facet_grid(Sex ~ Age)+
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE))+
  labs(fill = "Excess")+
  coord_cartesian(expand = 0)+
  # expand_limits(x = 0, y = 0)+
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        panel.spacing = unit(2, "lines"))

ggsave("Figures/TAG_pres/tag_p_score_excess_sign_comparison.png", width = 8, height = 4)






# comparisons with scatter plots 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
comparison2 <- 
  comparison %>% 
  spread(source, p_score)

comparison2 %>% 
  filter(Country != "Armenia") %>% 
  ggplot()+
  geom_point(aes(p_score, p_score_tag1))+
  facet_wrap(Sex~Age, scales = "free", ncol = 5)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  theme_bw()

comparison2 %>% 
  filter(Country != "Armenia") %>% 
  ggplot()+
  geom_point(aes(p_score, p_score_tag2))+
  facet_wrap(Sex~Age, scales = "free", ncol = 5)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  theme_bw()


comparison2 %>% 
  ggplot()+
  geom_point(aes(p_score, p_score_tag2))





# uncertainty included
comparison_un <- 
  own_est %>% 
  filter(Year == 2020,
         Sex != "t") %>% 
  mutate(mx = Deaths / Population,
         p_score = ifelse(Deaths > up_baseline | Deaths < lp_baseline,
                          p_score,
                          1)) %>% 
  select(Country, Sex, Age, Deaths, Population, mx, p_score) %>% 
  left_join(p_scores_tag) %>% 
  gather(starts_with("p_score"), key = source, value = p_score)


comparison_un %>% 
  filter(Country != "Armenia") %>% 
  ggplot()+
  geom_point(aes(p_score, Country, col = source), 
             alpha = 0.5, size = 1)+
  facet_grid(Sex~Age)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10()+
  theme_bw()+
  theme(
    axis.text = element_text(size = 5)
  )
ggsave("Figures/TAG_pres/tag_p_score_comparison_un.png",
       width = 8,
       height = 6)
