library(here)
source(here("Code", "00_functions.R"))

db_all_fit <- 
  read_rds("Output/p_scores_excess_raw_data.rds")

# ~~~~~~~~~~~~~~~~~
# Plotting p-scores
# ~~~~~~~~~~~~~~~~~

# Plotting all ages for countries with infant mortality by sex
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lvs_cts1 <- 
  db_all_fit %>% 
  filter(Year == 2020,
         Age == "Infant",
         Sex == "t") %>% 
  arrange(-p_score) %>% 
  pull(Country) %>% 
  unique()

db_gr1 <- 
  db_all_fit %>% 
  filter(Country %in% lvs_cts1,
         Year == 2020,
         Age != "Child (0-4)") %>% 
  mutate(exc = case_when(up > 1 & lp > 1 ~ "Positive",
                         up < 1 & lp < 1 ~ "Negative",
                         TRUE ~ "Non"),
         out = ifelse(exc == "Non", 0.5, 1),
         ins = ifelse(exc == "Non", 0.6, 0.4),
         Age = factor(Age, levels = c("Infant", "1_4", "5_9", "10_14", "15_24")),
         Country = factor(Country, levels = lvs_cts1))

unique(db_gr1$Age)

tx <- 10
cols <- c("Positive" = "#b7094c",
          "Negative" = "#0091ad",
          "Non" = "#5c4d7d")

db_gr1 %>% 
  ggplot()+
  geom_errorbar(aes(xmin = lp, xmax = up, y = Country, 
                    alpha = ins), 
                col = "black", size = 0.6, width = 0)+
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
  theme(legend.position = "none",
        strip.text.x = element_text(size = tx),
        axis.title.y = element_blank())

ggsave(paste0("Figures/excess_raw/excess_young_ages_sex_group1.png"), dpi = 600,
       width = 10, height = 5)



# Plotting all ages for countries *without* infant mortality
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lvs_cts2 <- 
  db_all_fit %>% 
  filter(Year == 2020,
         Age == "Child (0-4)",
         Sex == "t",
         !Country %in% lvs_cts1) %>% 
  arrange(-p_score) %>% 
  pull(Country) %>% 
  unique()

db_gr2 <- 
  db_all_fit %>% 
  filter(!Country %in% lvs_cts1,
         Year == 2020) %>% 
  mutate(exc = case_when(up > 1 & lp > 1 ~ "Positive",
                         up < 1 & lp < 1 ~ "Negative",
                         TRUE ~ "Non"),
         out = ifelse(exc == "Non", 0.5, 1),
         ins = ifelse(exc == "Non", 0.6, 0.4),
         Age = factor(Age, levels = c("Child (0-4)", "5_9", "10_14", "15_24")),
         Country = factor(Country, levels = lvs_cts2))

db_gr2 %>% 
  ggplot()+
  geom_errorbar(aes(xmin = lp, xmax = up, y = Country, 
                    alpha = ins), 
                col = "black", size = 0.6, width = 0)+
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
  theme(legend.position = "none",
        strip.text.x = element_text(size = tx),
        axis.title.y = element_blank())

ggsave(paste0("Figures/excess_raw/excess_young_ages_sex_group2.png"), dpi = 600,
       width = 10, height = 5)

# excluding Armenia because of huge excess in young ages
db_gr2 %>% 
  filter(Country != "Armenia") %>% 
  ggplot()+
  geom_errorbar(aes(xmin = lp, xmax = up, y = Country, 
                    alpha = ins), 
                col = "black", size = 0.6, width = 0)+
  geom_point(aes(p_score, Country, 
                 alpha = out, 
                 col = exc,
                 shape = Sex))+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  facet_wrap(~ Age, scales = "free_x", nrow = 1)+
  scale_x_log10()+
  scale_alpha_continuous(range = c(0.3, 0.8))+
  scale_color_manual(values = cols)+
  labs(y = "Country")+
  theme_bw()+
  theme(legend.position = "none",
        strip.text.x = element_text(size = tx),
        axis.title.y = element_blank())

ggsave(paste0("Figures/excess_raw/excess_young_ages_sex_group2_no_arm.png"), dpi = 600,
       width = 10, height = 5)


# Plotting all Countries together 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lvs_cts3 <- 
  db_all_fit %>% 
  filter(Year == 2020,
         Age == "Child (0-4)",
         Sex == "t") %>% 
  arrange(-p_score) %>% 
  pull(Country) %>% 
  unique()

db_gr3 <- 
  db_all_fit %>% 
  filter(Year == 2020,
         !Age %in% c("1_4", "Infant")) %>% 
  mutate(exc = case_when(up > 1 & lp > 1 ~ "Positive",
                         up < 1 & lp < 1 ~ "Negative",
                         TRUE ~ "Non"),
         out = ifelse(exc == "Non", 0.5, 1),
         ins = ifelse(exc == "Non", 0.6, 0.4),
         Age = factor(Age, levels = c("Child (0-4)", "5_9", "10_14", "15_24")),
         Country = factor(Country, levels = lvs_cts3))

db_gr3 %>% 
  filter(Country != "Armenia") %>% 
  ggplot()+
  geom_errorbar(aes(xmin = lp, xmax = up, y = Country, 
                    alpha = ins), 
                col = "black", size = 0.6, width = 0)+
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
  theme(legend.position = "bottom",
        legend.text = element_text(size = tx),
        legend.title = element_text(size = tx),
        legend.key.size = unit(3, "mm"),
        strip.text.x = element_text(size = tx),
        axis.title.y = element_blank())

ggsave(paste0("Figures/excess_raw/excess_young_ages_sex_group3_no_arm.png"), 
       dpi = 600, width = 10, height = 8)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plotting proportions according to excess
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db_all_fit2 <- 
  db_all_fit %>% 
  filter(Year == 2020) %>% 
  select(Country, Age, Sex, p_score, lp, up) %>% 
  mutate(p_score_out = ifelse(1 < up & 1 > lp, 1, p_score))

db_all_fit_only_sig <- 
  db_all_fit2 %>% 
  select(-p_score) %>% 
  rename(p_score = p_score_out)


db_summary <- 
  db_all_fit2 %>% 
  mutate(exc_all = case_when(p_score > 1 ~ "Positive",
                             p_score < 1 ~ "Negative",
                             p_score == 1 ~ "Non"),
         exc_out = case_when(p_score_out > 1 ~ "Positive",
                             p_score_out < 1 ~ "Negative",
                             p_score_out == 1 ~ "Non")) %>% 
  select(Age, Sex, Country, exc_all, exc_out) %>% 
  group_by(Age, Sex) %>% 
  mutate(N_cts = n()) %>% 
  ungroup() %>%  
  gather(exc_all, exc_out, key = type, value = is_excess) %>% 
  group_by(Age, Sex, N_cts, type, is_excess) %>% 
  summarise(N = n()) %>% 
  ungroup() %>% 
  mutate(Prop = N / N_cts,
         Age = factor(Age, levels = rev(c("Infant", "1_4", "Child (0-4)", "5_9", "10_14", "15_24"))),
         type = recode(type,
                       "exc_all" = "All estimates",
                       "exc_out" = "Inc. uncert."),
         type = factor(type, levels = c("All estimates", "Inc. uncert.")),
         is_excess = factor(is_excess, levels = c("Positive", "Non", "Negative")),
         Sex = recode(Sex,
                      "f" = "females",
                      "m" = "males",
                      "t" = "total"))

cols <- 
  c("Positive" = "#b7094c",
    "Non" = "#5c4d7d",
    # "Non" = "grey80",
    "Negative" = "#0091ad")

db_summary %>% 
  ggplot(aes(x = Age, y = Prop))+
  geom_bar(aes(fill = is_excess),
           stat = "identity", 
           position = "fill",
           alpha = 0.8)+
  geom_text(aes(label = paste0(round(Prop*100)), group = is_excess), 
            position = position_stack(vjust = 0.5, reverse = F), size = 3)+
  facet_grid(Sex ~ type)+
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE))+
  labs(fill = "Excess")+
  coord_flip(expand = 0)+
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        panel.spacing = unit(2, "lines"))
ggsave("Figures/excess_raw/excess_summary_by_age_sex.png", width = 8, height = 4)






