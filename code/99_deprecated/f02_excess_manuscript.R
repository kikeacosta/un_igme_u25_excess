rm (list = ls())
source("Code/00_functions.R")

all_fit <- 
  read_rds("Output/p_scores_excess_deaths_rates.rds")

db2 <- 
  all_fit %>% 
  filter(Year %in% 2020:2021,
         Sex == "t") %>% 
  mutate(exc = case_when(up > 1 & lp > 1 ~ "Positive",
                         up < 1 & lp < 1 ~ "Negative",
                         TRUE ~ "No-excess"),
         exc = factor(exc, 
                      levels = c("Positive", "No-excess", "Negative")),
         Year = Year %>% as.character(),
         age_type = ifelse(Age %in% c("Infant", "1-4"), "Under 5y", "5y groups"),
         age_type = factor(age_type, levels = rev(c("Under 5y", "5y groups"))))

cols <- c("Positive" = "#b7094c",
          "No-excess" = "#5c4d7d",
          "Negative" = "#0091ad")

lms <- c(0.5, 2)
bks <- c(0.1, 0.2, 0.5, 0.7, 1, 1.5, 2, 5, 10)
lbs <- paste0((bks - 1)*100, "%")

pl_dist <- 
  db2 %>% 
  ggplot(aes(y = Age, x = psc))+
  geom_violin(position = "dodge", alpha = 0.5, 
              col = "grey50", fill = "transparent", 
              draw_quantiles = c(0.25, 0.5, 0.75))+
  geom_jitter(aes(col = as.factor(exc)), size = 0.8, alpha = 0.7, height = 0.15,
              width = 0)+
  scale_color_manual(values = cols, 
                     guide = guide_legend(reverse = TRUE, 
                                          override.aes = list(size = 2)))+
  scale_x_log10(breaks = bks, labels = lbs)+
  coord_cartesian(xlim = lms) +
  geom_vline(xintercept = 1, linetype = "dashed")+
  facet_grid(age_type ~ Year, scales = "free", space = "free_y")+
  labs(x = "P-score", col = "Excess")+
  theme_bw()+
  theme(
    axis.text.y = element_text(size = 9, face = "bold"),
    axis.text.x = element_text(size = 7, face = "bold"),
    strip.text = element_text(size = 9, face = "bold"),
    strip.background = element_blank(),
    panel.spacing = unit(1, "lines"))


db2 %>% 
  select(Country, Year) %>% 
  unique() %>% 
  group_by(Year) %>% 
  summarise(n())
  
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


pl_dist <- 
  ggplot(db2, aes(x = Age, y = psc)) +
  geom_violin(trim = FALSE, fill = "white")+
  # geom_point(aes(fill = exc, col = exc),
  #            pch = 16, size = 3, 
  #            position = position_dodge(width = 0.2)) +
  geom_jitter(aes(col = as.factor(exc)), 
              size = 1, alpha = 0.7, 
              height = 0,
              width = 0.18)+
  # geom_dotplot(aes(fill = exc, col = exc),
  #              binaxis = "y",
  #              stackdir = "center",
  #              width = 1,
  #              stackratio=1,
  #              binwidth = 0.02,
  #              dotsize = 0.4,
  #              alpha = 0.65)+
  scale_y_log10(breaks = bks, labels = lbs)+
  scale_color_manual(values = cols, 
                     guide = guide_legend(reverse = TRUE, 
                                          override.aes = list(size = 2)))+
  scale_fill_manual(values = cols, 
                    guide = guide_legend(reverse = TRUE))+
  coord_flip(ylim = lms)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  stat_summary(fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = median)+
  facet_grid(age_type ~ Year, scales = "free", space = "free_y")+
  # coord_cartesian() +
  labs(y = "P-score", col = "Excess", fill = "Excess")+
  theme_bw()+
  theme(
    axis.text.y = element_text(size = 9, face = "bold"),
    axis.text.x = element_text(size = 7, face = "bold"),
    strip.text = element_text(size = 9, face = "bold"),
    strip.background = element_blank(),
    panel.spacing = unit(0.3, "lines"))
# ggsave("Figures/last version/manuscript/fig_04.png",
#        width = 8, height = 4)






lms <- c(0.3, 3)
bks <- c(0.1, 0.3, 0.5, 0.7, 1, 1.5, 2, 3, 5)
lbs <- paste0((bks - 1)*100, "%")

ggplot(db2, aes(x = psc, y = Age)) +
  geom_violin(trim = FALSE, fill = "white", size = 0.3)+
  geom_jitter(aes(col = as.factor(exc)), 
              size = 1, alpha = 0.7, 
              height = 0.18,
              width = 0)+
  scale_x_log10(breaks = bks, labels = lbs)+
  scale_color_manual(values = cols, 
                     guide = guide_legend(reverse = TRUE, 
                                          override.aes = list(size = 2)))+
  scale_fill_manual(values = cols, 
                    guide = guide_legend(reverse = TRUE))+
  coord_cartesian(xlim = lms)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  stat_summary(fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = median,
               size = 0.5, alpha = 0.8)+
  facet_grid(age_type ~ Year, scales = "free", space = "free_y")+
  labs(x = "P-score", col = "Excess", fill = "Excess")+
  theme_bw()+
  theme(
    axis.text.y = element_text(size = 9, face = "bold"),
    axis.text.x = element_text(size = 7, face = "bold"),
    strip.text = element_text(size = 9, face = "bold"),
    strip.background = element_blank(),
    panel.spacing = unit(0.3, "lines"))
ggsave("Figures/last version/manuscript/fig_04_pi.png",
       width = 8, height = 4)


db2 %>% 
  filter(!is.na(Income)) %>%
  mutate(age_type = ifelse(Age %in% c("Infant", "1-4"), "<5y", "5y groups"),
         age_type = factor(age_type, levels = rev(c("<5y", "5y groups")))) %>% 
  ggplot(aes(x = psc, y = Age)) +
  geom_violin(trim = FALSE, fill = "white", size = 0.3)+
  geom_jitter(aes(col = as.factor(exc)), 
              size = 1, alpha = 0.7, 
              height = 0.18,
              width = 0)+
  stat_summary(fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = median,
               size = 0.7, alpha = 0.7)+
  scale_x_log10(breaks = bks, labels = lbs)+
  scale_color_manual(values = cols, 
                     guide = guide_legend(reverse = TRUE, 
                                          override.aes = list(size = 2)))+
  scale_fill_manual(values = cols, 
                    guide = guide_legend(reverse = TRUE))+
  coord_cartesian(xlim = lms)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  facet_nested(Income + age_type ~ Year, scales = "free", space = "free_y",
               nest_line = element_line(colour = "black"))+
  labs(y = "P-score", col = "Excess", fill = "Excess")+
  theme_bw()+
  theme(
    axis.text.y = element_text(size = 9, face = "bold"),
    axis.text.x = element_text(size = 7, face = "bold"),
    strip.text = element_text(size = 9, face = "bold"),
    strip.background = element_blank(),
    panel.spacing = unit(0.3, "lines"))
ggsave("Figures/last version/manuscript/fig_04_income_pi.png",
       dpi = 600,
       width = 8, height = 6)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plotting proportions according to excess
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

all_fit3 <- 
  all_fit %>% 
  filter(Year >= 2020) %>%
  select(Country, Year, Age, Sex, psc, lp, up) %>% 
  mutate(psc_out = ifelse(1 < up & 1 > lp, 1, psc)) %>% 
  drop_na(psc_out) %>% 
  unique()

all_fit3 %>% 
  filter(Sex == "t") %>% 
  select(Country, Year) %>% 
  unique() %>% 
  group_by(Year) %>% 
  summarise(n())

dts_only_sig <- 
  all_fit3 %>% 
  select(-psc) %>% 
  rename(psc = psc_out)

unique(all_fit3$Age)
unique(all_fit3$Country)

db_summary <- 
  all_fit3 %>% 
  filter(Sex == "t") %>% 
  mutate(exc_all = case_when(psc > 1 ~ "Positive",
                             psc < 1 ~ "Negative",
                             psc == 1 ~ "No-excess"),
         exc_out = case_when(psc_out > 1 ~ "Positive",
                             psc_out < 1 ~ "Negative",
                             psc_out == 1 ~ "No-excess")) %>% 
  select(Year, Age, Sex, Country, exc_all, exc_out) %>% 
  group_by(Year, Age, Sex) %>% 
  mutate(N_cts = n()) %>% 
  ungroup() %>%  
  gather(exc_all, exc_out, key = type, value = is_excess) %>% 
  group_by(Year, Age, Sex, N_cts, type, is_excess) %>% 
  summarise(N = n()) %>% 
  ungroup() %>% 
  mutate(Prop = N / N_cts,
         Age = factor(Age, levels = c("Infant", "1-4", "0-4", "5-9", 
                                      "10-14", "15-19", "20-24")),
         type = recode(type,
                       "exc_all" = "All estimates",
                       "exc_out" = "Uncertainty"),
         type = factor(type, levels = c("All estimates", "Uncertainty")),
         is_excess = factor(is_excess, 
                            levels = c("Positive", "No-excess", "Negative")),
         Sex = recode(Sex,
                      "f" = "females",
                      "m" = "males",
                      "t" = "total"),
         age_type = ifelse(Age %in% c("Infant", "1-4"), "Under 5y", "5y groups"),
         age_type = factor(age_type, levels = rev(c("Under 5y", "5y groups"))))

cols <- 
  c("Positive" = "#b7094c",
    "No-excess" = "#5c4d7d",
    "Negative" = "#0091ad")

bks2 <- c(0, .25, .5, .75, 1)
lbs2 <- paste0(bks2*100, "%")

pl_prop <- 
  db_summary %>% 
  filter(type == "Uncertainty") %>% 
  ggplot(aes(x = Age, y = Prop))+
  geom_bar(aes(fill = is_excess),
           stat = "identity", 
           position = "fill",
           alpha = 0.8)+
  geom_text(aes(label = paste0(round(Prop*100), "%"), group = is_excess), 
            position = position_stack(vjust = 0.5, reverse = F), size = 2.7)+
  facet_grid(age_type ~ Year, scales = "free", space = "free_y")+
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE))+
  scale_y_continuous(breaks = bks2, labels = lbs2)+
  labs(fill = "Excess", 
       # title = "All countries",
       x = "Age", y = "Proportion")+
  coord_flip(expand = 0)+
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 9, face = "bold"),
        axis.text.x = element_text(size = 7, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        plot.title = element_text(size = 10),
        panel.spacing = unit(1, "lines"))

# ggsave("Figures/last version/manuscript/excess_summary_2020_2021_uncert.png", width = 8, height = 4)

plot_grid(pl_dist, pl_prop, 
          labels = c("A", "B"),
          label_size = 14,
          # label_x = 0.9, label_y = 0.99, hjust = 1,
          ncol = 1)

ggsave("Figures/last version/manuscript/fig_05_pi.png", width = 8, height = 8)

all_fit3 %>% 
  filter(Sex == "t") %>% 
  select(Country, Year) %>% 
  unique() %>% 
  group_by(Year) %>% 
  summarise(n())




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plots with additional details
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_summary %>% 
  mutate(age_type = ifelse(Age %in% c("Infant", "1-4"), "<5y", "5y groups"),
         age_type = factor(age_type, levels = rev(c("<5y", "5y groups")))) %>% 
  # filter(type == "Uncertainty") %>% 
  ggplot(aes(x = Age, y = Prop))+
  geom_bar(aes(fill = is_excess),
           stat = "identity", 
           position = "fill",
           alpha = 0.8)+
  geom_text(aes(label = paste0(round(Prop*100), "%"), group = is_excess), 
            position = position_stack(vjust = 0.5, reverse = F), size = 2.7)+
  facet_nested(type + age_type ~ Year, scales = "free", space = "free_y",
               nest_line = element_line(colour = "black"))+
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE))+
  scale_y_continuous(breaks = bks2, labels = lbs2)+
  labs(fill = "Excess", 
       # title = "All countries",
       x = "Age", y = "Proportion")+
  coord_flip(expand = 0)+
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 9, face = "bold"),
        axis.text.x = element_text(size = 7, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        plot.title = element_text(size = 10),
        panel.spacing = unit(0.5, "lines"))

ggsave("Figures/last version/manuscript/excess_summary_2020_2021.png", 
       width = 8, height = 4)



