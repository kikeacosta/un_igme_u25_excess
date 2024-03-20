rm (list = ls())
source("Code/00_functions.R")

# loading excess estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~
all_fit <- 
  read_rds("Output/p_scores_excess_deaths_rates.rds")

all_fit %>% 
  filter(Year %in% 2020:2021,
         Sex == "t") %>% 
  select(Country, Year, Age) %>% 
  unique() %>% 
  group_by(Year, Age) %>% 
  summarise(n = n())

# weighted averages
# ~~~~~~~~~~~~~~~~~
# total population by income
pop_income <- 
  read_rds("data_inter/population_by_income_2022.rds")

income_lvs <- c("High", "Upper-mid", "Lower-mid", "Low")

# weighted average P-scores in all populations
w_avs <- 
  all_fit %>% 
  filter(Sex == "t",
         Year >= 2020) %>% 
  # drop_na() %>% 
  group_by(Year, Sex, Age) %>% 
  mutate(c = Population / sum(Population)) %>% 
  summarize(w_av = sum(psc * c),
            w_av_lp = sum(lp * c),
            w_av_up = sum(up * c),
            av = mean(psc),
            av_lp = mean(lp),
            av_up = mean(up),
            Population = sum(Population)) %>% 
  ungroup() %>%
  mutate(Income = "Total")

# weighted average P-scores by income
w_avs_income <- 
  all_fit %>% 
  filter(Sex == "t",
         Year >= 2020) %>% 
  group_by(Year, Sex, Age, Income) %>% 
  mutate(c = Population / sum(Population)) %>% 
  summarize(w_av = sum(psc * c),
            w_av_lp = sum(lp * c),
            w_av_up = sum(up * c),
            av = mean(psc),
            av_lp = mean(lp),
            av_up = mean(up),
            Population = sum(Population)) %>% 
  ungroup() 

# appending weighted average p-scores
w_avs_all <- 
  bind_rows(w_avs_income, w_avs) %>% 
  mutate(Income = factor(Income, levels = c("Total", income_lvs))) %>% 
  mutate(exc = case_when(w_av_up > 1 & w_av_lp > 1 ~ "Positive",
                         w_av_up < 1 & w_av_lp < 1 ~ "Negative",
                         TRUE ~ "No-excess"),
         out = ifelse(exc == "No-excess", 0.5, 1),
         age_type = ifelse(Age %in% c("Infant", "1-4"), "Inf", "5y"),
         age_type = factor(age_type, levels = rev(c("Inf", "5y")))) %>% 
  left_join(pop_income) %>% 
  mutate(prop_pop = paste0("(", round(Population / tot_pop, 2) * 100, "%)"),
         Income = factor(Income, levels = c("Total", "High", "Upper-mid", "Lower-mid", "Low")),
         Age = factor(Age, levels = c("Infant", "1-4", "0-4", "5-9", 
                                      "10-14", "15-19", "20-24")))

unique(w_avs_all$Income)

write.excel(w_avs_all)
# write.excel(all_fit2)

bks <- c(0.5, 0.6, 0.8, 1, 1.2, 1.5, 2)
lbs <- paste0((bks - 1)*100, "%")

tx <- 10

w_avs_all %>% 
  ggplot()+
  geom_point(aes(w_av, Age, 
                 alpha = out))+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  facet_nested(Income + age_type  ~ Year, space = "free_y", scale = "free_y")+
  scale_x_log10(breaks = bks, labels = lbs, limits = c(0.75, 1.3))+
  scale_y_discrete(limits=rev) %>% 
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  geom_text(aes(0.75, Age, label = prop_pop), size = 2.5, hjust = 0)+
  labs(x = "Weighted average p-scores",
       y = "Age")+
  theme_bw()+
  theme(legend.position = "right",
        legend.text = element_text(size = tx),
        legend.title = element_text(size = tx),
        strip.text.y = element_text(size = tx - 2),
        strip.text.x = element_text(size = tx - 2),
        panel.spacing.y = unit(0,"lines"),
        axis.text.x = element_text(size = tx - 2),
        axis.title.x = element_text(size = tx - 1),
        axis.title.y = element_text(size = tx - 1),
        axis.text.y = element_text(size = tx - 2))

ggsave(paste0("Figures/last version/manuscript/weighted_average_pscores_income.png"), 
       dpi = 600, width = 6, height = 5)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Distribution of p-scores and weighted average
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
         age_type = ifelse(Age %in% c("Infant", "1-4"), "<5y", "5y groups"),
         age_type = factor(age_type, levels = rev(c("<5y", "5y groups"))))

cols <- c("Positive" = "#b7094c",
          "No-excess" = "#5c4d7d",
          "Negative" = "#0091ad")

lms <- c(0.3, 3)
bks <- c(0.1, 0.3, 0.5, 0.7, 1, 1.5, 2, 3, 5)
lbs <- paste0((bks - 1)*100, "%")


w_avs_all2 <- 
  w_avs_all %>% 
  filter(!is.na(Income), 
         Income != "Total") %>% 
  mutate(age_type = ifelse(Age %in% c("Infant", "1-4"), "<5y", "5y groups"),
         age_type = factor(age_type, levels = rev(c("<5y", "5y groups"))), 
         exc = case_when(w_av > 1  ~ "Positive",
                         w_av < 1 ~ "Negative",
                         TRUE ~ "No-excess"),
         exc = factor(exc, 
                      levels = c("Positive", "No-excess", "Negative")),
         Year = Year %>% as.character())


db2 %>% 
  filter(!is.na(Income)) %>%
  ggplot(aes(x = psc, y = Age)) +
  geom_violin(trim = FALSE, fill = "white", size = 0.3)+
  geom_jitter(aes(col = as.factor(exc)), 
              size = 1, alpha = 0.5, 
              height = 0.18,
              width = 0)+
  geom_point(data = w_avs_all2,
             aes(w_av, Age, fill = exc), 
             size = 3,
             col = "black",
             shape = 21)+
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
  labs(x = "P-score", col = "Excess", fill = "Excess")+
  theme_bw()+
  theme(
    axis.text.y = element_text(size = 9, face = "bold"),
    axis.text.x = element_text(size = 7, face = "bold"),
    strip.text = element_text(size = 9, face = "bold"),
    strip.background = element_blank(),
    panel.spacing = unit(0.3, "lines"))

ggsave("Figures/last version/manuscript/fig_04_income.png",
       dpi = 600,
       width = 8, height = 6)

