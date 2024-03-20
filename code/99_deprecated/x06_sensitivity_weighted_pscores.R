rm (list = ls())
source("Code/00_functions.R")

# loading excess estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~
all_fit <- 
  read_rds("Output/sensitivity_p_scores_excess_deaths_rates.rds") %>% 
  filter(Sex == "t")

# weighted averages
# ~~~~~~~~~~~~~~~~~
# total population by income
pop_income <- 
  read_rds("data_inter/population_by_income_2022.rds")

cts_exc <- c("Kenya", "Dominican Republic")
cts_exc <- c()
income_lvs <- c("Total", "High", "Upper-mid", "Lower-mid", "Low")

common_cts_17_20 <- 
  all_fit %>% 
  filter(Sex == "t",
         Year <= 2020) %>% 
  select(Country, Year, Age, psc) %>% 
  spread(Year, psc) %>% 
  drop_na() %>% 
  select(Country, Age) %>% 
  mutate(keep = 1)
  
# weighted average P-scores in all populations
w_avs <- 
  all_fit %>% 
  filter(!Country %in% cts_exc) %>% 
  left_join(common_cts_17_20) %>% 
  filter(keep == 1) %>% 
  group_by(Year, Sex, Age) %>% 
  mutate(c = Population / sum(Population)) %>% 
  summarize(w_av = sum(psc * c),
            w_av_lp = sum(lp * c),
            w_av_up = sum(up * c),
            av = mean(psc),
            av_lp = mean(lp),
            av_up = mean(up),
            Population = sum(Population),
            n = sum(keep)) %>% 
  ungroup() %>%
  mutate(Income = "Total")

# weighted average P-scores by income
w_avs_income <- 
  all_fit %>% 
  filter(!Country %in% cts_exc) %>% 
  drop_na(psc) %>% 
  left_join(common_cts_17_20) %>% 
  filter(keep == 1) %>% 
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
  scale_x_log10(breaks = bks, labels = lbs, limits = c(0.75, 1.2))+
  # scale_x_log10(breaks = bks, labels = lbs)+
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

ggsave(paste0("Figures/last version/sens_analysis/weighted_p_scores/weighted_average_pscores_2017_2020.png"), 
       dpi = 600, width = 8, height = 5)



# independent amount of countriesper year
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# weighted average P-scores in all populations
w_avs <- 
  all_fit %>% 
  filter(!Country %in% cts_exc) %>% 
  group_by(Year, Sex, Age) %>% 
  mutate(c = Population / sum(Population)) %>% 
  summarize(w_av = sum(psc * c),
            w_av_lp = sum(lp * c),
            w_av_up = sum(up * c),
            av = mean(psc),
            av_lp = mean(lp),
            av_up = mean(up),
            Population = sum(Population),
            n = n()) %>% 
  ungroup() %>%
  mutate(Income = "Total")

# weighted average P-scores by income
w_avs_income <- 
  all_fit %>% 
  filter(!Country %in% cts_exc) %>% 
  drop_na(psc) %>% 
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
  # scale_x_log10(breaks = bks, labels = lbs, limits = c(0.75, 1.2))+
  scale_x_log10(breaks = bks, labels = lbs)+
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

ggsave(paste0("Figures/last version/sens_analysis/weighted_p_scores/weighted_average_pscores_independent.png"), 
       dpi = 600, width = 8, height = 5)






