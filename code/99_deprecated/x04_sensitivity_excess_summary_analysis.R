rm (list = ls())
source("Code/00_functions.R")

# loading excess estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~
all_fit <- 
  read_rds("Output/sensitivity_p_scores_excess_deaths_rates.rds") %>% 
  filter(Sex == "t")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plotting proportions according to excess
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

all_fit2 <-
  all_fit %>%
  select(Country, Year, Age, Sex, Exposure, psc, lp, up) %>%
  mutate(psc_out = ifelse(1 < up & 1 > lp, 1, psc)) %>%
  unique()

all_fit2 %>% 
  filter(Sex == "t") %>% 
  select(Country, Year) %>% 
  unique() %>% 
  group_by(Year) %>% 
  summarise(n = n())

unique(all_fit2$Age)
unique(all_fit2$Country)


# restricting countries that appear in 2017 to 2020 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
common_cts_17_20 <- 
  all_fit2 %>% 
  filter(Sex == "t",
         Year <= 2020) %>% 
  select(Country, Year, Age, psc) %>% 
  unique() %>% 
  spread(Year, psc) %>% 
  drop_na() %>% 
  select(Country, Age)

db_summary_17_20 <- 
  all_fit2 %>% 
  filter(Sex == "t") %>% 
  inner_join(common_cts_17_20) %>% 
  mutate(exc_all = case_when(psc > 1 ~ "Excess",
                             psc < 1 ~ "Deficit",
                             psc == 1 ~ "No-change"),
         exc_out = case_when(psc_out > 1 ~ "Excess",
                             psc_out < 1 ~ "Deficit",
                             psc_out == 1 ~ "No-change")) %>% 
  select(Year, Age, Sex, Country, exc_all, exc_out) %>% 
  group_by(Year, Age, Sex) %>% 
  mutate(N_cts = n()) %>% 
  ungroup() %>%  
  gather(exc_all, exc_out, key = type, value = is_excess) %>% 
  group_by(Year, Age, N_cts, type, is_excess) %>% 
  summarise(N = n()) %>% 
  ungroup() %>% 
  mutate(Prop = N / N_cts,
         Age = factor(Age, levels = c("Stillbirths", "Neonatal", 
                                      "Infant", "1-4", "0-4", "5-9", 
                                      "10-14", "15-19", "20-24")),
         type = recode(type,
                       "exc_all" = "All estimates",
                       "exc_out" = "Uncertainty"),
         type = factor(type, levels = c("All estimates", "Uncertainty")),
         is_excess = factor(is_excess, 
                            levels = c("Excess", "No-change", "Deficit")),
         age_type = case_when(Age %in% c("Stillbirths", "Neonatal") ~ "Peri", 
                              Age %in% c("Infant", "1-4") ~ "Inf", 
                              TRUE ~ "5y"),
         age_type = factor(age_type, levels = rev(c("Peri", "Inf", "5y"))))

cts_inc <- 
  all_fit2 %>% 
  group_by(Country) %>% 
  filter(max(Year) >= 2020 & min(Year) == 2017) %>% 
  ungroup() %>% 
  pull(Country) %>% 
  unique() %>% 
  length()

cols <- c("Excess" = "#b7094c",
          "Deficit" = "#0091ad",
          "No-change" = "#5c4d7d")

db_summary_17_20 %>% 
  filter(type == "Uncertainty",
         !Age %in% c("Stillbirths", "Neonatal")) %>% 
  ggplot(aes(x = Age, y = Prop))+
  geom_bar(aes(fill = is_excess),
           stat = "identity", 
           alpha = 0.8)+
  geom_text(aes(label = paste0(round(Prop*100)), group = is_excess),
            position = position_stack(vjust = 0.5, reverse = F), size = 3)+
  # geom_hline(yintercept = 0.5, linetype = "dashed")+
  facet_nested(age_type ~ Year, scales = "free", space = "free_y",
               nest_line = element_line(linetype = 1))+
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE))+
  labs(fill = "Change")+
       # title = paste0("Countries with excess estimates in 2017-2020 (", cts_inc, ")"))+
  coord_flip(expand = 0)+
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        axis.text.x = element_text(size = 7),
        plot.title = element_text(size = 10),
        panel.spacing = unit(0.5, "lines"))

ggsave("Figures/manuscript/figS16_sensitivity_excess_summary_2017_2020.png", 
       width = 8, height = 4)



# db_summary <- 
#   all_fit2 %>% 
#   mutate(exc_all = case_when(psc > 1 ~ "Positive",
#                              psc < 1 ~ "Negative",
#                              psc == 1 ~ "No-excess"),
#          exc_out = case_when(psc_out > 1 ~ "Positive",
#                              psc_out < 1 ~ "Negative",
#                              psc_out == 1 ~ "No-excess")) %>% 
#   select(Year, Age, Sex, Country, exc_all, exc_out) %>% 
#   group_by(Year, Age, Sex) %>% 
#   mutate(N_cts = n()) %>% 
#   ungroup() %>%  
#   gather(exc_all, exc_out, key = type, value = is_excess) %>% 
#   group_by(Year, Age, Sex, N_cts, type, is_excess) %>% 
#   summarise(N = n()) %>% 
#   ungroup() %>% 
#   mutate(Prop = N / N_cts,
#          Age = factor(Age, levels = c("Stillbirths", "Neonatal", 
#                                       "Infant", "1-4", "0-4", "5-9", 
#                                           "10-14", "15-19", "20-24")),
#          type = recode(type,
#                        "exc_all" = "All estimates",
#                        "exc_out" = "Uncertainty"),
#          type = factor(type, levels = c("All estimates", "Uncertainty")),
#          is_excess = factor(is_excess, 
#                             levels = c("Positive", "No-excess", "Negative")),
#          Sex = recode(Sex,
#                       "f" = "females",
#                       "m" = "males",
#                       "t" = "total"),
#          age_type = case_when(Age %in% c("Stillbirths", "Neonatal") ~ "Peri", 
#                               Age %in% c("Infant", "1-4") ~ "Inf", 
#                               TRUE ~ "5y"),
#          age_type = factor(age_type, levels = rev(c("Peri", "Inf", "5y"))))
# 
# cts_inc <- 
#   all_fit2 %>% 
#   group_by(Country) %>% 
#   ungroup() %>% 
#   pull(Country) %>% 
#   unique() %>% 
#   length()
# 
# 
# cols <- 
#   c("Positive" = "#b7094c",
#     "No-excess" = "#5c4d7d",
#     "Negative" = "#0091ad")
# 
# db_summary %>% 
#   filter(type == "Uncertainty",
#          !Age %in% c("Stillbirths", "Neonatal")) %>% 
#   ggplot(aes(x = Age, y = Prop))+
#   geom_bar(aes(fill = is_excess),
#            stat = "identity", 
#            position = "fill",
#            alpha = 0.8)+
#   geom_text(aes(label = paste0(round(Prop*100)), group = is_excess), 
#             position = position_stack(vjust = 0.5, reverse = F), size = 3)+
#   # geom_hline(yintercept = 0.5, linetype = "dashed")+
#   facet_nested(age_type ~ Year, scales = "free", space = "free_y",
#                nest_line = element_line(linetype = 1))+
#   scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE))+
#   labs(fill = "Excess", 
#        title = "Independent countries in each year")+
#   coord_flip(expand = 0)+
#   theme(legend.position = "bottom",
#         strip.background = element_blank(),
#         strip.text = element_text(size = 9, face = "bold"),
#         axis.text.y = element_text(size = 9, face = "bold"),
#         axis.text.x = element_text(size = 7),
#         plot.title = element_text(size = 10),
#         panel.spacing = unit(0.5, "lines"))
# 
# ggsave("Figures/manuscript/figS16_sensitivity_excess_summary_independent.png", 
#        width = 8, height = 4)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# # changes in proportions between 2020 and 2021
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# db_summary_17_20 %>% 
#   mutate(Age = factor(Age, levels = c("Infant", "1-4", "0-4", "5-9", 
#                                       "10-14", "15-19", "20-24")),
#          age_type = factor(age_type, levels = c("Inf", "5y grps")),
#          Year = factor(Year, levels = c("2017", "2018", "2019", "2020", "2021")),
#          Prop = Prop * 100) %>% 
#   filter(is_excess != "No-excess") %>%
#   ggplot(aes(x = Year, y = Prop))+
#   geom_bar(aes(fill = is_excess), 
#            stat = "identity")+
#   # geom_hline(yintercept = 0.5, linetype = "dashed")+
#   geom_text(aes(label = paste0(round(Prop)), group = is_excess), 
#             position = position_stack(vjust = 0.5, reverse = F), size = 2)+
#   # scale_y_continuous(labels = paste0(Prop*100, "%"))+
#   facet_nested(type ~ is_excess + age_type + Age, scales = "free", space = "free_y",
#                nest_line = element_line(linetype = 1))+
#   scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE))+
#   labs(y = "%")+
#   theme_minimal()+
#   theme(strip.background = element_blank(),
#         strip.text = element_text(size = 9, face = "bold"),
#         axis.text.x = element_text(size = 7, face = "bold", 
#                                    angle = 90, vjust = 0.5),
#         axis.text.y = element_text(size = 8),
#         # axis.title.y = element_blank(),
#         plot.title = element_text(size = 10),
#         panel.spacing.x = unit(0.5, "lines"),
#         panel.spacing.y = unit(1, "lines"),
#         legend.position = "none")
# 
# ggsave("Figures/last version/sens_analysis/summary_excess/sensitivity_excess_summary_2017_2021_change.png", 
#        width = 8, height = 4)
# 
# 
# # ratio positive to negative by year
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# neg_to_pos <- 
#   db_summary_17_20 %>% 
#   select(-Prop) %>%
#   filter(type == "Uncertainty") %>% 
#   spread(is_excess, N) %>% 
#   replace_na(list(Negative = 0, Positive = 0)) %>% 
#   mutate(ratio = Negative / Positive,
#          Age = factor(Age, levels = c("Infant", "1-4", "0-4", "5-9", 
#                                       "10-14", "15-19", "20-24")),
#          Year = Year %>% as.character() %>% as.double(),
#          cts = Negative + Positive,
#          neg_prop = Negative / (Negative + Positive),
#          no_exc_prop = `No-excess` / (Negative + Positive + `No-excess`))
# 
# neg_to_pos %>% 
#   ggplot()+
#   geom_point(aes(ratio, Year))+
#   scale_x_log10(limits = c(0.2, 5))+
#   facet_grid(~Age)+
#   geom_vline(xintercept = 1, linetype = "dashed")+
#   labs(title = "ratio negative to positive excess")+
#   theme_bw()
# 
# neg_to_pos %>% 
#   ggplot()+
#   geom_point(aes(neg_prop, Year))+
#   scale_x_continuous(limits = c(0, 1))+
#   facet_grid(~Age)+
#   geom_vline(xintercept = 0.5, linetype = "dashed")+
#   labs(title = "Proportion of countries with negative excess")+
#   theme_bw()
# 
# neg_to_pos %>% 
#   mutate(Year = factor(Year)) %>% 
#   ggplot()+
#   geom_point(aes(ratio, Age, col = Year), alpha = .6)+
#   scale_x_log10(limits = c(0.2, 5))+
#   scale_color_manual(values = c(rep("black", 3), 
#                                 "#ff006e", "#8338ec"))+
#   geom_vline(xintercept = 1, linetype = "dashed")+
#   labs(title = "Ratio of negative to positive excess")+
#   theme_bw()
# 
# neg_to_pos %>% 
#   mutate(Year = factor(Year)) %>% 
#   ggplot()+
#   geom_point(aes(neg_prop, Age, col = Year), alpha = .6)+
#   scale_x_continuous(limits = c(0, 1))+
#   # facet_grid(~Age)+
#   scale_color_manual(values = c(rep("black", 3), 
#                                 "#ff006e", "#8338ec"))+
#   geom_vline(xintercept = 0.5, linetype = "dashed")+
#   labs(title = "Proportion of countries with negative excess")+
#   theme_bw()
# 
# 
# neg_to_pos %>% 
#   mutate(Year = factor(Year)) %>% 
#   ggplot()+
#   geom_point(aes(no_exc_prop, Age, col = Year), alpha = .5)+
#   scale_x_continuous(limits = c(0, 1))+
#   # facet_grid(~Age)+
#   scale_color_manual(values = c(rep("black", 3), 
#                                 "#ff006e", "#8338ec"))+
#   # scale_shape_manual(values = c(1, 1, 1, 16, 16))
#   geom_vline(xintercept = 0.5, linetype = "dashed")+
#   labs(title = "Proportion of countries with no excess")+
#   theme_bw()
# 
