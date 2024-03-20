rm (list = ls())
source("code/00_functions.R")

# loading excess estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~
all_fit <- 
  read_rds("data_output/sensitivity_p_scores_excess_deaths_rates.rds") %>% 
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

ggsave("figures/manuscript/figS16_sensitivity_excess_summary_2017_2020.png", 
       width = 8, height = 4)

