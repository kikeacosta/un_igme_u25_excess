rm (list = ls())
source("code/00_functions.R")

excl <- c("Armenia", "Azerbaijan")

# global population by income
pop_income <- 
  read_rds("data_inter/population_by_income_2022.rds")

# loading death data
dts_all <- 
  read_rds("data_inter/annual_deaths_rates_2010_2022.rds") %>% 
  filter(type_data == "counts",
         Income != "No income data",
         !Country %in% excl) %>% 
  mutate(Deaths = round(Deaths, 0),
         Exposure = round(Exposure, 0)) 

# guarantee that the same countries are in all years in each age and sex
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
comm_15_20 <- 
  dts_all %>% 
  filter(Year %in% 2015:2020) %>% 
  select(Country, Year, Age, Sex, Deaths) %>% 
  unique() %>% 
  spread(Year, Deaths) %>% 
  drop_na() %>% 
  select(Country, Age, Sex)

comm_15_21 <- 
  dts_all %>% 
  filter(Year %in% 2015:2021) %>% 
  select(Country, Year, Age, Sex, Deaths) %>% 
  unique() %>% 
  spread(Year, Deaths) %>% 
  drop_na() %>% 
  select(Country, Age, Sex)

comm_15_22 <- 
  dts_all %>% 
  filter(Year %in% 2015:2022) %>% 
  select(Country, Year, Age, Sex, Deaths) %>% 
  unique() %>% 
  spread(Year, Deaths) %>% 
  drop_na() %>% 
  select(Country, Age, Sex)

unique(comm_15_20$Country)
unique(comm_15_21$Country)
unique(comm_15_22$Country)

# 2020
dts_tgh_20 <- 
  dts_all %>% 
  filter(Year %in% 2015:2020) %>% 
  semi_join(comm_15_20) %>% 
  group_by(Income, Sex, Age, Year) %>% 
  summarise(Deaths = sum(Deaths),
            Exposure = sum(Exposure)) %>% 
  ungroup()

dts_tgh_20_tot <- 
  dts_tgh_20 %>% 
  group_by(Sex, Age, Year) %>% 
  summarise(Deaths = sum(Deaths),
            Exposure = sum(Exposure)) %>% 
  ungroup() %>% 
  mutate(Income = "Total")

dts_tgh_20_all <- 
  dts_tgh_20 %>% 
  bind_rows(dts_tgh_20_tot)

# 2021
dts_tgh_21 <- 
  dts_all %>% 
  filter(Year %in% 2015:2021) %>% 
  semi_join(comm_15_21) %>% 
  group_by(Income, Sex, Age, Year) %>% 
  summarise(Deaths = sum(Deaths),
            Exposure = sum(Exposure)) %>% 
  ungroup()

dts_tgh_21_tot <- 
  dts_tgh_21 %>% 
  group_by(Sex, Age, Year) %>% 
  summarise(Deaths = sum(Deaths),
            Exposure = sum(Exposure)) %>% 
  ungroup() %>% 
  mutate(Income = "Total")

dts_tgh_21_all <- 
  dts_tgh_21 %>% 
  bind_rows(dts_tgh_21_tot)

# 2022
dts_tgh_22 <- 
  dts_all %>% 
  filter(Year %in% 2015:2022) %>% 
  semi_join(comm_15_22) %>% 
  group_by(Income, Sex, Age, Year) %>% 
  summarise(Deaths = sum(Deaths),
            Exposure = sum(Exposure)) %>% 
  ungroup()

dts_tgh_22_tot <- 
  dts_tgh_22 %>% 
  group_by(Sex, Age, Year) %>% 
  summarise(Deaths = sum(Deaths),
            Exposure = sum(Exposure)) %>% 
  ungroup() %>% 
  mutate(Income = "Total")

dts_tgh_22_all <- 
  dts_tgh_22 %>% 
  bind_rows(dts_tgh_22_tot)

unique(dts_tgh_20_all$Income)
unique(dts_tgh_21_all$Income)
unique(dts_tgh_22_all$Income)

income_lvs <- c("Total", "High", "Upper-mid", "Lower-mid", "Low")
age_lvs <- c("Stillbirths", "Neonatal", 
             "Infant", "1-4", "0-4", "5-9", 
             "10-14", "15-19", "20-24")

# ~~~~~~~~~~~~~~~~~
# fitting baselines
# ~~~~~~~~~~~~~~~~~
fit_20 <- 
  dts_tgh_20_all %>% 
  group_by(Income, Sex, Age) %>% 
  do(est_baseline_pi(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(fit = "15_20")

fit_21 <- 
  dts_tgh_21_all %>% 
  group_by(Income, Sex, Age) %>% 
  do(est_baseline_pi(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(fit = "15_21")

fit_22 <- 
  dts_tgh_22_all %>% 
  group_by(Income, Sex, Age) %>% 
  do(est_baseline_pi(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(fit = "15_22")

# merging with total population
pop_income2 <- 
  pop_income %>% 
  bind_rows(pop_income %>% 
              filter(Age == "Infant") %>% 
              mutate(Age = "Stillbirths")) %>% 
  bind_rows(pop_income %>% 
              filter(Age == "Infant") %>% 
              mutate(Age = "Neonatal")) %>% 
  arrange(Sex, Age, Year, Income)

fit_all <- 
  bind_rows(fit_20, fit_21, fit_22) %>% 
  left_join(pop_income2) %>% 
  mutate(
    psc = Deaths / bsn,
    bsn_lp = ifelse(bsn_lp < 0, 0, bsn_lp),
    bsn_up = ifelse(bsn_up < 0, 0, bsn_up),
    up = Deaths / bsn_lp,
    lp = Deaths / bsn_up,
    Income = factor(Income, levels = income_lvs),
    prop_pop = paste0("(", round(Exposure / pop_tot, 2) * 100, "%)"))


psc <- 
  fit_all %>% 
  filter(Year %in% 2020:2022) %>% 
  mutate(exc = case_when(up > 1 & lp > 1 ~ "Excess",
                         up < 1 & lp < 1 ~ "Deficit",
                         TRUE ~ "No-change"),
         exc = factor(exc, levels = c("Deficit", "No-change", "Excess")),
         out = ifelse(exc == "No-change", 0.5, 1),
         ins = ifelse(exc == "No-change", 0.6, 0.4),
         Year = Year %>% as.character(),
         Age = factor(Age, levels = age_lvs),
         age_type = case_when(Age %in% c("Stillbirths", "Neonatal") ~ "Peri", 
                              Age %in% c("Infant", "1-4") ~ "Inf", 
                              TRUE ~ "5y"),
         age_type = factor(age_type, levels = rev(c("Peri", "Inf", "5y"))))

write_rds(psc, "data_inter/overall_pscores.rds")

bks <- c(0.5, 0.6, 0.8, 1, 1.2, 1.5, 2)
lbs <- paste0((bks - 1)*100, "%")

tx <- 11
cols <- c("Excess" = "#b7094c",
          "Deficit" = "#0091ad",
          "No-change" = "#5c4d7d")


# Figure 03 ====
# ~~~~~~~~~~~~~~~~~~~~~~~
# Figures all income 
# all overall p-scores, regardless of subset of countries in 2020-2022
psc %>%  
  filter(Sex == "t",
         fit == "15_20" | 
           (fit == "15_21" & Year == 2021) |
           (fit == "15_22" & Year == 2022),
         Income == "Total") %>% 
  ggplot()+
  geom_point(aes(psc, Age, 
                 alpha = out, 
                 col = exc))+
  geom_linerange(aes(xmin = lp, xmax = up, y = Age, 
                     alpha = out, 
                     col = exc))+
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.8, col = "black")+
  geom_text(aes(0.7, Age, label = prop_pop), size = 2.8, hjust = 0)+
  facet_nested(age_type ~ Year, space = "free_y", scale = "free_y",
               nest_line = element_line(linetype = 1))+
  scale_x_log10(breaks = bks, labels = lbs)+
  scale_y_discrete(limits=rev) %>% 
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols)+
  labs(x = "Overall p-score",
       y = "Age",
       col = "Change")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = tx),
        legend.title = element_text(size = tx),
        strip.text = element_text(size = tx - 2, face = "bold"),
        strip.background = element_blank(),
        panel.spacing.y = unit(0.2,"lines"),
        axis.text.x = element_text(size = tx - 2),
        axis.title.x = element_text(size = tx - 1),
        axis.title.y = element_text(size = tx - 1),
        axis.text.y = element_text(size = tx - 2))

ggsave(paste0("figures/fig04_overall_pscores_15_20.png"), 
       dpi = 700, width = 7, height = 3.5)


# overall p-scores in countries with complete data between 2015 and 2021
psc %>%  
  filter(Sex == "t",
         fit == "15_21",
         Income == "Total") %>% 
  ggplot()+
  geom_point(aes(psc, Age, 
                 alpha = out, 
                 col = exc))+
  geom_linerange(aes(xmin = lp, xmax = up, y = Age, 
                     alpha = out, 
                     col = exc))+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  geom_text(aes(0.7, Age, label = prop_pop), size = 2.8, hjust = 0)+
  facet_nested(Income + age_type ~ Year, space = "free_y", scale = "free_y",
               nest_line = element_line(linetype = 1))+
  scale_x_log10(breaks = bks, labels = lbs)+
  scale_y_discrete(limits=rev) %>% 
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  labs(x = "Overall p-score",
       y = "Age")+
  theme_bw()+
  theme(legend.position = "right",
        legend.text = element_text(size = tx),
        legend.title = element_text(size = tx),
        strip.text = element_text(size = tx - 2, face = "bold"),
        strip.background = element_blank(),
        panel.spacing.y = unit(0.2,"lines"),
        axis.text.x = element_text(size = tx - 2),
        axis.title.x = element_text(size = tx - 1),
        axis.title.y = element_text(size = tx - 1),
        axis.text.y = element_text(size = tx - 2))


# Figure S11 ====
# all overall p-scores, regardless of subset of countries
psc %>%  
  filter(Sex == "t",
         fit == "15_20" | 
           (fit == "15_21" & Year == 2021) | 
           (fit == "15_22" & Year == 2022),
         Income != "Total") %>% 
  ggplot()+
  geom_point(aes(psc, Age, 
                 alpha = out, 
                 col = exc))+
  geom_linerange(aes(xmin = lp, xmax = up, y = Age, 
                     alpha = out, 
                     col = exc))+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  geom_text(aes(0.7, Age, label = prop_pop), size = 2.8, hjust = 0)+
  facet_nested(Income + age_type ~ Year, space = "free_y", scale = "free_y",
               nest_line = element_line(linetype = 1))+
  scale_x_log10(breaks = bks, labels = lbs)+
  scale_y_discrete(limits=rev) %>% 
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  labs(x = "Overall p-score",
       y = "Age")+
  theme_bw()+
  theme(legend.position = "right",
        legend.text = element_text(size = tx),
        legend.title = element_text(size = tx),
        strip.text = element_text(size = tx - 2, face = "bold"),
        strip.background = element_blank(),
        panel.spacing.y = unit(0.2,"lines"),
        axis.text.x = element_text(size = tx - 2),
        axis.title.x = element_text(size = tx - 1),
        axis.title.y = element_text(size = tx - 1),
        axis.text.y = element_text(size = tx - 2))

ggsave(paste0("figures/figS11_overall_pscores_income_15_20.png"), 
       dpi = 700, width = 6, height = 5)


# Figure S12 ====
# overall p-scores in countries with complete data between 2015 and 2021
psc %>%  
  filter(Sex == "t",
         fit == "15_22",
         Income != "Total") %>% 
  ggplot()+
  geom_point(aes(psc, Age, 
                 alpha = out, 
                 col = exc))+
  geom_linerange(aes(xmin = lp, xmax = up, y = Age, 
                     alpha = out, 
                     col = exc))+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  geom_text(aes(0.7, Age, label = prop_pop), size = 2.8, hjust = 0)+
  facet_nested(Income + age_type ~ Year, space = "free_y", scale = "free_y",
               nest_line = element_line(linetype = 1))+
  scale_x_log10(breaks = bks, labels = lbs)+
  scale_y_discrete(limits=rev) %>% 
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  labs(x = "Overall p-score",
       y = "Age")+
  theme_bw()+
  theme(legend.position = "right",
        legend.text = element_text(size = tx),
        legend.title = element_text(size = tx),
        strip.text = element_text(size = tx - 2, face = "bold"),
        strip.background = element_blank(),
        panel.spacing.y = unit(0.2,"lines"),
        axis.text.x = element_text(size = tx - 2),
        axis.title.x = element_text(size = tx - 1),
        axis.title.y = element_text(size = tx - 1),
        axis.text.y = element_text(size = tx - 2))

ggsave(paste0("figures/figS12_overall_pscores_income_15_21.png"), 
       dpi = 700, width = 6, height = 5)

