rm (list = ls())
source("Code/00_functions.R")

excl <- c()

# global population by income
pop_income <- 
  read_rds("data_inter/population_by_income_2022.rds")

# loading death data
dts_all <- 
  read_rds("data_inter/annual_deaths_rates_2010_2021.rds") %>% 
  filter(type_data == "counts") %>% 
  mutate(Deaths = round(Deaths, 0),
         Population = round(Population, 0))

# guarantee that the same countries are in all years in each age and sex
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
comm_15_20 <- 
  dts_all %>% 
  filter(Year %in% 2015:2020) %>% 
  select(Country, Year, Age, Sex, Deaths) %>% 
  unique() %>% 
  spread(Year, Deaths) %>% 
  drop_na() %>% 
  select(Country, Age, Sex) %>% 
  mutate(keep = 1)

comm_15_21 <- 
  dts_all %>% 
  filter(Year %in% 2015:2021) %>% 
  select(Country, Year, Age, Sex, Deaths) %>% 
  unique() %>% 
  spread(Year, Deaths) %>% 
  drop_na() %>% 
  select(Country, Age, Sex) %>% 
  mutate(keep = 1)

unique(comm_15_20$Country)
unique(comm_15_21$Country)

# 2020
dts_tgh_20 <- 
  dts_all %>% 
  filter(Year %in% 2015:2020) %>% 
  left_join(comm_15_20) %>% 
  filter(keep == 1) %>% 
  group_by(Income, Sex, Age, Year) %>% 
  summarise(Deaths = sum(Deaths),
            Population = sum(Population)) %>% 
  ungroup()

dts_tgh_20_tot <- 
  dts_tgh_20 %>% 
  group_by(Sex, Age, Year) %>% 
  summarise(Deaths = sum(Deaths),
            Population = sum(Population)) %>% 
  ungroup() %>% 
  mutate(Income = "Total")

dts_tgh_20_all <- 
  dts_tgh_20 %>% 
  bind_rows(dts_tgh_20_tot)

# 2021
dts_tgh_21 <- 
  dts_all %>% 
  filter(Year %in% 2015:2021) %>% 
  left_join(comm_15_21) %>% 
  filter(keep == 1) %>% 
  group_by(Income, Sex, Age, Year) %>% 
  summarise(Deaths = sum(Deaths),
            Population = sum(Population)) %>% 
  ungroup()

dts_tgh_21_tot <- 
  dts_tgh_21 %>% 
  group_by(Sex, Age, Year) %>% 
  summarise(Deaths = sum(Deaths),
            Population = sum(Population)) %>% 
  ungroup() %>% 
  mutate(Income = "Total")

dts_tgh_21_all <- 
  dts_tgh_21 %>% 
  bind_rows(dts_tgh_21_tot)

income_lvs <- c("Total", "High", "Upper-mid", "Lower-mid", "Low")


# fitting baselines
fit_20 <- 
  dts_tgh_20_all %>% 
  group_by(Income, Sex, Age) %>% 
  do(est_baseline_pi(chunk = .data)) %>% 
  ungroup() %>% 
  left_join(pop_income) %>% 
  mutate(
    psc = Deaths / bsn,
    bsn_lp = ifelse(bsn_lp < 0, 0, bsn_lp),
    bsn_up = ifelse(bsn_up < 0, 0, bsn_up),
    up = Deaths / bsn_lp,
    lp = Deaths / bsn_up,
    Income = factor(Income, levels = income_lvs),
    prop_pop = paste0("(", round(Population / tot_pop, 2) * 100, "%)"))


fit_21 <- 
  dts_tgh_21_all %>% 
  group_by(Income, Sex, Age) %>% 
  do(est_baseline_pi(chunk = .data)) %>% 
  ungroup() %>% 
  left_join(pop_income) %>% 
  mutate(
    psc = Deaths / bsn,
    bsn_lp = ifelse(bsn_lp < 0, 0, bsn_lp),
    bsn_up = ifelse(bsn_up < 0, 0, bsn_up),
    up = Deaths / bsn_lp,
    lp = Deaths / bsn_up,
    Income = factor(Income, levels = income_lvs),
    prop_pop = paste0("(", round(Population / tot_pop, 2) * 100, "%)"))


dts_all %>% 
  filter(Year %in% 2015:2021) %>% 
  left_join(comm_15_21) %>% 
  filter(keep == 1) %>% 
  pull(Country) %>% 
  unique() %>% 
  length()

dts_all %>% 
  filter(Year %in% 2015:2020) %>% 
  left_join(comm_15_20) %>% 
  filter(keep == 1) %>% 
  pull(Country) %>% 
  unique() %>% 
  length()


# fitting 
tx <- 12
cols <- c("Positive" = "#b7094c",
          "Negative" = "#0091ad",
          "No-excess" = "#5c4d7d")

fit_21 %>% 
  filter(Sex == "t",
         Income == "Total",
         Age != "0-4") %>% 
  mutate(Rate = 1e3 * Deaths / Population,
         bsn_r = 1e3 * bsn / Population,
         bsn_lp_r = 1e3 * bsn_lp / Population,
         bsn_up_r = 1e3 * bsn_up / Population,
         Age = factor(Age, levels = c("Infant", "1-4", "0-4", "5-9", 
                                      "10-14", "15-19", "20-24")),
         exc = case_when(Year >= 2020 & up > 1 & lp > 1 ~ "Positive",
                         Year >= 2020 & up < 1 & lp < 1 ~ "Negative",
                         TRUE ~ "No-excess"),
         out = ifelse(exc == "No-excess", 0.5, 1)) %>% 
  ggplot()+
  geom_ribbon(aes(Year, ymin = bsn_lp_r, ymax = bsn_up_r), alpha = 0.2)+
  geom_point(aes(Year, Rate, col = exc, alpha = out), size = 3)+
  geom_line(aes(Year, bsn_r))+
  geom_vline(xintercept = 2019.5, linetype = "dashed")+
  scale_alpha_continuous(range = c(0.7, 1), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  scale_y_log10()+
  scale_x_continuous(breaks = 2015:2021)+
  facet_wrap(~Age, scales = "free_y")+
  # expand_limits(y = 0)+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text.y = element_text(size = tx - 1),
        strip.text.x = element_text(size = tx - 1),
        panel.spacing.y = unit(0,"lines"),
        axis.text.x = element_text(size = tx - 2),
        axis.title.x = element_text(size = tx - 1),
        axis.title.y = element_text(size = tx - 1),
        axis.text.y = element_text(size = tx - 2))

  
ggsave("Figures/last version/manuscript/global_baseline_fitting.png", 
       dpi = 600, width = 12, height = 3)

# ~~~~~~~~~~~~~~~~
# plotting results
# ~~~~~~~~~~~~~~~~
tx <- 10
cols <- c("Positive" = "#b7094c",
          "Negative" = "#0091ad",
          "No-excess" = "#5c4d7d")

psc_20 <- 
  fit_20 %>% 
  filter(Year %in% 2020) %>% 
  mutate(exc = case_when(up > 1 & lp > 1 ~ "Positive",
                         up < 1 & lp < 1 ~ "Negative",
                         TRUE ~ "No-excess"),
         out = ifelse(exc == "No-excess", 0.5, 1),
         ins = ifelse(exc == "No-excess", 0.6, 0.4),
         Year = Year %>% as.character(),
         Age = factor(Age, levels = c("Infant", "1-4", "0-4", "5-9", 
                                      "10-14", "15-19", "20-24")),
         age_type = ifelse(Age %in% c("Infant", "1-4"), "Inf", "5y"),
         age_type = factor(age_type, levels = rev(c("Inf", "5y"))))


bks <- c(0.5, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.5, 2)
lbs <- paste0((bks - 1)*100, "%")

tx <- 10
cols <- c("Positive" = "#b7094c",
          "Negative" = "#0091ad",
          "No-excess" = "#5c4d7d")

# total
# ~~~~~
psc_20 %>% 
  filter(Sex == "t",
         Income == "Total") %>% 
  ggplot()+
  geom_point(aes(psc, Age, 
                 alpha = out, 
                 col = exc))+
  geom_linerange(aes(xmin = lp, xmax = up, y = Age, 
                     alpha = out, 
                     col = exc))+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  geom_text(aes(0.8, Age, label = prop_pop), size = 2.5, hjust = 0)+
  facet_nested(Income + age_type ~ Year, space = "free_y", scale = "free_y")+
  scale_x_log10(breaks = bks, labels = lbs)+
  scale_y_discrete(limits=rev) %>% 
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  labs(x = "Global p-score",
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

ggsave(paste0("Figures/last version/manuscript/global_pscores_15_20.png"), 
       dpi = 600, width = 4, height = 2)

# by income level
# ~~~~~~~~~~~~~~~
psc_20 %>% 
  filter(Sex == "t",
         !is.na(Income)) %>% 
  ggplot()+
  geom_point(aes(psc, Age, 
                 alpha = out, 
                 col = exc))+
  geom_linerange(aes(xmin = lp, xmax = up, y = Age, 
                     alpha = out, 
                     col = exc))+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  geom_text(aes(0.7, Age, label = prop_pop), size = 2.5, hjust = 0)+
  facet_nested(Income + age_type ~ Year, space = "free_y", scale = "free_y")+
  scale_x_log10(breaks = bks, labels = lbs)+
  scale_y_discrete(limits=rev) %>% 
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  labs(x = "Global p-score",
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

ggsave(paste0("Figures/last version/manuscript/global_pscores_income_15_20.png"), 
       dpi = 600, width = 4, height = 5)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
psc_21 <- 
  fit_21 %>% 
  filter(Year %in% 2020:2021) %>% 
  mutate(exc = case_when(up > 1 & lp > 1 ~ "Positive",
                         up < 1 & lp < 1 ~ "Negative",
                         TRUE ~ "No-excess"),
         out = ifelse(exc == "No-excess", 0.5, 1),
         ins = ifelse(exc == "No-excess", 0.6, 0.4),
         Year = Year %>% as.character(),
         Age = factor(Age, levels = c("Infant", "1-4", "0-4", "5-9", 
                                      "10-14", "15-19", "20-24")),
         age_type = ifelse(Age %in% c("Infant", "1-4"), "Inf", "5y"),
         age_type = factor(age_type, levels = rev(c("Inf", "5y"))))


bks <- c(0.5, 0.6, 0.8, 1, 1.2, 1.5, 2)
lbs <- paste0((bks - 1)*100, "%")

tx <- 10
cols <- c("Positive" = "#b7094c",
          "Negative" = "#0091ad",
          "No-excess" = "#5c4d7d")



psc_21 %>% 
  filter(Sex == "t",
         Income == "Total") %>% 
  ggplot()+
  geom_point(aes(psc, Age, 
                 alpha = out, 
                 col = exc))+
  geom_linerange(aes(xmin = lp, xmax = up, y = Age, 
                     alpha = out, 
                     col = exc))+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  geom_text(aes(0.8, Age, label = prop_pop), size = 2.5, hjust = 0)+
  facet_nested(Income + age_type ~ Year, space = "free_y", scale = "free_y")+
  scale_x_log10(breaks = bks, labels = lbs)+
  scale_y_discrete(limits=rev) %>% 
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  labs(x = "Global p-score",
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

ggsave(paste0("Figures/last version/manuscript/global_pscores_15_21.png"), 
       dpi = 600, width = 6, height = 2)


# by income
# ~~~~~~~~~
psc_21 %>% 
  filter(Sex == "t",
         !is.na(Income)) %>% 
  ggplot()+
  geom_point(aes(psc, Age, 
                 alpha = out, 
                 col = exc))+
  geom_linerange(aes(xmin = lp, xmax = up, y = Age, 
                     alpha = out, 
                     col = exc))+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  geom_text(aes(0.7, Age, label = prop_pop), size = 2.5, hjust = 0)+
  facet_nested(Income + age_type ~ Year, space = "free_y", scale = "free_y")+
  scale_x_log10(breaks = bks, labels = lbs)+
  scale_y_discrete(limits=rev) %>% 
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  labs(x = "Global p-score",
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

ggsave(paste0("Figures/last version/manuscript/global_pscores_income_15_21.png"), 
       dpi = 600, width = 6, height = 5)






psc_20 %>% 
  filter(Sex == "t",
         !is.na(Income)) %>% 
  bind_rows(psc_21 %>% 
              filter(Sex == "t",
                     Year == 2021,
                     !is.na(Income))) %>% 
  ggplot()+
  geom_point(aes(psc, Age, 
                 alpha = out, 
                 col = exc))+
  geom_linerange(aes(xmin = lp, xmax = up, y = Age, 
                     alpha = out, 
                     col = exc))+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  geom_text(aes(0.7, Age, label = prop_pop), size = 2.5, hjust = 0)+
  facet_nested(Income + age_type ~ Year, space = "free_y", scale = "free_y")+
  scale_x_log10(breaks = bks, labels = lbs)+
  scale_y_discrete(limits=rev) %>% 
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  labs(x = "Global p-score",
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

ggsave(paste0("Figures/last version/manuscript/global_pscores_income_15_20_21.png"), 
       dpi = 600, width = 6, height = 5)

