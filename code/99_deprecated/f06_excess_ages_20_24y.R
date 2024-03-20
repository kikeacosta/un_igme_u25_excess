library(cowplot)
rm (list = ls())
source("Code/00_functions.R")

# loading excess estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~
yng_fit <- 
  read_rds("Output/p_scores_excess_deaths_rates.rds") %>% 
  filter(Age == "20-24")




# Plotting excess estimates for countries by ages
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sx = "f"; yr = 2020
plot_2024 <- function(sx = "f", yr = 2020){

  lvs_cts <-
    yng_fit %>%
    filter(Year == yr,
           Sex == sx) %>%
    arrange(-psc) %>%
    pull(Country) %>%
    unique()
  
  chunk_plot <- 
    yng_fit %>% 
    filter(Year %in% yr,
           Sex == sx) %>% 
    mutate(exc = case_when(up > 1 & lp > 1 ~ "Positive",
                           up < 1 & lp < 1 ~ "Negative",
                           TRUE ~ "No-excess"),
           out = ifelse(exc == "No-excess", 0.5, 1),
           ins = ifelse(exc == "No-excess", 0.6, 0.4),
           Year = Year %>% as.character(),
           Country = factor(Country, levels = lvs_cts),
           Sex = recode(Sex,
                        "f" = "Females",
                        "m" = "Males",
                        "t" = "Total"))

  range_size <- c(1, 3)

  tx <- 10
  cols <- c("Positive" = "#b7094c",
            "Negative" = "#0091ad",
            "No-excess" = "#5c4d7d")
  
  bks <- c(0.2, 0.3, 0.5, 0.7, 1, 1.2, 1.5, 2, 3, 8, 16)
  lbs <- paste0((bks - 1)*100, "%")
  
  plot <-
    chunk_plot %>% 
    ggplot()+
    geom_point(aes(psc, Country, 
                   alpha = out, 
                   col = exc,
                   shape = Year,
                   size = Population))+
    geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
    # facet_wrap(~ Year, scales = "free_x", nrow = 1)+
    facet_nested(Income ~ Sex, scales = "free", space = "free_y")+
    scale_x_log10(breaks = bks,
                  labels = lbs,
                  limits = c(0.3, 1/.3))+
    scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
    scale_color_manual(values = cols, guide = "none")+
    scale_size_continuous(range = range_size)+
    scale_shape_manual(values = c("2020" = 19,
                                  "2021" = 17))+
    guides(shape = guide_legend(override.aes = list(size = 3)))+
    labs(y = "Country", col = "Excess", x = "P-score")+
    theme_bw()+
    theme(legend.position = "none",
          legend.text = element_text(size = tx - 1),
          legend.title = element_text(size = tx - 1),
          strip.text.y = element_blank(),
          strip.text.x = element_text(size = tx - 2,
                                      margin = margin(b = .3, t = 0.3)),
          # strip.text.x = element_text(size = tx - 2,
          #                             margin = margin(b = 0.1, t = 0.1)),
          panel.spacing.y = unit(0,"lines"),
          axis.text.x = element_text(size = tx - 4),
          axis.title.x = element_text(size = tx - 2),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())
  
  return(plot)
  
}


pl_f_2020 <- plot_2024("f", 2020)
pl_f_2021 <- plot_2024("f", 2021)
pl_m_2020 <- plot_2024("m", 2020)
pl_m_2021 <- plot_2024("m", 2021)
pl_t_2020 <- plot_2024("t", 2020)
pl_t_2021 <- plot_2024("t", 2021)


plot_grid(pl_f_2020, pl_f_2021, pl_m_2020, pl_m_2021, 
          ncol = 4)


plot_grid(pl_f_2020, pl_m_2020, 
          ncol = 2)

ggsave("Figures/last version/ages_20_24/excess_ages_2024_in_2020.png", dpi = 600,
       width = 3, height = 5)

plot_grid(pl_f_2021, pl_m_2021, 
          ncol = 2)

ggsave("Figures/last version/ages_20_24/excess_ages_2024_in_2021.png", dpi = 600,
       width = 3, height = 4)









# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~10
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~10
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~10
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~10
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~10
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~10

# rm (list = ls())
# source("Code/00_functions.R")

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
  filter(Year %in% 2015:2021) %>% 
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
  do(est_baseline(chunk = .data)) %>% 
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
  do(est_baseline(chunk = .data)) %>% 
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
  filter(
    Income == "Total",
         Age == "20-24") %>% 
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
  scale_y_log10(limits = c(0.3, 1.5))+
  scale_x_continuous(breaks = 2015:2021)+
  facet_wrap(~Sex, scales = "free_y")+
  # expand_limits(y = c())+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text.y = element_text(size = tx - 1),
        strip.text.x = element_text(size = tx - 1),
        panel.spacing.y = unit(0,"lines"),
        axis.text.x = element_text(size = tx - 2),
        axis.title.x = element_text(size = tx - 1),
        axis.title.y = element_text(size = tx - 1),
        axis.text.y = element_text(size = tx - 2))


ggsave("Figures/last version/ages_20_24/global_baseline_fitting_ages_20_24.png",
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
         age_type = factor(age_type, levels = rev(c("Inf", "5y"))),
         Sex = recode(Sex,
                      "f" = "Females",
                      "m" = "Males",
                      "t" = "Total"),
         Sex = factor(Sex, levels = c("Males", "Females", "Total")))


bks <- c(0.5, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.4, 1.5, 2)
lbs <- paste0((bks - 1)*100, "%")

tx <- 12
cols <- c("Positive" = "#b7094c",
          "Negative" = "#0091ad",
          "No-excess" = "#5c4d7d")

# total
# ~~~~~
tx <- 12

psc_20 %>% 
  filter(Age == "20-24") %>% 
  ggplot()+
  geom_point(aes(psc, Sex, 
                 alpha = out, 
                 col = exc), size = 2.5)+
  geom_linerange(aes(xmin = lp, xmax = up, y = Sex, 
                     alpha = out, 
                     col = exc))+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  geom_text(aes(0.7, Sex, label = prop_pop), size = 3.5, hjust = 0)+
  geom_text(aes(psc, Sex, label = round(100*(psc - 1), 1)), size = 3, hjust = 0.5,
            vjust = 1.6)+
  facet_nested(Income ~ Year, space = "free_y", scale = "free_y")+
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
        axis.text.x = element_text(size = tx - 4),
        axis.title.x = element_text(size = tx - 1),
        axis.title.y = element_text(size = tx - 1),
        axis.text.y = element_text(size = tx - 2))

ggsave("Figures/last version/ages_20_24/global_excess_ages_20_24_v1.png",
       dpi = 600, width = 5, height = 5)





psc_20 %>% 
  filter(Age == "20-24") %>% 
  mutate(Income = factor(Income, levels = rev(income_lvs))) %>% 
  ggplot()+
  geom_point(aes(psc, Income, 
                 alpha = out, 
                 col = exc), size = 3)+
  geom_linerange(aes(xmin = lp, xmax = up, y = Income, 
                     alpha = out, 
                     col = exc))+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  # geom_text(aes(0.7, Income, label = prop_pop), size = 2.5, hjust = 0)+
  geom_text(aes(psc, Income, label = round(100*(psc - 1), 1)), size = 2.5, hjust = 0.5,
            vjust = 1.2)+
  facet_nested(Sex ~ Year, space = "free_y", scale = "free_y")+
  scale_x_log10(breaks = bks, labels = lbs)+
  # scale_y_discrete(limits=rev) %>%
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

ggsave("Figures/last version/ages_20_24/global_excess_ages_20_24_v2.png",
       dpi = 600, width = 5, height = 6)
