rm (list = ls())
source("Code/00_functions.R")

dts <- 
  read_rds("data_output/preliminary_pscores_2024-01-16.rds")

unique(dts$Age)

pop_income <- 
  read_rds("data_inter/population_by_income_2022.rds") %>% 
  filter(Sex == "t") %>% 
  select(income = Income, year = Year, age = Age, pop_tot) %>% 
  mutate(year = as.character(year))

pop_income2 <- 
  pop_income %>% 
  bind_rows(pop_income %>%
              filter(age == "Infant") %>% 
              mutate(age = "Stillbirths"),
            pop_income %>%
              filter(age == "Infant") %>% 
              mutate(age = "Neonatal"))

unique(dts$Income)
income_lvs <- c("High", "Upper-mid", "Lower-mid", "Low", "No income data")
unique(dts$Age)

dts2 <- 
  dts %>% 
  filter(Year %in% 2020:2021,
         Sex == "t",
         inc_in == 1,
         Age != "0-4") %>% 
  select(country = Country, code = Code, income = Income, 
         year = Year, age = Age, 
         pop = Exposure,
         dts = Deaths, bsn, bsn_up, bsn_lp, psc, up, lp) %>% 
  mutate(exc = case_when(up > 1 & lp > 1 ~ "Excess",
                         up < 1 & lp < 1 ~ "Deficit",
                         TRUE ~ "No-change"),
         exc = factor(exc, 
                      levels = c("Excess", "No-change", "Deficit")),
         year = year %>% as.character(),
         age_type = case_when(age %in% c("Stillbirths", "Neonatal") ~ "<1m", 
                              age %in% c("Infant", "1-4") ~ "<5y", 
                              TRUE ~ "5y groups"),
         age_type = factor(age_type, 
                           levels = rev(c("<1m", "<5y", "5y groups"))))

unique(dts2$age)

# extreme p-score values
# ~~~~~~~~~~~~~~~~~~~~~~

dts2 %>% 
  group_by(age, year) %>% 
  summarise(min_psc = min(psc) - 1,
            max_psc = max(psc) - 1)


# ~~~~~~~~~~~~~~~~~
# Weighted averages
# ~~~~~~~~~~~~~~~~~
# weighted average P-scores in all populations
wgths <- 
  dts2 %>% 
  filter(year >= 2020) %>% 
  group_by(year, age) %>% 
  mutate(w = pop / mean(pop)) %>% 
  ungroup()

# install.packages("drf")
# library(drf)
chunk <- 
  wgths %>% 
  filter(age == "Infant",
         year == 2020)


compute_qs <- function(chunk){
  x <- chunk$psc
  w <- chunk$w
  
  t <- 
    weighted.quantile(x, w, 
                      probs=seq(0, 1, 0.25), 
                      na.rm = TRUE) %>% 
    as.double()
  
  chunk %>% 
    group_by() %>% 
    summarise(q0 = t[1],
              q25 = t[2],
              q50 = t[3],
              q75 = t[4],
              q100 = t[5],
              mean = weighted.mean(x, w, na.rm = FALSE))
}

tot_stats <- 
  wgths %>% 
  group_by(age, age_type, year) %>% 
  do(compute_qs(chunk = .data)) %>% 
  ungroup()

tot_stats1 <- tot_stats

w_av1 <- 
  tot_stats %>% 
  select(age, year, mean)

inc_stats <- 
  wgths %>% 
  group_by(age, age_type, income, year) %>% 
  do(compute_qs(chunk = .data)) %>% 
  ungroup()


# extreme p-score values
# ~~~~~~~~~~~~~~~~~~~~~~

dts2 %>% 
  group_by(year) %>% 
  summarise(min_psc = min(psc) - 1,
            max_psc = max(psc) - 1)


tot_stats %>% 
  group_by() %>% 
  summarise(min_psc = min(q25) - 1,
            max_psc = max(q75) - 1)

# ~~~~~~~~~~~~~~~~~~~~~~
# Plotting distributions
# ~~~~~~~~~~~~~~~~~~~~~~

cols <- c("Excess" = "#b7094c",
          "No-change" = "#5c4d7d",
          "Deficit" = "#0091ad")

lms <- c(0.5, 2)
bks <- c(0.1, 0.2, 0.5, 0.7, 1, 1.5, 2, 5, 10)
lbs <- paste0((bks - 1)*100, "%")

pl_dist <-
  dts2 %>% 
  ggplot(aes(x = psc, y = age)) +
  geom_violin(trim = FALSE, fill = "white", linewidth = 0.3)+
  geom_jitter(aes(col = as.factor(exc)), 
              size = 1.5, alpha = 0.5, 
              height = 0.18,
              width = 0)+
  geom_point(data = tot_stats,
             aes(q50, age),
             size = 3,
             col = "black", alpha = 0.6)+
  geom_segment(data = tot_stats,
               aes(x = q25, y = age, xend = q75, yend = age))+
  scale_x_log10(breaks = bks, labels = lbs)+
  scale_color_manual(values = cols, 
                     guide = guide_legend(reverse = TRUE, 
                                          override.aes = list(size = 2)))+
  scale_fill_manual(values = cols, 
                    guide = guide_legend(reverse = TRUE))+
  coord_cartesian(xlim = lms)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  facet_grid(age_type ~ year, scales = "free", space = "free_y")+
  labs(x = "P-score", y = "Age", col = "Change", fill = "Change")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 8, face = "bold"),
        strip.text = element_text(size = 11, face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(0.3, "lines"))

# ~~~~~~~~~~~~~~~~~~~~
# Plotting proportions
# ~~~~~~~~~~~~~~~~~~~~

db_summary <- 
  dts2 %>% 
  select(country, year, age, age_type, psc, lp, up, exc_all = exc) %>% 
  mutate(psc_out = ifelse(1 < up & 1 > lp, 1, psc)) %>% 
  drop_na(psc_out) %>% 
  unique() %>% 
  mutate(exc_out = case_when(psc_out > 1 ~ "Excess",
                             psc_out < 1 ~ "Deficit",
                             psc_out == 1 ~ "No-change")) %>% 
  select(year, age, age_type, country, exc_all, exc_out) %>% 
  group_by(year, age) %>% 
  mutate(N_cts = n()) %>% 
  ungroup() %>%  
  gather(exc_all, exc_out, key = type, value = is_excess) %>% 
  group_by(year, age, age_type, N_cts, type, is_excess) %>% 
  summarise(N = n()) %>% 
  ungroup() %>% 
  mutate(prop = N / N_cts,
         age = factor(age, levels = c("Stillbirths", "Neonatal", 
                                      "Infant", "1-4", "0-4", "5-9", 
                                      "10-14", "15-19", "20-24")),
         type = recode(type,
                       "exc_all" = "All estimates",
                       "exc_out" = "Uncertainty"),
         type = factor(type, levels = c("All estimates", "Uncertainty")),
         is_excess = factor(is_excess, 
                            levels = c("Excess", "No-change", "Deficit")))

bks2 <- c(0, .25, .5, .75, 1)
lbs2 <- paste0(bks2*100, "%")

pl_prop <-
  db_summary %>% 
  filter(type == "Uncertainty") %>% 
  ggplot(aes(x = age, y = prop))+
  geom_bar(aes(fill = is_excess),
           stat = "identity", 
           position = "fill",
           alpha = 0.8)+
  geom_text(aes(label = paste0(round(prop*100)), group = is_excess), 
            position = position_stack(vjust = 0.5, reverse = F), size = 2.7)+
  facet_grid(age_type ~ year, scales = "free", space = "free_y")+
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE))+
  scale_y_continuous(breaks = bks2, labels = lbs2)+
  labs(fill = "Change", 
       x = "Age", y = "Proportion (%)")+
  coord_flip(expand = 0)+
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10, face = "bold"),
        panel.spacing = unit(0.5, "lines"))

plot_grid(pl_dist, pl_prop, 
          labels = c("A", "B"),
          label_size = 14,
          rel_heights = c(1, 1.2),
          ncol = 1)

ggsave("figures/01_manuscript/fig03.png",
       dpi = 700,
       width = 8, height = 7)

# range of p-scores by age and year

dts2 %>% 
  group_by(age, year) %>% 
  summarise(min_psc = min(psc),
            max_psc = max(psc)) %>% 
  ungroup() %>% 
  mutate(min_psc = min_psc - 1,
         max_psc = max_psc - 1)


tot_stats

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# by income level
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


unique(dts$Income)
income_lvs <- c("High", "Upper-mid", "Lower-mid", "Low", "No income data")
income_lvs <- rev(c("Lower-mid", "Upper-mid", "High"))

# ~~~~~~~~~~~~~~~~~~~~~~
# Plotting distributions
# ~~~~~~~~~~~~~~~~~~~~~~

cols <- c("Excess" = "#b7094c",
          "No-change" = "#5c4d7d",
          "Deficit" = "#0091ad")

lms <- c(0.5, 2)
bks <- c(0.1, 0.2, 0.5, 0.7, 1, 1.5, 2, 5, 10)
lbs <- paste0((bks - 1)*100, "%")

pl_dist_inc <-
  dts2 %>% 
  mutate(income = factor(income, levels = income_lvs)) %>% 
  ggplot(aes(x = psc, y = age)) +
  geom_violin(trim = FALSE, fill = "white", size = 0.3)+
  geom_jitter(aes(col = as.factor(exc)), 
              size = 1.5, alpha = 0.5, 
              height = 0.18,
              width = 0)+
  geom_point(data = inc_stats,
             aes(mean, age),
             size = 3,
             col = "black", alpha = 0.6)+
  geom_segment(data = inc_stats,
               aes(x = q25, y = age, xend = q75, yend = age))+
  scale_x_log10(breaks = bks, labels = lbs)+
  scale_color_manual(values = cols, 
                     guide = guide_legend(reverse = TRUE, 
                                          override.aes = list(size = 2)))+
  scale_fill_manual(values = cols, 
                    guide = guide_legend(reverse = TRUE))+
  coord_cartesian(xlim = lms)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  facet_nested(income + age_type ~ year, space = "free_y", scale = "free",
               nest_line = element_line(linetype = 1))+
  labs(x = "P-score", y = "Age", col = "Change", fill = "Change")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.y = element_text(size = 8, face = "bold"),
        axis.text.x = element_text(size = 8, face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(0.3, "lines"))

# ~~~~~~~~~~~~~~~~~~~~
# Plotting proportions
# ~~~~~~~~~~~~~~~~~~~~

db_summary <- 
  dts2 %>% 
  select(income, country, year, age, age_type, psc, lp, up, exc_all = exc) %>% 
  mutate(psc_out = ifelse(1 < up & 1 > lp, 1, psc)) %>% 
  drop_na(psc_out) %>% 
  unique() %>% 
  mutate(exc_out = case_when(psc_out > 1 ~ "Excess",
                             psc_out < 1 ~ "Deficit",
                             psc_out == 1 ~ "No-change")) %>% 
  select(income, year, age, age_type, country, exc_all, exc_out) %>% 
  group_by(income, year, age) %>% 
  mutate(N_cts = n()) %>% 
  ungroup() %>%  
  gather(exc_all, exc_out, key = type, value = is_excess) %>% 
  group_by(income, year, age, age_type, N_cts, type, is_excess) %>% 
  summarise(N = n()) %>% 
  ungroup() %>% 
  mutate(prop = N / N_cts,
         age = factor(age, levels = c("Stillbirths", "Neonatal", 
                                      "Infant", "1-4", "5-9", 
                                      "10-14", "15-19", "20-24")),
         type = recode(type,
                       "exc_all" = "All estimates",
                       "exc_out" = "Uncertainty"),
         type = factor(type, levels = c("All estimates", "Uncertainty")),
         is_excess = factor(is_excess, 
                            levels = c("Excess", "No-change", "Deficit")))

bks2 <- c(0, .25, .5, .75, 1)
lbs2 <- paste0(bks2*100, "%")

pl_prop_inc <-
  db_summary %>% 
  mutate(income = factor(income, levels = income_lvs)) %>% 
  filter(type == "Uncertainty") %>% 
  ggplot(aes(x = age, y = prop))+
  geom_bar(aes(fill = is_excess),
           stat = "identity", 
           position = "fill",
           alpha = 0.8)+
  geom_text(aes(label = paste0(round(prop*100)), group = is_excess), 
            position = position_stack(vjust = 0.5, reverse = F), size = 2.4)+
  facet_nested(income + age_type ~ year, space = "free_y", scale = "free",
               nest_line = element_line(linetype = 1))+
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE))+
  scale_y_continuous(breaks = bks2, labels = lbs2)+
  labs(fill = "Change", 
       x = "Age", y = "Proportion (%)")+
  coord_flip(expand = 0)+
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(size = 8, face = "bold"),
        axis.text = element_text(size = 8, face = "bold"),
        panel.spacing = unit(0.3, "lines"))

pl_dist_inc
ggsave("figures/01_manuscript/figS19_pscore_distribution_proportions_by_income.png", 
       dpi = 1000,
       width = 7, height = 7)

pl_prop_inc
ggsave("figures/01_manuscript/figS20_pscore_distribution_proportions_by_income.png", 
       dpi = 1000,
       width = 7, height = 7)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

