rm (list = ls())
source("Code/00_functions.R")

# ~~~~~~~~~~~~~~~~~~~~
# Sensitivity analysis
# ~~~~~~~~~~~~~~~~~~~~
# Running analyses, using different training periods (2017:2019)

cts_exc <- c("Uzbekistan", "Armenia")

pop <- 
  read_rds("data_inter/wpp_populations_ages_0_24_v2022.rds")

# filtering countries with at least 500K population under 25
cds_500kplus <- 
  pop %>% 
  filter(sex == "t",
         year == 2020, age <= 24) %>% 
  group_by(country, code) %>% 
  filter(sum(pop) > 5e5) %>% 
  pull(code) %>% 
  unique()

dt <- 
  read_rds("data_inter/annual_deaths_rates_2010_2022.rds")


# testing consecutive fitting to predict 2017, 2018, and 2019
# using 10 (2007-2016) to 5 years (2011-2016) for the baseline estimation
dt2 <- 
  dt %>% 
  filter(type_data == "counts",
         !Country %in% cts_exc,
         Code %in% cds_500kplus) %>% 
  mutate(Deaths = round(Deaths, 0),
         Exposure = round(Exposure, 0)) %>% 
  # rename_with(tolower) %>% 
  drop_na(Deaths, Exposure) %>% 
  group_by(Country, Sex, Age) %>% 
  filter(min(Year) <= 2010) %>% 
  ungroup()

dts_out <- tibble()
for(i in 2010:2015){
  
  temp1 <- 
    dt2 %>% 
    filter(Year >= i)
  
  temp2 <- 
    temp1 %>% 
    group_by(Country, Sex, Age) %>% 
    arrange(Year) %>%
    mutate(w = ifelse(Year >= 2020, 0, 1),
           t = 1:n()) %>% 
    do(est_baseline_pi(chunk = .data)) %>% 
    ungroup() %>% 
    mutate(period = paste0(i, "-2019"))
  
  dts_out <- 
    dts_out %>% 
    bind_rows(temp2) 
  
}
write_rds(dts_out, "data_inter/baselines_5_to_10_years_periods.rds")
dts_out <- read_rds("data_inter/baselines_5_to_10_years_periods.rds")

bsn <- 
  dts_out %>% 
  rename_with(tolower) %>% 
  rename(dts = deaths)
  
unique(bsn$country)
unique(bsn$period)
unique(bsn$age)

cts_exc <- c("Peru")
cts_exc <- c("Brazil", "Honduras")

bsn2 <- 
  bsn %>% 
  filter(
         age %in% c("5-9", "10-14", "15-19", "20-24", "1-4", "Infant"),
         sex == "t") %>% 
  mutate(bsn_lp = ifelse(bsn_lp < 0, 0, bsn_lp),
         bsn_up = ifelse(bsn_up < 0, 0, bsn_up),
         mx_dts = 1e5*dts/exposure,
         mx_bsn = 1e5*bsn/exposure,
         mx_bsn_lp = 1e5*bsn_lp/exposure,
         mx_bsn_up = 1e5*bsn_up/exposure,
         psc = dts / bsn,
         out = ifelse(dts < bsn_lp | dts > bsn_up, 1, 0),
         exc = case_when(out == 1 & psc > 1 ~ "Positive",
                         out == 1 & psc < 1  ~ "Negative",
                         out == 0 ~ "No-excess",
                         TRUE ~ "oth"), 
         exc = factor(exc, 
                      levels = c("Positive", "No-excess", "Negative")),
         age = factor(age, 
                      levels = c("Infant", "1-4", "5-9", "10-14", "15-19", "20-24"))) %>% 
  filter(!country %in% cts_exc) %>% 
  group_by(country, age) %>% 
  filter(max(year) == 2022) %>% 
  ungroup()



cts <- unique(bsn2$country)
cts

plot_example <- function(ct = "Sweden"){
  
  cols <- c("#7400b8", "#6930c3", "#5e60ce", "#5390d9", "#4ea8de", "#48bfe3")
  
  p1 <- 
    bsn2 %>% 
    filter(country == ct) %>% 
    mutate(age = fct_rev(age)) %>% 
    ggplot()+
    geom_ribbon(aes(year, ymin = mx_bsn_lp, ymax = mx_bsn_up, fill = period), 
                alpha = 0.15)+
    geom_line(aes(year, mx_bsn, col = period), linewidth = 0.8)+
    geom_point(aes(year, mx_dts))+
    geom_vline(xintercept = 2019.5, linetype = "dashed")+
    scale_x_continuous(breaks = 2010:2022)+
    facet_grid(age~., scales = "free")+
    scale_color_manual(values = cols)+
    scale_fill_manual(values = cols)+
    theme_bw()+
    labs(y = "death rates (/100K)")+
    theme(legend.position = "none")+
    labs(title = paste0("baselines fitting for ", ct))
  
  p2 <- 
    bsn2 %>% 
    filter(country == ct,
           year %in% 2020:2022) %>% 
    ggplot()+
    geom_point(aes(psc, age, col = period, alpha = out))+
    facet_grid(~year, scales = "free")+
    geom_vline(xintercept = 1, linetype = "dashed")+
    scale_y_discrete(position = "right")+
    scale_alpha_continuous(range = c(0.3, 1), guide = "none")+
    scale_x_log10()+
    scale_color_manual(values = cols)+
    theme_bw()+
    labs(title = paste0("p-scores for ", ct))+
    theme(legend.position = "left")
  
  plot_grid(p1, p2, ncol = 1, rel_heights = c(1, 0.5))
  
  ggsave(paste0("figures/reports_2023/period_fitting_estimates_", ct, ".png"),
         w = 10,
         h = 8)
}
plot_example("USA")
plot_example("Colombia")
plot_example("Turkey")
plot_example("Sweden")
plot_example("Romania")
plot_example("Guatemala")
plot_example("Romania")

# ~~~~~~~~~~~~~~~~~
# Weighted averages
# ~~~~~~~~~~~~~~~~~
# weighted average P-scores in all populations
wgths <- 
  bsn2 %>% 
  filter(year >= 2020) %>% 
  group_by(year, age) %>% 
  mutate(w = exposure / mean(exposure)) %>% 
  ungroup()

# install.packages("drf")
# library(drf)
chunk <- 
  wgths %>% 
  filter(age == "0-4",
         year == 2020,
         period == "2015-2019")


compute_qs <- function(chunk){
  
  x <- chunk$psc %>% as.double()
  w <- chunk$w
  ag <- unique(chunk$age)
  yr <- unique(chunk$year)
  pr <- unique(chunk$period)
  
  t <- 
    weighted.quantile(x, w, probs=seq(0,1,0.25), na.rm = TRUE) %>% 
    as.double()
  
  tibble(year = yr, age = ag, period = pr,
         q0 = t[1],q25 = t[2],
         q50 = t[3],
         q75 = t[4],
         q100 = t[5],
         mean = weighted.mean(x, w, na.rm = FALSE))
}

tot_stats <- 
  wgths %>% 
  group_by(year, age, period) %>% 
  do(compute_qs(chunk = .data)) %>% 
  ungroup()

bsn3 <- 
  bsn2 %>% 
  left_join(tot_stats)

# ~~~~~~~~~~~~~~~~~~~~~~
# Plotting distributions
# ~~~~~~~~~~~~~~~~~~~~~~
cols <- c("Positive" = "#b7094c",
          "No-excess" = "#5c4d7d",
          "Negative" = "#0091ad")

lms <- c(0.5, 2)
bks <- c(0.1, 0.2, 0.5, 0.7, 1, 1.5, 2, 5, 10)
lbs <- paste0((bks - 1)*100, "%")

# pl_dist <-
bsn3 %>%
  filter(year %in% 2020:2022) %>% 
  ggplot(aes(x = psc, y = period)) +
  geom_violin(trim = FALSE, fill = "white", linewidth = 0.3)+
  geom_jitter(aes(col = as.factor(exc)), 
              size = 1, alpha = 0.5, 
              height = 0.18,
              width = 0)+
  geom_point(aes(q50, period), size = 1.5, col = "black", alpha = 0.6)+
  geom_segment(aes(x = q25, y = period, xend = q75, yend = period, 
                   group = period))+
  scale_x_log10(breaks = bks, labels = lbs)+
  scale_color_manual(values = cols, 
                     guide = guide_legend(reverse = TRUE, 
                                          override.aes = list(size = 2)))+
  scale_fill_manual(values = cols, 
                    guide = guide_legend(reverse = TRUE))+
  coord_cartesian(xlim = lms)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  facet_grid(fct_rev(age) ~ year, scales = "free", space = "free_y")+
  labs(x = "P-score", y = "Training periods", col = "Excess", fill = "Excess")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.y = element_text(size = 8, face = "bold"),
        axis.text.x = element_text(size = 8, face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(0.3, "lines"))

ggsave("figures/reports_2023/distributions_periods_by_age.png",
       w = 10,
       h = 5)  


# ~~~~~~~~~~~~~~~~~~~~
# Plotting proportions
# ~~~~~~~~~~~~~~~~~~~~
db_summary <- 
  bsn3 %>% 
  filter(year %in% 2020:2022) %>% 
  select(country, period, year, age, psc, exc) %>% 
  group_by(year, age, period) %>% 
  mutate(N_cts = n()) %>% 
  ungroup() %>%  
  group_by(period, year, age, N_cts, exc) %>% 
  summarise(N = n()) %>% 
  ungroup() %>% 
  mutate(prop = N / N_cts,
         age = factor(age, levels = c("Infant", "1-4", "5-9", 
                                      "10-14", "15-19", "20-24")),
         exc = factor(exc,
                      levels = c("Positive", "No-excess", "Negative")))

bks2 <- c(0, .25, .5, .75, 1)
lbs2 <- paste0(bks2*100, "%")

db_summary %>% 
  ggplot(aes(x = period, y = prop))+
  geom_bar(aes(fill = exc),
           stat = "identity", 
           position = "fill",
           alpha = 0.8)+
  geom_text(aes(label = paste0(round(prop*100)), group = exc), 
            position = position_stack(vjust = 0.5, reverse = F), size = 2)+
  facet_grid(fct_rev(age) ~ year, scales = "free", space = "free_y")+
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE))+
  scale_y_continuous(breaks = bks2, labels = lbs2)+
  labs(fill = "Excess", 
       x = "Period for baseline", y = "Proportion (%)")+
  coord_flip(expand = 0)+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        strip.background = element_blank(),
        # strip.text.x = element_blank(),
        # strip.text.x = element_text(size = 8, face = "bold"),
        strip.text = element_text(size = 8, face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 7, face = "bold"),
        panel.spacing = unit(0.5, "lines"))

ggsave("figures/reports_2023/proportions_periods_by_age.png",
       w = 10,
       h = 5)  

# average p-scores proportions across baselines
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_summary2 <- 
  bsn3 %>% 
  filter(year %in% 2020:2022) %>% 
  select(country, period, year, age, psc, exc) %>% 
  group_by(year, age) %>% 
  mutate(N_cts = n()) %>% 
  ungroup() %>%  
  group_by(year, age, N_cts, exc) %>% 
  summarise(N = n()) %>% 
  ungroup() %>% 
  mutate(prop = N / N_cts,
         age = factor(age, levels = c("Infant", "1-4", "5-9", 
                                      "10-14", "15-19", "20-24")),
         exc = factor(exc,
                      levels = c("Positive", "No-excess", "Negative")))

bks2 <- c(0, .25, .5, .75, 1)
lbs2 <- paste0(bks2*100, "%")

db_summary2 %>% 
  ggplot(aes(x = age, y = prop))+
  geom_bar(aes(fill = exc),
           stat = "identity", 
           position = "fill",
           alpha = 0.8)+
  geom_text(aes(label = paste0(round(prop*100)), group = exc), 
            position = position_stack(vjust = 0.5, reverse = F), size = 2.7)+
  facet_grid( ~ year, scales = "free", space = "free_y")+
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE))+
  scale_y_continuous(breaks = bks2, labels = lbs2)+
  labs(fill = "Excess", 
       x = "Age", y = "Proportion (%)")+
  coord_flip(expand = 0)+
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        # strip.text.x = element_blank(),
        strip.text.x = element_text(size = 10, face = "bold"),
        strip.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10, face = "bold"),
        panel.spacing = unit(0.5, "lines"))

ggsave("figures/reports_2023/proportions_periods_average_by_age.png",
       w = 10,
       h = 5)  

tot_avg <- 
  tot_stats %>% 
  group_by(age, year) %>% 
  summarise(mean = mean(mean)) %>% 
  ungroup()

cols <- c("#7400b8", "#6930c3", "#5e60ce", "#5390d9", "#4ea8de", "#48bfe3")

tot_stats %>% 
  ggplot()+
  geom_point(aes(mean, age, col = period), size = 1.5, alpha = 0.6)+
  geom_point(data = tot_avg, 
             aes(mean, age), size = 2, alpha = 0.8)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  facet_grid(~year)+
  scale_x_log10(limits = c(0.9, 1.35))+
  scale_color_manual(values = cols)+
  labs(title = "Population-weighted median p-scores",
       y = "Age",
       x = "P-score")+
  theme_bw()+
  theme(strip.background = element_blank(),
        # strip.text.x = element_blank(),
        strip.text.x = element_text(size = 10, face = "bold"),
        strip.text.y = element_text(size = 10, face = "bold"),
        # axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10, face = "bold"),
        panel.spacing = unit(0.5, "lines"))
ggsave("figures/reports_2023/average_pscores_all_countries_by_age.png",
       w = 8,
       h = 4)  

# changes in p-scores by periods
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cols <- c("#7400b8", "#6930c3", "#5e60ce", "#5390d9", "#4ea8de", "#48bfe3")


tot_stats %>% 
  ggplot()+
  geom_point(aes(year, mean, col = period))+
  geom_line(aes(year, mean, col = period, group = period))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  facet_grid(~age)+
  scale_y_log10(breaks = c(0.9, 1, 1.1, 1.2))+
  scale_x_continuous(breaks = 2020:2022)+
  coord_cartesian(ylim = c(0.9, 1.3), xlim = c(2019.8, 2022.2))+
  scale_color_manual(values = cols)+
  labs(title = "Population-weighted median p-scores",
       y = "P-score")+
  theme_bw()+
  theme(strip.background = element_blank(),
        # strip.text.x = element_blank(),
        strip.text.x = element_text(size = 10, face = "bold"),
        strip.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        panel.spacing = unit(0.5, "lines"))
ggsave("figures/reports_2023/averages_pscores_2020_2022.png",
       w = 8,
       h = 4)  

