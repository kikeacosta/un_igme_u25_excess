rm (list = ls())
source("Code/00_functions.R")

# loading baselines estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

db_in <- read_rds("data_inter/sbs_neo_baselines.rds")
test_in <- read_rds("data_inter/sens_analysis_sbs_neo_baselines.rds")

test_sbs_neo <- 
  test_in %>% 
  filter(year == 2019) %>% 
  mutate(psc = value / bsn,
         psc_lp = value / up,
         psc_up = value / lp) %>%
  mutate(excess = case_when(psc_up < 1 ~ "Negative",
                            psc_lp > 1 ~ "Positive",
                            TRUE ~ "No-excess"),
         bts = ifelse(country == "South Africa", 899303, bts),
         exposure = ifelse(country == "South Africa", 899303, exposure),
         measure = recode(measure,
                          "neo_r" = "neo",
                          "sbs_r" = "sbs")) %>% 
  drop_na()

sbs_neo <- 
  db_in %>% 
  filter(year >= 2020) %>% 
  mutate(psc = value / bsn,
         psc_lp = value / up,
         psc_up = value / lp) %>%
  mutate(excess = case_when(psc_up < 1 ~ "Negative",
                            psc_lp > 1 ~ "Positive",
                            TRUE ~ "No-excess"),
         bts = ifelse(country == "South Africa", 899303, bts),
         exposure = ifelse(country == "South Africa", 899303, exposure),
         measure = recode(measure,
                          "neo_r" = "neo",
                          "sbs_r" = "sbs")) %>% 
  drop_na()

sbs_neo2 <- 
  bind_rows(test_sbs_neo,
            sbs_neo) %>% 
  mutate(measure = factor(measure, levels = c("sbs", "neo", "sbs_neo")))

# weighted average p-score
w_av <- 
  sbs_neo2 %>% 
  group_by(year, measure) %>% 
  mutate(c = exposure / sum(exposure)) %>% 
  summarise(psc_w = sum(psc * c),
            psc_lp = sum(psc_lp * c),
            psc_up = sum(psc_up * c)) %>% 
  ungroup() %>% 
  arrange(measure, year)

w_av

w_av %>% 
  ggplot()+
  geom_point(aes(psc_w, measure))+
  geom_errorbar(aes(xmin = psc_lp, xmax = psc_up, y = measure))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10()+
  theme_bw()+
  facet_grid(~year)

cols <- c("Positive" = "#b7094c",
          "Negative" = "#0091ad",
          "No-excess" = "#5c4d7d")

unique(sbs_neo$country)
unique(sbs_neo$measure)
cts_exc <- c("Nauru", "Andorra")
# cts_exc <- c()



# loading country contextual variables from WPP documentation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# https://population.un.org/wpp/Download/Metadata/Documentation/
income_levels <- read_xlsx("Data/world_bank/CLASS.xlsx") %>% 
  drop_na(Region) %>% 
  select(code = 2,
         Region = 3,
         income = 4) %>% 
  mutate(income = case_when(income == "High income" ~ "High",
                            income == "Low income" ~ "Low",
                            income == "Upper middle income" ~ "Upper-mid",
                            income == "Lower middle income" ~ "Lower-mid",
                            TRUE ~ income))

unique(income_levels$income)

# merging locations with data
income_lvs <- c("High", "Upper-mid", "Lower-mid", "Low")

sbs_neo3 <- 
  sbs_neo2 %>% 
  left_join(income_levels, by = "code") %>% 
  mutate(income = factor(income, levels = income_lvs))


# stillbirths
# ~~~~~~~~~~~
sbs_neo3 %>% 
  filter(measure %in% c("sbs", "sbs_r")) %>% 
  ggplot(aes(psc, reorder(country, psc)))+
  geom_point(aes(psc, reorder(country, psc), 
                 size = bts, 
                 col = excess),
             alpha = 0.8)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10(breaks = c(0.6, 0.8, 1, 1.2, 1.5, 2, 2.5))+
  facet_nested(income ~ year, scales = "free", space = "free")+
  labs(shape = "Year",
       col = "Type",
       size = "Births",
       x = "p-score")+
  # scale_alpha_continuous(range = c(0.3, 0.8))+
  scale_color_manual(values = cols)+
  theme_bw()+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 9))

ggsave(paste0("Figures/last version/sens_analysis/excess_sbs.png"), 
       dpi = 600, width = 8, height = 6)

sbs_neo3 %>% 
  filter(measure %in% c("sbs", "sbs_r")) %>% 
  pull(country) %>% 
  unique() %>% 
  length()


# neonatal
# ~~~~~~~~
sbs_neo3 %>% 
  filter(measure %in% c("neo", "neo_r")) %>% 
  filter(!country %in% cts_exc) %>% 
  filter(psc >= 0.5 & psc <= 1.5) %>% 
  ggplot(aes(psc, reorder(country, psc)))+
  geom_point(aes(psc, reorder(country, psc), 
                 size = bts, 
                 col = excess),
             alpha = 0.8)+
  # geom_pointrange(aes(xmin=ps_ll, xmax=ps_ul), alpha = 0.4)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10(breaks = c(0.6, 0.8, 1, 1.2, 1.5, 2, 2.5))+
  facet_nested(income ~ year, scales = "free", space = "free")+
  labs(shape = "Year",
       col = "Type",
       size = "Births",
       x = "p-score")+
  # scale_alpha_continuous(range = c(0.3, 0.8))+
  scale_color_manual(values = cols)+
  theme_bw()+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 9))

ggsave(paste0("Figures/last version/sens_analysis/excess_neo.png"), 
       dpi = 600, width = 8, height = 6)

sbs_neo3 %>% 
  filter(measure %in% c("neo", "neo_r")) %>% 
  pull(country) %>% 
  unique() %>% 
  length()



# stillbirths and neonatal combined
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sbs_neo3 %>% 
  filter(measure %in% c("sbs_neo")) %>% 
  filter(!country %in% cts_exc) %>% 
  filter(psc >= 0.5 & psc <= 1.5) %>% 
  ggplot(aes(psc, reorder(country, psc)))+
  geom_point(aes(psc, reorder(country, psc), 
                 size = bts, 
                 col = excess),
             alpha = 0.8)+
  # geom_pointrange(aes(xmin=ps_ll, xmax=ps_ul), alpha = 0.4)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10(breaks = c(0.6, 0.8, 1, 1.2, 1.5, 2, 2.5))+
  facet_nested(income ~ year, scales = "free", space = "free")+
  labs(shape = "Year",
       col = "Type",
       size = "Births",
       x = "p-score")+
  # scale_alpha_continuous(range = c(0.3, 0.8))+
  scale_color_manual(values = cols)+
  theme_bw()+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 9))

ggsave(paste0("Figures/last version/sens_analysis/excess_sbs_neo.png"), 
       dpi = 600, width = 8, height = 6)

sbs_neo3 %>% 
  filter(measure %in% c("sbs_neo")) %>% 
  pull(country) %>% 
  unique() %>% 
  length()




# stillbirths neonatal correlation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sbs_neo_corr <- 
  sbs_neo3 %>% 
  # filter(measure %in% c("neo", "sbs")) %>% 
  select(code, country, year, measure, psc) %>%
  mutate(measure = recode(measure,
                          "sbs_r" = 'sbs',
                          'neo_r' = 'neo')) %>% 
  spread(measure, psc) %>% 
  drop_na(neo, sbs)

# Quadrant count ratio
# ~~~~~~~~~~~~~~~~~~~~
qcr <- 
  sbs_neo_corr %>% 
  # filter(country == "India") %>% 
  mutate(q = case_when(sbs > 1 & neo > 1 ~ "I",
                       sbs < 1 & neo > 1 ~ "II",
                       sbs < 1 & neo < 1 ~ "III",
                       sbs > 1 & neo < 1 ~ "IV")) %>% 
  group_by(q) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  spread(q, n) %>% 
  replace_na(list(I=0, II=0, III=0, IV=0)) %>%
  mutate(qcr = (I + III - II - IV) / (I + III + II + IV))

qcr


library("ggpubr")
library("ggpmisc")

ggscatter(sbs_neo_corr, x = "sbs", y = "neo", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Stillbirths p-scores", ylab = "Neonatal p-scores")+
  scale_y_log10(breaks = seq(0.6, 2, 0.2))+
  scale_x_log10(breaks = seq(0.6, 2, 0.2))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  theme(axis.text = element_text(size = 8))
# ggsave("Figures/last version/stillbirths_neonatal_correlation.png", 
#        dpi = 600, width = 5, height = 3)


# Quadrant count ratio
ggplot(sbs_neo_corr, aes(sbs, neo)) +
  geom_quadrant_lines(colour = "blue", xintercept = 1, yintercept = 1) +
  stat_quadrant_counts(colour = "blue", xintercept = 1, yintercept = 1) +
  geom_point() +
  scale_y_continuous(expand = expansion(mult = 0.15, add = 0))


sbs_neo_corr %>% 
  ggplot(aes(sbs, neo))+ 
  geom_point()+
  stat_cor(method = "pearson")+
  geom_text(data = qcr, aes(Inf,Inf), vjust = 3.5, hjust = 2, 
            label = paste0("QCR = ", round(qcr$qcr, 2)))+
  scale_y_log10(breaks = seq(0.6, 2, 0.2))+
  scale_x_log10(breaks = seq(0.6, 2, 0.2))+
  geom_vline(xintercept = 1, linetype = "dashed")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  geom_smooth(method = "lm")+
  theme_bw()



# Quadrant count ratio
ggplot(sbs_neo_corr, aes(sbs, neo)) +
  geom_quadrant_lines(colour = "blue", xintercept = 1, yintercept = 1) +
  stat_quadrant_counts(colour = "blue", xintercept = 1, yintercept = 1) +
  geom_point() +
  scale_y_continuous(expand = expansion(mult = 0.15, add = 0))

(16+12-9-5)/42


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plotting proportions according to excess
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sbs_neo4 <- 
  sbs_neo3 %>% 
  filter(measure %in% c("sbs_neo", "neo", "neo_r", "sbs", "sbs_r")) %>% 
  mutate(measure = recode(measure,
                          "neo_r" = "neo",
                          "sbs_r" = "sbs"),
         measure = factor(measure, levels = c("sbs", "neo", "sbs_neo"))) %>% 
  filter(year >= 2019) %>%
  select(country, measure, year, psc, psc_lp, psc_up) %>% 
  mutate(psc_out = ifelse(1 < psc_up & 1 > psc_lp, 1, psc)) %>% 
  drop_na(psc_out) %>% 
  unique()

sbs_neo_only_sig <- 
  sbs_neo4 %>% 
  select(-psc) %>% 
  rename(psc = psc_out)

unique(sbs_neo4$country)

sbs_neo_summary <- 
  sbs_neo4 %>% 
  mutate(exc_all = case_when(psc > 1 ~ "Positive",
                             psc < 1 ~ "Negative",
                             psc == 1 ~ "No-excess"),
         exc_out = case_when(psc_out > 1 ~ "Positive",
                             psc_out < 1 ~ "Negative",
                             psc_out == 1 ~ "No-excess")) %>% 
  select(year, country, measure, exc_all, exc_out) %>% 
  group_by(year, measure) %>% 
  mutate(N_cts = n()) %>% 
  ungroup() %>%  
  gather(exc_all, exc_out, key = type, value = is_excess) %>% 
  group_by(year, measure, N_cts, type, is_excess) %>% 
  summarise(N = n()) %>% 
  ungroup() %>% 
  mutate(Prop = N / N_cts,
         type = recode(type,
                       "exc_all" = "All estimates",
                       "exc_out" = "Uncertainty"),
         type = factor(type, levels = c("All estimates", "Uncertainty")),
         is_excess = factor(is_excess, 
                            levels = c("Positive", "No-excess", "Negative")))

cols <- 
  c("Positive" = "#b7094c",
    "No-excess" = "#5c4d7d",
    "Negative" = "#0091ad")


# stillbirths and neonatal summary 
sbs_neo_summary %>% 
  filter(measure %in% c("sbs", "neo", "sbs_neo"),
         type == "Uncertainty") %>% 
  mutate(measure = recode(measure,
                          "sbs" = "Stillbirths",
                          "neo" = "Neonatal",
                          "sbs_neo" = "Still + Neo"),
         year = factor(year, levels = c("2019", "2020", "2021"))) %>% 
  ggplot(aes(x = year, y = Prop))+
  geom_bar(aes(fill = is_excess),
           stat = "identity", 
           position = "fill",
           alpha = 0.8)+
  geom_text(aes(label = paste0(round(Prop*100)), group = is_excess), 
            position = position_stack(vjust = 0.5, reverse = F), size = 3)+
  facet_nested(measure ~., scales = "free", space = "free_y",
               nest_line = element_line(linetype = 1))+
  scale_fill_manual(values = cols, 
                    guide = guide_legend(reverse = TRUE, 
                                         direction = "vertical"))+
  scale_x_discrete(position = 'top')+
  labs(fill = "Excess",
       y = 'Proportion')+
  # coord_flip(expand = 0)+
  coord_cartesian(expand = 0)+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 9, face = "bold"),
        legend.position = "right",
        axis.text = element_text(size = 9, face = "bold"),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 10),
        panel.spacing = unit(0.5, "lines"))

ggsave("Figures/last version/sens_analysis/sbs_neo_excess_summary_2020_2021_v2.png", 
       dpi = 600,
       width = 4, height = 5)







# stillbirths and neonatal together
unique(sbs_neo_summary$measure)
sbs_neo_summary %>% 
  filter(measure %in% c("sbs", "neo")) %>%
  mutate(measure = recode(measure,
                          "sbs" = "Stillbirths",
                          "neo" = "Neonatal",
                          "sbs_neo" = "Stillbirths + Neonatal"),
         measure = factor(measure, levels = c("Stillbirths", "Neonatal", "Stillbirths + Neonatal")),
         year = factor(year, levels = c("2019", "2020", "2021"))) %>% 
  ggplot(aes(x = measure, y = Prop))+
  geom_bar(aes(fill = is_excess),
           stat = "identity", 
           position = "fill",
           alpha = 0.8)+
  geom_text(aes(label = paste0(round(Prop*100)), group = is_excess), 
            position = position_stack(vjust = 0.5, reverse = F), size = 4)+
  facet_nested(type ~ year, scales = "free", space = "free_y",
               nest_line = element_line(linetype = 1))+
  scale_fill_manual(values = cols, 
                    guide = guide_legend(reverse = TRUE, 
                                         direction = "vertical"))+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), 
                     labels = c("0", "25", "50", "75", "100"))+
  coord_flip(expand = 0)+
  labs(fill = "Excess",
       y = 'Proportion')+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        legend.position = "right",
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 7),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 10),
        panel.spacing = unit(0.5, "lines"))

ggsave("Figures/last version/sens_analysis/sbs_neo_excess_summary_2020_2021_v2.png", 
       dpi = 600,
       width = 12, height = 3)
