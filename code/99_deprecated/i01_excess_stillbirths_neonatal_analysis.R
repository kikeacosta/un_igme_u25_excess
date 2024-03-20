rm (list = ls())
source("Code/00_functions.R")

# loading baselines estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_in <- 
  read_rds("data_inter/sbs_neo_baselines.rds")
  

# ~~~~~~~~~~~~~~~~~~~~~~~~~
# baseline fitting examples
# ~~~~~~~~~~~~~~~~~~~~~~~~~
bsn <- 
  db_in %>% 
  mutate(measure = case_when(measure %in% c("neo_r", "neo") ~ "Neonatal",
                             measure %in% c("sbs_r", "sbs") ~ "Stillbirths"),
         measure = factor(measure, levels = c("Stillbirths", "Neonatal")), 
         rate = 1e5 * value / exposure,
         bsn_r = 1e5 * bsn / exposure,
         bsn_r_lp = 1e5 * bsn_lp / exposure,
         bsn_r_up = 1e5 * bsn_up / exposure,
         # is_2020 = ifelse(year == 2020, "y", "n"),
         exc = case_when(year >= 2020 & up > 1 & lp > 1 ~ "Positive",
                         year >= 2020 & up < 1 & lp < 1 ~ "Negative",
                         year >= 2020 & up > 1 & lp < 1 ~ "No-excess",
                         TRUE ~ "oth"))


cts <- c("South Africa", "Uzbekistan", "Switzerland")

cols <- c("Positive" = "#b7094c",
          "Negative" = "#0091ad",
          "No-excess" = "#5c4d7d",
          "oth" = "black")

sizes <- c("Positive" = 1,
           "Negative" = 1,
           "No-excess" = 1,
           "oth" = 1)
tx <- 8
bsn %>% 
  filter(country %in% cts) %>% 
  ggplot()+
  geom_point(aes(year, rate, col = exc))+
  geom_ribbon(aes(year, ymin = bsn_r_lp, ymax = bsn_r_up), 
              fill = "grey70", alpha = 0.4)+
  geom_line(aes(year, bsn_r))+
  geom_vline(xintercept = 2019.5, linetype = "dashed")+
  scale_color_manual(values = cols, 
                     breaks = c("Negative", "No-excess", "Positive"))+
  scale_x_continuous(limits = c(2015, 2021), breaks = 2015:2021)+
  expand_limits(y = 0)+
  facet_nested_wrap(country ~ measure, scales = "free", ncol = 2)+
  labs(col = "Excess")+
  theme_bw()+
  theme(legend.position = "bottom",
        strip.text.x = element_text(margin = margin(b = 0.1, t = 0.1),
                                    size = tx + 2),
        axis.text.x = element_text(size = tx - 2, angle = 60, hjust = 1),
        axis.text.y = element_text(size = tx),
        strip.background = element_rect(fill = "transparent")) 

ggsave("Figures/last version/manuscript/figS4_sbs_neo_fitting_examples.png",
       dpi = 700,
       w = 4, 
       h = 4)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~
# p-scores
# ~~~~~~~~
sbs_neo <- 
  bsn %>% 
  filter(year >= 2020) %>% 
  mutate(bts = ifelse(country == "South Africa", 899303, bts),
         exposure = ifelse(country == "South Africa", 899303, exposure))

cols <- c("Positive" = "#b7094c",
          "Negative" = "#0091ad",
          "No-excess" = "#5c4d7d")

unique(sbs_neo$country)
unique(sbs_neo$measure)
cts_exc <- c("Nauru", "Andorra")
# cts_exc <- c()

bks <- c(0.5, 0.7, 1, 1.2, 2)
lbs <- paste0((bks - 1)*100, "%")

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

sbs_neo2 <- 
  sbs_neo %>% 
  left_join(income_levels, by = "code") %>% 
  mutate(income = factor(income, levels = income_lvs))

sbs_neo2 %>% 
  # filter(measure %in% c("sbs")) %>% 
  ggplot(aes(psc, reorder(country, psc)))+
  geom_point(aes(psc, reorder(country, psc), 
                 size = bts, 
                 col = exc,
                 shape = as.character(year)),
             alpha = 0.8)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10(breaks = bks,
                labels = lbs,
                limits = c(0.5, 2))+
  facet_nested(income ~ measure, scales = "free", space = "free_y")+
  labs(shape = "Year",
       col = "Excess",
       size = "Births",
       x = "p-score")+
  # scale_alpha_continuous(range = c(0.3, 0.8))+
  scale_color_manual(values = cols)+
  theme_bw()+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        strip.background = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 9))

ggsave(paste0("Figures/last version/manuscript/figS5_sbs_neo_pscores.png"), 
       dpi = 700, width = 8, height = 6)

