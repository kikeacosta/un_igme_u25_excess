rm (list = ls())
source("code/00_functions.R")

# loading excess estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~
db_dts_fit <- 
  read_rds("data_output/sens_analysis_p_scores_excess_deaths.rds") %>% 
  select(Country, Code, Year, Sex, Age, Deaths, Population, psc, up, lp)

db_rts_fit <- 
  read_rds("data_output/sens_analysis_p_scores_excess_rates.rds") %>% 
  select(Country, Code, Year, Sex, Age, Rate, Population, psc, up, lp)

dts <- 
  bind_rows(db_dts_fit, db_rts_fit) %>% 
  # filter(!Country %in% excl) %>% 
  arrange(Country, Code, Age, Sex, Year) %>% 
  mutate(Age = factor(Age, 
                      levels = c("Infant", "1-4", "0-4", "5-9", 
                                 "10-14", "15-19", "20-24"))) %>% 
  # excluding groups with fitting issues
  filter(psc != 0 & !is.na(psc) & !is.na(lp) & !is.na(up) &
           lp != Inf & up != Inf)

# # countries with data on births
# birth_counts_cts <- 
#   read_rds("data_inter/annual_births.rds") %>% 
#   pull(Country) %>% 
#   unique() %>% 
#   sort()

# birth_cts <- c(birth_counts_cts, unique(db_rts_fit$Country)) %>% sort

# loading country contextual variables from WPP documentation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# https://population.un.org/wpp/Download/Metadata/Documentation/
locs <- read_xlsx(here("data_input", "WPP2022_F01_LOCATIONS.xlsx"),
                  skip = 16) %>% 
  select(Country = 2, 
         Code_join = 5,
         high = 27,
         mid = 28,
         upp_mid = 29,
         low_mid = 30,
         low = 31) %>% 
  filter(!is.na(Code_join)) %>% 
  mutate(Income = case_when(!is.na(high) ~ "High",
                            !is.na(upp_mid) ~ "Upper-mid",
                            !is.na(low_mid) ~ "Lower-mid",
                            !is.na(low) ~ "Low")) %>% 
  select(-high, -mid, -upp_mid, -low_mid, -low, -Country)

# merging locations with data
income_lvs <- c("High", "Upper-mid", "Lower-mid", "Low")
uk_cts <- c("England and Wales", "Northern Ireland", "Scotland")

dts2 <- 
  dts %>% 
  mutate(Code_join = ifelse(Country %in% uk_cts,
                            "GBR",
                            Code)) %>%
  left_join(locs, by = "Code_join") %>% 
  mutate(Income = factor(Income, levels = income_lvs))

# test all merged
dts2 %>% 
  filter(is.na(Income)) %>% 
  pull(Country) %>% unique()



# Plotting all ages for countries with infant mortality by sex
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
yref <- 2018

dts_inf <- 
  dts2 %>% 
  filter(Year %in% yref,
         Age %in% c("Infant", "1-4"),
         Sex == "t") %>% 
  mutate(exc = case_when(up > 1 & lp > 1 ~ "Positive",
                         up < 1 & lp < 1 ~ "Negative",
                         TRUE ~ "No-excess"),
         out = ifelse(exc == "No-excess", 0.5, 1),
         ins = ifelse(exc == "No-excess", 0.6, 0.4),
         Year = Year %>% as.character(),
         Country2 = Country)

unique(dts_inf$Country) %>% 
  length()

lvs_cts <- 
  dts_inf %>% 
  filter(Age == "Infant") %>% 
  arrange(-psc) %>% 
  pull(Country2) %>% 
  unique()

unique(dts_inf$Age)
unique(dts_inf$Country2)

tx <- 10
cols <- c("Positive" = "#b7094c",
          "Negative" = "#0091ad",
          "No-excess" = "#5c4d7d")

dts_inf2 <- 
  dts_inf %>% 
  filter(Country2 %in% lvs_cts) %>%
  mutate(Country2 = factor(Country2, levels = lvs_cts))


dts_inf2 %>% 
  ggplot()+
  geom_point(aes(psc, Country2, 
                 alpha = out, 
                 col = exc,
                 shape = Year),
             size = 1)+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  # facet_wrap(~ Age, scales = "free_x", nrow = 1)+
  facet_nested(Income ~ Age, scales = "free", space = "free_y")+
  scale_x_log10(breaks = c(0.2, 0.5, 0.7, 1, 1.5, 2, 4, 8, 16))+
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  guides(shape = guide_legend(override.aes = list(size = 3)))+
  labs(y = "Country", col = "Excess")+
  theme_bw()+
  theme(legend.position = "right",
        legend.text = element_text(size = tx - 3),
        legend.title = element_text(size = tx - 3),
        strip.text.y = element_text(size = tx - 1,
                                    margin = margin(b = .3, t = 0.3)),
        strip.text.x = element_text(size = tx,
                                    margin = margin(b = 0.1, t = 0.1)),
        panel.spacing.y = unit(0,"lines"),
        axis.text.x = element_text(size = tx - 4),
        axis.title.x = element_text(size = tx - 2),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = tx - 5))

ggsave(paste0("Figures/last version/sens_analysis/test_excess_infant.png"), dpi = 600,
       width = 6, height = 6)

ggsave(paste0("Figures/last version/sens_analysis/test_excess_infant.pdf"),
       width = 6, height = 6)

# summary of data in the plot
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
length(dts_inf2 %>% filter(Year == 2020) %>% pull(Country) %>% unique)
length(dts_inf2 %>% filter(Year == 2021) %>% pull(Country) %>% unique)

# birth_year <- 
#   read_rds("data_inter/annual_births.rds") %>% 
#   filter(Year >= yref) %>% 
#   select(Country, Year) %>% 
#   left_join(dts_inf2 %>% filter(Age == "Infant") %>% 
#               select(Country, Year) %>% 
#               mutate(bts = 1) %>% 
#               mutate(Year = Year %>% as.double())) %>% 
#   drop_na(bts) %>% 
#   group_by(Year) %>% 
#   summarise(n = sum(bts))


# filtering population
# ~~~~~~~~~~~~~~~~~~~~
# minimum 20K infants in 2020
cts_pop_min <- 
  dts_inf2 %>% 
  filter(Age == "Infant",
         Year == yref,
         Population < 2e5) %>% 
  pull(Country)

dts_inf2 %>% 
  filter(!Country %in% cts_pop_min) %>% 
  ggplot()+
  geom_point(aes(psc, Country2, 
                 alpha = out, 
                 col = exc,
                 shape = Year),
             size = 1)+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  # facet_wrap(~ Age, scales = "free_x", nrow = 1)+
  facet_nested(Income ~ Age, scales = "free", space = "free_y")+
  
  scale_x_log10(breaks = c(0.2, 0.5, 0.7, 1, 1.5, 2, 4, 8, 16))+
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  guides(shape = guide_legend(override.aes = list(size = 3)))+
  labs(y = "Country", col = "Excess",
       subtitle = "Countries with at least 20.000 infants in 2020")+
  theme_bw()+
  theme(legend.position = "right",
        legend.text = element_text(size = tx - 3),
        legend.title = element_text(size = tx - 3),
        strip.text.y = element_text(size = tx - 1,
                                    margin = margin(b = .3, t = 0.3)),
        strip.text.x = element_text(size = tx,
                                    margin = margin(b = 0.1, t = 0.1)),
        panel.spacing.y = unit(0,"lines"),
        axis.text.x = element_text(size = tx - 2),
        axis.title.x = element_text(size = tx),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = tx-2))

ggsave(paste0("Figures/last version/sens_analysis/test_excess_infant_minpop.png"), dpi = 600,
       width = 6, height = 6)

ggsave(paste0("Figures/last version/sens_analysis/test_excess_infant_minpop.pdf"),
       width = 6, height = 6)


dts_inf2 %>% 
  filter(!Country %in% cts_pop_min) %>% 
  pull(Country) %>% 
  unique() %>% 
  length()



# Plotting all ages for countries *without* infant mortality
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lvs_cts2 <- 
  dts2 %>% 
  filter(Year == yref,
         Age == "0-4",
         Sex == "t") %>% 
  arrange(-psc) %>% 
  pull(Country) %>% 
  unique()

dts_yng <- 
  dts2 %>% 
  filter(Year %in% yref,
         Sex == "t",
         !Age %in% c("1-4", "Infant"),
         Country %in% lvs_cts2) %>% 
  mutate(exc = case_when(up > 1 & lp > 1 ~ "Positive",
                         up < 1 & lp < 1 ~ "Negative",
                         TRUE ~ "No-excess"),
         out = ifelse(exc == "No-excess", 0.5, 1),
         ins = ifelse(exc == "No-excess", 0.6, 0.4),
         Country = factor(Country, levels = lvs_cts2),
         Year = Year %>% as.character())

unique(dts_yng$Sex)
unique(dts_yng$Country)

length(dts_yng %>% pull(Country) %>% unique)
# length(dts_yng %>% filter(Year == 2021) %>% pull(Country) %>% unique)

dts_yng %>% 
  ggplot()+
  geom_point(aes(psc, Country, 
                 alpha = out, 
                 col = exc,
                 shape = Year))+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  facet_nested(Income ~ Age, scales = "free", space = "free_y")+
  scale_x_log10()+
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  labs(y = "Country")+
  theme_bw()+
  theme(legend.position = "right",
        legend.text = element_text(size = tx),
        legend.title = element_text(size = tx),
        strip.text.y = element_text(size = tx - 1,
                                    margin = margin(b = .3, t = 0.3)),
        strip.text.x = element_text(size = tx,
                                    margin = margin(b = 0.1, t = 0.1)),
        panel.spacing.y = unit(0,"lines"),
        axis.text.x = element_text(size = tx - 2),
        axis.title.x = element_text(size = tx - 2),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = tx - 2))

ggsave(paste0("Figures/last version/sens_analysis/test_excess_young_ages.png"), 
       dpi = 600, width = 10, height = 10)

ggsave(paste0("Figures/last version/sens_analysis/test_excess_young_ages.pdf"), 
       width = 10, height = 10)


length(dts_yng %>% filter(Year == yref) %>% pull(Country) %>% unique)


# filtering population
# minimum 10K infants in 2020
cts_pop_min <- 
  dts_yng %>% 
  filter(Age == "0-4",
         Sex == "t",
         Year == 2019,
         Population < 1e6 | Country == "Australia") %>% 
  pull(Country)

dts_yng %>% 
  filter(!Country %in% cts_pop_min) %>% 
  ggplot()+
  geom_point(aes(psc, Country, 
                 alpha = out, 
                 col = exc,
                 shape = Year))+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  facet_nested(Income ~ Age, scales = "free", space = "free_y")+
  scale_x_log10()+
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  labs(y = "Country")+
  theme_bw()+
  theme(legend.position = "right",
        legend.text = element_text(size = tx),
        legend.title = element_text(size = tx),
        legend.key.size = unit(3, "mm"),
        strip.text.y = element_text(size = tx,
                                    margin = margin(b = 0.3, t = 0.3)),
        strip.text.x = element_text(size = tx,
                                    margin = margin(b = 0.3, t = 0.3)),
        panel.spacing.y = unit(0,"lines"),
        axis.text.x = element_text(size = tx - 2),
        axis.title.x = element_text(size = tx),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = tx))

ggsave(paste0("Figures/last version/sens_analysis/test_excess_young_ages_minpop.png"), 
       dpi = 600, width = 9, height = 7)

ggsave(paste0("Figures/last version/sens_analysis/test_excess_young_ages_minpop.pdf"), 
       width = 7, height = 8)


