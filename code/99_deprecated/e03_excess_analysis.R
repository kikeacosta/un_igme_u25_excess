rm (list = ls())
source("Code/00_functions.R")

# loading excess estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~
dts_fit <- 
  read_rds("Output/p_scores_excess_deaths.rds") %>% 
  select(Country, Code, Year, Sex, Age, psc, up, lp, Population, Source)

rts_fit <- 
  read_rds("Output/p_scores_excess_rates.rds") %>% 
  select(Country, Code, Year, Sex, Age, psc, up, lp, Population, Source)

all_fit <- 
  bind_rows(dts_fit, rts_fit) %>% 
  # filter(!Country %in% excl) %>% 
  arrange(Country, Code, Age, Sex, Year) %>% 
  mutate(Age = factor(Age, 
                      levels = c("Infant", "1-4", "0-4", "5-9", 
                                 "10-14", "15-19", "20-24"))) %>% 
  # excluding groups with fitting issues
  filter(psc != 0 & !is.na(psc) & !is.na(lp) & !is.na(up) &
           lp != Inf & up != Inf)

length(all_fit %>% filter(Year == 2020) %>% pull(Country) %>% unique)
length(all_fit %>% filter(Year == 2021) %>% pull(Country) %>% unique)


# countries with data on births
birth_counts_cts <- 
  read_rds("data_inter/annual_births.rds") %>% 
  filter(Year >= 2020) %>% 
  select(Country, Year) %>% 
  unique() %>% 
  bind_rows(rts_fit %>% 
              filter(Year >= 2020,
                     Age %in% c("Infant", "0-4")) %>% 
              select(Country, Year) %>% 
              unique()) %>% 
  arrange(Country, Year)

birth_cts_2020 <- 
  birth_counts_cts %>% 
  group_by(Country) %>% 
  mutate(n = n()) %>% 
  filter(n == 1) %>% 
  pull(Country) %>% 
  sort

birth_cts_2021 <- 
  birth_counts_cts %>% 
  filter(Year == 2021) %>% 
  pull(Country)

# loading country contextual variables from WPP documentation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# https://population.un.org/wpp/Download/Metadata/Documentation/
locs <- read_xlsx(here("Data", "WPP2022_F01_LOCATIONS.xlsx"),
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

all_fit2 <- 
  all_fit %>% 
  mutate(Code_join = ifelse(Country %in% uk_cts,
                       "GBR",
                       Code)) %>%
  left_join(locs, by = "Code_join") %>% 
  mutate(Income = factor(Income, levels = income_lvs))

# test all merged
all_fit2 %>% 
  filter(is.na(Income)) %>% 
  pull(Country) %>% unique()


# weighted averages
# ~~~~~~~~~~~~~~~~~
w_avs <- 
  all_fit2 %>% 
  filter(Sex == "t",
         Year >= 2020,
         !Country %in% c("Australia", "Kenya"),
         Population >= 1e5,
         ) %>% 
  drop_na() %>% 
  group_by(Year, Sex, Age, Income) %>% 
  mutate(c = Population / sum(Population)) %>% 
  summarize(w_av = sum(psc * c),
            w_av_lp = sum(lp * c),
            w_av_up = sum(up * c),
            av = mean(psc),
            av_lp = mean(lp),
            av_up = mean(up)) %>% 
  ungroup() %>%
  mutate(exc = case_when(w_av_up > 1 & w_av_lp > 1 ~ "Positive",
                         w_av_up < 1 & w_av_lp < 1 ~ "Negative",
                         TRUE ~ "No-excess"),
         out = ifelse(exc == "No-excess", 0.5, 1))

write.excel(w_avs)
write.excel(all_fit2)

tx <- 10

w_avs %>% 
  ggplot()+
  geom_point(aes(w_av, Age, 
                 alpha = out))+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  facet_nested(Income ~ Year)+
  scale_x_log10()+
  scale_y_discrete(limits=rev) %>% 
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  labs(t = "weighted average p-scores")+
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

ggsave(paste0("Figures/last version/weighted_average_pscores.png"), 
       dpi = 600, width = 5, height = 5)

ggsave(paste0("Figures/last version/weighted_average_pscores.pdf"), 
       width = 5, height = 5)



# Plotting all ages for countries with infant mortality by sex
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dts_inf <- 
  all_fit2 %>% 
  filter(Year %in% 2020:2021,
         Age %in% c("Infant", "1-4"),
         Sex == "t") %>% 
  mutate(exc = case_when(up > 1 & lp > 1 ~ "Positive",
                         up < 1 & lp < 1 ~ "Negative",
                         TRUE ~ "No-excess"),
         out = ifelse(exc == "No-excess", 0.5, 1),
         ins = ifelse(exc == "No-excess", 0.6, 0.4),
         Year = Year %>% as.character(),
         Country2 = case_when(Country %in% birth_cts_2020 ~ paste0(Country, "*"), 
                              Country %in% birth_cts_2021 ~ paste0(Country, "**"), 
                              TRUE ~ Country))

unique(dts_inf$Country) %>% 
  length()

# dts_inf %>%
#   filter(Country %in% birth_cts) %>%
#   pull(Country) %>%
#   unique() %>%
#   length()


cts_ord <-
  dts_inf %>%
  filter(Year == 2020,
         Age == "Infant") %>%
  arrange(-psc) %>%
  select(Country) %>%
  mutate(order = 1:n())

lvs_cts <-
  dts_inf %>%
  filter(Year == 2020,
         Age == "Infant") %>%
  arrange(-psc) %>%
  pull(Country2) %>%
  unique()

unique(dts_inf$Age)
unique(dts_inf$Country2)

tx <- 10
cols <- c("Positive" = "#b7094c",
          "Negative" = "#0091ad",
          "No-excess" = "#5c4d7d")

# Adding label colors to countries that have data on 2020 births 
axis_y_cts <- 
  dts_inf %>% 
  filter(Age == "Infant", 
         Sex == "t") %>% 
  left_join(cts_ord) %>% 
  arrange(order) %>%
  select(Country2, Income) %>% 
  unique()

unique(axis_y_cts$Income)

axis_y_cols <-
  case_when(axis_y_cts$Income == "Lower-mid" ~ "#e63946", 
            axis_y_cts$Income == "Upper-mid" ~ "#3f37c9", 
            axis_y_cts$Income == "High" ~ "black")

dts_inf2 <- 
  dts_inf %>% 
  filter(Country2 %in% lvs_cts) %>%
  mutate(Country2 = factor(Country2, levels = lvs_cts))

bks <- c(0.2, 0.3, 0.5, 0.7, 1, 1.2, 1.5, 2, 3, 8, 16)
lbs <- paste0((bks - 1)*100, "%")


dts_inf2 %>% 
  ggplot()+
  geom_point(aes(psc, Country2, 
                 alpha = out, 
                 col = exc,
                 shape = Year),
             size = 1.5)+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  # facet_wrap(~ Age, scales = "free_x", nrow = 1)+
  facet_nested(Income ~ Age, scales = "free", space = "free_y")+
  scale_x_log10(breaks = bks,
                labels = lbs,
                limits = c(0.3, 3))+
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  guides(shape = guide_legend(override.aes = list(size = 3)))+
  labs(y = "Country", col = "Excess", x = "P-score")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = tx - 1),
        legend.title = element_text(size = tx - 1),
        strip.text.y = element_text(size = tx - 2,
                                    margin = margin(b = .3, t = 0.3)),
        strip.text.x = element_text(size = tx - 2,
                                    margin = margin(b = 0.1, t = 0.1)),
        panel.spacing.y = unit(0,"lines"),
        axis.text.x = element_text(size = tx - 4),
        axis.title.x = element_text(size = tx - 2),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = tx - 3))

ggsave(paste0("Figures/last version/excess_infant.png"), dpi = 600,
       width = 6, height = 8)

ggsave(paste0("Figures/last version/excess_infant.pdf"),
       width = 6, height = 8)

# summary of data in the plot
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
length(dts_inf2 %>% filter(Year == 2020) %>% pull(Country) %>% unique)
length(dts_inf2 %>% filter(Year == 2021) %>% pull(Country) %>% unique)

birth_year <- 
  read_rds("data_inter/annual_births.rds") %>% 
  filter(Year >= 2020) %>% 
  select(Country, Year) %>% 
  left_join(dts_inf2 %>% filter(Age == "Infant") %>% 
              select(Country, Year) %>% 
              mutate(bts = 1) %>% 
              mutate(Year = Year %>% as.double())) %>% 
  drop_na(bts) %>% 
  group_by(Year) %>% 
  summarise(n = sum(bts))


# filtering population
# ~~~~~~~~~~~~~~~~~~~~
# minimum 20K infants in 2020
cts_pop_min <- 
  dts_inf2 %>% 
  filter(Age == "Infant",
         Year == 2020,
         Population < 2e5) %>% 
  pull(Country)

# bks <- c(0.2, 0.3, 0.5, 0.7, 1, 1.2, 1.5, 2, 4, 8, 16)
# lbs <- paste0((bks - 1)*100, "%")

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
  
  scale_x_log10(breaks = bks,
                labels = lbs,
                limits = c(0.3, 3))+
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

ggsave(paste0("Figures/last version/excess_infant_minpop.png"), dpi = 600,
       width = 6, height = 6)

ggsave(paste0("Figures/last version/excess_infant_minpop.pdf"),
       width = 6, height = 6)


dts_inf2 %>% 
  filter(!Country %in% cts_pop_min) %>% 
  pull(Country) %>% 
  unique() %>% 
  length()



# Plotting all ages for countries *without* infant mortality
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lvs_cts2 <- 
  all_fit2 %>% 
  filter(Year == 2020,
         Age == "0-4" | Age == "5-9",
         Sex == "t") %>% 
  arrange(-psc) %>% 
  pull(Country) %>% 
  unique()

dts_yng <- 
  all_fit2 %>% 
  filter(Year %in% 2020:2021,
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

length(dts_yng %>% filter(Year == 2020) %>% pull(Country) %>% unique)
length(dts_yng %>% filter(Year == 2021) %>% pull(Country) %>% unique)

bks <- c(0.2, 0.5, 1, 2, 5)
lbs <- paste0((bks - 1)*100, "%")

dts_yng %>% 
  ggplot()+
  geom_point(aes(psc, Country, 
                 alpha = out, 
                 col = exc,
                 shape = Year))+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  facet_nested(Income ~ Age, scales = "free", space = "free_y")+
  # scale_x_log10()+
  scale_x_log10(breaks = bks,
                labels = lbs,
                limits = c(0.2, 5))+
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  labs(x = "P-score")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = tx),
        legend.title = element_text(size = tx),
        strip.text.y = element_text(size = tx - 1,
                                    margin = margin(b = .3, t = 0.3)),
        strip.text.x = element_text(size = tx,
                                    margin = margin(b = 0.1, t = 0.1)),
        panel.spacing.y = unit(0,"lines"),
        axis.text.x = element_text(size = tx - 3.5),
        axis.title.x = element_text(size = tx - 2),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = tx - 2))

ggsave(paste0("Figures/last version/excess_young_ages.png"), 
       dpi = 600, width = 8, height = 10)

ggsave(paste0("Figures/last version/excess_young_ages.pdf"), 
       width = 8, height = 10)


length(dts_yng %>% filter(Year == 2020) %>% pull(Country) %>% unique)
length(dts_yng %>% filter(Year == 2021) %>% pull(Country) %>% unique)


# filtering population
# minimum 10K infants in 2020
cts_pop_min <- 
  dts_yng %>% 
  filter(Age == "0-4",
         Sex == "t",
         Year == 2020,
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

ggsave(paste0("Figures/last version/excess_young_ages_minpop.png"), 
       dpi = 600, width = 9, height = 7)

ggsave(paste0("Figures/last version/excess_young_ages_minpop.pdf"), 
       width = 7, height = 8)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plotting proportions according to excess
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

all_fit3 <- 
  all_fit2 %>% 
  filter(Year >= 2020) %>%
  select(Country, Year, Age, Sex, psc, lp, up) %>% 
  mutate(psc_out = ifelse(1 < up & 1 > lp, 1, psc)) %>% 
  drop_na(psc_out) %>% 
  unique()

dts_only_sig <- 
  all_fit3 %>% 
  select(-psc) %>% 
  rename(psc = psc_out)

unique(all_fit3$Age)
unique(all_fit3$Country)

db_summary <- 
  all_fit3 %>% 
  filter(Sex == "t") %>% 
  mutate(exc_all = case_when(psc > 1 ~ "Positive",
                             psc < 1 ~ "Negative",
                             psc == 1 ~ "No-excess"),
         exc_out = case_when(psc_out > 1 ~ "Positive",
                             psc_out < 1 ~ "Negative",
                             psc_out == 1 ~ "No-excess")) %>% 
  select(Year, Age, Sex, Country, exc_all, exc_out) %>% 
  group_by(Year, Age, Sex) %>% 
  mutate(N_cts = n()) %>% 
  ungroup() %>%  
  gather(exc_all, exc_out, key = type, value = is_excess) %>% 
  group_by(Year, Age, Sex, N_cts, type, is_excess) %>% 
  summarise(N = n()) %>% 
  ungroup() %>% 
  mutate(Prop = N / N_cts,
         Age = factor(Age, levels = rev(c("Infant", "1-4", "0-4", "5-9", 
                                          "10-14", "15-19", "20-24"))),
         type = recode(type,
                       "exc_all" = "All estimates",
                       "exc_out" = "Uncertainty"),
         type = factor(type, levels = c("All estimates", "Uncertainty")),
         is_excess = factor(is_excess, 
                            levels = c("Positive", "No-excess", "Negative")),
         Sex = recode(Sex,
                      "f" = "females",
                      "m" = "males",
                      "t" = "total"),
         age_type = ifelse(Age %in% c("Infant", "1-4"), "Inf", "5y"),
         age_type = factor(age_type, levels = c("Inf", "5y")))

cols <- 
  c("Positive" = "#b7094c",
    "No-excess" = "#5c4d7d",
    "Negative" = "#0091ad")

db_summary %>% 
  ggplot(aes(x = Age, y = Prop))+
  geom_bar(aes(fill = is_excess),
           stat = "identity", 
           position = "fill",
           alpha = 0.8)+
  geom_text(aes(label = paste0(round(Prop*100)), group = is_excess), 
            position = position_stack(vjust = 0.5, reverse = F), size = 3)+
  facet_nested(type + age_type ~ Year, scales = "free", space = "free_y",
               nest_line = element_line(linetype = 1))+
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE))+
  labs(fill = "Excess", 
       title = "All countries")+
  coord_flip(expand = 0)+
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        plot.title = element_text(size = 10),
        panel.spacing = unit(0.5, "lines"))

ggsave("Figures/last version/excess_summary_2020_2021.png", width = 8, height = 4)


# restricting countries that appear in 2020 and 2021 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_summary_21 <- 
  all_fit3 %>% 
  filter(Sex == "t") %>% 
  group_by(Country) %>% 
  filter(max(Year) == 2021) %>% 
  mutate(exc_all = case_when(psc > 1 ~ "Positive",
                             psc < 1 ~ "Negative",
                             psc == 1 ~ "No-excess"),
         exc_out = case_when(psc_out > 1 ~ "Positive",
                             psc_out < 1 ~ "Negative",
                             psc_out == 1 ~ "No-excess")) %>% 
  select(Year, Age, Sex, Country, exc_all, exc_out) %>% 
  group_by(Year, Age, Sex) %>% 
  mutate(N_cts = n()) %>% 
  ungroup() %>%  
  gather(exc_all, exc_out, key = type, value = is_excess) %>% 
  group_by(Year, Age, Sex, N_cts, type, is_excess) %>% 
  summarise(N = n()) %>% 
  ungroup() %>% 
  mutate(Prop = N / N_cts,
         Age = factor(Age, levels = rev(c("Infant", "1-4", "0-4", "5-9", 
                                          "10-14", "15-19", "20-24"))),
         type = recode(type,
                       "exc_all" = "All estimates",
                       "exc_out" = "Uncertainty"),
         type = factor(type, levels = c("All estimates", "Uncertainty")),
         is_excess = factor(is_excess, 
                            levels = c("Positive", "No-excess", "Negative")),
         Sex = recode(Sex,
                      "f" = "females",
                      "m" = "males",
                      "t" = "total"),
         age_type = ifelse(Age %in% c("Infant", "1-4"), "Inf", "5y"),
         age_type = factor(age_type, levels = c("Inf", "5y")))

all_fit3 %>% 
  group_by(Country) %>% 
  filter(max(Year) == 2021) %>% 
  ungroup() %>% 
  pull(Country) %>% 
  unique() %>% 
  length()
  

length(all_fit3 %>% filter(Year == 2020) %>% pull(Country) %>% unique)
length(all_fit3 %>% filter(Year == 2021) %>% pull(Country) %>% unique)


db_summary_21 %>% 
  ggplot(aes(x = Age, y = Prop))+
  geom_bar(aes(fill = is_excess),
           stat = "identity", 
           # position = "fill",
           alpha = 0.8)+
  geom_text(aes(label = paste0(round(Prop*100)), group = is_excess),
            position = position_stack(vjust = 0.5, reverse = F), size = 3)+
  facet_nested(type + age_type ~ Year, scales = "free", space = "free_y",
               nest_line = element_line(linetype = 1))+
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE))+
  labs(fill = "Excess",
       title = "Only countries with data in both years")+
  coord_flip(expand = 0)+
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        plot.title = element_text(size = 10),
        panel.spacing = unit(0.5, "lines"))

ggsave("Figures/last version/excess_summary_2020_2021_both_years.png", width = 8, height = 4)



# changes in proportions between 2020 and 2021
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_summary_21 %>% 
  mutate(Age = factor(Age, levels = c("Infant", "1-4", "0-4", "5-9", 
                                      "10-14", "15-19", "20-24")),
         Year = factor(Year, levels = c("2020", "2021"))) %>% 
  filter(is_excess != "No-excess") %>%
  ggplot(aes(x = Year, y = Prop))+
  geom_bar(aes(fill = is_excess), 
           stat = "identity")+
  geom_text(aes(label = paste0(round(Prop*100)), group = is_excess), 
            position = position_stack(vjust = 0.5, reverse = F), size = 2.5)+
  facet_nested(type ~ is_excess + age_type + Age, scales = "free", space = "free_y",
               nest_line = element_line(linetype = 1))+
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE))+
  theme_minimal()+
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 9, face = "bold"),
        axis.text.x = element_text(size = 8, face = "bold", angle = 90),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 10),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(1, "lines"),
        legend.position = "none")

ggsave("Figures/last version/excess_summary_2020_2021_change.png", 
       width = 8, height = 4)

db_summary_21 %>% 
  mutate(Age = factor(Age, levels = c("Infant", "1-4", "0-4", "5-9", 
                                      "10-14", "15-19", "20-24")),
         Year = factor(Year, levels = c("2020", "2021"))) %>% 
  filter(is_excess != "No-excess") %>%
  ggplot(aes(x = Year, y = Prop))+
  geom_bar(aes(fill = is_excess), 
           stat = "identity")+
  geom_text(aes(label = paste0(round(Prop*100)), group = is_excess), 
            position = position_stack(vjust = 0.5, reverse = F), size = 2.5)+
  facet_nested(type ~ age_type + Age + is_excess, scales = "free", space = "free_y",
               nest_line = element_line(linetype = 1))+
  scale_fill_manual(values = cols, guide = guide_legend(reverse = TRUE))+
  theme_minimal()+
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 8, face = "bold"),
        axis.text.x = element_text(size = 8, face = "bold", angle = 90),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 10),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(1, "lines"),
        legend.position = "none")

ggsave("Figures/last version/excess_summary_2020_2021_change_v2.png", 
       width = 8, height = 4)




# 

# try for coloring countries with birth data
# # +++++++++++++++++++++++++++++
# 
# lvs_cts <- 
#   dts_inf %>% 
#   filter(Year == 2020,
#          Age == "Infant") %>% 
#   arrange(-psc) %>% 
#   pull(Country) %>% 
#   unique()
# 
# dts_inf2 <- 
#   dts_inf %>% 
#   filter(Country %in% lvs_cts) %>%
#   mutate(Country = factor(Country, levels = lvs_cts),
#          bts = ifelse(Country %in% birth_cts, "y", "n"))
# 
# # cts_ord <- 
# #   dts_inf2 %>% 
# #   filter(Year == 2020,
# #          Age == "Infant") %>% 
# #   arrange(Income, psc) %>%
# #   arrange(psc) %>% 
# #   select(Country) %>% 
# #   mutate(order = 1:n()) %>% 
# #   arrange(-order) %>% 
# #   mutate(order = 1:n())
# 
# cts_ord <- 
#   dts_inf2 %>% 
#   filter(Year == 2020,
#          Age == "Infant") %>% 
#   # arrange(Income, psc) %>%
#   arrange(-psc) %>% 
#   select(Country) %>% 
#   mutate(order = 1:n())
# 
# axis_y_cts <- 
#   cts_ord %>% 
#   mutate(col = ifelse(Country %in% birth_cts, "#f94144", "black")) %>% 
#   select(Country, col) 
# 
# axis_y_cols <-
#   axis_y_cts$col
# case_when(axis_y_cts$bts == "y" ~ "green", 
#           axis_y_cts$bts == "n" ~ "black")
# 
# 
# length(dts_inf2 %>% filter(Year == 2020) %>% pull(Country) %>% unique)
# length(dts_inf2 %>% filter(Year == 2021) %>% pull(Country) %>% unique)
# 
# 
# dts_inf2 %>% 
#   ggplot()+
#   geom_point(aes(psc, Country, 
#                  alpha = out, 
#                  col = exc,
#                  shape = Year),
#              size = 1)+
#   geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
#   # facet_wrap(~ Age, scales = "free_x", nrow = 1)+
#   facet_nested(Income ~ Age, scales = "free", space = "free_y")+
#   
#   scale_x_log10(breaks = c(0.2, 0.5, 0.7, 1, 1.5, 2, 4, 8, 16))+
#   scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
#   scale_color_manual(values = cols, guide = "none")+
#   guides(shape = guide_legend(override.aes = list(size = 3)))+
#   labs(y = "Country", col = "Excess")+
#   theme_bw()+
#   theme(legend.position = "right",
#         legend.text = element_text(size = tx - 3),
#         legend.title = element_text(size = tx - 3),
#         strip.text.y = element_text(size = tx - 2,
#                                     margin = margin(b = 0, t = 0)),
#         strip.text.x = element_text(size = tx - 3,
#                                     margin = margin(b = 0.1, t = 0.1)),
#         panel.spacing.y = unit(0,"lines"),
#         axis.text.x = element_text(size = tx - 4),
#         axis.title.x = element_text(size = tx - 2),
#         axis.title.y = element_blank(),
#         axis.text.y = element_text(color = axis_y_cols, size = tx - 5))
# 
# ggsave(paste0("Figures/last version/excess_infant.png"), dpi = 600,
#        width = 6, height = 6)
# 
# ggsave(paste0("Figures/last version/excess_infant.pdf"),
#        width = 6, height = 6)
# 
