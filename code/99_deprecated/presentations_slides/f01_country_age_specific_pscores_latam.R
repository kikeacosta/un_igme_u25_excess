rm (list = ls())
source("Code/00_functions.R")

# loading excess estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~
all_fit <- 
  read_rds("Output/p_scores_excess_deaths_rates.rds")


# latam countries
# ~~~~~~~~~~~~~~~
data(UNlocations)

latam <- 
  UNlocations %>% 
  filter(location_type == 4) %>% 
  select(name, reg_name, area_name) %>% 
  filter(area_name == "Latin America and the Caribbean") %>% 
  mutate(name = case_when(name == "Bolivia (Plurinational State of)" ~ "Bolivia",
                          name == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                          TRUE ~ name))

unique(all_fit$Country) %>% sort()
unique(latam$name) %>% sort()

latam_cts_fit <-
  all_fit %>% 
  filter(Country %in% unique(latam$name)) %>% 
  pull(Country) %>% 
  unique()

latam_cts_fit
length(latam_cts_fit)

all_fit_la <- 
  all_fit %>% 
  filter(Country %in% latam_cts_fit)
  
all_fit_la %>% 
  filter(Year %in% 2020:2021,
         Sex == "t",
         inc_in == 1) %>% 
  select(Country, Year, Age) %>% 
  unique() %>% 
  group_by(Year, Age) %>% 
  summarise(n = n())

length(all_fit_la %>% filter(Year == 2020) %>% pull(Country) %>% unique)
length(all_fit_la %>% filter(Year == 2021) %>% pull(Country) %>% unique)


# countries with input data in rates (implicit information on births)1
bts_rts <- 
  all_fit_la %>% 
  filter(Year %in% 2020:2021,
         Sex == "t",
         inc_in == 1,
         is.na(Deaths),
         Age %in% c("Infant", "0-4")) %>% 
  select(Country, Year) %>% unique()
         
# countries with data on births
birth_counts_cts <- 
  read_rds("data_inter/annual_births.rds") %>% 
  filter(Year >= 2020,
         Country %in% latam_cts_fit) %>% 
  select(Country, Year) %>% 
  unique() %>% 
  bind_rows(bts_rts) %>% 
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

# Plotting all ages for countries with infant mortality by sex
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
income_lvs <- c("High", "Upper-mid", "Lower-mid", "Low", "No income data")
sbs_fit <- 
  all_fit_la %>% 
  filter(Year %in% 2020:2021,
         Age %in% c("Stillbirths", "Neonatal"),
         Sex == "t",
         inc_in == 1) %>% 
  mutate(exc = case_when(up > 1 & lp > 1 ~ "Positive",
                         up < 1 & lp < 1 ~ "Negative",
                         TRUE ~ "No-excess"),
         out = ifelse(exc == "No-excess", 0.5, 1),
         ins = ifelse(exc == "No-excess", 0.6, 0.4),
         Year = Year %>% as.character(),
         Income = factor(Income, levels = income_lvs),
         Country2 = case_when(Country %in% birth_cts_2020 ~ paste0(Country, " *"), 
                              Country %in% birth_cts_2021 ~ paste0(Country, "**"), 
                              TRUE ~ paste0(Country, "  ")))

unique(sbs_fit$Country) %>% 
  length()

unique(sbs_fit$Income)

lvs_cts <-
  sbs_fit %>%
  filter(Year == 2020,
         Age == "Stillbirths") %>%
  arrange(-psc) %>%
  pull(Country2) %>%
  unique()

unique(sbs_fit$Age)
unique(sbs_fit$Country2)

tx <- 10
cols <- c("Positive" = "#b7094c",
          "Negative" = "#0091ad",
          "No-excess" = "#5c4d7d")

sbs_fit2 <- 
  sbs_fit %>% 
  filter(Country2 %in% lvs_cts) %>%
  mutate(Country2 = factor(Country2, levels = lvs_cts),
         Income = factor(Income, levels = income_lvs))

bks <- c(0.2, 0.3, 0.5, 0.7, 1, 1.2, 1.5, 2, 3, 8, 16)
lbs <- paste0((bks - 1)*100, "%")

sbs_fit2 %>% 
  mutate(psc = case_when(psc < 0.5 ~ 0.5,
                         psc > 2 ~ 2,
                         TRUE ~ psc)) %>% 
  ggplot()+
  geom_point(aes(psc, Country2, 
                 alpha = out, 
                 col = exc,
                 shape = Year),
             size = 2)+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  facet_grid(Income ~ Age, scales = "free", space = "free_y")+
  scale_x_log10(breaks = bks,
                labels = lbs,
                limits = c(0.5, 2))+
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols)+
  guides(shape = guide_legend(override.aes = list(size = 3)))+
  labs(y = "Country", col = "Excess", x = "P-score")+
  theme_bw()+
  theme(legend.position = "none",
        legend.text = element_text(size = tx-1),
        legend.title = element_text(size = tx-1),
        strip.background = element_blank(),
        strip.text.y = element_text(size = tx + 1),
        strip.text.x = element_text(size = tx + 1),
        panel.spacing.y = unit(0,"lines"),
        axis.text.x = element_text(size = tx - 2.5, angle = 60, hjust = 1),
        axis.title.x = element_text(size = tx + 1),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = tx))

ggsave("Figures/slides/colmex/figS06_sbs_neo_pscores.png", 
       dpi = 1000,
       width = 4, height = 3.5)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all_fit2 <- 
  all_fit_la %>% 
  filter(Year %in% 2020:2021,
         !Age %in% c("0-4", "Stillbirths", "Neonatal"),
         Sex == "t",
         inc_in == 1) %>% 
  mutate(exc = case_when(up > 1 & lp > 1 ~ "Positive",
                         up < 1 & lp < 1 ~ "Negative",
                         TRUE ~ "No-excess"),
         out = ifelse(exc == "No-excess", 0.5, 1),
         ins = ifelse(exc == "No-excess", 0.6, 0.4),
         Year = Year %>% as.character(),
         Income = factor(Income, levels = income_lvs),
         Country2 = case_when(Country %in% birth_cts_2020 ~ paste0(Country, " *"), 
                              Country %in% birth_cts_2021 ~ paste0(Country, "**"), 
                              TRUE ~ paste0(Country, "  ")))

unique(all_fit2$Country) %>% 
  length()

unique(all_fit2$Income)

lvs_cts <-
  all_fit2 %>%
  filter(Year == 2020,
         Age == "Infant") %>%
  arrange(-psc) %>%
  pull(Country2) %>%
  unique()

unique(all_fit2$Age)
unique(all_fit2$Country2)

tx <- 10
cols <- c("Positive" = "#b7094c",
          "Negative" = "#0091ad",
          "No-excess" = "#5c4d7d")

all_fit3 <- 
  all_fit2 %>% 
  filter(Country2 %in% lvs_cts) %>%
  mutate(Country2 = factor(Country2, levels = lvs_cts),
         Income = factor(Income, levels = income_lvs))

bks <- c(0.2, 0.3, 0.5, 0.7, 1, 1.2, 1.5, 2, 3, 8, 16)
lbs <- paste0((bks - 1)*100, "%")

all_fit3 %>% 
  mutate(psc = case_when(psc < 0.5 ~ 0.5,
                         psc > 2 ~ 2,
                         TRUE ~ psc)) %>% 
  ggplot()+
  geom_point(aes(psc, Country2, 
                 alpha = out, 
                 col = exc,
                 shape = Year),
             size = 1.5)+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  facet_grid(Income ~ Age, scales = "free", space = "free_y")+
  scale_x_log10(breaks = bks,
                labels = lbs,
                limits = c(0.5, 2))+
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols)+
  guides(shape = guide_legend(override.aes = list(size = 3)))+
  labs(y = "Country", col = "Excess", x = "P-score")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = tx+2),
        legend.title = element_text(size = tx+2),
        strip.background = element_blank(),
        strip.text.y = element_text(size = tx + 2),
        strip.text.x = element_text(size = tx + 2),
        panel.spacing.y = unit(0,"lines"),
        axis.text.x = element_text(size = tx - 3.5, angle = 60, hjust = 1),
        axis.title.x = element_text(size = tx + 1),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = tx))

ggsave("Figures/slides/colmex/figS07_age_specific_pscores.png", 
       dpi = 1000,
       width = 8, height = 5)

