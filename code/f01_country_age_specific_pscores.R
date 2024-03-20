rm (list = ls())
source("code/00_functions.R")

# loading excess estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~
all_fit <- 
  read_rds("data_output/p_scores_excess_deaths_rates.rds")

all_fit %>% 
  filter(Year %in% 2020:2022,
         Sex == "t",
         inc_in == 1) %>% 
  select(Country, Year, Age) %>% 
  unique() %>% 
  group_by(Year, Age) %>% 
  summarise(n = n())

length(all_fit %>% filter(Year == 2020) %>% pull(Country) %>% unique)
length(all_fit %>% filter(Year == 2021) %>% pull(Country) %>% unique)
length(all_fit %>% filter(Year == 2022) %>% pull(Country) %>% unique)

# countries with input data in rates (implicit information on births)1
bts_rts <- 
  all_fit %>% 
  filter(Year %in% 2020:2022,
         Sex == "t",
         inc_in == 1,
         is.na(Deaths),
         Age %in% c("Infant", "0-4")) %>% 
  select(Code, Year) %>% unique()
         
# countries with data on births
birth_counts_cts <- 
  read_rds("data_inter/annual_births.rds") %>% 
  filter(Year >= 2020) %>% 
  select(Code, Year) %>% 
  unique() %>% 
  bind_rows(bts_rts) %>% 
  arrange(Code, Year)

birth_cts_2020 <- 
  birth_counts_cts %>% 
  group_by(Code) %>% 
  mutate(n = n()) %>% 
  filter(n == 1) %>% 
  pull(Code) %>% 
  sort

birth_cts_2021 <- 
  birth_counts_cts %>% 
  filter(Year == 2021) %>% 
  pull(Code)

# Plotting all ages for countries with infant mortality by sex
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
income_lvs <- c("High", "Upper-mid", "Lower-mid", "Low", "No income data")
sbs_fit <- 
  all_fit %>% 
  filter(Year %in% 2020:2021,
         Age %in% c("Stillbirths", "Neonatal"),
         Sex == "t",
         inc_in == 1) %>% 
  mutate(exc = case_when(up > 1 & lp > 1 ~ "Excess",
                         up < 1 & lp < 1 ~ "Deficit",
                         TRUE ~ "No-change"),
         exc = factor(exc, levels = c("Deficit", "No-change", "Excess")),
         out = ifelse(exc == "No-change", 0.5, 1),
         ins = ifelse(exc == "No-change", 0.6, 0.4),
         Year = Year %>% as.character(),
         Income = factor(Income, levels = income_lvs),
         Country2 = case_when(Code %in% birth_cts_2020 ~ paste0(Country, " *"), 
                              Code %in% birth_cts_2021 ~ paste0(Country, "**"), 
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
cols <- c("Excess" = "#b7094c",
          "Deficit" = "#0091ad",
          "No-change" = "#5c4d7d")

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
  labs(y = "Country", col = "Change", x = "P-score")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.box="vertical",
        legend.margin=margin(),
        legend.text = element_text(size = tx-2),
        legend.title = element_text(size = tx-1),
        strip.background = element_blank(),
        strip.text.y = element_text(size = tx - 1),
        strip.text.x = element_text(size = tx + 1),
        panel.spacing.y = unit(0,"lines"),
        axis.text.x = element_text(size = tx - 2.5, angle = 60, hjust = 1),
        axis.title.x = element_text(size = tx + 1),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = tx - 1.5))

ggsave("figures/figS07_sbs_neo_pscores.png", 
       dpi = 1000,
       width = 6, height = 8)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all_fit2 <- 
  all_fit %>% 
  filter(Year %in% 2020:2021,
         !Age %in% c("0-4", "Stillbirths", "Neonatal"),
         Sex == "t",
         inc_in == 1) %>% 
  mutate(exc = case_when(up > 1 & lp > 1 ~ "Excess",
                         up < 1 & lp < 1 ~ "Deficit",
                         TRUE ~ "No-change"),
         exc = factor(exc, levels = c("Deficit", "No-change", "Excess")),
         out = ifelse(exc == "No-change", 0.5, 1),
         ins = ifelse(exc == "No-change", 0.6, 0.4),
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
cols <- c("Excess" = "#b7094c",
          "Deficit" = "#0091ad",
          "No-change" = "#5c4d7d")

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
             size = 2)+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  facet_grid(Income ~ Age, scales = "free", space = "free_y")+
  scale_x_log10(breaks = bks,
                labels = lbs,
                limits = c(0.5, 2))+
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols)+
  guides(shape = guide_legend(override.aes = list(size = 3)))+
  labs(y = "Country", col = "Change", x = "P-score")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = tx+2),
        legend.title = element_text(size = tx+2),
        strip.background = element_blank(),
        strip.text.y = element_text(size = tx + 2),
        strip.text.x = element_text(size = tx + 2),
        panel.spacing.y = unit(0,"lines"),
        axis.text.x = element_text(size = tx - 2.5, angle = 60, hjust = 1),
        axis.title.x = element_text(size = tx + 1),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = tx - 1))

ggsave("figures/figS08_age_specific_pscores.png", 
       dpi = 1000,
       width = 10, height = 12)

