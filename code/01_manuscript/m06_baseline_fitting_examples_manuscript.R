rm (list = ls())
source("code/00_functions.R")

# loading excess estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~
all_fit <- 
  read_rds("data_output/p_scores_excess_deaths_rates.rds") %>% 
  mutate(is_covid = ifelse(Year %in% 2020:2022, "y", "n"),
         bsn_on = ifelse(is.na(Deaths), "rates", "deaths"),
         bsn_r = ifelse(bsn_on == "deaths", 1e5*bsn/Exposure, bsn),
         bsn_r_lp = ifelse(bsn_on == "deaths", 1e5*bsn_lp/Exposure, bsn_lp),
         bsn_r_up = ifelse(bsn_on == "deaths", 1e5*bsn_up/Exposure, bsn_up),
         exc = case_when(Year >= 2020 & up > 1 & lp > 1 ~ "Excess",
                         Year >= 2020 & up < 1 & lp < 1 ~ "Deficit",
                         Year >= 2020 & up > 1 & lp < 1 ~ "No-change",
                         TRUE ~ "oth"),
         exc = factor(exc, levels = c("Deficit", "No-change", "Excess")),
         out = ifelse(exc == "No-change", 0.5, 1))

unique(all_fit$Age)
unique(all_fit$Country)

cts <- c("Colombia", "USA", "Israel", "Brazil", "Sweden", "South Africa", "Uzbekistan", "Spain")

cols <- c("Excess" = "#b7094c",
          "Deficit" = "#0091ad",
          "No-change" = "#5c4d7d",
          "oth" = "black")

sizes <- c("Excess" = 1,
           "Deficit" = 1,
           "No-change" = 1,
           "oth" = 1)


# ~~~~~~~~~~~~~~~~~~~~~~~~
# Stillbirths and neonatal
# ~~~~~~~~~~~~~~~~~~~~~~~~
cts <- c("South Africa", "Colombia", "Switzerland")
tx <- 8

test <- 
  all_fit %>% 
  filter(Sex == "t",
         Age %in% c("Stillbirths", "Neonatal"),
         Year >= 2015) 
  

all_fit %>% 
  filter(Country %in% cts,
         Sex == "t",
         Age %in% c("Stillbirths", "Neonatal"),
         Year >= 2015) %>% 
  ggplot()+
  geom_point(aes(Year, Rate, col = exc))+
  geom_ribbon(aes(Year, ymin = bsn_r_lp, ymax = bsn_r_up), 
              fill = "grey70", alpha = 0.4)+
  geom_line(aes(Year, bsn_r))+
  geom_vline(xintercept = 2019.5, linetype = "dashed")+
  scale_color_manual(values = cols, 
                     breaks = c("Deficit", "No-change", "Excess"))+
  scale_x_continuous(limits = c(2015, 2022), breaks = 2015:2022)+
  expand_limits(y = 0)+
  facet_nested_wrap(Country ~ Age, scales = "free", ncol = 2)+
  labs(col = "Change",
       y = "Rate (/100K)")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = tx + 2),
        strip.text = element_text(size = tx + 2, margin = margin(1,1,1,1)),
        axis.text.x = element_text(size = tx - 1, angle = 60, hjust = 1),
        axis.text.y = element_text(size = tx),
        strip.background = element_rect(fill = "transparent")) 

ggsave("figures/01_manuscript/figS06_sbs_neo_fitting_examples.png",
       dpi = 700,
       w = 4, 
       h = 4)


# ~~~~~~~~~
# Ages 0-24
# ~~~~~~~~~
tx <- 6
cts <- c("Colombia", "USA", "Israel", "Brazil", "South Africa", "Uzbekistan")

all_fit %>% 
  filter(Country %in% cts,
         Sex == "t",
         !Age %in% c("Stillbirths", "Neonatal", "0-4"),
         Year >= 2015) %>% 
  ggplot()+
  geom_point(aes(Year, Rate, col = exc))+
  geom_ribbon(aes(Year, ymin = bsn_r_lp, ymax = bsn_r_up), 
              fill = "grey70", alpha = 0.4)+
  geom_line(aes(Year, bsn_r))+
  geom_vline(xintercept = 2019.5, linetype = "dashed")+
  scale_color_manual(values = cols, 
                     breaks = c("Deficit", "No-change", "Excess"))+
  scale_x_continuous(limits = c(2015, 2021), breaks = 2015:2021)+
  expand_limits(y = 0)+
  facet_nested_wrap(Country ~ Age, scales = "free", ncol = 6)+
  labs(col = "Change",
       y = "Rate (/100K)")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = tx + 2),
        strip.text = element_text(size = tx + 2, margin = margin(1,1,1,1)),
        axis.text.x = element_text(size = tx - 1, angle = 60, hjust = 1),
        axis.text.y = element_text(size = tx),
        strip.background = element_rect(fill = "transparent")) 

ggsave("figures/01_manuscript/figS07_fitting_examples.png",
       dpi = 700,
       w = 8, 
       h = 8)

