library(cowplot)
rm (list = ls())
source("Code/00_functions.R")

# loading excess estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~
all_fit <- 
  read_rds("Output/p_scores_excess_deaths_rates.rds")

all_fit %>% 
  filter(Year %in% 2020:2021,
         Sex == "t") %>% 
  select(Country, Year, Age) %>% 
  unique() %>% 
  group_by(Year, Age) %>% 
  summarise(n = n())

length(all_fit %>% filter(Year == 2020) %>% pull(Country) %>% unique)
length(all_fit %>% filter(Year == 2021) %>% pull(Country) %>% unique)

max_pop_all <- 
  all_fit %>% 
  filter(Year %in% 2020:2021, 
         Sex == "t") %>% 
  pull(Population) %>% 
  max()

min_pop_all <- 
  all_fit %>% 
  filter(Year %in% 2020:2021, 
         Sex == "t") %>% 
  pull(Population) %>% 
  min()

whole_range <- max_pop_all - min_pop_all

# Plotting excess estimates for countries by ages
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ag <- "0-4"
plot_age_x <- function(ag, yrs = 2020:2021){
  
  lvs_cts <-
    all_fit %>%
    filter(Year == min(yrs),
           Age == ag,
           Sex == "t") %>%
    arrange(-psc) %>%
    pull(Country) %>%
    unique()
  
  chunk_plot <- 
    all_fit %>% 
    filter(Year %in% yrs,
           Age %in% ag,
           Sex == "t") %>% 
    mutate(exc = case_when(up > 1 & lp > 1 ~ "Positive",
                           up < 1 & lp < 1 ~ "Negative",
                           TRUE ~ "No-excess"),
           out = ifelse(exc == "No-excess", 0.5, 1),
           ins = ifelse(exc == "No-excess", 0.6, 0.4),
           Year = Year %>% as.character(),
           Country = factor(Country, levels = lvs_cts))
  
  max_pop <- max(chunk_plot$Population)
  min_pop <- min(chunk_plot$Population)
  
  min_adj <- 2 + (min_pop - min_pop_all) / whole_range
  max_adj <- 2 + 5 * (max_pop - min_pop_all) / whole_range
  
  range_size <- c(min_adj, max_adj)
  
  unique(chunk_plot$Country) %>% 
    length()
  
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
    # facet_wrap(~ Age, scales = "free_x", nrow = 1)+
    facet_nested(Income ~ Age, scales = "free", space = "free_y")+
    scale_x_log10(breaks = bks,
                  labels = lbs,
                  limits = c(0.5, 2))+
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
          strip.text = element_blank(),
          # strip.text.y = element_text(size = tx - 2,
          #                             margin = margin(b = .3, t = 0.3)),
          # strip.text.x = element_text(size = tx - 2,
          #                             margin = margin(b = 0.1, t = 0.1)),
          panel.spacing.y = unit(0,"lines"),
          axis.text.x = element_text(size = tx - 4),
          axis.title.x = element_text(size = tx - 2),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())
  
  return(plot)
 
}

unique(all_fit$Age)

{ # 2020 & 2021
  pl_inf <- plot_age_x("Infant", 2020:2021)
  pl_1_4 <- plot_age_x("1-4", 2020:2021)
  pl_0_4 <- plot_age_x("0-4", 2020:2021)
  pl_5_9 <- plot_age_x("5-9", 2020:2021)
  pl_10_14 <- plot_age_x("10-14", 2020:2021)
  pl_15_19 <- plot_age_x("15-19", 2020:2021)
  pl_20_24 <- plot_age_x("20-24", 2020:2021)
  
  plot_grid(pl_inf, pl_1_4, pl_0_4, pl_5_9, pl_10_14, pl_15_19, pl_20_24, 
            labels = c("Infant", "1-4", "0-4", "5-9", "10-14", "15-19", "20-24"),
            label_size = 14,
            label_x = 0.9, label_y = 0.99, hjust = 1,
            ncol = 7)
  
  ggsave("Figures/last version/slides/excess_by_age_2020_2021.png", dpi = 600,
         width = 13, height = 7.5)
}

{ # 2020
  pl_inf <- plot_age_x("Infant", 2020)
  pl_1_4 <- plot_age_x("1-4", 2020)
  pl_0_4 <- plot_age_x("0-4", 2020)
  pl_5_9 <- plot_age_x("5-9", 2020)
  pl_10_14 <- plot_age_x("10-14", 2020)
  pl_15_19 <- plot_age_x("15-19", 2020)
  pl_20_24 <- plot_age_x("20-24", 2020)
  
  plot_grid(pl_inf, pl_1_4, pl_0_4, pl_5_9, pl_10_14, pl_15_19, pl_20_24, 
            labels = c("Infant", "1-4", "0-4", "5-9", "10-14", "15-19", "20-24"),
            label_size = 14,
            label_x = 0.9, label_y = 0.99, hjust = 1,
            ncol = 7)
  
  ggsave("Figures/last version/slides/excess_by_age_2020.png", dpi = 600,
         width = 13, height = 7.5)
}

{ # 2021
  pl_inf <- plot_age_x("Infant", 2021)
  pl_1_4 <- plot_age_x("1-4", 2021)
  pl_0_4 <- plot_age_x("0-4", 2021)
  pl_5_9 <- plot_age_x("5-9", 2021)
  pl_10_14 <- plot_age_x("10-14", 2021)
  pl_15_19 <- plot_age_x("15-19", 2021)
  pl_20_24 <- plot_age_x("20-24", 2021)
  
  plot_grid(pl_inf, pl_1_4, pl_0_4, pl_5_9, pl_10_14, pl_15_19, pl_20_24, 
            labels = c("Infant", "1-4", "0-4", "5-9", "10-14", "15-19", "20-24"),
            label_size = 14,
            label_x = 0.9, label_y = 0.99, hjust = 1,
            ncol = 7)
  
  ggsave("Figures/last version/slides/excess_by_age_2021.png", dpi = 600,
         width = 13, height = 5.5)
}

{ # 15-19 in 2020 and 2021
  pl_15_19_2020 <- plot_age_x("15-19", 2020)
  pl_15_19_2021 <- plot_age_x("15-19", 2021)
  plot_grid(pl_15_19_2020, pl_15_19_2021, 
            labels = c("2020", "2021"),
            label_size = 14,
            label_x = 0.9, label_y = 0.99, hjust = 1,
            ncol = 2)
  
  ggsave("Figures/last version/slides/excess_ages_15_19.png", dpi = 600,
         width = 4, height = 5.5)
}

{ # 20-24 in 2020 and 2021
  pl_20_24_2020 <- plot_age_x("20-24", 2020)
  pl_20_24_2021 <- plot_age_x("20-24", 2021)
  plot_grid(pl_20_24_2020, pl_20_24_2021, 
            labels = c("2020", "2021"),
            label_size = 14,
            label_x = 0.9, label_y = 0.99, hjust = 1,
            ncol = 2)
  
  ggsave("Figures/last version/slides/excess_ages_20_24.png", dpi = 600,
         width = 4, height = 5.5)
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bts_rts <- 
  all_fit %>% 
  filter(Year %in% 2020:2021,
         Sex == "t",
         is.na(Deaths),
         Age %in% c("Infant", "0-4")) %>% 
  select(Country, Year) %>% unique()

# countries with data on births
birth_counts_cts <- 
  read_rds("data_inter/annual_births.rds") %>% 
  filter(Year >= 2020) %>% 
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
dts_inf <- 
  all_fit %>% 
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

lvs_cts <-
  dts_inf %>%
  filter(Year == 2020,
         Age == "Infant") %>%
  arrange(-psc) %>%
  pull(Country2) %>%
  unique()

tx <- 10
cols <- c("Positive" = "#b7094c",
          "Negative" = "#0091ad",
          "No-excess" = "#5c4d7d")

dts_inf2 <- 
  dts_inf %>% 
  filter(Country2 %in% lvs_cts) %>%
  mutate(Country2 = factor(Country2, levels = lvs_cts))

bks <- c(0.3, 1, 3)
lbs <- paste0((bks - 1)*100, "%")

plot_inf <- 
  dts_inf2 %>% 
  ggplot()+
  geom_point(aes(psc, Country2, 
                 alpha = out, 
                 col = exc,
                 shape = Year),
             size = 2)+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  facet_nested(Income ~ Age, scales = "free", space = "free_y")+
  scale_x_log10(breaks = bks,
                labels = lbs,
                limits = c(0.2, 5))+
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  guides(shape = guide_legend(override.aes = list(size = 3)))+
  labs(y = "Country", col = "Excess", x = "P-score")+
  theme_bw()+
  theme(legend.position = "none",
        legend.text = element_text(size = tx - 1),
        legend.title = element_text(size = tx - 1),
        # strip.text.y = element_text(size = tx,
        #                             margin = margin(b = .3, t = 0.3)),
        strip.text.y = element_blank(),
        strip.text.x = element_text(size = tx + 3,
                                    margin = margin(b = 1, t = 1)),
        panel.spacing.y = unit(0,"lines"),
        axis.text.x = element_text(size = tx - 1),
        axis.title.x = element_text(size = tx - 2),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = tx - 3))

# Plotting all ages for countries *without* infant mortality
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lvs_cts2 <- 
  all_fit %>% 
  filter(Year == 2020,
         Age == "0-4",
         Sex == "t") %>% 
  arrange(-psc) %>% 
  pull(Country) %>% 
  unique()

dts_yng <- 
  all_fit %>% 
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

bks <- c(0.3, 1, 3)
lbs <- paste0((bks - 1)*100, "%")

plot_yng <- 
  dts_yng %>% 
  ggplot()+
  geom_point(aes(psc, Country, 
                 alpha = out, 
                 col = exc,
                 shape = Year), size = 2)+
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  facet_nested(Income ~ Age, scales = "free", space = "free_y")+
  scale_x_log10(breaks = bks,
                labels = lbs,
                limits = c(0.2, 5))+
  scale_alpha_continuous(range = c(0.3, 0.8), guide = "none")+
  scale_color_manual(values = cols, guide = "none")+
  labs(x = "P-score")+
  theme_bw()+
  theme(legend.position = "none",
        legend.text = element_text(size = tx + 2),
        legend.title = element_text(size = tx + 1),
        strip.text.y = element_text(size = tx + 3,
                                    margin = margin(5,5,5,5),
                                    face = "bold"),
        strip.text.x = element_text(size = tx + 3,
                                    margin = margin(b = 1, t = 1)),
        panel.spacing.y = unit(0,"lines"),
        axis.text.x = element_text(size = tx - 1),
        axis.title.x = element_text(size = tx - 2),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = tx - 2))

plot_grid(plot_inf, plot_yng, 
          rel_widths = c(3/10, 7/10),
          ncol = 2)

ggsave(paste0("Figures/last version/slides/excess_by_age_country_names.png"),
       dpi = 600, width = 15, height = 9.5)





# distributions