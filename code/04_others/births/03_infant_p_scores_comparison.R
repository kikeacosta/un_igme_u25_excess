library(here)
source(here("Code", "00_functions.R"))

exc_b <- 
  read_rds("Output/p_scores_excess_raw_data_infants_inc_births.rds")

exc_c <- 
  read_rds("Output/p_scores_excess_raw_data.rds")

cts <- 
  exc_b %>% 
  pull(Country) %>% 
  unique()

exc <- 
  exc_c %>% 
  filter(Country %in% cts,
         Age == "Infant") %>% 
  select(-Age) %>% 
  mutate(Model = "Only deaths") %>% 
  bind_rows(exc_b %>% 
              mutate(Model = "Including births")) %>% 
  mutate(is_2020 = ifelse(Year == 2020, "y", "n"))


tx <- 5
exc %>% 
  ggplot()+
  geom_point(aes(Year, Deaths, size = is_2020))+
  geom_ribbon(aes(Year, ymin = lp_baseline, ymax = up_baseline, 
                  fill = Model), alpha = 0.3)+
  geom_line(aes(Year, Baseline, col = Model))+
  scale_color_manual(values = c("#457b9d", "#e63946"))+
  scale_fill_manual(values = c("#457b9d", "#e63946"))+
  scale_size_manual(values = c(1, 1.5), guide = "none")+
  facet_wrap( ~ Country, scales = "free", ncol = 5)+
  theme_bw()+
  theme(legend.position = "bottom",
        strip.text.x = element_text(margin = margin(b = 1, t = 1),
                                    size = tx + 5),
        axis.text = element_text(size = tx + 1),
        axis.title = element_text(size = tx + 3),
        strip.background = element_rect(fill = "transparent"))

ggsave("Figures/excess_raw/comparing_fitting_including_births.png", dpi = 600)


exc %>% 
  filter(Year == 2020) %>% 
  ggplot()+
  geom_errorbar(aes(xmin = lp, xmax = up, y = Country, col = Model), 
                size = 0.8, width = 0.1, alpha = 0.6)+
  geom_point(aes(p_score, Country, col = Model), alpha = 0.9, size = 2.5)+
  # facet_wrap(~ Countr, scales = "free_x", nrow = 1)+
  scale_x_log10()+
  scale_y_discrete(position = "right") +
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.8, col = "black")+
  scale_color_manual(values = c("#457b9d", "#e63946"))+
  coord_flip()+
  labs(y = "Country", x = "P-score")+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text = element_text(size = tx + 8),
        axis.title = element_text(size = tx + 6))

ggsave(paste0("Figures/excess_raw/comparing_p_scores_including_births.png"), dpi = 600, width = 4, height = 4)

