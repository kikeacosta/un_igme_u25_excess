library(here)
source(here("Code", "00_functions.R"))

region_bsln <- read_rds("Output/excess/brazil_monthly_excess_fetal_deaths_region.rds")

# plots
# =====

region_bsln %>% 
  filter(weeks == "28+") %>% 
  mutate(type_excess = case_when(
    date >= "2020-03-15" & fds < ll ~ "Negative",
    date >= "2020-03-15" & fds > ul ~ "Positive",
    TRUE ~ "No-excess")) %>% 
  ggplot()+
  geom_point(aes(date, fds, col = type_excess), size = 1)+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.3,
              fill = "#118ab2")+
  geom_line(aes(date, bsn), col = "#118ab2", alpha = 0.7)+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed", 
             col = "black",
             alpha = 0.5)+
  scale_color_manual(values = c("blue", "black", "red"))+
  facet_grid(region  ~ ., scales = "free")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 10)
  )

ggsave("Figures/TAG_pres/brazil_excess_regional_stillbirths.png", dpi = 1000, 
       width = 10, height = 5)


p_score_region <- 
  region_bsln %>% 
  filter(date >= "2020-01-01" & date <= "2020-12-31") %>% 
  group_by(region) %>% 
  summarise(fds = sum(fds),
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(p_score = fds / bsn)










