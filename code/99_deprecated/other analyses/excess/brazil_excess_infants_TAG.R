library(here)
source(here("Code", "00_functions.R"))

region_bsln <- read_rds("Output/excess/brazil_monthly_excess_neonatal_region.rds")


# plots
# =====

# regions
# ~~~~~~~

# counts
region_bsln %>%
  mutate(type_excess = case_when(
    date >= "2020-03-15" & dts < ll ~ "Negative",
    date >= "2020-03-15" & dts > ul ~ "Positive",
    TRUE ~ "No-excess"),
         age = factor(age, levels = c("neo_day", "neo_week", "neo_month", "neo"))) %>% 
  filter(age == "neo") %>%
  ggplot()+
  geom_point(aes(date, dts, col = type_excess), size = 1)+
  geom_ribbon(aes(date, ymin = ll, ymax = ul), alpha = 0.3,
              fill = "#118ab2")+
  geom_line(aes(date, bsn), col = "#118ab2", alpha = 0.7)+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed", 
             col = "black",
             alpha = 0.5)+
  scale_x_date(limits = c(ymd("2015-01-01"), ymd("2020-12-31")))+
  scale_color_manual(values = c("blue", "black", "red"))+
  facet_grid(region ~ ., scales = "free")+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 10)
  )
ggsave("Figures/TAG_pres/brazil_regional_neonatal_deaths.png", dpi = 1000, 
       width = 10, height = 5)


p_score_region <- 
  region_bsln %>% 
  filter(date >= "2020-01-01" & date <= "2020-12-31",
         age == "neo") %>% 
  group_by(region) %>% 
  summarise(dts = sum(dts),
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(p_score = dts / bsn)






# p-scores
region_bsln %>% 
  filter(age == "neo") %>% 
  mutate(out = ifelse(p_score_un != 1, "1", "0"),
         age = factor(age, levels = c("neo_day", "neo_week", "neo_month", "neo"))) %>% 
  ggplot()+
  geom_point(aes(date, p_score, col = out), size = 1, alpha = 0.8)+
  facet_grid(age ~ region, scales = "free")+
  geom_hline(yintercept = 1, linetype = "dashed", col = "#118ab2")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed", 
             col = "black",
             alpha = 0.5)+
  scale_color_manual(values = c("black", "#ef476f"))+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 10)
  )

ggsave("Figures/excess/excess_regional_neonatal_p_scores.png", dpi = 1000, 
       width = 8, height = 2.5)
