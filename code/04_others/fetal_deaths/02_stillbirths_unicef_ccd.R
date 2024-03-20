# rm (list = ls())
source("Code/00_functions.R")

sbs_annual <- 
  read_rds("data_inter/neo_sbs_unicef.rds") %>% 
  arrange(country, year) %>%
  group_by(country) %>% 
  filter(any(year >= 2020)) %>% 
  mutate(t = 1:n(),
         w = ifelse(year <= 2019, 1, 0)) %>% 
  ungroup() %>% 
  mutate(exposure = sbs + bts)

# fitting baselines
# ~~~~~~~~~~~~~~~~~
fit_sbs <- function(chunk){
  
  model <- glm(sbs ~ t + offset(log(exposure)),
               data = chunk,
               weights = w,
               family = "quasipoisson")
  
  test <- try(pred <- predict(model, 
                              se.fit = TRUE,
                              type = "response"))
  
  try(out <- 
        chunk %>%
        mutate(bsn = pred$fit,
               ll = (pred$fit - 1.96 * pred$se.fit),
               ul = (pred$fit + 1.96 * pred$se.fit)))
  
  if(class(test) == "try-error"){
    out <- 
      chunk %>% 
      mutate(bsn = NA,
             ll = NA,
             ul = NA)
    
  }
  
  return(out)
}



bsn_sbs_annual <- 
  sbs_annual %>%  
  group_by(country) %>% 
  do(fit_sbs(chunk = .data)) %>% 
  ungroup()
  
exc_sbs_annual <- 
  bsn_sbs_annual %>% 
  mutate(exc = sbs - bsn,
         pscore = sbs / bsn,
         ps_ll = sbs / ul,
         ps_ul = sbs / ll)

exc_sbs_20 <-
  exc_sbs_annual %>%
  # filter(country != "Estonia") %>%
  filter(year >= 2020) %>% 
  mutate(excess = case_when(ps_ul < 1 ~ "Negative",
                            ps_ll > 1 ~ "Positive",
                            TRUE ~ "No-excess"))


cols <- c("Positive" = "#b7094c",
          "Negative" = "#0091ad",
          "No-excess" = "#5c4d7d")

cts_exc <- c("Andorra")

exc_sbs_20 %>% 
  filter(!country %in% cts_exc) %>% 
  ggplot(aes(pscore, reorder(country, pscore)))+
  geom_point(aes(pscore, reorder(country, pscore), 
                 size = bts,
                 col = excess,
                 shape = as.character(year)),
             alpha = 0.8)+
  # geom_pointrange(aes(xmin=ps_ll, xmax=ps_ul), alpha = 0.4)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  scale_x_log10(breaks = c(0.6, 0.8, 1, 1.2, 1.5, 2, 2.5))+
  scale_color_manual(values = cols)+
  # scale_size_continuous(guide = "none")+
  labs(y = "", title = "stillbirths (>28w) excess",
       shape = "Year",
       col = "Type",
       size = "Births")+
  theme_bw()+
  theme(legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        axis.text = element_text(size = 7))

ggsave(paste0("Figures/last version/excess_stillbirths.png"), 
       dpi = 600, width = 8, height = 3.5)


unique(exc_sbs_annual$country)

# exc_sbs_annual %>% 
#   filter(country %in% c("Kazakhstan", 
#                         "Slovakia",
#                         "Austria",
#                         "Lithuania")) %>%
#   ggplot()+
#   geom_point(aes(year, sbs))+
#   geom_line(aes(year, bsn))+
#   facet_wrap(~country, scales = "free")+
#   geom_ribbon(aes(year, ymin = ll, ymax = ul), alpha = 0.3)+
#   theme_bw()
# 
# ggsave(paste0("Figures/last version/excess_stillbirths_examples.png"), 
#        dpi = 600, width = 5, height = 3)
# 
# 
# exc_sbs_annual %>% 
#   filter(country %in% c("Kazakhstan", 
#                         "Slovakia",
#                         "Austria",
#                         "Lithuania")) %>%
#   mutate(sbs_r = sbs / exposure,
#          bsn_r = bsn / exposure,
#          ll_r = ll / exposure,
#          ul_r = ul / exposure) %>% 
#   ggplot()+
#   geom_point(aes(year, sbs_r))+
#   geom_line(aes(year, bsn_r))+
#   facet_wrap(~country, scales = "free")+
#   geom_ribbon(aes(year, ymin = ll_r, ymax = ul_r), alpha = 0.3)+
#   theme_bw()
# 
# ggsave(paste0("Figures/last version/excess_stillbirths_examples.png"), 
#        dpi = 600, width = 5, height = 3)

exc_sbs_20
exc_neo_20

test <- 
  exc_sbs_20 %>% 
  select(country, year, pscore_sbs = pscore) %>% 
  left_join(exc_neo_20 %>% 
              select(country, year, pscore_neo = pscore))

cts_exc <- c("Nauru", "Andorra")

test %>% 
  filter(!country %in% cts_exc) %>% 
  ggplot()+
  geom_point(aes(pscore_sbs, pscore_neo))+
  geom_smooth(aes(pscore_sbs, pscore_neo), method = lm, col = "#ae2012",
              fill = "#ae2012")+
  # geom_text(aes(pscore_sbs, pscore_neo, label = country), hjust = -0.2, size = 2)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  scale_y_log10()+
  scale_x_log10()+
  theme_bw()
ggsave("Figures/last version/stillbirths_neonatal_correlation.png", 
       dpi = 600, width = 3, height = 3)

test %>% 
  filter(!country %in% cts_exc) %>% 
  ggplot()+
  geom_point(aes(pscore_sbs, pscore_neo))+
  # geom_smooth(aes(pscore_sbs, pscore_neo), method = lm)+
  geom_text(aes(pscore_sbs, pscore_neo, label = country), hjust = -0.2, size = 2)+
  geom_vline(xintercept = 1, linetype = "dashed")+
  geom_hline(yintercept = 1, linetype = "dashed")+
  scale_y_log10()+
  scale_x_log10()+
  theme_bw()
ggsave("Figures/last version/stillbirths_neonatal_plot.png", 
       dpi = 600, width = 3, height = 3)

