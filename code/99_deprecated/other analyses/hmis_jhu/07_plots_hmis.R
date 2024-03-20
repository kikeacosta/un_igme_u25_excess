source("Code/00_functions.R")

cts_lvs <- 
  c("Ethiopia",
    "Kenya", 
    "Uganda", 
    "Burundi", 
    "Zambia", 
    "Malawi", 
    "Zimbabwe",
    "Mozambique",
    "Eswatini", 
    "Madagascar",
    "Bangladesh",
    "India")

all_cts <- 
  read_rds("Output/hmis_baselines_all_countries.rds") %>% 
  mutate(country = factor(country, levels = cts_lvs))
  

unique(all_cts$measure)
unique(all_cts$country)


# ==========================

plot_this <- function(ms, excl, d_ini, d_end, nc){
  all_cts %>% 
    filter(measure %in% ms,
           !country %in% excl) %>% 
    filter(date >= d_ini & date <= d_end) %>% 
    mutate(type_excess = case_when(
      date >= "2020-03-15" & value < bsn_gam_l ~ "Negative",
      date >= "2020-03-15" & value > bsn_gam_u ~ "Positive",
      TRUE ~ "No-excess")) %>% 
    ggplot()+
    # geom_line(aes(date, value), alpha = 0.5)+
    geom_line(aes(date, bsn_gam), alpha = 0.5, col = "#118ab2")+
    geom_ribbon(aes(date, ymin = bsn_gam_l, ymax = bsn_gam_u), alpha = 0.3,
                fill = "#118ab2")+
    geom_point(aes(date, value, col = type_excess), size = 1)+
    facet_wrap(country~., scales = "free_y", ncol = nc)+
    geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed",
               col = "red")+
    scale_x_date(date_breaks = "4 months", date_labels = "%b%y")+
    scale_color_manual(values = c("blue", "black", "red"))+
    labs(y = "counts", subtitle = ms)+
    theme_bw()+
    theme(
      legend.position = "right",
      plot.subtitle = element_text(size = 9),
      axis.text.x = element_text(size = 7),
      axis.text.y = element_text(size = 5),
      strip.text = element_text(margin = margin(b = 0.4, t = 0.4),
                                size = 8) 
    )
  ggsave(paste0("Figures/last version/hmis/", 
                ms, "_",d_ini, "_", d_end,".png"), 
         dpi = 600,
         height = 5,
         width = 9)
    
}



# monthly values
# ~~~~~~~~~~~~~~

# counts
# ~~~~~~

# infants
plot_this("inf", 
          excl = "Mozambique",
          d_ini = "2017-01-01",
          d_end = "2020-12-31",
          1)

# children
plot_this("0_4", 
          excl = "",
          d_ini = "2018-01-01",
          d_end = "2020-12-31",
          1)

plot_this("0_4", 
          excl = "",
          d_ini = "2020-01-01",
          d_end = "2020-12-31",
          2)

# maternal
plot_this("mat", 
          excl = "",
          d_ini = "2018-01-01",
          d_end = "2020-12-31",
          1)

# maternal
plot_this("mat", 
          excl = "",
          d_ini = "2020-01-01",
          d_end = "2020-12-31",
          2)


# =====
# rates
# ~~~~~~
# neonatal rates
plot_this("neo_r", 
          excl = "",
          d_ini = "2018-01-01",
          d_end = "2020-12-31",
          1)

# stillbirth rates
plot_this("sbs_r", 
          excl = "",
          d_ini = "2018-01-01",
          d_end = "2020-12-31",
          1)

# only 2020

# neonatal rates
plot_this("neo_r", 
          excl = "",
          d_ini = "2020-01-01",
          d_end = "2020-12-31",
          2)

# stillbirth rates
plot_this("sbs_r", 
          excl = "",
          d_ini = "2020-01-01",
          d_end = "2020-12-31",
          2)




# Mozambique
# child and young mortality
all_cts %>% 
  filter(measure %in% c("inf", "0_4","5_9","10_14","15_19","20_24")) %>% 
  filter(date >= "2017-01-01" & date <= "2021-12-31") %>% 
  filter(country == "Mozambique") %>% 
  mutate(type_excess = case_when(
    date >= "2020-03-15" & value < bsn_glm_l ~ "Negative",
    date >= "2020-03-15" & value > bsn_glm_u ~ "Positive",
    TRUE ~ "No-excess")) %>% 
  mutate(measure = factor(measure, 
                          levels = c("inf", "0_4","5_9",
                                     "10_14","15_19","20_24"))) %>% 
  ggplot()+
  geom_line(aes(date, bsn_glm), alpha = 0.7, col = "#118ab2")+
  geom_ribbon(aes(date, ymin = bsn_glm_l, ymax = bsn_glm_u), alpha = 0.3,
              fill = "#118ab2")+
  geom_point(aes(date, value, col = type_excess), size = 1)+
  scale_color_manual(values = c("blue", "black", "red"))+
  facet_grid(measure~., scales = "free")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed",
             col = "red")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b%y")+
  labs(y = "counts", subtitle = "Mozambique Child and young deaths")+
  theme_bw()+
  theme(
    legend.position = "right",
    plot.subtitle = element_text(size = 9),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0.6, t = 0.6),
                              size = 9) 
  )
ggsave("Figures/last version/hmis/mozambique_child_young.png", dpi = 600,
       height = 5,
       width = 9)




# ======





# annual rates ====
# ~~~~~~~~~~~~~~~~~
annual_r <- 
  all_cts %>% 
  # filter(measure %in% c("inf", "bts")) %>%
  filter(date < "2021-01-01") %>%
  filter(!(country == "Mozambique" & date < "2019-01-01")) %>% 
  mutate(year = year(date)) %>% 
  group_by(country, year, measure) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  spread(measure, value) %>% 
  mutate(inf_r = inf / bts,
         neo_r = neo / bts,
         sbs_r = sbs / (sbs + bts)) %>% 
  select(country, year, inf_r, neo_r, sbs_r, `0_4`) %>% 
  gather(-country, -year, key = measure, value = value) %>% 
  drop_na()

# annual infant rates
annual_r %>% 
  filter(measure == "inf_r") %>% 
  ggplot()+
  geom_point(aes(year, value))+
  facet_grid(country~., scales = "free")+
  theme_bw()

# annual neo rates
annual_r %>% 
  filter(measure == "neo_r",
         country != "Mozambique") %>% 
  ggplot()+
  geom_point(aes(year, value))+
  geom_line(aes(year, value))+
  facet_grid(country~., scales = "free")+
  theme_bw()

# annual sbs rates
annual_r %>% 
  filter(measure == "sbs_r",
         country != "Mozambique") %>% 
  ggplot()+
  geom_point(aes(year, value))+
  geom_line(aes(year, value))+
  facet_wrap(country~., scales = "free", ncol = 2)+
  theme_bw()



annual_r %>% 
  filter(measure %in% c("sbs_r", "neo_r"),
         country != "Mozambique") %>% 
  ggplot()+
  geom_point(aes(year, value))+
  geom_line(aes(year, value))+
  facet_grid(country~measure, scales = "free")+
  # facet_wrap(country~measure, scales = "free", ncol = 2)+
  theme_bw()+
  theme(    strip.text = element_text(margin = margin(b = 0.6, t = 0.6),
                                      size = 9) 
  )
