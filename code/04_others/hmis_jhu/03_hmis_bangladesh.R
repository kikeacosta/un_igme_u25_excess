library(here)
source(here("Code", "00_functions.R"))
library(readxl)
library(mgcv)

# Health Information System (HMIS) data for Bangladesh
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# data preparation
# ================

db_raw <- read_xlsx(here("Data", "HMIS", "HMIS_data_Bangladesh_2017_2021.xlsx"))
  
unique(db_raw$Source)
unique(db_raw$Indicator)
unique(db_raw$Year) %>% sort

# comparing both sources
# suggested to use "Community Server - Health Assistant Report", which should 
# include "Central Server - Monthly EmONC Report". However, Stillbirths are 
# systematically higher in the latter. 

db_har <- 
  db_raw %>% 
  filter(Source == "Community Server - Health Assistant Report") %>% 
  select(Indicator, Year, Month, value)

db_mer <- 
  db_raw %>% 
  filter(Source == "Central Server - Monthly EmONC Report") %>% 
  select(Indicator, Year, Month, val_mer = value)

db_compar <- 
  db_har %>% 
  rename(val_har = value) %>% 
  left_join(db_mer) %>% 
  mutate(diff = val_har / val_mer) %>% 
  drop_na() %>% 
  mutate(date = make_date(d = 15, m = match(Month, month.abb), y = Year)) 

# plot comparingg both sources 
db_compar %>% 
  gather(val_har, val_mer, key = source, value = value) %>% 
  ggplot()+
  geom_line(aes(date, value, col = source))+
  facet_wrap(~ Indicator, ncol = 1, scales = "free")+
  theme_bw()+
  theme(
    # legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text.x = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 8) 
  )
ggsave("Figures/HMIS/bangladesh_sources.png", 
       dpi = 600,
       width = 6,
       height = 6)


db_compar_annual <- 
  db_compar %>% 
  group_by(Indicator, Year) %>% 
  summarise(val_har = sum(val_har),
            val_mer = sum(val_mer)) %>% 
  ungroup() %>% 
  mutate(diff = val_har / val_mer)

db_max <- 
  db_har %>% 
  rename(val_har = value) %>% 
  left_join(db_mer) %>% 
  mutate(val_mer = ifelse(is.na(val_mer), 0, val_mer),
         value = ifelse(val_har > val_mer, val_har, val_mer)) %>% 
  select(-val_har, -val_mer)

db <- db_max

db2 <- 
  db %>% 
  mutate(Indicator = recode(Indicator,
                            "Stillbirths" = "stillbirths",
                            "Early neonatal deaths" = "neo_ear", 
                            "Late neonatal deaths" = "neo_lat", 
                            "Postneonatal deaths" = "post_neo",
                            "Child deaths age 1 to 4" = "child_1_4",
                            "Maternal deaths" = "maternal_deaths",
                            "Live birth" = "live_births",
                            "Total deliveries" = "deliveries",
                            "Infant deaths" = "infant",
                            "Neonatal deaths" = "neonatal", 
                            "Under-five deaths" = "child"),
         date = make_date(d = 15, 
                          month = match(Month, month.abb), 
                          year = Year))
  
unique(db2$Indicator)

db_annual <- 
  db2 %>% 
  group_by(Indicator, Year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  filter(Year <= 2020) %>% 
  mutate(date = make_date(d = 1, m = 7, y = Year))

table(db2$Indicator)
unique(db2$Indicator)
unique(db2$Year)
# ===========


# quick plots of counts
# =====================

plot_it <- function(db, inds){
  db %>% 
    filter(Indicator %in% inds) %>%  
    ggplot()+
    geom_point(aes(date, value), size = 0.5)+
    geom_line(aes(date, value), alpha = 0.8)+
    geom_vline(xintercept = c(ymd("2020-03-23"), ymd("2020-05-30")), 
               col = "red", alpha = 0.5)+
    facet_wrap(~Indicator, ncol = 1, scales = "free")+
    theme_bw()+
    theme(
      # legend.position = "none",
      # axis.text.y = element_blank(),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8),
      strip.text = element_text(margin = margin(b = 0, t = 0),
                                size = 11) 
    )
}

# births
plot_it(db_annual, c("deliveries", "live_births"))
plot_it(db2, c("deliveries", "live_births"))
ggsave("Figures/HMIS/bangladesh_births.png", 
       dpi = 600,
       width = 6,
       height = 3)

# stillbirths
plot_it(db_annual, c("stillbirths"))
plot_it(db2, c("stillbirths"))

# Neonatal deaths
plot_it(db2, c("neonatal", "neo_ear", "neo_lat", "post_neo"))
plot_it(db_annual, c("neonatal", "neo_ear", "neo_lat", "post_neo"))

# infant deaths
plot_it(db2, c("infant"))
plot_it(db_annual, c("infant"))

# ==========


# rates
# ========

db_rates <- 
  db_annual %>% 
  filter(Indicator %in% c("Infant deaths", 
                          "Neonatal deaths", 
                          "Stillbirths",
                          "Facility deliveries",
                          "Live birth",
                          "Postneonatal deaths")) %>% 
  mutate(Indicator = recode(Indicator,
                            "Infant deaths" = "infant",
                            "Neonatal deaths" = "neonatal", 
                            "Stillbirths" = "stillbirths",
                            "Facility deliveries" = "deliveries",
                            "Live birth" = "live_births",
                            "Postneonatal deaths" = "postneonatal")) %>% 
  spread(Indicator, value) %>% 
  mutate(neonatal_rate = neonatal / live_births,
         still_rate = stillbirths / (stillbirths + live_births),
         # deliveries are different from stillbirths + live_births!!
         test = stillbirths + live_births,
         is_2020 = ifelse(Year == 2020, "y", "n"),
         y2 = str_sub(Year, 3, 4))

db_rates %>% 
  ggplot()+
  geom_point(aes(y2, neonatal_rate, col = is_2020))+
  facet_wrap(~ Region, scales = "free")+
  scale_color_manual(values = c("black", "red"))

db_rates %>% 
  ggplot()+
  geom_point(aes(y2, still_rate, col = is_2020))+
  facet_wrap(~ Region, scales = "free")+
  scale_color_manual(values = c("black", "red"))

# maternal mortality
db_annual %>% 
  filter(Indicator == "Maternal deaths") %>% 
  ggplot()+
  geom_point(aes(Year, value))+
  facet_wrap(~ Region, scales = "free")


# stillbirths trends
# ~~~~~~~~~~~~~~~~~~

# Increase in stillbirths after lockdown measures in place
# In India, the lockdown started on 24 March 2020, and it was sustained
# until the end of May

db_still <- 
  db2 %>% 
  filter(Indicator %in% c("stillbirths",
                          "live_births")) %>% 
  spread(Indicator, value) %>% 
  mutate(exposure = stillbirths + live_births,
         still_rate = stillbirths / exposure,
         lockdown = ifelse(date >= ymd("2020-03-23") &
                             date <= ymd("2020-05-30"), "y", "n"),
         month = month(date)) %>% 
  arrange(date) %>% 
  mutate(t = 1:n(),
         t2 = t^2)

db_still %>% 
  ggplot()+
  geom_line(aes(date, still_rate), col = "black", alpha = 0.8)+
  geom_point(aes(date, still_rate, col = lockdown), size = 0.5)+
  geom_vline(xintercept = c(ymd("2020-03-23"), ymd("2020-05-30")), 
             col = "red", alpha = 0.5)+
  scale_color_manual(values = c("black", "red"))+
  theme_bw()+
  theme(
    legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 11) 
  )
ggsave("Figures/HMIS/bangladesh_monthly_stillbirths.png", 
       dpi = 600,
       width = 6,
       height = 2)



# infant and child mortality trends
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

unique(db2$Indicator)

db_neo <- 
  db2 %>% 
  filter(Indicator %in% c("live_births",
                          "neo_ear", 
                          "neo_lat", 
                          "post_neo",
                          "neonatal")) %>% 
  spread(Indicator, value) %>% 
  mutate(neonatal_r = neonatal / live_births,
         neo_ear_r = neo_ear / live_births,
         neo_lat_r = neo_lat / live_births,
         post_neo_r = post_neo / live_births) %>% 
  select(date, neonatal_r, neo_ear_r, neo_lat_r, post_neo_r) %>% 
  gather(-date, key = measure, value = rate) %>% 
  mutate(rate = rate * 1000,
         measure = factor(measure, levels = c("neo_ear_r", 
                                              "neo_lat_r",
                                              "neonatal_r", 
                                              "post_neo_r")))

db_neo %>% 
  ggplot()+
  geom_line(aes(date, rate), col = "black", alpha = 0.3)+
  geom_point(aes(date, rate), col = "black", size = 0.3)+
  facet_wrap(~ measure, scales = "free")+
  geom_vline(xintercept = c(ymd("2020-03-24"), ymd("2020-05-31")), 
             col = "red", alpha = 0.5)+
  theme_bw()+
  theme(
    legend.position = "none",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 11) 
  )
ggsave("Figures/HMIS/bangladesh_monthly_neonatal.png", 
       dpi = 600,
       width = 6,
       height = 4)


db2 %>%
  filter(Indicator %in% c("child_1_4", "child")) %>% 
  ggplot()+
  geom_line(aes(date, value, col = Indicator), alpha = 0.8)+
  geom_point(aes(date, value, col = Indicator), size = 0.3)+
  geom_vline(xintercept = c(ymd("2020-03-24"), ymd("2020-05-31")), 
             col = "red", alpha = 0.5)+
  scale_color_manual(values = c("black", "blue"))+
  theme_bw()+
  theme(
    legend.position = "right",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 11) 
  )
ggsave("Figures/HMIS/bangladesh_monthly_child.png", 
       dpi = 600,
       width = 8,
       height = 3)

db2 %>%
  filter(Indicator %in% c("child_1_4", "child")) %>% 
  ggplot()+
  geom_line(aes(date, value, col = Indicator), alpha = 0.8)+
  geom_point(aes(date, value, col = Indicator), size = 0.3)+
  geom_vline(xintercept = c(ymd("2020-03-24"), ymd("2020-05-31")), 
             col = "red", alpha = 0.5)+
  scale_color_manual(values = c("black", "blue"))+
  scale_y_continuous(limits = c(0, 4000))+
  theme_bw()+
  theme(
    legend.position = "right",
    # axis.text.y = element_blank(),
    axis.text = element_text(size = 8),
    strip.text = element_text(margin = margin(b = 0, t = 0),
                              size = 11) 
  )
ggsave("Figures/HMIS/bangladesh_monthly_child_adj.png", 
       dpi = 600,
       width = 8,
       height = 3)



