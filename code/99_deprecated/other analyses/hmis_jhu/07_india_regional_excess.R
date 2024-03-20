
library(here)
source(here("Code", "00_functions.R"))

# India
# =====

db <- 
  read_csv(here("Data", "HMIS", "HMIS_data_India_byState_2015_2021.csv")) %>% 
  rename(State = Region) %>% 
  mutate(State = recode(State,
                        "Andaman & Nicobar Islands" = "Andaman and Nicobar Islands",
                        "Dadra & Nagar Haveli" = "Dadra and Nagar Haveli and Daman and Diu",
                        "Daman & Diu" = "Dadra and Nagar Haveli and Daman and Diu",
                        "The Dadra And Nagar Haveli And Daman And Diu" = "Dadra and Nagar Haveli and Daman and Diu",
                        "Jammu And Kashmir" = "Jammu and Kashmir")) %>% 
  group_by(Indicator, Year, Month, State) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()

db_regs <- read_csv("Data/india_regions_states.csv") %>% 
  rename(State = State_Territory)

unique(db$State) %>% sort
unique(db_regs$State) %>% sort


db2 <- 
  db %>% 
  left_join(db_regs) %>% 
  mutate(Region = ifelse(State == "All India", "All India", Region)) %>% 
  group_by(Indicator, Year, Month, Region) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  filter(!Region %in% c("Arabian Sea", "Bay of Bengal"))


unique(db2$Indicator)
unique(db2$Region)
unique(db2$Year) %>% sort

db3 <- 
  db2 %>% 
  mutate(Indicator = recode(Indicator,
                            "Infant deaths" = "inf",
                            "Neonatal deaths" = "neo", 
                            "Stillbirths" = "sbs",
                            "Facility deliveries" = "dvs_fct",
                            "Home deliveries" = "dvs_home",
                            "Live birth" = "bts",
                            "Postneonatal deaths" = "pos",
                            "Child deaths age 1 to 4" = "1_4",
                            "Maternal deaths" = "mat",
                            "Under-five deaths" = "0_4",
                            "Deaths 1 month to 5 years" = "1m_4y"),
         date = make_date(d = 15, 
                          month = match(Month, month.abb), 
                          year = Year))

db4 <- 
  db3 %>%
  filter(Indicator %in% c("bts",
                          "sbs",
                          "neo")) %>% 
  spread(Indicator, value) %>% 
  drop_na()


db_neo <- 
  db4 %>% 
  rename(value = neo,
         exposure = bts) %>% 
  mutate(measure = "neo") %>% 
  select(-sbs)

db_sbs <- 
  db4 %>% 
  mutate(exposure = bts + sbs,
         measure = "sbs") %>% 
  rename(value = sbs) %>% 
  select(-bts, -neo)

db_to_fit <- 
  bind_rows(db_neo,
            db_sbs) %>% 
  mutate(month = match(Month, month.abb)) %>% 
  select(-Year, -Month) %>% 
  arrange(Region, measure, date) %>% 
  group_by(Region, measure) %>% 
  mutate(t = 1:n(),
         w = ifelse(date <= "2020-03-15", 1, 0)) %>% 
  ungroup()
  

est_baseline <- 
  function(chunk){
    
    chunk2 <- 
      chunk %>% 
      arrange(date) %>% 
      mutate(t = 1:n(),
             w = ifelse(date <= "2020-03-15", 1, 0),
             month = month(date))
    
    gam_model <- gam(value ~ t + s(month, bs = 'cp') + offset(log(exposure)),
                     weights = w,
                     data = chunk2,
                     family = "quasipoisson")
    
    gam_pred <- predict(gam_model, newdata = chunk2, type = "response", se.fit = TRUE)

    chunk %>% 
      bind_cols(tibble(bsn = gam_pred$fit,
                       bsn_u = bsn + 1.96*gam_pred$se.fit,
                       bsn_l = bsn - 1.96*gam_pred$se.fit))
  }

bslns_reg <- 
  db_to_fit %>% 
  group_by(Region, measure) %>% 
  do(est_baseline(chunk = .data)) %>% 
  mutate(type_excess = case_when(
    date >= "2020-03-15" & value < bsn_l ~ "Negative",
    date >= "2020-03-15" & value > bsn_u ~ "Positive",
    TRUE ~ "No-excess"),
    type_excess = factor(type_excess, 
                         levels = c("Negative", "No-excess", "Positive")))

bslns_reg %>% 
  filter(measure == "sbs") %>% 
  ggplot()+
  geom_line(aes(date, bsn), alpha = 0.5, col = "#118ab2")+
  geom_ribbon(aes(date, ymin = bsn_l, ymax = bsn_u), alpha = 0.3,
              fill = "#118ab2")+
  geom_point(aes(date, value, col = type_excess), size = 1)+
  facet_grid(Region~., scales = "free")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed",
             col = "red")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b%y")+
  scale_color_manual(values = c("blue", "black", "red"))+
  labs(y = "counts", subtitle = "Stillbirths")+
  theme_bw()+
  theme(
    plot.subtitle = element_text(size = 9),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0.4, t = 0.4),
                              size = 10) 
  )
ggsave("Figures/TAG_pres/hmis/india_excess_sbs.png", dpi = 600,
       height = 5,
       width = 10)




bslns_reg %>% 
  filter(measure == "neo") %>% 
  ggplot()+
  geom_line(aes(date, bsn), alpha = 0.5, col = "#118ab2")+
  geom_ribbon(aes(date, ymin = bsn_l, ymax = bsn_u), alpha = 0.3,
              fill = "#118ab2")+
  geom_point(aes(date, value, col = type_excess), size = 1)+
  facet_grid(Region~., scales = "free")+
  geom_vline(xintercept = ymd("2020-03-15"), linetype = "dashed",
             col = "red")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b%y")+
  scale_color_manual(values = c("blue", "black", "red"))+
  labs(y = "counts", subtitle = "Neonatal deaths")+
  theme_bw()+
  theme(
    # legend.position = "none",
    plot.subtitle = element_text(size = 9),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 5),
    strip.text = element_text(margin = margin(b = 0.4, t = 0.4),
                              size = 10) 
  )
ggsave("Figures/TAG_pres/hmis/india_excess_neo.png", dpi = 600,
       height = 5,
       width = 10)



all_p_scores <- 
  bslns_reg %>% 
  filter(date >= "2020-01-01" & date <= "2020-12-31") %>% 
  group_by(Region, measure) %>% 
  summarize(value = sum(value),
            bsn = sum(bsn)) %>% 
  ungroup() %>% 
  mutate(all_p_score = value / bsn) %>% 
  arrange(measure, Region)
